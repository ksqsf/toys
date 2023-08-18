## This bot is mainly written by GitHub Copilot. I did very little work.
import os
import json
import logging
import asyncio
from telegram import ForceReply, Update
from telegram.ext import Application, CommandHandler, ContextTypes, MessageHandler, filters
import sqlite3
from dotenv import load_dotenv
import re

load_dotenv()
BOT_TOKEN = os.getenv('AVGI_BOT_TOKEN')
MY_ID = os.getenv('MY_ID')


# Enable logging
logging.basicConfig(
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s", level=logging.INFO
)
logger = logging.getLogger(__name__)

# Create a sqlite3 database with two tables
db = sqlite3.connect('logs.db')
db.execute('create table if not exists logs (id integer primary key, userid integer, username text, fullname text, chatid integer, chatname text, message text, time timestamp default current_timestamp)')
db.execute('create table if not exists chat_state (id integer primary key, chatid integer, auth_state text)')

async def message_handler(update: Update, context: ContextTypes.DEFAULT_TYPE) -> None:
    userid = update.message.from_user.id
    username = update.message.from_user.username
    fullname = update.message.from_user.full_name
    chatid = update.message.chat.id
    chatname = update.message.chat.title
    message = update.message.text
    print(f'收到消息 from {username} in {chatname}: {message[:10]}')
    a = db.execute('insert into logs (userid, username, fullname, chatid, chatname, message) values (?, ?, ?, ?, ?, ?)', (userid, username, fullname, chatid, chatname, message))
    db.commit()

async def start(update: Update, context: ContextTypes.DEFAULT_TYPE) -> None:
    await update.message.reply_text('彳亍')

async def check_search_ok(update: Update, context: ContextTypes.DEFAULT_TYPE) -> bool:
    is_group = update.message.chat.type == 'group' or update.message.chat.type == 'supergroup'
    if not is_group:
        return True

    # Check the auth_state
    chatid = update.message.chat.id
    senderid = update.message.from_user.id
    auth_state = db.execute('select auth_state from chat_state where chatid = ?', (str(chatid),)).fetchone()
    if auth_state:
        if auth_state[0] == 'member':
            return True
        elif auth_state[0] == 'admin':
            member = await update.message.chat.get_member(senderid)
            return member.status == 'administrator' or member.status == 'creator'
        else:
            return str(sender_id) == str(MY_ID)
    else:
        # No auth state yet, insert the default state 'admin'
        db.execute('insert into chat_state (chatid, auth_state) values (?, ?)', (str(chatid), 'admin'))
        db.commit()
        # Proceed as if auth_state is 'admin'
        member = await update.message.chat.get_member(senderid)
        return member.status == 'administrator' or member.status == 'creator'


async def search(update: Update, context: ContextTypes.DEFAULT_TYPE, order: str) -> None:
    ok = await check_search_ok(update, context)
    if not ok:
        return

    userid = update.message.from_user.id
    search_terms = update.message.text.lstrip('/__search').lstrip('@avgi_bot').split()
    if len(search_terms) == 0:
        return

    page = 0
    match_page_number = re.match(r'\d+', search_terms[0])
    if match_page_number:
        page = int(match_page_number.string)
        search_terms = search_terms[1:]
    search_term_query = '%' + '%'.join(search_terms) + '%'

    chatid = update.message.chat.id
    reply = ''
    cursor = db.execute('select chatname, username, fullname, message from logs where chatid = ? and message like ? order by time %s' % order, (str(chatid), search_term_query))
    for (chatname, username, fullname, message) in cursor.fetchall():
        reply += f'[{chatname}] {fullname}: {message}\n'
    if reply:
        await update.message.reply_text(reply[page*4096:(page+1)*4096])
    else:
        logging.info('empty response for search term ' + str(search_terms))


async def search_old_first(update: Update, context: ContextTypes.DEFAULT_TYPE) -> None:
    await search(update, context, 'asc')

async def search_new_first(update: Update, context: ContextTypes.DEFAULT_TYPE) -> None:
    await search(update, context, 'desc')

async def setsearch(update: Update, context: ContextTypes.DEFAULT_TYPE) -> None:
    chat_type = update.message.chat.type

    if chat_type != 'group' and chat_type != 'supergroup':
        await update.message.reply_text('只有群组需要设置')
        return

    chatid = update.message.chat.id
    senderid = update.message.from_user.id
    sender_can_set = False
    if str(senderid) == str(MY_ID):
        sender_can_set = True
    else:
        member = await update.message.chat.get_member(senderid)
        sender_is_admin = member.status == 'administrator' or member.status == 'creator'
        sender_can_set = sender_is_admin

    if not sender_can_set:
        await update.message.reply_text('你没有权限设置')
        return

    msg = update.message.text.split()
    if len(msg) != 2:
        await update.message.reply_text('格式：/setsearch [ admin | member ]')
        return

    auth_state = msg[1]
    if auth_state not in ['admin', 'member']:
        await update.message.reply_text('格式：/setsearch [ admin | member ]')
        return

    db.execute('insert or replace into chat_state (chatid, auth_state) values (?, ?)', (str(chatid), auth_state))
    db.commit()
    await update.message.reply_text('搜索权限已设置为 ' + auth_state)


def main() -> None:
    """Start the bot."""
    # Create the Application and pass it your bot's token.
    application = Application.builder().token(BOT_TOKEN).build()

    # on different commands - answer in Telegram
    application.add_handler(CommandHandler("start", start))
    application.add_handler(CommandHandler("__search", search_old_first))
    application.add_handler(CommandHandler("search__", search_new_first))
    application.add_handler(CommandHandler("setsearch", setsearch))

    # on non command i.e message
    application.add_handler(MessageHandler(filters.TEXT & ~filters.COMMAND, message_handler))

    # Run the bot until the user presses Ctrl-C
    application.run_polling()
if __name__ == "__main__":
    main()

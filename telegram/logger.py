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
ALWAYS_ALLOW_CHATS = [
    "-1001407473692"
]

# Enable logging
logging.basicConfig(
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s", level=logging.INFO
)
logger = logging.getLogger(__name__)

# Create a sqlite3 database with two tables
db = sqlite3.connect('logs.db')
db.execute('create table if not exists logs (id integer primary key, userid integer, username text, fullname text, chatid integer, chatname text, message text, messageid integer, time timestamp default current_timestamp)')
db.execute('create table if not exists chat_state (id integer primary key, chatid integer, auth_state text)')

async def new_message_handler(update: Update, context: ContextTypes.DEFAULT_TYPE) -> None:
    userid = update.message.from_user.id
    username = update.message.from_user.username
    fullname = update.message.from_user.full_name
    chatid = update.message.chat.id
    chatname = update.message.chat.title
    message = update.message.text
    messageid = update.message.id
    print(f'收到新消息 from {username} in {chatname}: {message[:10]}')
    db.execute('insert into logs (userid, username, fullname, chatid, chatname, message, messageid) values (?, ?, ?, ?, ?, ?, ?)', (userid, username, fullname, chatid, chatname, message, messageid))
    db.commit()

async def edited_message_handler(update: Update, context: ContextTypes.DEFAULT_TYPE) -> None:
    userid = update.edited_message.from_user.id
    username = update.edited_message.from_user.username
    fullname = update.edited_message.from_user.full_name
    chatid = update.edited_message.chat.id
    chatname = update.edited_message.chat.title
    message = update.edited_message.text
    messageid = update.edited_message.id
    print(f'{username} 编辑了消息 in {chatname}: {message[:10]}')
    db.execute('update logs set username = ?, fullname = ?, message = ? where chatid = ? and messageid = ?', (username, fullname, message, chatid, messageid))
    db.commit()

async def start(update: Update, context: ContextTypes.DEFAULT_TYPE) -> None:
    await update.message.reply_text('彳亍')

async def check_search_ok(update: Update, context: ContextTypes.DEFAULT_TYPE) -> bool:
    is_group = update.message.chat.type == 'group' or update.message.chat.type == 'supergroup'
    if not is_group:
        return True

    chatid = update.message.chat.id
    senderid = update.message.from_user.id
    if str(chatid) in ALWAYS_ALLOW_CHATS:
        return True
    # if str(senderid) == str(MY_ID):
    #     return True

    # Check the auth_state
    auth_state = db.execute('select auth_state from chat_state where chatid = ?', (str(chatid),)).fetchone()
    if auth_state:
        if auth_state[0] == 'member':
            return True
        elif auth_state[0] == 'admin':
            member = await update.message.chat.get_member(senderid)
            return member.status == 'administrator' or member.status == 'creator'
        else:
            return str(senderid) == str(MY_ID)
    else:
        # No auth state yet, insert the default state 'admin'
        db.execute('insert into chat_state (chatid, auth_state) values (?, ?)', (str(chatid), 'admin'))
        db.commit()
        # Proceed as if auth_state is 'admin'
        member = await update.message.chat.get_member(senderid)
        return member.status == 'administrator' or member.status == 'creator'


def remove_prefix(str, prefix):
    if str.startswith(prefix):
        return str[len(prefix):]
    else:
        return str


def message_link(chat, messageid) -> str:
    if chat.username:
        return f'https://t.me/{chat.username}/{messageid}'
    else:
        return f'https://t.me/c/{remove_prefix(str(chat.id), "-100")}/{messageid}'


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

    chat = update.message.chat
    chatid = chat.id
    reply = ''
    page_size = 20
    query = f'SELECT chatname, username, fullname, message, messageid FROM logs WHERE chatid = ? AND message like ? ORDER BY time {order} LIMIT {page_size} OFFSET ?'
    cursor = db.execute(query, (str(chatid), search_term_query, page*page_size))
    for (chatname, username, fullname, message, messageid) in cursor.fetchall():
        message = message.replace('<', '&lt;').replace('>', '&gt;')
        if messageid:
            reply += f'[{chatname}] {fullname}: <a href="{message_link(chat, messageid)}">⤴️</a> {message}\n'
        else:
            reply += f'[{chatname}] {fullname}: {message}\n'
    if reply:
        await update.message.reply_html(reply)
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
    application.add_handler(MessageHandler(filters.TEXT & ~filters.COMMAND & filters.UpdateType.MESSAGE, new_message_handler))
    application.add_handler(MessageHandler(filters.TEXT & ~filters.COMMAND & filters.UpdateType.EDITED, edited_message_handler))

    # Run the bot until the user presses Ctrl-C
    application.run_polling()
if __name__ == "__main__":
    main()

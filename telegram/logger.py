import os
import json
import logging
import asyncio
from telegram import ForceReply, Update
from telegram.ext import Application, CommandHandler, ContextTypes, MessageHandler, filters
import sqlite3
from dotenv import load_dotenv

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

async def search(update: Update, context: ContextTypes.DEFAULT_TYPE) -> None:
    userid = update.message.from_user.id
    if str(userid) != str(MY_ID):
        return
    search_term = update.message.text.lstrip('/__search').lstrip('@avgi_bot').strip()
    chatid = update.message.chat.id
    reply = ''
    cursor = db.execute('select chatname, username, message from logs where chatid = ? and message like ?', (str(chatid), "%" + search_term + "%"))
    for (chatname, username, message) in cursor.fetchall():
        reply += f'[{chatname}] {username}: {message}\n'
    if reply:
        await update.message.reply_text(reply[:4096])
    else:
        logging.info('empty response')

def main() -> None:
    """Start the bot."""
    # Create the Application and pass it your bot's token.
    application = Application.builder().token(BOT_TOKEN).build()

    # on different commands - answer in Telegram
    application.add_handler(CommandHandler("start", start))
    application.add_handler(CommandHandler("__search", search))

    # on non command i.e message
    application.add_handler(MessageHandler(filters.TEXT & ~filters.COMMAND, message_handler))

    # Run the bot until the user presses Ctrl-C
    application.run_polling()
if __name__ == "__main__":
    main()

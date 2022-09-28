import logging
import asyncio
from telegram import Update
from telegram.ext import ApplicationBuilder, ContextTypes, CommandHandler
import pandas as pd
import datetime
import pickle
import os
import numpy as np

logging.basicConfig(
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    level=logging.INFO
)

def load_transient():
    try:
        with open('looklookbook_transient.pickle') as f:
            result = pickle.load(f)
        os.remove('looklookbook_transient.pickle')
        return result
    except:
        return {}

def save_transient():
    if len(transient) == 0:
        return
    with open('looklookbook_transient.pickle', 'w') as f:
        pickle.dump(transient, f)

# 记录已经 checkin 但没有 checkout 的信息
transient = load_transient()

def read_record():
    df = pd.read_csv('looklookbook.csv', parse_dates=True, infer_datetime_format=True)
    df['CheckinTime'] = pd.to_datetime(df['CheckinTime'])
    df['CheckoutTime'] = pd.to_datetime(df['CheckoutTime'])
    df['Elapsed'] = pd.to_timedelta(df['Elapsed'])
    return df

def add_record(user_id, major, minor, checkin_time, checkout_time):
    df = pd.read_csv('looklookbook.csv', parse_dates=True)
    df.loc[len(df)] = user_id, major, minor, checkin_time, checkout_time, checkout_time - checkin_time
    df.to_csv('looklookbook.csv', index=False)

async def cmd_start(update: Update, context: ContextTypes.DEFAULT_TYPE):
    await context.bot.send_message(chat_id=update.effective_chat.id, text="我是机器人，跟我聊聊天吧！")

async def cmd_checkin(update: Update, context: ContextTypes.DEFAULT_TYPE):
    chat_id = update.effective_chat.id
    user_id = update['message']['from']['id']
    args_str = update['message']['text'].strip('/checkin')

    it = iter(args_str.split(','))
    major = next(it, '').strip()
    minor = next(it, '').strip()

    if major == '' or minor == '':
        await update.message.reply_text('请提供书名和章节号, 用法: /checkin 书名, 章节 (书名和章节名中不能有英文逗号)')
        return

    if user_id in transient:
        await update.message.reply_text('你之前已经 checkin 过了，请先 /abandon 或 /checkout')
        return
    else:
        transient[user_id] = (major, minor, pd.Timestamp.now())
        # await update.message.reply_text('OK')

async def cmd_abandon(update: Update, context: ContextTypes.DEFAULT_TYPE):
    user_id = update['message']['from']['id']
    if user_id in transient:
        del transient[user_id]
    #     await update.message.reply_text('完事儿')
    # else:
    #     await update.message.reply_text('不要停下来啊！！')

async def cmd_checkout(update: Update, context: ContextTypes.DEFAULT_TYPE):
    user_id = update['message']['from']['id']

    if user_id not in transient:
        await update.message.reply_text('还没开始计时呢')
        return

    major, minor, checkin_time = transient[user_id]
    checkout_time = pd.Timestamp.now()
    add_record(user_id, major, minor, checkin_time, checkout_time)
    del transient[user_id]
    # await update.message.reply_text('完事儿')

async def cmd_me(update: Update, context: ContextTypes.DEFAULT_TYPE):
    user_id = update['message']['from']['id']
    result = ''
    now = pd.Timestamp.now()

    if user_id in transient:
        major, minor, checkin_time = transient[user_id]
        result += f"当前正在阅读 {major} {minor}, 开始时间 {checkin_time}\n\n"

    df = read_record()
    df = df[df['UserID'] == user_id]
    result += f"共有 {len(df)} 条记录\n"
    result += f"总计时间 {df['Elapsed'].sum()}\n"
    if len(df) > 0:
        result += f"平均一次阅读时间 {df['Elapsed'].sum() / len(df)}\n"
    result += '\n'

    if len(df) > 10:
        result += '最后 5 条记录为\n'
        df = df[-5:]

    result += '<pre>'
    result += df.to_string(columns=['Major', 'Minor', 'Elapsed'])
    result += '</pre>'
    await update.message.reply_html(result.strip())

async def cmd_board(update: Update, context):
    df = read_record()
    df['Name'] = ''
    all_ids = list(df['UserID'])
    for id in all_ids:
        member = await update.effective_chat.get_member(id)
        if member.status == 'LEFT':
            continue
        user = member.user
        fullname = (user.first_name or '') + (user.last_name or '')
        username = user.username
        if fullname is not '':
            display = fullname
        elif username:
            display = username
        else:
            display = str(id)
        df['Name'] = np.where(df['UserID'] == id, display, df['Name'])
    df.sort_values('Elapsed', ascending=False)
    result = '排行榜前 10 名:\n<pre>'
    result += df.to_string(columns=['Name', 'Elapsed'])
    result += '</pre>'
    await update.message.reply_html(result.strip())

async def cmd_ping(update, context):
    await update.message.reply_text('我还活着!')

def main():
    # TOKEN
    app = ApplicationBuilder().token(TOKEN).build()

    app.add_handler(CommandHandler('start', cmd_start))
    app.add_handler(CommandHandler('checkin', cmd_checkin))
    app.add_handler(CommandHandler('checkout', cmd_checkout))
    app.add_handler(CommandHandler('abandon', cmd_abandon))
    app.add_handler(CommandHandler('me', cmd_me))
    app.add_handler(CommandHandler('board', cmd_board))
    app.add_handler(CommandHandler('ping', cmd_ping))

    app.run_polling()

if __name__ == '__main__':
    try:
        main()
    except KeyboardInterrupt:
        save_transient()

# looklookbook.py - A Telegram bot that keeps track of reading sessions

# Copyright (C) 2022-2023 ksqsf

# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation files
# (the "Software"), to deal in the Software without restriction,
# including without limitation the rights to use, copy, modify, merge,
# publish, distribute, sublicense, and/or sell copies of the Software,
# and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:

# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
# BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
# ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

import logging
import asyncio
import telegram
from telegram import Update
from telegram.ext import ApplicationBuilder, ContextTypes, CommandHandler
import pandas as pd
import datetime
import pickle
import os
import numpy as np
import threading
import math
from persistent_dict import persistent_dict

start_time = pd.Timestamp.now()

ADMIN_USER_ID = 256844732
CHAT_ID = -1001689552628  # 读书群 chat ID

logging.basicConfig(
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    level=logging.INFO
)

beicui = persistent_dict('beicui.pickle')  # user_id -> days
next_cui = persistent_dict('next_cui.pickle')  # user_id -> time

# user_id -> (flag, add_date, finish_date|None)
flags = persistent_dict('flags.pickle')

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
    with open('looklookbook_transient.pickle', 'wb') as f:
        pickle.dump(transient, f)

# 记录已经 checkin 但没有 checkout 的信息
transient = load_transient()

def read_record():
    df = pd.read_csv('.looklookbook.csv', parse_dates=True, infer_datetime_format=True)
    df['CheckinTime'] = pd.to_datetime(df['CheckinTime'])
    df['CheckoutTime'] = pd.to_datetime(df['CheckoutTime'])
    df['Elapsed'] = pd.to_timedelta(df['Elapsed'])
    return df

def add_record(user_id, major, minor, checkin_time, checkout_time):
    df = pd.read_csv('.looklookbook.csv', parse_dates=True)
    df.loc[len(df)] = user_id, major, minor, checkin_time, checkout_time, checkout_time - checkin_time
    df.to_csv('.looklookbook.csv', index=False)

async def cmd_start(update: Update, context: ContextTypes.DEFAULT_TYPE):
    await context.bot.send_message(chat_id=update.effective_chat.id, text="我是机器人，跟我聊聊天吧！")

async def cmd_checkin(update: Update, context: ContextTypes.DEFAULT_TYPE):
    chat_id = update.effective_chat.id
    user_id = update['message']['from']['id']
    args_str = update['message']['text'].lstrip('/checkin@looklookbook_bot').lstrip('/checkin')

    it = iter(args_str.split(','))
    major = next(it, '').strip()
    minor = next(it, '').strip()

    if major == '' and minor == '':
        await update.message.reply_text('请提供书名和章节号, 用法: /checkin 书名, 章节 (书名和章节名中不能有英文逗号)')
        return

    if user_id in transient:
        await update.message.reply_text('你之前已经 checkin 过了，请先 /abandon 或 /checkout')
        return
    else:
        transient[user_id] = (major, minor, pd.Timestamp.now())
        await update.message.reply_text('OK')

async def cmd_abandon(update: Update, context: ContextTypes.DEFAULT_TYPE):
    user_id = update['message']['from']['id']
    if user_id in transient:
        del transient[user_id]
        await update.message.reply_text('完事儿')
    else:
        await update.message.reply_text('不要停下来啊！！')

async def cmd_checkout(update: Update, context: ContextTypes.DEFAULT_TYPE):
    user_id = update['message']['from']['id']

    if user_id not in transient:
        await update.message.reply_text('还没开始计时呢')
        return

    major, minor, checkin_time = transient[user_id]
    checkout_time = pd.Timestamp.now()
    add_record(user_id, major, minor, checkin_time, checkout_time)
    del transient[user_id]
    await update.message.reply_text(f'读了 {checkout_time - checkin_time}, 常来玩~')

def stats_for_user(user):
    display = get_display_name(user.first_name, user.last_name, user.username, user.id)
    return stats_for_id(user.id, display)

def stats_for_id(user_id, display_name=''):
    result = display_name + ' '

    if user_id in transient:
        major, minor, checkin_time = transient[user_id]
        result += f"当前正在阅读 {major} {minor}, 开始时间 {checkin_time}\n"

    if user_id in beicui:
        result += f'TA 开启了每 {beicui[user_id]} 天催一次读书功能\n'

    result += '\n'

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

    return result.strip()


async def cmd_me(update: Update, context: ContextTypes.DEFAULT_TYPE):
    result = stats_for_user(update['message']['from'])
    await update.message.reply_html(result)

def get_display_name(first_name, last_name, username, user_id, escape_html=True):
    def f():
        fullname = ((first_name or '') + ' ' + (last_name or '')).strip()
        if fullname:
            return fullname
        if username:
            return username
        return str(user_id)
    res = f()
    if escape_html:
        return res.replace('<', '&lt;').replace('>', '&gt;')
    else:
        return res

async def display_name_of_id(bot, user_id):
    member = await bot.get_chat_member(CHAT_ID, user_id)
    return display_name_of_user(member)

def display_name_of_user(user):
    username = user.username
    first = user.first_name
    last = user.last_name
    reference = username or str(user.id)
    display = get_display_name(first, last, username, user_id)
    return display

async def at_link_of_id(bot, user_id):
    display = await display_name_of_id(bot, user_id)
    return f'<a href="tg://user?id={user_id}">{display}</a>'

async def cmd_board(update: Update, context):
    df = read_record()
    df = df.groupby('UserID').sum().sort_values('Elapsed', ascending=False).reset_index()
    df['Name'] = '(神秘人)'
    all_ids = list(df['UserID'])
    for id in all_ids:
        try:
            member = await update.effective_chat.get_member(id)
        except:
            print('Failed to get member:', id)
            continue
        if member.status == 'LEFT':
            continue
        user = member.user
        display = get_display_name(user.first_name, user.last_name, user.username, id)
        df['Name'] = np.where(df['UserID'] == id, display, df['Name'])
    n_mysterious = len(df[df['Name'] == '(神秘人)'])
    df = df[df['Name'] != '(神秘人)']
    result = '排行榜前 15 名:\n<pre>'
    result += df[:15].to_string(columns=['Name', 'Elapsed'])
    result += '</pre>\n'
    result += f'（有 {n_mysterious} 人因长期未与 bot 交流被自动除名，使用 /start 重回榜首吧！）'
    await update.message.reply_html(result.strip())

async def cmd_ping(update, context):
    date = update.message.date
    now = datetime.datetime.now(date.tzinfo)
    delta = now - date
    await update.message.reply_text(f'咱还活着呢！（气鼓鼓）\n你的消息发到咱这花了{delta}时间')

async def cmd_now(update, context):
    n = len(transient)
    if n == 0:
        result = '现在没人在读\n'
    else:
        result = f'现在有 {n} 人在读'
    for user_id, (major, minor, start_date) in transient.items():
        result += f'{await display_name_of_id(context.bot, user_id)} 在读 {major} {minor} ({pd.Timestamp.now() - start_date})\n'
    await update.message.reply_text(result.strip())

async def cmd_cancanneed(update, context):
    if not update['message']['reply_to_message']:
        await update.message.reply_text('把这个命令 *回复* 到想看的人的一条消息上就可以了哦~')
    else:
        result = stats_for_user(update['message']['reply_to_message']['from'])
        await update.message.reply_html(result)

async def cmd_uptime(update, context):
    await update.message.reply_text(f"uptime: {pd.Timestamp.now() - start_time}")

async def cmd_cuiwo(update, context):
    """
    每天催当前用户读书。
    """
    user_id = update['message']['from']['id']
    try:
        days = float(update['message']['text'].strip('/cuiwo').strip('@looklookbook_bot'))
        if math.isnan(days):
            await update.message.reply_text(f'你……太调皮了？！')
        elif days == 114514:
            await update.message.reply_text(f'1919810')
        elif days > 1000:
            await update.message.reply_text(f'你是不是在找 /biecuiwole')
        elif days < 0:
            await update.message.reply_text(f'诶诶？？咱不会时光穿越。。。')
        elif days < 0.1:
            await update.message.reply_text(f'想要累死咱吗？！')
        elif start_cui(user_id, days) == True:
            await update.message.reply_text(f'收到收到~ 以后每 {days:g} 天都会催你读一次书哦~')
        else:
            await update.message.reply_text('记录出错了 QAQ')
    except:
        await update.message.reply_text('请提供一个大于 0 的数字 n，在 n 天后，我会提醒你读书哦~')

async def cmd_biecuiwole(update, context):
    """
    取消催读书功能。
    """
    user_id = update['message']['from']['id']
    if cancel_cui(user_id) == True:
        await update.message.reply_text('咱不催你的日子里，你也要好好读书哇 QAQ')
    else:
        await update.message.reply_text('本来就没有在催你嘛 ?_?')

def start_cui(user_id, days):
    """
    True -> 成功添加
    False -> 出错了
    """
    global beicui
    try:
        beicui[user_id] = days
        # fix next_cui
        # we should rebase next_cui to be last_read_time + beicui[user_id]
        df = read_record()[['UserID', 'CheckoutTime']].groupby('UserID').max()
        last_read_time = df.loc[user_id]['CheckoutTime']
        next_cui[user_id] = last_read_time + pd.Timedelta(days=beicui[user_id])
        return True
    except Exception as e:
        logging.error('加催时有点问题', e)
        return False

def cancel_cui(user_id):
    global beicui
    try:
        if user_id not in beicui:
            return False
        del beicui[user_id]
        return True
    except Exception as e:
        logging.warning('Error during cancel_cui:', str(e))
        return False

def human_readable_timedelta(delta):
    if delta.days > 0:
        return f'{delta.days} 天'
    secs = delta.seconds
    hours = secs // 3600
    if hours > 0:
        return f'{hours} 小时'
    mins = secs // 60
    if mins > 0:
        return f'{mins} 分钟'
    return f'{secs} 秒'

async def do_cui(bot):
    first = {}
    df = read_record()[['UserID', 'CheckoutTime']].groupby('UserID').max()
    to_remove = []
    for user_id in beicui.keys():
        # logging.info(f'try cui for {user_id}')
        now = pd.Timestamp.now()
        try:
            last_read_time = df.loc[user_id]['CheckoutTime']
        except KeyError:
            last_read_time = now
        next_cui_time_ub = last_read_time + pd.Timedelta(days=beicui[user_id])
        next_cui_time_lb = next_cui.get(user_id, next_cui_time_ub)
        # print('now', now)
        # print('last_read_time', last_read_time)
        # print('next_cui_time', next_cui_time)
        if now > max(next_cui_time_ub, next_cui_time_lb):
            try:
                at_link = await at_link_of_id(bot, user_id)
            except Exception as e:
                to_remove.append(user_id)
                continue
            # send messages
            await bot.send_message(CHAT_ID, f'{at_link} 喂喂喂, 你已经连续 {human_readable_timedelta(now - last_read_time)} 没读过书啦!!', telegram.constants.ParseMode.HTML)
            # mark this time has been cui'd
            next_cui[user_id] = now + pd.Timedelta(days=beicui[user_id])
        else:
            # do nothing
            pass

    if to_remove:
        notify = '#bot 催以下用户时出现错误，已自动停止提醒功能: '
        for uid in to_remove:
            notify += f'<a href="tg://user?id={uid}">{uid}</a> '
            cancel_cui(user_id)
        await bot.send_message(CHAT_ID, notify, telegram.constants.ParseMode.HTML)

async def cmd_sudo_cui(update, context):
    if update['message']['from']['id'] == ADMIN_USER_ID:
        await do_cui(context.bot)
    else:
        await update.message.reply_text('permission denied')

async def cui_coro(bot):
    while True:
        try:
            await asyncio.sleep(5)
            logging.debug(f'trying to cui...')
            await do_cui(bot)
        except Exception as e:
            logging.warning('failed to cui', e)


async def cmd_help(update, context):
    await update.message.reply_html('''欢迎欢迎！咱可以助你多多读书哦！

<b>读书打卡功能</b>
    <b>/checkin</b>    开始计时
    <b>/checkout</b>   结束计时并记录本次打卡
    <b>/abandon</b>    放弃计时和本次打卡
<b>立旗功能</b>
    <b>/flag</b>       立下读完这本书的flag
    <b>/myflags</b>    看看自己的flag完成情况
    <b>/theirflags</b> 看看别人的flag完成情况
<b>内卷功能</b>
    <b>/board</b>      比一比谁读得更多！
    <b>/me</b>         看看自己的记录
    <b>/cancanneed</b> 看看别人的记录
<b>读书提醒功能</b>
    <b>/cuiwo</b>      催我读书
    <b>/biecuiwole</b> 别催我了
''')

async def cmd_flag(update, context):
    user_id = update['message']['from']['id']
    args_str = update['message']['text'].lstrip('/flag@looklookbook_bot').lstrip('/flag').strip()
    now = pd.Timestamp.now()

    if args_str.startswith('-clear'):
        try:
            del flags[user_id]
        except KeyError:
            pass
        await update.message.reply_text('已清空')
    elif len(args_str) > 0:  # add a new flag
        flag = args_str
        flag_list = flags.get(user_id, [])
        flag_list.append((flag, now, None))
        flags[user_id] = flag_list
        await update.message.reply_text('已添加, 用 /myflags 看看自己立了哪些 flag 吧')
    else:
        await update.message.reply_text('''请提供一个 flag

flag 使用方法:
/flag -rm <flag>   删除某个flag
/flag -done <flag> 完成flag
/flag -clear       清空当前所有flag''')

def format_flags(user_id, numbers=True):
    flag_list = flags.get(user_id, [])
    now = pd.Timestamp.now()
    result = ''
    ncomplete = 0
    ntotal = len(flag_list)
    for (flag, add_date, finish_date) in flag_list:
        if finish_date:
            result += f'- {flag}, 完成于{human_readable_timedelta(now-finish_date)}前\n'
            ncomplete += 1
        else:
            result += f'- {flag}, 未完成, 添加于 {human_readable_timedelta(now-add_date)}前\n'
    if numbers:
        return f'共{ntotal}个flag, {ncomplete}个已完成\n\n{result}'
    else:
        return result

async def cmd_myflags(update, context):
    user_id = update['message']['from']['id']
    await update.message.reply_text(format_flags(user_id))

async def cmd_theirflags(update, context):
    if not update['message']['reply_to_message']:
        await update.message.reply_text('把这个命令 *回复* 到想看的人的一条消息上就可以了哦~')
    else:
        try:
            result = format_flags(update['message']['reply_to_message']['from']['id'])
            await update.message.reply_text(result)
        except KeyError:
            await update.message.reply_text("他还没立过 flag")

def main():
    async def setup(app):
        await asyncio.sleep(5)
        asyncio.create_task(cui_coro(app.bot))

    # TOKEN
    app = ApplicationBuilder().post_init(setup).token(os.getenv("TOKEN")).build()

    app.add_handler(CommandHandler('start', cmd_start))
    app.add_handler(CommandHandler('checkin', cmd_checkin))
    app.add_handler(CommandHandler('checkout', cmd_checkout))
    app.add_handler(CommandHandler('abandon', cmd_abandon))
    app.add_handler(CommandHandler('me', cmd_me))
    app.add_handler(CommandHandler('board', cmd_board))
    app.add_handler(CommandHandler('ping', cmd_ping))
    app.add_handler(CommandHandler('now', cmd_now))
    app.add_handler(CommandHandler('cancanneed', cmd_cancanneed))
    app.add_handler(CommandHandler('uptime', cmd_uptime))
    app.add_handler(CommandHandler('cuiwo', cmd_cuiwo))
    app.add_handler(CommandHandler('biecuiwole', cmd_biecuiwole))
    app.add_handler(CommandHandler('sudo_cui', cmd_sudo_cui))
    app.add_handler(CommandHandler('help', cmd_help))
    app.add_handler(CommandHandler('flag', cmd_flag))
    app.add_handler(CommandHandler('myflags', cmd_myflags))
    app.add_handler(CommandHandler('theirflags', cmd_theirflags))
    # app.add_handler(CommandHandler('card', cmd_card))

    app.run_polling()


################################################################################
# Card Generation

async def cmd_card(update, context):
    user = update['message']['from']
    user_id = user['id']
    if user_id != ADMIN_USER_ID:
        await update.message.reply_text('该功能尚未开放')
        return
    bot = context.bot
    avatar = await get_profile_photo_url(bot, user)
    streak = calc_streak_days(user_id)
    today = calc_today_mins(user_id)
    total = calc_total_days(user_id)
    name = display_name_of_user(user)
    banner = ...
    date = ...
    try:
        generate_card(banner, avatar, name, date, today, streak, total, 'card.png')
        # TODO: send image card.png
    except:
        await update.message.reply_text('创建卡片失败')

def calc_today_mins(user_id):
    pass

def calc_streak_days(user_id):
    pass

def calc_total_days(user_id):
    pass

async def get_profile_photo_url(bot, user):
    pass

################################################################################
if __name__ == '__main__':
    try:
        main()
    except KeyboardInterrupt:
        save_transient()


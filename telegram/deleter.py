#!/usr/bin/env python3

import traceback
import asyncio
import sys
import logging
from dotenv import dotenv_values
from aiotdlib.api import SearchMessagesFilterEmpty, MessageSenderUser
from aiotdlib import Client
from datetime import datetime

cfg = dotenv_values()
chat_id = cfg['CHAT_ID']

async def print_msg(client, msg, f):
    if msg.sender_id.ID == 'messageSenderUser':
        sender_id = msg.sender_id.user_id
        sender = await client.api.get_user(sender_id)
        fullname = sender.first_name + ' ' + sender.last_name
    elif msg.sender_id.ID == 'messageSenderChat':
        sender_id = msg.sender_id.chat_id
        sender = await client.api.get_chat(sender_id)
        fullname = sender.title
    else:
        fullname = msg.sender_id.ID
        
    suffix = f'id={msg.id}, date={datetime.fromtimestamp(msg.date)}'
    if msg.content.ID == 'messageText':
        print(f'{fullname}: {msg.content.text.text} ({suffix})', file=f)
    elif msg.content.ID == 'messageSticker':
        print(f'{fullname}: <sticker {msg.content.sticker.emoji}> ({suffix})', file=f)
    else:
        print(f'{fullname}: <{msg.content.ID}> ({suffix})', file=f)
    f.flush()


async def main(chat_id):
    client = Client(
        api_id=cfg['API_ID'],
        api_hash=cfg['API_HASH'],
        phone_number=cfg['PHONE'],
    )
    async with client:
        me = await client.api.get_me()
        my_id = me.id
        username = me.username
        logging.info(f'Successfully logged in as @{username} (id={my_id})')
        batch = await client.api.search_chat_messages(
            chat_id=chat_id,
            query='',
            from_message_id=0,
            limit=1,
            filter_=SearchMessagesFilterEmpty(),
            message_thread_id=0,
            offset=0,
            sender_id=MessageSenderUser(user_id=my_id),
            request_timeout=100,
        )

        latest_message = batch.messages[0]
        cur_id = latest_message.id
        while True:
            batch = await client.api.search_chat_messages(
                chat_id=chat_id,
                query='',
                from_message_id=cur_id,
                limit=100,
                filter_=SearchMessagesFilterEmpty(),
                message_thread_id=0,
                offset=0,
                sender_id=MessageSenderUser(user_id=my_id),
                request_timeout=30
            )

            if batch.total_count == 0:
                break
            for msg in batch.messages:
                cur_id = msg.id
                if msg.can_be_deleted_for_all_users:
                    await print_msg(client, msg, f=sys.stdout)
                    await client.api.delete_messages(chat_id=chat_id, message_ids=[msg.id], revoke=True, request_timeout=30)

logging.basicConfig(level=logging.INFO)
def run():
    try:
        asyncio.run(main(chat_id))
    except:
        print('Exception raised during deleting messages of chat_id ', chat_id)
        traceback.print_exc()

if __name__ == '__main__':
    run()

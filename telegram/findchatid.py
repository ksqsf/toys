#!/usr/bin/env python3
import asyncio
import logging
from dotenv import dotenv_values
from aiotdlib import Client
from aiotdlib.api import API, BaseObject, UpdateNewMessage

cfg = dotenv_values()

async def on_update_new_message(client: Client, update: UpdateNewMessage):
    chat_id = update.message.chat_id
    chat = await client.api.get_chat(chat_id)
    print(f'{chat.title} -> {chat_id}')

async def fuck():
    print('Signing in...')
    client = Client(
        api_id=cfg['API_ID'],
        api_hash=cfg['API_HASH'],
        phone_number=cfg['PHONE']
    )
    print('Waiting for updates...')
    client.add_event_handler(on_update_new_message, update_type=API.Types.UPDATE_NEW_MESSAGE)
    async with client:
        await client.idle()

print('Starting...')
asyncio.run(fuck())

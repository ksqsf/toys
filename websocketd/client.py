#!/usr/bin/env python3

# This script requires 'websocket-client'.
from websocket import create_connection
import time

ws = create_connection("ws://127.0.0.1:54321/")
print('send: Hello world')
ws.send('Hello world\n')
print('send ok')
result = ws.recv()
print('recv:', result[:-1])
ws.close()

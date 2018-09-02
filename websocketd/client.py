#!/usr/bin/env python3

# This script requires 'websocket-client'.
from websocket import create_connection
import time

print('connect')
ws = create_connection("ws://127.0.0.1:54321/")
print('connect ok')

print('send: Hello world')
ws.send('Hello world\n')
print('send ok')

print('recving')
result = ws.recv()
print('recv ok:', result[:-1])

print('closing')
ws.close()
print('closed')

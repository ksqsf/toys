#!/usr/bin/env python3

# This script requires 'websocket-client'.
from websocket import create_connection
import time
import threading

def test1():
    ws = create_connection("ws://127.0.0.1:54321/")
    print('connect ok')

    ws.send('Hello world\n')
    print('send ok')

    result = ws.recv()
    print('recv ok:', result[:-1])

    ws.close()
    print('close ok')

def test(num_tests):
    for i in range(0, num_tests):
        test1()

for worker_id in range(0, 4):
    t = threading.Thread(target=test, args=(1000,))
    t.start()

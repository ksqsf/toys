#!/usr/bin/env python3

# This script requires 'websocket-client'.
from websocket import create_connection
import sys
import time
import multiprocessing

# After each test, elapsed time is reported in second
NUM_TESTS = 1

# How many threads working at the same time
NUM_THREADS = 2

# How many connections one thread initiates
NUM_WORK = 2000


def test1(data):
    ws = create_connection("ws://127.0.0.1:54321/")
    print('connect ok')

    ws.send(data + '\n')
    print('send ok')

    result = ws.recv()
    print('recv ok:', result[:-1])

    ws.close()
    print('close ok')

    return result


def thread(num_tests):
    for i in range(0, num_tests):
        x = test1(str(i))
        x = int(x.strip())
        if x != i*3:
            blow_up()


def test():
    workers = []
    for worker_id in range(0, NUM_THREADS):
        p = multiprocessing.Process(target=thread, args=(NUM_WORK,))
        p.start()
        workers.append(p)
    for worker in workers:
        worker.join()


for _ in range(0, NUM_TESTS):
    start = time.time()
    test()
    end = time.time()
    print(end-start, file=sys.stderr)

#!/usr/bin/env python3

try:
    while True:
        line = input()
        print(line)
except EOFError:
    pass
except KeyboardInterrupt:
    pass


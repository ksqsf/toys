# 二码虎码码表处理脚本

from table import Table

tc = Table('/tmp/tiger_chars.txt', code_first=False)
chars = list(tc.t2c.keys())
with open('/tmp/2tiger.txt', 'w') as f:
    for c in chars:
        fullcode = tc.t2c[c][-1]
        print(f'{c}\t{fullcode[:2]}', file=f)

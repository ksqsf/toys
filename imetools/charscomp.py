# 尋到含有某個部件的所有 Unicode 已编码的漢字.

import requests
import json
from table import Table

CHAR = '垂'

res = requests.get('https://zi.tools/api/zi/' + CHAR).json()
rel = json.loads(res['relatives'])

# ['var', 'iss', 'isp', 'pho']
# var:

def convert(input):
    res = set()
    for [x, ys, *_] in input:
        res.add(x)
        for y in ys:
            res.add(y)
    return res

s = set(rel['var'])
s = s.union(convert(rel['iss']))
s = s.union(convert(rel['isp']))
s = s.union(convert(rel['pho']))

wb = Table('98wbU.dict.yaml', rime=1)

for ch in s:
    try:
        print('%s\t%s' % (ch, wb.t2c[ch][-1]))
    except KeyError:
        print('%s' % ch)

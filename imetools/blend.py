from table import *

tiger = Table('tiger_full.txt', code_first=True)
zrm = Table('/tmp/rime-zrm/zrm_pinyin.dict.yaml', code_first=False)

with open('/tmp/虾码.txt', 'w') as f:
    for char, zcode in zrm.t2c.items():
        try:
            tcode = tiger.t2c[char][0]
        except KeyError:
            print('Char not found', char)
            continue
        xcode = zcode[0][0] + tcode[:1] + tcode[-1]
        print(f'{char}\t{xcode}', file=f)

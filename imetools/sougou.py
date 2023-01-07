# 生成搜狗用的自定义短语格式
from table import Table

tub = open('tub10000.txt').read()
tiger = Table('tiger.txt', rime=True, charset=tub)

with open('再次更新.ini', 'w', encoding='utf-16') as f:
    for code, texts in tiger.c2t.items():
        for i, text in enumerate(texts):
            print(f'{code},{i+1}={text}', file=f, end='\r\n')

"""
生成长流跟打器可以用的词提文件。
"""

from table import Table

tiger = Table('/tmp/tiger.txt', code_first=False, only_chars=False)
wb = Table('/tmp/98wb.txt', code_first=False, only_chars=False)

def pad_code(code, number, max_len) -> str:
    if number == 1 and len(code) < max_len:
        return code + '_'
    elif number == 1 and len(code) == max_len:
        return code
    elif number > 1:
        return code + str(number)

def convert(table: Table, output_path: str = '/tmp/词组提示码表.txt'):
    with open(output_path, 'w') as f:
        codes = list(table.c2t.keys())
        for code in codes:
            texts = table.c2t[code]
            for i, text in enumerate(texts):
                # print(f'{text}\t{pad_code(code, i+1, table.max_len)}')
                print(f'{text}\t{pad_code(code, i+1, table.max_len)}', file=f)

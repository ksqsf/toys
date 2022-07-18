from typing import List, Tuple
import pandas


class Table:
    def __init__(self, name, maxlen=None):
        self.name = name
        self.maxlen = maxlen
        self.t2c = dict()
        self.c2t = dict()

    def add_code(self, text, code):
        if text not in self.t2c:
            self.t2c[text] = []
        self.t2c[text].append(code)
        if code not in self.c2t:
            self.c2t[code] = []
        self.c2t[code].append(text)

    def lookup_shortest(self, text):
        s = self.t2c[text]
        s.sort(key=len)
        return s[0]

    @staticmethod
    def load_from_file(file, name, maxlen=None, reversed=False):
        table = Table(name, maxlen)
        with open(file) as f:
            for line in f:
                if not reversed:
                    code, word = line.split()
                else:
                    word, code = line.split()
                table.add_code(word, code)
        return table

    @staticmethod
    def load_from_csv(file, name, maxlen=None):
        import csv
        table = Table(name, maxlen)
        with open(file) as f:
            rdr = csv.reader(f)
            next(rdr)
            for [item, _, code] in rdr:
                table.add_code(item, code)
        return table

    def chars2code(self, chars: str, method='SHORTEST') -> pandas.DataFrame:
        ret = []
        for c in chars:
            if method == 'SHORTEST':
                code = self.lookup_shortest(c)
            elif method == 'NOSELECT':
                code = self.lookup_noselect(c)
            else:
                raise ValueError('Unknown lookup method: ' + str(method))
            ret.append((c, code))
        return pandas.DataFrame(ret, columns=['char', 'code'])


CF = Table.load_from_csv('charfreq_result.csv', name='CF')
WF = Table.load_from_csv('wordfreq_result.csv', name='WF')
wb98w = Table.load_from_file('98五笔-词组优先表-【单义】.txt', name='wb98w', reversed=True)
newwb = Table.load_from_file('newwb.txt', name='newwb')

#
def compare_common_chars(table, first_n):
    from common import CHARS
    r = table.chars2code(CHARS[:first_n])
    print(f'{table.name}前{min(first_n, len(CHARS))}常用字平均:', r['code'].apply(len).mean())
    print(r)
n = 2000
compare_common_chars(CF, n)
compare_common_chars(wb98w, n)
compare_common_chars(newwb, n)

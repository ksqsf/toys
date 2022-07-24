from opencc import OpenCC


opencc = OpenCC('s2t.json')


class Table:
    def __init__(self, table_file,
                 code_first=True,  # code_first=True  => abcd 选项1 选项2
                                   # code_first=False => 文字 abcd efgh
                 only_chars=True,  # True 表示去掉词语
                 to_trad=False,    # 繁体字优先
                 only_full=False,
                 max_len=4,
                 rime=None         # rime dict.yaml, 类型整数；表示 code 在第几列
                 ):
        # code to text.
        self.c2t = dict()
        # text to code.
        self.t2c = dict()
        # 全码
        self.c2t = dict()
        self.only_full = only_full
        self.max_len = max_len
        if rime is not None:
            self._load_rime(table_file, only_chars, rime)
        else:
            if code_first:
                self._load_code_first(table_file, only_chars)
            else:
                self._load_text_first(table_file, only_chars)
            
        if to_trad:
            self.to_trad()

    def add_c2t(self, code, text):
        if code not in self.c2t:
            self.c2t[code] = []
        self.c2t[code].append(text)

    def add_t2c(self, text, code):
        if text not in self.t2c:
            self.t2c[text] = []
        self.t2c[text].append(code)

    def _load_code_first(self, table_file, only_chars):
        with open(table_file) as file:
            for line in file:
                [input, *outputs] = line.split()
                if len(outputs) == 0:
                    continue
                for output in outputs:
                    if only_chars and len(output) > 1:
                        continue
                    self.add_c2t(input, output)
                    self.add_t2c(output, input)

    def _load_text_first(self, table_file, only_chars):
        with open(table_file) as file:
            for line in file:
                [output, *inputs] = line.split()
                if len(inputs) == 0:
                    continue
                if only_chars and len(output) > 1:
                    continue
                for input in inputs:
                    self.add_c2t(input, output)
                    self.add_t2c(output, input)

    def _load_rime(self, table_file, only_chars, code_column):
        with open(table_file) as file:
            for line in file:
                parts = line.split()
                try:
                    if code_column == 0:
                        code = parts[0]
                        text = parts[1]
                    elif code_column > 0:
                        code = parts[code_column]
                        text = parts[0]
                except IndexError:
                    # 应该是注释一类的东西. 忽略.
                    continue
                if only_chars and len(text) > 1:
                    continue
                self.add_c2t(code, text)
                self.add_t2c(text, code)

    def find_nodup(self, char):
        """
        返回：最佳码，第几重 (首选是 1)
        """
        tmp = []
        for code in self.t2c[char]:
            if self.c2t[code][0] == char:
                return code, 1
            else:
                for i, char_ in enumerate(self.c2t[code]):
                    if char_ == char:
                        tmp.append((i, len(code), code))
        # 无论如何都有重码, 取 (序号, 码长) 最小的
        tmp.sort()
        idx, _, code = tmp[0]
        return code, idx + 1

    def find_short(self, char):
        """
        返回：最佳码，第几重 (首选是 1)
        """
        tmp = []
        for code in self.t2c[char]:
            for i, char_ in enumerate(reversed(self.c2t[code])):
                if char_ == char:
                    tmp.append((len(code), i, code))
        tmp.sort()
        _, idx, code = tmp[0]
        return code, idx+1

    def find_nodup_2allowed(self, char):
        """
        同 find_nodup, 但是允许次选.
        """
        # 查找有没有在 1 / 2 选位置的
        tmp = []
        for code in self.t2c[char]:
            num_cands = len(self.c2t[code])
            for i in range(min(2, num_cands)):
                if self.c2t[code][i] == char:
                    tmp.append((len(code), i+1, code))
        tmp.sort()
        if len(tmp)>0:
            codelen, idx, code = tmp[0]
            return code, idx

        # 1 / 2 选处没找到, 重新找码, 取 (序号, 码长) 最小的
        tmp = []
        for code in self.t2c[char]:
            for i, char_ in enumerate(self.c2t[code]):
                if char_ == char:
                    tmp.append((i, len(code), code))
        tmp.sort()
        idx, _, code = tmp[0]
        return code, idx + 1

    def to_trad(self):
        codes = list(self.c2t.keys())
        for code in codes:
            # 把非繁体字挪到后排.
            self.c2t[code].sort(key=lambda c: not is_trad(c))


def is_trad(ch):
    # 只是一个近似.
    return ch == opencc.convert(ch)





# from typing import List, Tuple
# import pandas


# class Table:
#     def __init__(self, name, maxlen=None):
#         self.name = name
#         self.maxlen = maxlen
#         self.t2c = dict()
#         self.c2t = dict()

#     def add_code(self, text, code):
#         if text not in self.t2c:
#             self.t2c[text] = []
#         self.t2c[text].append(code)
#         if code not in self.c2t:
#             self.c2t[code] = []
#         self.c2t[code].append(text)

#     def lookup_shortest(self, text):
#         s = self.t2c[text]
#         s.sort(key=len)
#         return s[0]

#     @staticmethod
#     def load_from_file(file, name, maxlen=None, reversed=False):
#         table = Table(name, maxlen)
#         with open(file) as f:
#             for line in f:
#                 if not reversed:
#                     code, word = line.split()
#                 else:
#                     word, code = line.split()
#                 table.add_code(word, code)
#         return table

#     @staticmethod
#     def load_from_csv(file, name, maxlen=None):
#         import csv
#         table = Table(name, maxlen)
#         with open(file) as f:
#             rdr = csv.reader(f)
#             next(rdr)
#             for [item, _, code] in rdr:
#                 table.add_code(item, code)
#         return table

#     def chars2code(self, chars: str, method='SHORTEST') -> pandas.DataFrame:
#         ret = []
#         for c in chars:
#             if method == 'SHORTEST':
#                 code = self.lookup_shortest(c)
#             elif method == 'NOSELECT':
#                 code = self.lookup_noselect(c)
#             else:
#                 raise ValueError('Unknown lookup method: ' + str(method))
#             ret.append((c, code))
#         return pandas.DataFrame(ret, columns=['char', 'code'])


# CF = Table.load_from_csv('charfreq_result.csv', name='CF')
# WF = Table.load_from_csv('wordfreq_result.csv', name='WF')
# wb98w = Table.load_from_file('98五笔-词组优先表-【单义】.txt', name='wb98w', reversed=True)
# newwb = Table.load_from_file('newwb.txt', name='newwb')

# #
# def compare_common_chars(table, first_n):
#     from common import CHARS
#     r = table.chars2code(CHARS[:first_n])
#     print(f'{table.name}前{min(first_n, len(CHARS))}常用字平均:', r['code'].apply(len).mean())
#     print(r)
# n = 2000
# compare_common_chars(CF, n)
# compare_common_chars(wb98w, n)
# compare_common_chars(newwb, n)

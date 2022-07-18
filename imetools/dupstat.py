# 码表格式:
# 输入码 输出文本
#
# 1. 输入码可以重复，在前的码优先级更高。
# 2. 允许简码和全码在同一张表内，但全码必须是最后一条。
#
# 给定字符集，程序将计算：
# 1. 避重情况, 即尽可能避重后的重码数量及其码长。
# 2. 选重情况, 即全码的重码数量及其码长。


from common import *
from opencc import OpenCC


opencc = OpenCC('s2t.json')


class Table:
    def __init__(self, table_file,
                 code_first=True,  # code_first=True  => abcd 选项1 选项2
                                   # code_first=False => 文字 abcd efgh
                 only_chars=True,  # True 表示去掉词语
                 to_trad=False,    # 繁体字优先
                 only_full=False,
                 max_len=4
                 ):
        # code to text.
        self.c2t = dict()
        # text to code.
        self.t2c = dict()
        # 全码
        self.c2t = dict()
        self.only_full = only_full
        self.max_len = max_len
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

    def find_nodup(self, char):
        """
        返回：最佳码，第几重 (首选是 1)
        """
        tmp = []
        for code in self.t2c[char]:
            if len(self.c2t[code]) == 1:
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

    def to_trad(self):
        codes = list(self.c2t.keys())
        for code in codes:
            # 把非繁体字挪到后排.
            self.c2t[code].sort(key=lambda c: not is_trad(c))


def is_trad(ch):
    # 只是一个近似.
    return ch == opencc.convert(ch)


def report_dup(
        charset,  # 字符集
        find_fn,  # 查找单字的函数
        selection_to_length=None,   # select_to_lengths[n] 表示第 n 选位上上; 如果是 None, 除首选外全部视为 1
        notfound_len=None,   # 没找到的字相当于 notfound_len 长度的字。如果是 None，忽略这些字，仅在最后报告。
        max_len=4            # 最大码长
):
    first_selection = []   # 首选
    second_selection = []  # 次选
    third_selection = []   # 三选
    other_selection = []   # 三选以上

    n_chars = 0
    total_len = 0

    notfound = []

    def code_length(code, idx):
        if idx == 1:
            # 若不足 max_len 长，需补空格
            if len(code) < max_len:
                return len(code) + 1
            else:
                return len(code)
        else:
            if selection_to_length is None:
                return len(code) + 1
            else:
                return len(code) + selection_to_length[idx]

    for ch in charset:
        try:
            code, idx = find_fn(ch)
        except KeyError:
            notfound.append(ch)
            continue
        l = code_length(code, idx)
        n_chars += 1
        total_len += l
        if idx == 1:
            first_selection.append((ch, code, l))
        elif idx == 2:
            second_selection.append((ch, code, l))
        elif idx == 3:
            third_selection.append((ch, code, l))
        else:
            other_selection.append((ch, code, l))

    if notfound_len is not None:
        for ch in notfound:
            n_chars += 1
            total_len += notfound_len

    print(' - 总字数:', n_chars)
    print(' - 平均码长:', total_len / n_chars)
    print(' - 未编码字:', notfound)
    print(' - 首选率:', (len(first_selection) / n_chars) * 100, '%', ', 共', len(first_selection), '字')
    print(' - 选重率:', (len(second_selection) + len(third_selection) + len(other_selection)) / n_chars * 100, '%', ', 共', (len(second_selection) + len(third_selection) + len(other_selection)), '字')
    print('   - 次选率:', (len(second_selection) / n_chars) * 100, '%', ', 共', len(second_selection), '字')
    print('   - 三选率:', (len(third_selection) / n_chars) * 100, '%', ', 共', len(third_selection), '字')
    print('   - 其他选率:', (len(other_selection) / n_chars) * 100, '%', ', 共', len(other_selection), '字')

    print(' * 次选字:', second_selection)
    print(' * 三选字:', third_selection)
    print(' * 三选以上字:', other_selection)

def report(charset, table):
    # find_nodup: 尽量取不重的码。例如，「華」若有码 a，而「工」有码 a 并且优先级更高，那么「華」不会取到 a，而是 agag。
    #             如果无论如何都要
    # find_short: 不顾选重，尽量取最短的码。
    report_dup(charset, table.find_nodup, max_len=table.max_len)


newwb = Table('newwb.txt', code_first=False)
newwb_full = Table('newwb_full.txt')
# newwbt = Table('newwb.txt', code_first=False, to_trad=True)
wb98 = Table('98wb.txt')
wb98_full = Table('98wb_full.txt')
# wb98t = Table('98wb.txt', to_trad=True)
tiger = Table('tiger.txt')
tiger_full = Table('tiger_full.txt')
# tigert = Table('tiger.txt', to_trad=True)
xuma = Table('xuma.txt')
xuma_full = Table('xuma_full.txt')
# xumat = Table('xuma.txt', to_trad=True)
smzm = Table('smzm.txt', code_first=False, max_len=3)
# smzmt = Table('smzm.txt', code_first=False, to_trad=True, max_len=3)
cj5 = Table('Cangjie5.txt', code_first=False, max_len=5)


# GB1_SIMP 国标一级字
# GB2_SIMP 国标二级字
# GB_SIMP  加起来
# *_TRAD   用 opencc 暴力转的
# BIG5     big5-1.txt, 5401 字
#
# report 用法例子:
#    report(GB1_SIMP, newwb.find_nodup)

# 一些结论：
#
# * 三码郑码静重太高，真没法用。
#      一级字（3755字）范围就有 10.07% 的静重了。
#      在 BIG5-1 上 41% 的字都要选重（更可怕的是，这里面过半是次选之上的）。
# * 牛码五笔在 4 码定长中重码较低，但有些依赖出简去重，学习难度略高些。
#         在 BIG5-1 上，出简选重 140 字，全码则 695 字。
#         在 GB2312 上，出简选重 37 字，全码 357 字。
# * 徐码的简繁静重和码长出简时略均高于牛码，并且不太依赖简码，只看全码的话是很优秀的。
#         不出简，GB2312 选重 189字，BIG5-1 选重 224 字。
#           出简，GB2312 选重 181字，BIG5-1 选重 224 字。
# * 五笔 98 版在我算的几个 4 码方案中最烂。
# * 仓颉静重无敌！码长也无敌……长！
#
# 数据看 dupstat.xlsx。
#
# 注意：结果随字集、码表质量而变！！不过与实际情况误差应该也不会太离谱。

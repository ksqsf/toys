# 码表格式:
# 输入码 输出文本
#
# 1. 输入码可以重复，在前的码优先级更高。
# 2. 允许简码和全码在同一张表内，但全码必须是最后一条。
#
# 给定字符集，程序将计算：
# 1. 避重情况, 即尽可能避重后的重码数量及其码长。
# 2. 选重情况, 即全码的重码数量及其码长。

from table import Table
from common import *

def report_dup(
        charset,  # 字符集
        find_fn,  # 查找单字的函数
        selection_to_length=None,   # select_to_lengths[n] 表示第 n 选位上上; 如果是 None, 除首选外全部视为 1
        notfound_len=None,   # 没找到的字相当于 notfound_len 长度的字。如果是 None，忽略这些字，仅在最后报告。
        max_len=4,            # 最大码长
        stdout=True,
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
        if ch.strip() == '':
            continue
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
            other_selection.append((ch, code, l, idx))

    if notfound_len is not None:
        for ch in notfound:
            n_chars += 1
            total_len += notfound_len

    if stdout:
        print(' - 总字数:', n_chars)
        print(' - 平均码长:', total_len / n_chars)
        print(' - 未编码字 (%d):' % len(notfound), notfound)
        print(' - 首选率:', (len(first_selection) / n_chars) * 100, '%', ', 共', len(first_selection), '字')
        print(' - 选重率:', (len(second_selection) + len(third_selection) + len(other_selection)) / n_chars * 100, '%', ', 共', (len(second_selection) + len(third_selection) + len(other_selection)), '字')
        print('   - 次选率:', (len(second_selection) / n_chars) * 100, '%', ', 共', len(second_selection), '字')
        print('   - 三选率:', (len(third_selection) / n_chars) * 100, '%', ', 共', len(third_selection), '字')
        print('   - 其他选率:', (len(other_selection) / n_chars) * 100, '%', ', 共', len(other_selection), '字')

        # print(' * 次选字:', second_selection)
        # print(' * 三选字:', third_selection)
        # print(' * 三选以上字:', other_selection)
        
        # for ch, code, _ in first_selection:
        #     print('%s\t%s\t%s' % (ch, code, '1'))
        for ch, code, _ in second_selection:
            print('%s\t%s\t%s' % (ch, code, '2'))
        for ch, code, _ in third_selection:
            print('%s\t%s\t%s' % (ch, code, '3'))
        for ch, code, _, idx in other_selection:
            print('%s\t%s\t%s' % (ch, code, str(idx)))

    return total_len / n_chars, (len(second_selection) + len(third_selection) + len(other_selection)), len(notfound)


def report(charset, table, stdout=True):
    # find_nodup: 尽量取不重的码。例如，「華」若有码 a，而「工」有码 a 并且优先级更高，那么「華」不会取到 a，而是 agag。
    #             如果无论如何都要
    # find_nodup_2allowed: 同 find_nodup，但是允许次选被优先选中。
    # find_short: 不顾选重，尽量取最短的码。
    return report_dup(charset, table.find_nodup, max_len=table.max_len, stdout=stdout)


# newwb = Table('newwb.txt', code_first=False)
# # newwb_full = Table('newwb_full.txt')
# # # newwbt = Table('newwb.txt', code_first=False, to_trad=True)
# wb86 = Table('wb86.txt')
# # wb86_full = Table('wb86_full.txt')
# wb98 = Table('98wb.txt')
# # wb98_full = Table('98wb_full.txt')
# # wb06 = Table('wb06.txt', code_first=False)
# # wb06_full = Table('wb06_full.txt')
# # # wb98t = Table('98wb.txt', to_trad=True)
# tiger0 = Table('tiger.txt')
# tiger = Table('tiger.txt')
# tiger_trad = Table('/tmp/huma.txt', code_first=False, only_chars=True)
# # tiger_full = Table('tiger_full.txt')
# # # tigert = Table('tiger.txt', to_trad=True)
# xuma = Table('xuma.txt', only_charsets=['big5'])
# xuma_full = Table('xuma_full.txt')
# # xumat = Table('xuma.txt', to_trad=True)
# smzm = Table('smzm.txt', code_first=False, max_len=3)
# smzmt = Table('smzm.txt', code_first=False, to_trad=True, max_len=3)
# cj5 = Table('Cangjie5.txt', code_first=False, max_len=5)
# # 986五笔
# t = Table('986wb.txt')
# t_full = Table('986wb_full.txt')

# zm = Table('zm.txt', code_first=False)
# srm = Table('srm.dict.yaml', rime=1)

# A = Table('NewTimeA.dict.yaml', rime=1)
# B = Table('NewTimeB.dict.yaml', rime=1)
# S = Table('NewTimeS.dict.yaml', rime=1)

# xhup = Table('/tmp/xhup.txt', code_first=True)
# sm = Table('/tmp/sm.dict.yaml', rime=1)

# array = Table('/tmp/array.txt', code_first=False)
# lbs = Table('/tmp/lbs.txt', code_first=False)

# xm = Table('/tmp/虾码.txt', code_first=False, max_len=4)

sbfd = Table('sbfd.dict.yaml', rime=1)

def report_all():
    CHARSETS = [
        ('前500 (简体)', SIMP[:500]),
        ('前1500 (简体)', SIMP[:1500]),
        ('前500 (繁体)', TRAD[:500]),
        ('前1500 (繁体)', TRAD[:1500]),
        ('     GB2312-1', GB1_SIMP),
        ('    GB2312-全', GB_SIMP),
        ('       BIG5-1', BIG5)
    ]

    TABLES = [
        # ('五  笔  86', wb86),
        # ('五  笔  98', wb98),
        # ('五笔新世纪', wb06),
        # ('986  五 笔', t),
        # ('牛      码', newwb),
        # ('徐      码', xuma),
        # ('虎      码', tiger),
        # ('仓颉 五代', cj5),
        # ('三码 郑码', smzm)
        # ('郑码', zm)
        # ('山人', srm)
        # ('新纪元A', A),
        # ('新纪元B', B),
        # ('新纪元S', S),
        # ('小鹤音形', xhup)
        # ('矧码', sm)
        # ('10.2', ue)
        # ('蓝宝石', lbs)
        # ('虎', tiger_trad)
        ('声笔飞单', sbfd)
    ]

    import itertools

    for ((table_name, table), (charset_name, charset)) in itertools.product(TABLES, CHARSETS):
#    for ((charset_name, charset), (table_name, table)) in itertools.product(CHARSETS, TABLES):
        length, ndups, nnotfound = report(charset, table, stdout=False)
        print(f'{charset_name}\t{table_name}\t{length:.3f}\t{ndups}\t{nnotfound}')

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
# * 五笔 98 版在我算的几个 4 码方案中最烂，但在五笔家族中最好。
# * 仓颉静重无敌！码长也无敌……长！
#
# 数据看 dupstat.xlsx。
#
# 注意：结果随字集、码表质量而变！！不过与实际情况误差应该也不会太离谱。

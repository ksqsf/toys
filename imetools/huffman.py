# Compute the optimal prefix-free, variable-length encodings for a given corpus.
#
# Advantage: never need to "confirm" input.
#  1. every code corresponds to a unique string by construction
#  2. prefix-freeness means the IME software can "know" precisely if you have completed a code.
#  3. the average code length is provably optimal.
#
# Disadvantage:
#  1. You cannot construct your own words anymore. Every new word added means a new table.
#  2. The max length can be very large.
#  3. Does not take into account the correlation between items. 蝴 almost always is followed by 蝶.
#
# The disadvantages are fatal. This method is not practical.
#
# Compare:
#  - dzjabac    Prefix-free  Length=7
#  - dzj_a_bac_ Fixed-length Length=10

import csv
import queue
from dataclasses import dataclass, field
from typing import Any, List


# freq.csv: only chars
# wordfreq.csv: words + chars
FILE = 'freq.csv'
ARITY = 26             # how many branches does the huffman tree has


def load():
    ret = []
    total_weight = 0.0
    with open(FILE) as f:
        rdr = csv.reader(f)
        next(rdr)
        for [_, item, _, freq, *_] in rdr:
            f = float(freq)
            ret.append((f, item))
            total_weight += f
    # normalize
    for i in range(len(ret)):
        ret[i] = ret[i][0] / total_weight, ret[i][1]
    # add fillers
    for _ in range(len(ret)):
        ret.append((0.0, '#'))

        return ret


@dataclass(order=True)
class Node:
    weight: float
    item: Any=field(compare=False)
    children: List['Node']=field(compare=False)


def huffman_tree(data, arity):
    pq = queue.PriorityQueue()
    for (freq, char) in data:
        assert isinstance(char, str)
        pq.put(Node(freq, char, []))
    while pq.qsize() > 1:
        children = [pq.get() for _ in range(min(arity, pq.qsize()))]
        weight = sum(c.weight for c in children)
        pq.put(Node(weight, None, children))
    return pq.get()


def get_code(node: Node):
    def go(node: Node, path: str, result: dict):
        if node.item and node.item != '#':
            result[node.item] = (node.weight, path)
        else:
            for (i, c) in enumerate(node.children):
                go(c, path + chr(i + ord('a')), result)
    result = dict()
    go(node, '', result)
    return result


def write_result(d):
    count = 0
    weighted_length = 0.0
    max_length = 0
    with open('result.csv', 'w') as csvfile:
        wtr = csv.writer(csvfile)
        wtr.writerow(['item', 'weight', 'code'])
        for (item, (weight, path)) in d.items():
            wtr.writerow([item, weight, path])
            count += 1
            weighted_length += weight * len(path)
            max_length = max(max_length, len(path))
    print('全部有 %d 码' % count)
    print('加权码长: %f' % weighted_length)
    print('最大码长: %f' % max_length)


# Compute.
x = load()
t = huffman_tree(x, ARITY)
d = get_code(t)
write_result(d)


def only_common_chars(d, first_n):
    from common import CHARS
    text = CHARS[:first_n]
    weighted_len = 0.0
    for c in text:
        w, code = d[c]
        weighted_len += w * len(code)
    return weighted_len

print('常用字平均码长')
print('前500:', only_common_chars(d, 500))
print('前1500:', only_common_chars(d, 1500))
print('前3000:', only_common_chars(d, 3000))

# 只保留全码
# 码表格式同 dupstat.py

import sys
fp = sys.argv[1]

t2c = dict()
with open(fp) as f:
    for line in f:
        [text, *codes] = line.split()
        for code in codes:
            t2c[text] = code
        # [code, *texts] = line.split()
        # for text in texts:
        #     t2c[text] = code

c2t = dict()
for t in t2c.keys():
    c = t2c[t]
    if c not in c2t:
        c2t[c] = []
    c2t[c].append(t)

for code, texts in c2t.items():
    print("%s\t%s" % (code, ' '.join(texts)))

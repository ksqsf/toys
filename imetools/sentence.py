
dictfile = "/Users/kaima/Library/Rime/s6.dict.yaml"

charmap = dict()
wordmap = dict()

def add_char(ch, code, weight):
    if ch not in charmap:
        charmap[ch] = []
    charmap[ch].append((code, weight))

def add_word(w, code, weight):
    if w not in wordmap:
        wordmap[w] = []
    wordmap[w].append((code, weight))

def word_code(w):
    codes = []
    for c in w:
        codes.append(charmap[c][-1][0])
    return ' '.join(codes)

with open(dictfile) as f:
    for l in f:
        try:
            [text, code, weight] = l.split('\t')
            weight = weight.strip()
        except:
            continue
        if len(text) == 1:
            add_char(text, code, weight)
        else:
            add_word(text, code, weight)

all_words = list(wordmap.keys())

with open('output', 'w') as f:
    for w in all_words:
        weight = wordmap[w][0][1]
        print(f'{w}\t{word_code(w)}\t{weight}', file=f)

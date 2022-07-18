import sys
fp = sys.argv[1]
with open(fp) as f:
    for l in f:
        try:
            [text, code] = l.split()
            print(code, text)
        except:
            print('ERROR!! ', l)

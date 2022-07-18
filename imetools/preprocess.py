import sys
fp = sys.argv[1]
with open(fp) as f:
    for l in f:
        try:
            [code, text, weight] = l.split()
            print(code, text)
        except:
            print('ERROR!! ', l)

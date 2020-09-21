# 生成求解幻方的 Python 脚本

n = 8

print('from z3 import *')
print('s = Solver()')

for i in range(0, n):
    for j in range(n):
        print('x%d%d = Int("x%d%d")' % (i,j,i,j))
        print('s.add(And(x%d%d >= 1, x%d%d <= %d))' % (i,j,i,j,n**2))

def var(i):
    return 'x%d%d' % (i//n, i%n)

for i in range(n*n):
    for j in range(i+1, n*n):
        print('s.add(%s != %s)' % (var(i), var(j)))

print('sum = Int("sum")')


def gen_sum(start, n, stride):
    res = var(start)
    cur = start + stride
    for i in range(1, n):
        res += ' + ' + var(cur)
        cur += stride
    return res

for row in range(n):
    print('s.add(sum == %s)' % gen_sum(row*n, n, 1))
for col in range(n):
    print('s.add(sum == %s)' % gen_sum(row, n, n))
print('s.add(sum == %s)' % gen_sum(0, n, n+1))
print('s.add(sum == %s)' % gen_sum(n-1, n, n-1))

print('print(s.check())')
print('print(s.model())')

print('# there are %d assertions' % (n*(n-1)/2 + 4 * n + 2))

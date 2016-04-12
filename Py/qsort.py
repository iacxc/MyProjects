#!/usr/bin/python -O

# $Id: qsort.py 223 2013-05-29 11:14:36Z chengxin.cai $
"""some quicksort program"""

import random
from PyUtil import time_it

def qsort1(x, l, u):
    if l >= u: return

    m = l
    for i in xrange(l+1, u+1):
        if x[i] < x[l]:
            m += 1
            x[m], x[i] = x[i], x[m]

    x[m], x[l] = x[l], x[m]

    qsort1(x, l, m-1)
    qsort1(x, m+1, u)

def qsort3(x, l, u):
    if l >= u: return

    t, i, j = x[l], l, u+1

    pos = 0
    while 1:
        while 1:
            i += 1
            if i > u or x[i] >= t: break

        while 1:
            j -= 1
            if x[j] <= t: break

        if i > j: break

        x[i], x[j] = x[j], x[i]

    x[l], x[j] = x[j], x[l]
    qsort3(x, l, j-1)
    qsort3(x, j+1, u)

cutoff = 4
def qsort4(x, l, u):
    global cutoff
    if u-l < cutoff: return

    pos = random.randint(l, u)
    x[l], x[pos] = x[pos], x[l]

    t, i, j = x[l], l, u+1

    while 1:
        while 1:
            i += 1
            if i > u or x[i] >= t: break

        while 1:
            j -= 1
            if x[j] <= t: break

        if i > j : break

        x[i], x[j] = x[j], x[i]

    x[l], x[j] = x[j], x[l]
    qsort4(x, l, j-1)
    qsort4(x, j+1, u)

if __name__ == '__main__':

    MAXNUM = 1024 * 1024 * 1024
    NUM = 1024 * 1024 

    x = [ random.randint(1, MAXNUM) for i in range(NUM) ]
    y = list(x)
    z = list(x)
    
    import sys
    if getattr(sys, 'pypy_version_info', None):
        v = list(x)
    else:
        import numpy as np
        v = np.array(x)

    print 'Start sorting %d numbers ...' % NUM
    time_it('sort', v.sort)
    time_it('qsort1', qsort1, x, 0, NUM-1)
    time_it('qsort3', qsort3, y, 0, NUM-1)
    time_it('qsort4', qsort4, z, 0, NUM-1)

    print 'Finished'

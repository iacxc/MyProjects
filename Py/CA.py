
import numpy

class CA(object):
    def __init__(self, rule, n=100, ratio=2):
        self.table = self.make_table(rule)
        self.n = n
        self.m = ratio * n + 1
        self.array = numpy.zeros((n, self.m), dtype=numpy.int8)
        self.next = 0


    def make_table(self, rule):
        table = {}
        for i,bit in enumerate(binary(rule, 8)):
            t = binary(7-i, 3)
            table[t] = bit

        return table


    def start_single(self):
        self.array[0, self.m/2] = 1
        self.next += 1


    def start_random(self):
        self.array[0] = numpy.random.random([1, self.m]).round()
        self.next += 1


    def loop(self, steps=1):
        [self.step() for i in xrange(steps)]


    def step(self):
        i = self.next
        self.next += 1

        a = self.array
        t = self.table
        for j in xrange(1, self.m-1):
            a[i,j] = t[tuple(a[i-1, j-1:j+2])]

    def get_array(self, start=0, end=None):
        if start == 0 and end == None:
            return self.array
        else:
            return self.array[:, start:end]


def binary(n, digits):
    t = []
    for i in range(digits):
        n,r = divmod(n, 2)
        t.append(r)

    return tuple(reversed(t))

def print_table(table):
    t = table.items()
    t.sort(reverse=True)

    print '\\beforefig'
    print '\\centerline{'
    print '\\begin{tabluar}{|c|c|c|c|c|c|c|c|c|}'
    print '\\hline'

    res = ['prev']
    for k,v in t:
        s = ''.join([str(x) for x in k])
        res.append(s)
    print ' & '.join(res) + ' \\\\ \n\\hline'

    res = ['next']
    for k,v in t:
        res.append(str(v))
    print ' &  '.join(res) + ' \\\\ \n\\hline'

    print '\\end{tabluar}}'


if __name__ == "__main__":
    import sys

    rule = 1
    if len(sys.argv) > 1:
        rule = int(sys.argv[1])

    ca = CA(rule, 600)
    ca.start_random()
    ca.loop(599)

    a = ca.get_array()
    for i in xrange(0, len(a)):
        for j in xrange(0, len(a[i])):
            if a[i,j]:
                print "%d,%d" % (j, 100-i)



import sys
import csv
from time import mktime, strptime

def load_csv (fname):

    output = []
    with open(fname, "rb") as csvfile:
        reader = csv.reader(csvfile)

        datas = []
        for row in reader:
            ts = mktime(strptime(row[0], '%Y-%m-%d %H:%M:%S.%f'))
            a_exec     = int(row[2])
            a_complete = int(row[3])

            datas = [d for d in datas if ts - d[0] < 30*60 ]
            datas.append((ts, a_exec, a_complete))

            sum_exec     = sum(d[1] for d in datas)
            sum_complete = sum(d[2] for d in datas)

            if sum_exec == 0:
                ratio = 1.000
            else:
                ratio = float(sum_complete) / sum_exec

            output.append( [(row[0], row[2], row[3]), 
                            sum_exec, sum_complete, ratio] )
#           yield [(row[0], row[2], row[3]), sum_exec, sum_complete, ratio]

    return output


if len(sys.argv) < 2: sys.exit(0)

stats = load_csv(sys.argv[1])

FORMAT = '(%-26s, %4s, %4s) => %5s, %5s, %.3s'

print FORMAT % ('gen_ts_lct', 'exec', 'comp', 'exec', 'comp', 'ratio')

for row in stats:
    print FORMAT % (row[0][0], row[0][1], row[0][2], 
                    row[1], row[2], row[3])

print
print FORMAT % ('gen_ts_lct', 'exec', 'comp', 'exec', 'comp', 'ratio')

for row in sorted(stats, key=lambda d: d[3]):
    print FORMAT % (row[0][0], row[0][1], row[0][2], 
                    row[1], row[2], row[3])


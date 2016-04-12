#!/usr/bin/python -O

import csv
import mgd
import sys
import itertools


def load_csv (fname, node, skipidle):
    with open(fname, "rb") as csvfile:
        reader = csv.reader(csvfile)

        data = [ row for row in reader
                     if (node == "ALL" or row[0].find(node) > 0) and
                        (not skipidle or int(row[2]) > 0) ]

    return data


if __name__ == "__main__":
    from optparse import OptionParser

    parser = OptionParser()
    parser.add_option("-f", "--file",  dest="file", action="store",
           help="csv file name")

    parser.add_option("-n", "--node",  dest="node", action="store",
           help="node name to focus")

    parser.add_option("-i", "--index",  dest="indices",
           action="append",
           help="indices to check")

    parser.add_option("-s", "--skipidle",  dest="skipidle",
           action="store_true", default=False,
           help="whether skip idle")

    (opts, args) = parser.parse_args()

    opts.indices = ','.join(opts.indices).split(',')

    csvdata = load_csv(opts.file, opts.node, opts.skipidle)

    data = []
    for row in csvdata:
        data.append( [ int(row[i]) for i in map(int,opts.indices) ] )

    if __debug__: print data

    px = mgd.ComputeMGD(data)

    for (k,v) in itertools.izip(csvdata, px):
        print ' '.join(k), ' '.join(map(str, v))

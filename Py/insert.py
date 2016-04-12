#!/usr/bin/python -O

""" seaquest test, using 'hpdci'
"""
import sys
import threading
from PyUtil import time_it

import SeaQuest


def do_insert(opts, tid):

    hpdci = time_it('Connect', SeaQuest.HpdCI,
                    opts.hpdci, {'ip'   : opts.host,
                                 'port' : opts.port,
                                 'user' : opts.user,
                                 'pass' : opts.password,
                                 'dsn'  : 'Admin_Load_DataSource'}, 300)


    insStr = "\n".join(["INSERT INTO neo.usr.a%d(keya,keyb,keyc,txt)" % tid,
        "SELECT x0, x1, x2, '012345678901234567890123456789'",
        "FROM (VALUES(0)) T",
        "transpose 0,1,2,3,4,5,6,7,8,9 AS x0",
        "transpose 0,1,2,3,4,5,6,7,8,9 AS x1",
        "transpose 0,1,2,3,4,5,6,7,8,9 AS x2",
        "transpose 0,1,2,3,4,5,6,7,8,9 AS x3",
        "transpose 0,1,2,3,4,5,6,7,8,9 AS x4",
        "transpose 0,1,2,3,4,5,6,7,8,9 AS x5",
        "transpose 0,1,2,3,4,5,6,7,8,9 AS x6"])

    print insStr
    print hpdci.execute(insStr)


def main(opts):
    threads = []
    for i in xrange(1, 51):
        threads.append(threading.Thread(target=do_insert, args=(opts, i)))

    for t in threads:
        t.start()

    for t in threads:
        t.join()


#----------------------------------
HPDCI = '/opt/hp/dci3.0/bin/hpdci.sh'

if __name__ == '__main__':
    from optparse import OptionParser

    parser = OptionParser()
    parser.add_option('--hpdci',  dest='hpdci',
           action='store',  default=HPDCI,
           help='path of hpdci, [default: %default]')

    parser.add_option('--host',  dest='host',
           action='store',  default='sqws148',
           help='hostname of seaquest odbc server, [default: %default]')

    parser.add_option('--port',  dest='port',
           action='store', type=int, default=18650,
           help='port of seaquest odbc server, [default: %default]')

    parser.add_option('--user',  dest='user',
           action='store', default='sql_user',
           help='username to seaquest odbc server, [default: %default]')

    parser.add_option('--password',  dest='password',
           action='store', default='redhat06',
           help='password to seaquest odbc server, [default: %default]')


    (opts, args) = parser.parse_args()

    main(opts)

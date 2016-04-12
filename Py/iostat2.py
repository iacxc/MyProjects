#!/usr/bin/python

import time


def get_diskstats():
    stats = {}
    for line in file('/proc/diskstats'):
        fields = line.strip().split()

        major = int(fields[0])
        if major in (1, 7) or major < 0:
            continue

        if len(fields) == 14:
            stats[fields[2]] = {'major'     : major,
                                'minor'     : int(fields[1]),
                                'r_ios'     : int(fields[3]),  # times
                                'r_merges'  : int(fields[4]),  # times
                                'r_sectors' : int(fields[5]),  # No.
                                'r_ms'      : int(fields[6]),  # ms
                                'w_ios'     : int(fields[7]),  # times
                                'w_merges'  : int(fields[8]),  # times
                                'w_sectors' : int(fields[9]),  # No.
                                'w_ms'      : int(fields[10]), # ms
                                'ios_pgr'   : int(fields[11]), # No.
                                'tot_ms'    : int(fields[12]), # ms
                                'rq_ms'     : int(fields[13])  # ms
                               }
        else:
            stats[fields[2]] = {'major'     : major,
                                'minor'     : int(fields[1]),
                                'r_ios'     : int(fields[3]), # times
                                'r_sectors' : int(fields[4]), # No.
                                'w_ios'     : int(fields[5]), # times
                                'w_sectors' : int(fields[6]), # No.
                               }

    return stats


def get_iostats(prev_stats, diskstats, interval):
    iostats = []
    for dev_name in diskstats.keys():
        prev = prev_stats[dev_name]
        curr = diskstats[dev_name]

        #calculate delta
        delta = dict((key, float(curr[key] - prev[key])) for key in curr.keys())

        #summary of read and write
        rw_ios     = delta['r_ios'] + delta['w_ios']
        rw_ms      = delta['r_ms']  + delta['w_ms']
        rw_sectors = delta['r_sectors'] + delta['w_sectors']

        iostats.append({
            'device'   : dev_name,
            'rrqm/s'   : delta['r_merges'] / interval,
            'wrqm/s'   : delta['w_merges'] / interval,
            'r/s'      : delta['r_ios'] / interval,
            'w/s'      : delta['w_ios'] / interval,
            'rkB/s'    : delta['r_sectors'] / 2 / interval, # 1 sector = 512B
            'wkB/s'    : delta['w_sectors'] / 2 / interval, # 1 sector = 512B
            'avgrq-sz' : rw_sectors / rw_ios if rw_ios > 0 else 0.0,
            'avgqu-sz' : delta['rq_ms'] / 1000 / interval,
            'await'    : rw_ms / rw_ios if rw_ios > 0 else 0.0,
            'r_await'  : delta['r_ms'] / delta['r_ios'] \
                             if delta['r_ios'] > 0 else 0.0,
            'w_await'  : delta['w_ms'] / delta['w_ios'] \
                             if delta['w_ios'] > 0  else 0.0,
            'svctm'    : delta['tot_ms'] / rw_ios if rw_ios > 0 else 0.0,
            'util'     : min(delta['tot_ms'] / interval, 100.0),
            'interval' : interval
        })

    return iostats


def print_iostats(iostats):
    formatter1 = ' '.join(['{0:8}:'] + ['{{{0}:<8}}'.format(i)
                                             for i in range(1, 15)])
    formatter2 = ' '.join(['{0:8}:'] + ['{{{0}:<8.2f}}'.format(i) 
                                             for i in range(1, 15)])
    print formatter1.format('Device',
                           'rrqm/s',
                           'wrqm/s',
                           'r/s',
                           'w/s',
                           'rkB/s',
                           'wkB/s',
                           'avgrq-sz',
                           'avgqu-sz',
                           'await',
                           'r_await',
                           'w_await',
                           'svctm',
                           '%util',
                           'interval')
    for iostat in iostats:
        print formatter2.format(iostat['device'],
                               iostat['rrqm/s'],
                               iostat['wrqm/s'],
                               iostat['r/s'],
                               iostat['w/s'],
                               iostat['rkB/s'],
                               iostat['wkB/s'],
                               iostat['avgrq-sz'],
                               iostat['avgqu-sz'],
                               iostat['await'],
                               iostat['r_await'],
                               iostat['w_await'],
                               iostat['svctm'],
                               iostat['util'],
                               iostat['interval'])
    print


def main():
    curr_ts = time.time()
    diskstats = get_diskstats()

    while True:
        time.sleep(2)

        prev_ts = curr_ts
        prev_stats = diskstats

        curr_ts = time.time()
        diskstats = get_diskstats()

        interval = float(curr_ts - prev_ts)

        iostats = get_iostats(prev_stats, diskstats, interval)

        print_iostats(iostats)

main()

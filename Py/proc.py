#!/usr/bin/python -O

import os, pwd
import platform

CLK_TCK = os.sysconf('SC_CLK_TCK')
PAGESIZE = os.sysconf('SC_PAGE_SIZE')

def get_all_cpu_stats():
    """ 
       Return a list containing all the cpu stat data
    """
    stats = []
    for line in file('/proc/stat'):
        if not line.startswith('cpu'):
            continue
        fields = line.split()
        cpu = fields[0]
        if cpu == 'cpu': continue

        stat = {'utime' : int(fields[1]) * 1000 / CLK_TCK,
                'nice'  : int(fields[2]) * 1000 / CLK_TCK,
                'stime' : int(fields[3]) * 1000 / CLK_TCK,
                'idle'  : int(fields[4]) * 1000 / CLK_TCK,
                'iowait': int(fields[5]) * 1000 / CLK_TCK,
                'irq':    int(fields[6]) * 1000 / CLK_TCK,
                'softirq':int(fields[7]) * 1000 / CLK_TCK,
               }
        stat['total'] = stat['utime'] + stat['nice'] + stat['stime'] + \
                stat['idle'] + stat['iowait'] + stat['irq'] + stat['softirq']
        stats.append(stat)

    return stats


def get_proc_stat(pid):
    """
       Return the process statistic data for a specific process id
    """
    try:
        stat_file = '/proc/{0}/stat'.format(pid)
        st = os.stat(stat_file)
        pw = pwd.getpwuid(st.st_uid)

        fields = open(stat_file).readline().split()

        stat = {'pid'         : int(pid),
                'owner'       : pw.pw_name,
                'comm'        : fields[1].strip('()'),
                'state'       : fields[2],
               }

        #following fields all need to covert to integer
        field_names = [
                'ppid',
                'pgrp',
                'session',
                'tty_nr',
                'tpgid',
                'flags',
                'minflt',
                'cminflt',
                'majflt',
                'cmajflt',
                'utime',
                'stime',
                'cutime',
                'cstime',
                'priority',
                'nice',
                'num_threads',
                'itrealvalue',
                'starttime',
                'vsize',
                'rss',
                'rsslim',
                'startcode',
                'endcode',
                'startstack',
                'kstkesp',
                'kstkeip',
                'signal',
                'blocked',
                'sigignore',
                'sigcatch',
                'wchan',
                'nswap',
                'cnswap',
                'exit_signal',
                'processor',
                'rt_priority',
                'policy',
                'delayacct_blkio_ticks',
                'guest_time',
                'cguest_time'
               ]

        for i, field_name in enumerate(field_names):
            try:
                stat[field_name] = int(fields[i+3])
        
                if field_name in ('utime', 'stime', 'cutime',
                                  'cstime', 'starttime'):
                    stat[field_name] *= 1000 / CLK_TCK
            except ValueError:
                #convert error, just ignore
                pass
            except IndexError:
                #reach the last field
                break

        stat['runtime_ms'] = stat['utime'] + stat['stime'] 
#                            + stat['cutime'] + stat['cstime']
        stat['memory_bytes'] = stat['rss'] * PAGESIZE

        cmd_file = '/proc/{0}/cmdline'.format(pid)
        stat['cmdline'] = ' '.join(file(cmd_file).readline().split(chr(0)))

        return stat


    except OSError:
        #from os.stat()
        return None
    except IOError:
        #from open(), open file error
        return None
    except KeyError:
        #dict key not found
        return None


import sys
import json
import time
if len(sys.argv) < 2: sys.exit(0)

kernel, major, minor, _ = platform.release().split('.', 3)
minor = minor.split('-')[0]

print CLK_TCK
print PAGESIZE
print 'Release: {0}.{1}.{2}'.format(kernel, major, minor)

N = 5

pid = sys.argv[1]
cpus = get_all_cpu_stats()
proc = get_proc_stat(pid)

cpu_index = proc['processor']
cpu_pct_lst = []
for i in range(N):
    time.sleep(1)
    p_cpus = cpus
    p_proc = proc

    cpus = get_all_cpu_stats()
    proc = get_proc_stat(pid)

    delta_proc = proc['runtime_ms'] - p_proc['runtime_ms']
    delta_cpu = cpus[cpu_index]['total'] - p_cpus[cpu_index]['total']

    cpu_pct = 100 * delta_proc / delta_cpu
    cpu_pct_lst.append(cpu_pct)
    print delta_proc, delta_cpu, cpu_pct

proc['cpu_pct'] = sum(cpu_pct_lst) / N
print json.dumps(proc, indent=4)


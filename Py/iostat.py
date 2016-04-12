#!/usr/bin/python -O

import subprocess
import json


def run_cmd(cmd):
    p = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE,
                                          stderr=subprocess.STDOUT)
    out, err = p.communicate()

    return out


def get_iostat(line):
    fields = line.split()
    stat = {'device': fields[0]}

    field_names = ['rrqm/s',
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
                   'util']

    for index, field_name in enumerate(field_names):
        try:
            stat[field_name] = float(fields[index+1])
        except ValueError:
            pass
            
    return stat


lines = run_cmd('iostat -p -xyk 5 1').split('\n')
    
index = 1
for line in lines:
    if line.startswith('Device'):
        break
    index += 1

stats = [get_iostat(line) for line in lines[index:]
                          if len(line.strip()) > 0 ]

print json.dumps(stats, indent=4)

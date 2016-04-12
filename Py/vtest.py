#!/usr/bin/python -O

""" vertica test, using 'vsql'
"""
from PyUtil import time_it
from dbcli import VerticaCli

def do_insert(vsql, tablename):
    return vsql.execute(["insert into %s values(%d, 'name%d')" % 
                                 (tablename, i, i+10)
                              for i in range(5)] 
                        + ['commit'])

def do_select(vsql, tablename):
    return vsql.execute(['select * from %s' % tablename])

vsql = time_it('Connect', VerticaCli, host='sq168', 
                          uid='seapilot', passwd='seapilot1')
print time_it('Insert', do_insert, vsql, 'seapilot.insert_table')
print time_it('Select', do_select, vsql, 'seapilot.insert_table')

print time_it('Wait', vsql.close)

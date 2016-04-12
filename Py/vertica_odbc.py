#!/usr/bin/python -O

import pyodbc
import json

#the driver 'Vertica' must be defined in /etc/odbcinst.ini or ~/.odbcinst.ini
connstr = "Driver=Vertica;Server=bronto10.usa.hp.com;Port=5433;Database=seapilot;UID=seapilot;PWD=seapilot1"
conn = pyodbc.connect(connstr)
cursor = conn.cursor()
print 'All tables in schema seapilot'
for table in cursor.tables(schema='seapilot', tableType='TABLE'):
    print json.dumps({'table_name'  : table.table_name,
                      'description' : table.remarks}, indent=4)

print 'All columns in table event_text_table'
for col in cursor.columns(table='event_text_table', schema='seapilot'):
    print json.dumps({'col_name' : col.column_name, 
                      'col_type' : '{0}:{1}'.format(col.data_type, 
                                                    col.type_name),
                      'col_size' : col.column_size,
                      'default'  : col.column_def}, indent=4)


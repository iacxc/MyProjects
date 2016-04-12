
import pypyodbc as pyodbc
from PyUtil import time_it

"""
   create table insert_table (id int, text varchar(200));
"""
def do_connect(connstr):
    conn = pyodbc.connect(connstr)
    return conn

def do_alltables(cursor):
    print 'All tables'
    for row in cursor.tables().fetchall():
        print row
    print '...\n'

def do_columns(cursor, tablename):
    print 'Columns in %s' % tablename
    for row in cursor.columns(table=tablename):
        print row,

    print

def do_insert(cursor, tablename):
#   cursor.execute('begin')
    sqlstr = "insert into %s values(?, ?)" % tablename
    print sqlstr
    for i in range(100):
        print i+10, i*10
        cursor.execute( sqlstr, [i+10, 'name%d' % (i*10)] )

    cursor.commit()

def do_select(cursor, tablename):
    cursor.execute('select * from %s' % tablename)

    rows = cursor.fetchall()
    print rows

## ---- main ----
conn = time_it('Connect', do_connect, 'DSN=vert-sq169;UID=vertica;PWD=vert')
cursor = conn.cursor()

#ime_it('All tables', do_alltables, cursor)

# insert_table (id int, name vachar(100))
#ime_it('Fetch columns', do_columns, cursor, 'insert_table')

time_it('Insert 100 rows', do_insert, cursor, 'insert_table')

time_it('Select', do_select, cursor, 'insert_table')

cursor.close()



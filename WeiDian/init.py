#!/usr/bin/python

# -*- coding: utf-8 -*-

import os, sys
import resource as R

from collections import namedtuple
import sqlite3


SQL_METAS = [ """CREATE TABLE T_TABLES (
    name text not null primary key,
    description text
)"""
, """CREATE TABLE T_COLUMNS (
    tablename text not null,
    col_id integer not null,
    name text not null,
    type text not null,
    is_primary integer default 0,
    primary key (tablename, col_id, name)
)""" 
, "INSERT INTO T_TABLES  VALUES ('T_TABLES', 'Table Metadata')"
, "INSERT INTO T_COLUMNS VALUES ('T_TABLES', 1, 'name',        'text', 1)"
, "INSERT INTO T_COLUMNS VALUES ('T_TABLES', 2, 'description', 'text', 0)"

, "INSERT INTO T_TABLES  VALUES ('T_COLUMNS', 'Column Metadata')"
, "INSERT INTO T_COLUMNS VALUES ('T_COLUMNS', 1, 'tablename',   'text',    1)"
, "INSERT INTO T_COLUMNS VALUES ('T_COLUMNS', 2, 'col_id',      'integer', 1)"
, "INSERT INTO T_COLUMNS VALUES ('T_COLUMNS', 3, 'name',        'text',    1)"
, "INSERT INTO T_COLUMNS VALUES ('T_COLUMNS', 4, 'type',        'text',    0)"
, "INSERT INTO T_COLUMNS VALUES ('T_COLUMNS', 5, 'is_primary',  'integer', 0)"
]

Field = namedtuple('FieldSpec',
                   'col_id name type nullable default is_primary title')

TableSpecs = {
    'CATALOG': [
        Field(1, 'prod_id',   'text', 0, "''", 1, R.String.ST_PRODID),
        Field(2, 'prod_desc', 'text', 1, "''", 0, R.String.ST_PRODDESC),
        Field(3, 'barcode',   'text', 1, "''", 0, R.String.ST_BARCODE)],
    'CUSTOMER' : [
        Field(1, 'cust_id',  'integer', 0, "''", 1, R.String.ST_CUSTID),
        Field(2, 'name',     'text',    0, "''", 0, R.String.ST_CUSTNAME),
        Field(3, 'cellphone','text',    0, "''", 0, R.String.ST_CUSTCELL),
        Field(4, 'weixin',   'text',    1, "''", 0, R.String.ST_CUSTWEIXIN),
        Field(5, 'qq',       'text',    1, "''", 0, R.String.ST_CUSTQQ),
        Field(6, 'address',  'text',    1, "''", 0, R.String.ST_CUSTADDRESS),
        Field(7, 'other',    'text',    1, "''", 0, R.String.ST_CUSTOTHER)],
    'SELL' : [
        Field(1, 'sell_date', 'date',    0, "''",  0, 'Date'),
        Field(2, 'prod_id',   'integer', 0, "''",  0, 'Product ID'),
        Field(3, 'cust_id',   'integer', 0, "''",  0, 'Customer ID'),
        Field(4, 'price',     'double',  0, "''",  0, 'Price'),
        Field(5, 'amount',    'integer', 0, "''",  0, 'Amount'),
        Field(6, 'post_fee',  'double',  0, "0",   0, 'Post Fee')]
}


def exec_sql(cursor, sqlstr, params=None):
    if __debug__:
        print sqlstr, params
        if params:
            cursor.execute(sqlstr, params)
        else:
            cursor.execute(sqlstr)


def init_metadata(conn):
    cursor = conn.cursor()
    for sqlstr in SQL_METAS:
        exec_sql(cursor, sqlstr)


def init_tables(conn):
    cursor = conn.cursor()
    for tname, rows in TableSpecs.items():
        fieldspecs = []
        view_fieldspecs = []
        primarykeys = []
        for row in rows:
            fieldspecs.append('%s %s %s %s' % (
                row.name, row.type,
                'not null' if not row.nullable else '',
                'default %s' % row.default))

            view_fieldspecs.append('%s as "%s"' % (row.name, row.title))
            if row.is_primary:
                primarykeys.append(row.name)

        sqlstr = 'CREATE TABLE T_%s (%s' % (tname, ', '.join(fieldspecs))
        if len(primarykeys) > 0:
            sqlstr += ', primary key (%s)' % ','.join(primarykeys)
        sqlstr += ')'
        exec_sql(cursor, sqlstr)

        sqlstr = 'CREATE VIEW V_%s AS SELECT %s FROM T_%s' % (
            tname, ', '.join(view_fieldspecs), tname)
        if len(primarykeys) > 0:
            sqlstr += ' ORDER BY %s' % ', '.join(primarykeys)
        exec_sql(cursor, sqlstr)
        
        exec_sql(cursor, "INSERT INTO T_TABLES VALUES (?, ?)", ('T_' + tname, tname))
        for row in rows:
            exec_sql(cursor, "INSERT INTO T_COLUMNS VALUES (?, ?, ?, ?, ?)",
                    ('T_' + tname, row.col_id, row.name, row.type, row.is_primary))
                   

def init_db(conn):
    init_metadata(conn)
    init_tables(conn)
    pass


if __name__ == "__main__":
    if os.path.exists(R.Value.DBFILE):
        if len(sys.argv) > 1 and sys.argv[1] == '-force':
            os.remove(R.Value.DBFILE)
        else:
            print "Database file exists, initialization failed"
            sys.exit(1)

    conn = sqlite3.connect(R.Value.DBFILE)
    init_db(conn)

    conn.commit()
    conn.close()

    print "Database  initialization succeed!"


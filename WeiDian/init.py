#!/usr/bin/python

import os, sys
import sqlite3

DBFILE = 'WeiDian.db3'

SQL_METAS = [ """CREATE TABLE T_TABLES (
    name text not null primary key,
    description text
)"""
, """CREATE TABLE T_COLUMNS (
    tablename text,
    name text,
    col_id integer,
    is_primary integer default 0,
    title text,
    primary key (tablename, name, col_id)
)""" 
, "INSERT INTO T_TABLES VALUES ('T_TABLES', 'Table Metadata')"
, "INSERT INTO T_TABLES VALUES ('T_COLUMNS', 'Column Metadata')"

, "INSERT INTO T_COLUMNS VALUES \
        ('T_TABLES', 'name',        1, 1, 'Name')"
, "INSERT INTO T_COLUMNS VALUES \
        ('T_TABLES', 'description', 2, 0, 'Descriptin')"
, "INSERT INTO T_COLUMNS VALUES \
      ('T_COLUMNS', 'tablename',  1, 1, 'Table Name')"
, "INSERT INTO T_COLUMNS VALUES \
      ('T_COLUMNS', 'name',       2, 1, 'Column Name')"
, "INSERT INTO T_COLUMNS VALUES \
      ('T_COLUMNS', 'col_id',     3, 1, 'Column Id')"
, "INSERT INTO T_COLUMNS VALUES \
      ('T_COLUMNS', 'is_primary', 4, 0, 'Primary(Y/N)')"
, "INSERT INTO T_COLUMNS VALUES \
      ('T_COLUMNS', 'title',      5, 0, 'Column Title')"
]

SQL_TABLES = [ """CREATE TABLE T_CATALOG(
    prod_id text not null primary key,
    prod_desc text,
    barcode text
)"""
, "INSERT INTO T_TABLES VALUES ('T_CATALOG', 'Catalog')"
, "INSERT INTO T_COLUMNS VALUES ('T_CATALOG', 'prod_id',   1, 1, 'ID')"
, "INSERT INTO T_COLUMNS VALUES ('T_CATALOG', 'prod_desc', 2, 0, 'Description')"
, "INSERT INTO T_COLUMNS VALUES ('T_CATALOG', 'barcode',   3, 0, 'BarCode')"
]

def init_metadata(conn):
    cursor = conn.cursor()
    for sqlstr in SQL_METAS:
        if __debug__:
            print sqlstr
        cursor.execute(sqlstr)


def init_tables(conn):
    cursor = conn.cursor()
    for sqlstr in SQL_TABLES:
        if __debug__:
            print sqlstr
        cursor.execute(sqlstr)


def init_db(conn):
    init_metadata(conn)
    init_tables(conn)
    pass


if __name__ == "__main__":
    if os.path.exists(DBFILE):
        print "Database file exists, initialization failed"
        sys.exit(1)

    conn = sqlite3.connect(DBFILE)
    init_db(conn)

    conn.commit()
    conn.close()

    print "Database  initialization succeed!"


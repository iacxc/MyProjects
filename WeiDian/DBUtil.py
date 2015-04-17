#!/usr/bin/python -O

import os
import sqlite3

import resource as R

def connect():
    assert os.path.exists(R.Value.DBFILE)
    return sqlite3.connect(R.Value.DBFILE)


def get_table_columns(conn, tablename, hastitle=True):
    cursor = conn.cursor()

    sqlstr = "SELECT name, title, is_primary FROM T_COLUMNS WHERE " \
             "tablename=? ORDER BY col_id"

    cursor.execute(sqlstr, (tablename,))

    columns = []
    orderbys = []

    for row in cursor.fetchall():
        if hastitle:
             columns.append("{0} as '{1}'".format(row[0],  row[1]))
        else:
             columns.append(row[0])

        if row[2]:
            orderbys.append(row[0])

    return (columns, orderbys)


def get_table_data(tablename):
    conn = connect()

    columns, orderbys = get_table_columns(conn, tablename)

    sqlstr = "SELECT {0} FROM {1}".format(", ".join(columns), tablename)

    if len(orderbys) > 0:
        sqlstr += " ORDER BY {0}".format(",".join(orderbys))

    cursor = conn.cursor()
    cursor.execute(sqlstr)

    results = ([desc[0] for desc in cursor.description],
            cursor.fetchall())

    conn.close()

    return results


def set_table_data(tablename, rows):
    conn = connect()
    cursor = conn.cursor()

    columns, orderbys = get_table_columns(conn, tablename, hastitle=False)

    cursor.execute("DELETE FROM {0}".format(tablename))

    sqlstr = "INSERT INTO {0} ({1}) VALUES ({2})".format(
        tablename, ",".join(columns), ",".join(["?"] * len(columns)))

    cursor.executemany(sqlstr, rows)

    conn.commit()
    conn.close()


def insert(tablename, row):
    conn = connect()
    cursor = conn.cursor()

    columns, orderbys = get_table_columns(conn, tablename, hastitle=False)
    sqlstr = "INSERT INTO {0} ({1}) VALUES ({2})".format(
        tablename, ",".join(columns), ",".join(["?"] * len(columns)))

    cursor.execute(sqlstr, row)

    conn.commit()
    conn.close()


def delete(tablename, row):
    conn = connect()
    cursor = conn.cursor()

    columns, orderbys = get_table_columns(conn, tablename, hastitle=False)

    where_str = " and ".join(col + "=?" for col in columns)
    sqlstr = "DELETE FROM {0} WHERE {1}".format(tablename, where_str)

    cursor.execute(sqlstr, row)

    print sqlstr, row
    conn.commit()
    conn.close()


def dump(path, tablename, rows):
    conn = connect()

    columns, orderbys = get_table_columns(conn, tablename, hastitle=False)
    with file(path, "w") as sqlfile:
        for row in rows:
            sqlfile.write("INSERT INTO {0} ({1}) VALUES ({2});\n".format(
                                tablename, ",".join(columns), ",".join(row)))


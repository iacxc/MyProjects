#!/usr/bin/python -O

import os
import csv
import sqlite3

import resource as R

def connect():
    assert os.path.exists(R.Value.DBFILE)
    return sqlite3.connect(R.Value.DBFILE)


def get_data(sqlstr):
    conn = connect()

    cursor = conn.cursor()
    cursor.execute(sqlstr)

    results = ([desc[0] for desc in cursor.description], cursor.fetchall())

    conn.close()

    return results


def get_table_columns(conn, tablename):
    cursor = conn.cursor()

    sqlstr = "SELECT name FROM T_COLUMNS WHERE tablename=? ORDER BY col_id"

    cursor.execute(sqlstr, (tablename,))

    return [row[0] for row in cursor.fetchall()]


def load_table(tablename, path):
    conn = connect()
    cursor = conn.cursor()
    cursor.execute("DELETE FROM {0}".format(tablename))

    with file(path, "rb") as csvfile:
        reader = csv.reader(csvfile)

        rows = [ [unicode(item, "utf-8") for item in row]
                                            for row in reader ]

    columns = get_table_columns(conn, tablename)
    sqlstr = "INSERT INTO {0} ({1}) VALUES ({2})".format(
        tablename, ",".join(columns), ",".join(["?"] * len(columns)))

    cursor.executemany(sqlstr, rows)

    conn.commit()
    conn.close()


def save_table(tablename, path):
    conn = connect()
    columns = get_table_columns(conn, tablename)

    cursor = conn.cursor()
    cursor.execute("SELECT %s FROM %s" % (",".join(columns), tablename))

    with file(path, "wb") as csvfile:
        writer = csv.writer(csvfile, quoting=csv.QUOTE_ALL)

        for row in cursor.fetchall():

            writer.writerow(row)

    conn.close()


def insert(tablename, row):
    conn = connect()
    cursor = conn.cursor()

    columns = get_table_columns(conn, tablename)
    sqlstr = "INSERT INTO {0} ({1}) VALUES ({2})".format(
        tablename, ",".join(columns), ",".join(["?"] * len(columns)))

    cursor.execute(sqlstr, row)

    conn.commit()
    conn.close()


def delete(tablename, row):
    conn = connect()
    cursor = conn.cursor()

    columns = get_table_columns(conn, tablename)

    where_str = " and ".join(col + "=?" for col in columns)
    sqlstr = "DELETE FROM {0} WHERE {1}".format(tablename, where_str)

    cursor.execute(sqlstr, row)

    conn.commit()
    conn.close()


def dump(path, tablename, rows):
    conn = connect()

    columns = get_table_columns(conn, tablename)
    with file(path, "w") as sqlfile:
        for row in rows:
            sqlfile.write("INSERT INTO {0} ({1}) VALUES ({2});\n".format(
                                tablename, ",".join(columns), ",".join(row)))

    conn.close()
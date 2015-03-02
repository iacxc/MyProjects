
import sqlite3

DBFILE = "WeiDian.db3"

def connect():
    return sqlite3.connect(DBFILE)


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

    return ([desc[0] for desc in cursor.description],
            cursor.fetchall())


def set_table_data(tablename, rows):
    conn = connect()
    cursor = conn.cursor()

    columns, orderbys = get_table_columns(conn, tablename, hastitle=False)

    sqlstr = "INSERT INTO {0} ({1}) VALUES ({2})".format(
        tablename, ",".join(columns), ",".join(["?"] * len(columns)))

    cursor.execute("DELETE FROM {0}".format(tablename))
    for row in rows:
        cursor.execute(sqlstr, row)

    conn.commit()
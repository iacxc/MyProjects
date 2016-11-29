
import os
import sys
import java
import csv

import vertica

DRIVERSTR = "com.hp.jdbc.HPT4Driver"
CONNSTR   = "jdbc:hpt4jdbc://%s:%s"

# (Linux) export CLASSPATH={path to hpt4jdbc.jar}/hpt4jdbc.jar
# (Windows) set CLASSPATH={path to hpt4jdbc.jar}\hpt4jdbc.jar
def get_connstr(host, port):
    """ doc string """
    return "jdbc:hpt4jdbc://%s:%s" % (host, port)


def get_connection(conn_info):
    """ doc string """
    try:
        java.lang.Class.forName(DRIVERSTR)
    except java.lang.ClassNotFoundException:
        msg = "Class %s not found, please check CLASSPATH" % DRIVERSTR
        raise EnvironmentError, msg

    conn_str = CONNSTR % (conn_info['host'], conn_info['port'])

    conn_prop = java.util.Properties()
    conn_prop.put("user", conn_info['user'])
    conn_prop.put("password", conn_info['password'])
    conn_prop.put("serverDataSource", conn_info['dsn'])

    conn = java.sql.DriverManager.getConnection(conn_str, conn_prop)

    return conn


def make_string(val, coltype):
    """ make string """
    StringTypes = set([1, 12, 91, 93])

    if coltype in StringTypes:
        if coltype == 93: #timestamp
            if val:
                a_str = "timestamp '%s'" % val
            else:
                a_str = "timestamp '1970-01-01 00:00:00'"
        elif coltype == 91: #date
            if val:
                a_str = "date '%s'" % val
            else:
                a_str = "date '1970-01-01'"
        else:
            if val:
                a_str = "'%s'" % val.replace("'", "''").strip()
            else:
                a_str = "''"

    else: #numeric
        if val:
            a_str = val
        else:
            a_str = "0"

    return a_str


def dump_data(conn_info, table_full, sqlstmt, output, schema_o):
    """ dump data """
    conn = get_connection(conn_info)

    statement = conn.createStatement()

    if output == 'csv':
        writer = csv.writer(sys.stdout)

    catalog, schema, table = table_full.split(".")

    if table:
        rs = statement.executeQuery("select [any 1] * from %s" % table_full)
        strCreation = vertica.createTable(rs, schema_o, table)

        print "%s" % strCreation

        rsSrc = statement.executeQuery("select * from %s " % table_full)

    if sqlstmt:
        rsSrc = statement.executeQuery(sqlstmt)

    rsmd = rsSrc.getMetaData()

    columnCount = rsmd.columnCount
    col_types = [ rsmd.getColumnType(colindex)
                      for colindex in range(1, columnCount+1) ]
    headers = [rsmd.getColumnName(colindex)
                      for colindex in range(1, columnCount+1) ]

    nRows = 0
    if output == "csv": writer.writerow(headers)

    while rsSrc.next():
        values = []

        for colindex in range(1, columnCount+1):
            coltype = col_types[colindex-1]
            val = rsSrc.getString(colindex)
            if output == "sql":
                values.append(make_string(val, coltype))
            else:
                values.append(val)

        if output == "sql":
            sqlstr = "insert into %s.%s(%s) values (%s);" % (schema_o, table,
                                    ",".join(headers), ",".join(values))

            print "%s" % sqlstr.encode("utf-8")

        else:
            writer.writerow(values)

        nRows += 1

        if nRows % 1000 == 0 and output == "sql":   print "commit;"

    if output == "sql": print "commit;"

    if output == "sql":
        print "-- %d rows dumped" % nRows


if __name__ == "__main__":
    from optparse import OptionParser

    parser = OptionParser()
    parser.add_option("--host",  dest="host", action="store",
           help="ip address of db server")

    parser.add_option("--port",  dest="port",
           action="store",  type=int,
           help="port number of db server")

    parser.add_option("--user",  dest="user",
           action="store",  default="sql_admin",
           help="user name of db server, [default: %default]")

    parser.add_option("--password",  dest="password",
           action="store",  default="redhat06",
           help="password of db server, [default: %default]")

    parser.add_option("--dsn",  dest="dsn",
           action="store", default="Admin_Load_DataSource",
           help="db name or dsn [default: %default]")

    parser.add_option("--table",  dest="table", action="store",
           help="table name")

    parser.add_option("--sql",  dest="sql", action="store",
           help="sql statement")

    parser.add_option("--output",  dest="output", action="store",
           default="sql",
           help="output type, csv or sql, [default: %default]")

    parser.add_option("--schema", dest="schema", action="store",
           default="public",
           help="schema name when output is sql, [default: %default]")

    (opts, args) = parser.parse_args()

    if not (opts.host or opts.port or opts.user or opts.password):
        print "some fields are empty"
        parser.print_help()
        sys.exit(1)

    if not (opts.table or opts.sql):
        print "you must provide table or sql"
        parser.print_help()
        sys.exit(1)

    if not (opts.output != "sql" or (opts.table and opts.schema)):
        print "you must specify table and schema if generate sql output"
        sys.exit(1)

    dump_data({'host' : opts.host, 'port' : opts.port, 'user' : opts.user,
               'password' : opts.password, 'dsn' : opts.dsn},
               opts.table, opts.sql, opts.output, opts.schema)


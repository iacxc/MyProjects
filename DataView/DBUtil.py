
import pypyodbc

def connect(conn_str):
    return pypyodbc.connect(conn_str)


def fetch_table(conn, query_str, params=None):
    cursor = conn.cursor()
    cursor.execute(query_str)

    return ([ fd[0] for fd in cursor.description ], cursor.fetchall())



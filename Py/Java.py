
import java

def import_jar(jarfile):
    from java.net import URL, URLClassLoader
    from java.lang import ClassLoader
    from java.io import File

    if __debug__:
        print 'importing {0}'.format(jarvile)

    method = URLClassLoader.getDeclaredMethod("addURL", [URL])
    method.accessible = 1
    method.invoke(ClassLoader.getSystemClassLoader(), [File(jarFile).toURL()])


def load_jdbc_driver(dbtype):
    driver = {'trafodion' : 'org.trafodion.jdbc.t4.T4Driver'
             }.get(dbtype)
    java.lang.Class.forName(driver)


def get_jdbc_connection(dbtype, host, port, user, password):
    import java.sql

    connstr = {'trafodion' : 'jdbc:t4jdbc://{0}/{1}'.format(host, port)
              }.get(dbtype)

    return java.sql.DriverManager.getConnection(connstr, user, password) 


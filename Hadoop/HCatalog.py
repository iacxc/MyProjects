

#exports
__all__ = ("HCatalog",)

from HadoopUtil import HadoopUtil, Request, \
                       STATUS_OK

class HCatalog(HadoopUtil):
    operations = ("get_database",
                  "get_table",)
    rootpath = "/templeton/v1"

    def __init__(self, host, user, curl_out=False):
        super(HCatalog, self).__init__("http", host, 50111)
        self.__user = user
        self.__curl = curl_out

    def __iter__(self):
        return iter(self.operations)


    @staticmethod
    def Get(url, user=None, auth=None, params=None, curl=False):
        return Request("GET", url, user, auth, params, curl=curl)


    @property
    def weburl(self):
        return self.baseurl + self.rootpath


    def get_database(self, dbname=None):
        url = self.weburl + "/ddl/database"
        if dbname is not None:
            url += "/" + dbname

        resp = HCatalog.Get(url, self.__user, curl=self.__curl)

        if resp.status_code == STATUS_OK:
            return resp.json()
        else:
            if __debug__: print resp.status_code


    def get_table(self, dbname, tablename=None):
        url = self.weburl + "/ddl/database/%s/table" % dbname 
        if tablename is not None:
            url += "/" + tablename

        resp = HCatalog.Get(url, self.__user, curl=self.__curl)

        if resp.status_code == STATUS_OK:
            return resp.json()
        else:
            if __debug__: print resp.status_code


#
# ---- main ----
if __name__ == "__main__":
    import sys
    import json
    from optparse import OptionParser

    parser = OptionParser()
    parser.add_option("--host", default="localhost")
    parser.add_option("--curl", action="store_true", default=False)
    parser.add_option("-u", "--user", default="caiche")

    opts, args = parser.parse_args()

    if __debug__: print opts, args

    if len(args) < 1:
        print "Missing arguments, supported methods are", HCatalog.operations
        sys.exit(1)

    hcatalog = HCatalog(opts.host, opts.user, opts.curl)

    method = args[0]
    if not method in hcatalog:
        print "Unsupported method '%s'" % method
        sys.exit(1)

    try:
        fun = getattr(hcatalog, method)
        result = fun(*args[1:])
        if isinstance(result, (dict, list)):
            print json.dumps(result, indent=4)
        else:
            print result

    except AttributeError as e:
        print e


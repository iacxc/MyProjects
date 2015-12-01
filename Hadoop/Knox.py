

#exports
__all__ = ("Knox",)


from HadoopUtil import HadoopUtil, Request, \
                       gen_fileinfo, \
                       STATUS_OK, STATUS_CREATED, STATUS_NOCONTENT 
from Hdfs import Hdfs
from HCatalog import HCatalog


class Knox(HadoopUtil):
    operations = ("ls", "cat", 
                  "get_database", "get_table",)

    def __init__(self, host, user, password, topo="default", curl=False):
        super(Knox, self).__init__("https", host, 8443)
        self.__user = user
        self.__password = password
        self.__curl = curl

        self.weburl = self.baseurl + "/gateway/" + topo
        self.hdfsurl = self.weburl + Hdfs.rootpath
        self.hcaturl = self.weburl + HCatalog.rootpath


    def __iter__(self):
        return iter(self.operations)


    @property
    def auth(self):
        return (self.__user, self.__password)

    def ls(self, dirname):
        resp = Hdfs.Get(self.hdfsurl + dirname, "ls",
                        auth=self.auth, curl=self.__curl)

        if resp.status_code == STATUS_OK:
            fs_list = resp.json()["FileStatuses"]["FileStatus"]
            return [gen_fileinfo(fs) for fs in fs_list]
        else:
            if __debug__: print resp.status_code


    def cat(self, filename):
        resp = Hdfs.Get(self.hdfsurl + filename, "cat", 
                        auth=self.auth, curl=self.__curl)

        if resp.status_code == STATUS_OK:
            return resp.text
        else:
            if __debug__: print resp.status_code


    def get_database(self, dbname=None):
        url = self.hcaturl + "/ddl/database"
        if dbname is not None:
            url += "/" + dbname

        resp = HCatalog.Get(url, auth=self.auth)

        if resp.status_code == STATUS_OK:
            return resp.json()
        else:
            if __debug__: print resp.status_code


    def get_table(self, dbname, tablename=None):
        url = self.hcaturl + "/ddl/database/%s/table" % dbname 
        if tablename is not None:
            url += "/" + tablename

        resp = HCatalog.Get(url, auth=self.auth)

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
    parser.add_option("-p", "--password", default="caiche-password")

    opts, args = parser.parse_args()

    if __debug__: print opts, args

    if len(args) < 1:
        print "Missing arguments, supported methods are", Knox.operations
        sys.exit(1)

    knox = Knox(opts.host, opts.user, opts.password, curl=opts.curl)

    method = args[0]
    if not method in knox:
        print "Unsupported method '%s'" % method
        sys.exit(1)

    try:
        fun = getattr(knox, method)
        result = fun(*args[1:])
        if isinstance(result, (dict, list)):
            print json.dumps(result, indent=4)
        else:
            print result

    except AttributeError as e:
        print e


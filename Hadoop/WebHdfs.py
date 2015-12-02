

#exports
__all__ = ('WebHdfs',)


from HadoopUtil import HadoopUtil, Request, \
                       gen_fileinfo, \
                       STATUS_OK, STATUS_CREATED

def get_opstr(op):
    return {"ls"    : "LISTSTATUS",
            "stat"  : "GETFILESTATUS",
            "cat"   : "OPEN",
            "mkdir" : "MKDIRS",
            "cp"    : "CREATE",
            "append": "APPEND",
            "chmod" : "SETPERMISSION",
            "chown" : "SETOWNER",
            "delete": "DELETE",
            "rename": "RENAME"}[op]


class WebHdfs(HadoopUtil):
    operations = ("ls", "mkdir", "cp", "append", "delete",
                  "stat", "cat", "rename", "chmod", "chown", )
    rootpath = "/webhdfs/v1"

    def __init__(self, host, user, curl=False):
        super(WebHdfs, self).__init__("http", host, 50070)
        self.__user = user
        self.__curl = curl


    def __iter__(self):
        return iter(self.operations)


    @property
    def weburl(self):
        return self.baseurl + self.rootpath


    @staticmethod
    def Get(url, op, user=None, auth=None, params=None, curl=False, 
                 text=False):
        if params is None: params = {}
        params["op"] = get_opstr(op)

        return Request("GET", url, user, auth, params, curl=curl, text=text)


    @staticmethod
    def Delete(url, user=None, auth=None, curl=False):
        params = {"op" : "DELETE"}
        return Request("DELETE", url, user, auth, params=params, curl=curl)


    @staticmethod
    def Put(url, op, user=None, auth=None, params=None, data=None, 
                 curl=False, text=False, expected=(STATUS_OK,)):

        if params is None: params = {}
        params["op"] = get_opstr(op)

        return Request("PUT", url, user, auth, params, data, 
                              curl=curl, text=text, expected = expected)


    @staticmethod
    def Post(url, op, user=None, auth=None, params=None, data=None, 
                  curl=False):
        if params is None: params = {}
        params["op"] = get_opstr(op)
        return Request("POST", url, user, auth, params, data, curl=curl)


#-- operations
    def ls(self, dirname):
        result = self.Get(self.weburl + dirname, "ls", self.__user, 
                          curl=self.__curl)

        if result is not None:
            fs_list = result["FileStatuses"]["FileStatus"]
            return [gen_fileinfo(fs) for fs in fs_list]


    def mkdir(self, dirname, perm="777") : 
        return self.Put(self.weburl + dirname, "mkdir", self.__user, 
                        params={"permission" : perm},
                        curl=self.__curl)


    def cp(self, localfile, filename) :
        with file(localfile) as f:
            result = self.Put(self.weburl + filename, "cp", self.__user, 
                              params={"overwrite" : "true"}, 
                              data=f.read(),
                              curl=self.__curl, 
                              text=True,
                              expected=(STATUS_CREATED,))

            if result is not None:
                return {"status" : "OK"}


    def append(self, localfile, filename) :
        with file(localfile) as f:
            result = self.Post(self.weburl + filename, "append", self.__user,
                               data=f.read(),
                               curl=self.__curl)

            if result is not None:
                return {"status" : "OK"}


    def delete(self, filename) : 
        return self.Delete(self.weburl + filename, self.__user,
                           curl=self.__curl)


    def stat(self, filename):
        r = self.Get(self.weburl + filename, "stat", self.__user,
                     curl=self.__curl)
        if r is not None:
            return gen_fileinfo(result["FileStatus"])


    def cat(self, filename):
        return self.Get(self.weburl + filename, "cat", self.__user,
                        curl=self.__curl,
                        text=True)


    def rename(self, srcname, destname):
        return self.Put(self.weburl + srcname, "rename", self.__user,
                        params={"destination" : destname}, 
                        curl=self.__curl)


    def chmod(self, filename, perm="777"):
        r = self.Put(self.weburl + filename, "chmod", self.__user,
                     params={"permission" : perm}, curl=self.__curl)
        if r is not None:
            return {"status" : "OK"}


    def chown(self, filename, owner, group=None):
        r = self.Put(self.weburl + filename, "chown", self.__user,
                     params={"owner" : owner, "group" : group}, 
                     curl=self.__curl)
        if r is not None:
            return {"status" : "OK"}


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
        print "Missing arguments, supported methods are", WebHdfs.operations
        sys.exit(1)

    hdfs = WebHdfs(opts.host, opts.user, opts.curl)

    method = args[0]
    if not method in hdfs:
        print "Unsupported method '%s'" % method
        sys.exit(1)

    try:
        fun = getattr(hdfs, method)
        result = fun(*args[1:])
        if isinstance(result, (dict, list)):
            print json.dumps(result, indent=4)
        else:
            print result

    except AttributeError as e:
        print e



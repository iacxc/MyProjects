

#exports
__all__ = ('Hdfs',)


from HadoopUtil import HadoopUtil, Request, \
                       gen_fileinfo, \
                       STATUS_OK, STATUS_CREATED, STATUS_NOCONTENT

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


class Hdfs(HadoopUtil):
    operations = ("ls", "mkdir", "cp", "append", "delete",
                  "stat", "cat", "rename", "chmod", "chown", )
    rootpath = "/webhdfs/v1"

    def __init__(self, host, user, curl_out=False):
        super(Hdfs, self).__init__("http", host, 50070)
        self.__user = user
        self.__curl = curl_out


    def __iter__(self):
        return iter(self.operations)


    @property
    def weburl(self):
        return self.baseurl + self.rootpath


    @staticmethod
    def Get(url, op, user=None, auth=None, params=None, curl=False):
        if params is None: params = {}
        params["op"] = get_opstr(op)

        return Request("GET", url, user, auth, params, curl=curl)


    @staticmethod
    def Delete(url, user=None, auth=None, curl=False):
        params = {"op" : "DELETE"}
        return Request("DELETE", url, user, auth, params=params, curl=curl)


    @staticmethod
    def Put(url, op, user=None, auth=None, params=None, data=None, 
                 curl=False):
        if params is None: params = {}
        params["op"] = get_opstr(op)
        return Request("PUT", url, user, auth, params, data, curl=curl)


    @staticmethod
    def Post(url, op, user=None, auth=None, params=None, data=None, 
                  curl=False):
        if params is None: params = {}
        params["op"] = get_opstr(op)
        return Request("POST", url, user, auth, params, data, curl=curl)


    def ls(self, dirname):
        resp = Hdfs.Get(self.weburl + dirname, "ls", self.__user, 
                        curl=self.__curl)
        if resp.status_code == STATUS_OK:
            fs_list = resp.json()["FileStatuses"]["FileStatus"]
            return [gen_fileinfo(fs) for fs in fs_list]
        else:
            if __debug__: print resp.status_code


    def mkdir(self, dirname, perm="777") : 
        resp = Hdfs.Put(self.weburl + dirname, "mkdir", self.__user, 
                   params={"permission" : perm})
        if resp.status_code == STATUS_OK:
            return resp.json()
        else:
            if __debug__: print resp.status_code


    def cp(self, localfile, filename) :
        with file(localfile) as f:
            resp = Hdfs.Put(self.weburl + filename, "cp", self.__user, 
                       params={"overwrite" : "true"}, data=f.read())

            if resp.status_code == STATUS_CREATED:
                return {"status" : "OK"}
            else:
                if __debug__: print resp.status_code


    def append(self, localfile, filename) :
        with file(localfile) as f:
            resp = Hdfs.Post(self.weburl + filename, "append", self.__user,
                        data=f.read())

            if resp.status_code == STATUS_OK:
                return {"status" : "OK"}
            else:
                if __debug__: print resp.status_code, resp.text


    def delete(self, filename) : 
        resp = Hdfs.Delete(self.weburl + filename, self.__user)
        if resp.status_code == STATUS_OK:
            return resp.json()
        else:
            if __debug__: print resp.status_code


    def stat(self, filename):
        resp = Hdfs.Get(self.weburl + filename, "stat", self.__user)
        if resp.status_code == STATUS_OK:
            return gen_fileinfo(resp.json()["FileStatus"])
        else:
            if __debug__: print resp.status_code


    def cat(self, filename):
        resp = Hdfs.Get(self.weburl + filename, "cat", self.__user)
        if resp.status_code == STATUS_OK:
            return resp.text
        else:
            if __debug__: print resp.status_code


    def rename(self, srcname, destname):
        resp = Hdfs.Put(self.weburl + srcname, "rename", self.__user,
                   params={"destination" : destname})
        if resp.status_code == STATUS_OK:
            return resp.json()
        else:
            if __debug__: print resp.status_code


    def chmod(self, filename, perm="777"):
        resp = Hdfs.Put(self.weburl + filename, "chmod", self.__user,
                   params={"permission" : perm})
        if resp.status_code == STATUS_OK:
            return {"status" : "OK"}
        else:
            if __debug__: print resp.status_code


    def chown(self, filename, owner, group=None):
        resp = Hdfs.Put(self.weburl + filename, "chown", self.__user,
                   params={"owner" : owner, "group" : group})
        if resp.status_code == STATUS_OK:
            return {"status" : "OK"}
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
        print "Missing arguments, supported methods are", Hdfs.operations
        sys.exit(1)

    hdfs = Hdfs(opts.host, opts.user, opts.curl)

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



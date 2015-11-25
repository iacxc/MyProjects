

#exports
__all__ = ('Hdfs', 'Knox')


import requests
from datetime import datetime
from FileUtils import getpermission

from requests.packages.urllib3.exceptions import InsecureRequestWarning
requests.packages.urllib3.disable_warnings(InsecureRequestWarning)


def ts2str(timestamp):
    """ convert timestamp to string """
    dt = datetime.fromtimestamp(timestamp * 0.001)
    return dt.strftime("%m %d %H:%M")


def get_opstr(operation):
    return {"ls"    : "LISTSTATUS",
            "stat"  : "GETFILESTATUS",
            "cat"   : "OPEN",
            "mkdir" : "MKDIRS",
            "cp"    : "CREATE",
            "append": "APPEND",
            "chmod" : "SETPERMISSION",
            "chown" : "SETOWNER",
            "delete": "DELETE",
            "rename": "RENAME"}[operation]


def Request(method, url, operation, user=None, auth=None, 
                                    params=None, data=None):
    if params is None:
        params = {}

    params["op"] = get_opstr(operation)
    if user:
        params["user.name"] = user

    return requests.request(method, url, auth=auth, verify=False, 
                                         params=params, data=data)


def Get(url, operation, user=None, auth=None, params=None):
    return Request("GET", url, operation, user, auth, params)

def Delete(url, user=None, auth=None, params=None):
    return Request("DELETE", url, "delete", user, auth, params)

def Put(url, operation, user=None, auth=None, params=None, data=None):
    return Request("PUT", url, operation, user, auth, params, data)

def Post(url, operation, user=None, auth=None, params=None, data=None):
    return Request("POST", url, operation, user, auth, params, data)


class HadoopUtil(object):
    def __init__(self, prefix, host, port):
        self.__prefix = prefix
        self.__host = host
        self.__port = port

    @property
    def baseurl(self):
        return "%s://%s:%s" % (self.__prefix, self.__host, self.__port)

    @staticmethod
    def gen_filespec(fs):
        return "%s%-10s  %-8s%-8s%-6s%-12s%-20s" % (
             'd' if fs.get("type", "FILE") == "DIRECTORY" else '-',
             getpermission(fs.get("permission", "000")),
             fs.get("owner", "<no user>"),
             fs.get("group", "<no group>"),
             fs.get("length", 0),
             ts2str(fs.get("modificationTime", 0)),
             fs.get("pathSuffix", "<no name>"))


class Hdfs(HadoopUtil):
    def __init__(self, host, user):
        super(Hdfs, self).__init__("http", host, 50070)
        self.__user = user

    @property
    def weburl(self):
        return self.baseurl + "/webhdfs/v1"


    def ls(self, dirname):
        resp = Get(self.weburl + dirname, "ls", self.__user)
        if resp.status_code == requests.codes.ok:
            fs_list = resp.json()["FileStatuses"]["FileStatus"]
            return [self.gen_filespec(fs) for fs in fs_list]
        else:
            if __debug__: print resp.status_code
            return {}


    def mkdir(self, dirname, perm="777") : 
        resp = Put(self.weburl + dirname, "mkdir", self.__user, 
                   params={"permission" : perm})
        if resp.status_code == requests.codes.ok:
            return resp.json()["boolean"]
        else:
            if __debug__: print resp.status_code
            return False


    def cp(self, localfile, filename) :
        with file(localfile) as f:
            resp = Put(self.weburl + filename, "cp", self.__user, 
                       params={"overwrite" : "true"}, data=f.read())

            if resp.status_code in (requests.codes.ok, requests.codes.created):
                return True
            else:
                if __debug__: print resp.status_code
                return False


    def append(self, localfile, filename) :
        with file(localfile) as f:
            resp = Post(self.weburl + filename, "append", self.__user,
                        data=f.read())

            if resp.status_code == requests.codes.ok:
                return True
            else:
                if __debug__: print resp.status_code, resp.text
                return False


    def delete(self, filename) : 
        resp = Delete(self.weburl + filename, self.__user)
        if resp.status_code == requests.codes.ok:
            return resp.json()["boolean"]
        else:
            if __debug__: print resp.status_code
            return False


    def stat(self, filename):
        resp = Get(self.weburl + filename, "stat", self.__user)
        if resp.status_code == requests.codes.ok:
            return self.gen_filespec(resp.json()["FileStatus"])
        else:
            if __debug__: print resp.status_code
            return {}


    def cat(self, filename):
        resp = Get(self.weburl + filename, "cat", self.__user)
        if resp.status_code == requests.codes.ok:
            return resp.text
        else:
            if __debug__: print resp.status_code
            return ""


    def rename(self, srcname, destname):
        resp = Put(self.weburl + srcname, "rename", self.__user,
                   params={"destination" : destname})
        if resp.status_code == requests.codes.ok:
            return resp.json()["boolean"]
        else:
            if __debug__: print resp.status_code
            return False


    def chmod(self, filename, perm="777"):
        resp = Put(self.weburl + filename, "chmod", self.__user,
                   params={"permission" : perm})
        if resp.status_code == requests.codes.ok:
            return True
        else:
            if __debug__: print resp.status_code
            return False


    def chown(self, filename, owner, group=None):
        resp = Put(self.weburl + filename, "chown", self.__user,
                   params={"owner" : owner,
                           "group" : group})
        if resp.status_code == requests.codes.ok:
            return True
        else:
            if __debug__: print resp.status_code
            return False


class Knox(HadoopUtil):
    def __init__(self, host, user, password, schema="default"):
        super(Knox, self).__init__("https", host, 8443)
        self.__user = user
        self.__password = password

        self.weburl = self.baseurl + "/gateway/" + schema
        self.hdfsurl = self.weburl + "/webhdfs/v1"

    @property
    def auth(self):
        return (self.__user, self.__password)

    def ls(self, dirname):
        resp = Get(self.hdfsurl + dirname, "ls", auth=self.auth)

        if resp.status_code == requests.codes.ok:
            fs_list = resp.json()["FileStatuses"]["FileStatus"]
            return [self.gen_filespec(fs) for fs in fs_list]
        else:
            if __debug__: print resp.status_code
            return {}

    def cat(self, filename):
        resp = Get(self.hdfsurl + filename, "cat", auth=self.auth)

        if resp.status_code == requests.codes.ok:
            return resp.text
        else:
            if __debug__: print resp.status_code
            return ""

#
# ---- main ----
if __name__ == "__main__":
    import sys

    host = sys.argv[1] if len(sys.argv) > 1 else "localhost"
    user = sys.argv[2] if len(sys.argv) > 2 else "caiche"
    user_pass = sys.argv[3] if len(sys.argv) > 3 else "caiche-password"
    hdfs = Hdfs(host, user)
    #print hdfs.cat("/user/caiche/test.x")
    #print hdfs.mkdir("/user/caiche/test2")
    #print hdfs.stat("/user/caiche/test.1")
    #print hdfs.delete("/user/caiche/test.3")
    #print hdfs.rename("/user/caiche/test.4", "/user/caiche/test.t")
    #print hdfs.append("run.cmd1", "/user/caiche/test.x")
    #print "\n".join(hdfs.ls("/user/caiche"))

    knox = Knox(host, user, user_pass)
    print "\n".join(knox.ls("/user/caiche"))
    #print knox.cat("/user/caiche/test.1")

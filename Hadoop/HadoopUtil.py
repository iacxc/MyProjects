
from datetime import datetime
from FileUtils import getpermission


def ts2str(timestamp):
    dt = datetime.fromtimestamp(timestamp * 0.001)
    return dt.strftime("%m %d %H:%M")


import requests
from requests.packages.urllib3.exceptions import InsecureRequestWarning
requests.packages.urllib3.disable_warnings(InsecureRequestWarning)


def get_opstr(operation):
    return {"ls" : "LISTSTATUS",
            "stat" : "GETFILESTATUS",
            "cat" : "OPEN",
            "mkdir" : "MKDIRS",
            "cp"    : "CREATE",
            "append": "APPEND",
            "chmod" : "SETPERMISSION",
            "chown" : "SETOWNER",
            "rename": "RENAME"}.get(operation) 


def Get(url, operation, user=None, auth=None, params=None):
    if params is None: 
        params = {}

    params["op"] = get_opstr(operation)
    if user:
        params["user.name"] = user

    return requests.get(url, auth=auth, verify=False, params=params)


def Put(url, operation, user=None, auth=None, params=None, filename=None):
    if params is None:
        params = {}

    params["op"] = get_opstr(operation)
    if user:
        params["user.name"] = user

    if filename is None:
        return requests.put(url, auth=auth, verify=False, params=params)
    else:
        with file(filename) as f:
            return requests.put(url, data=f.read(), 
                                auth=auth, verify=False, params=params)


def Post(url, operation, filename, user=None, auth=None, params=None):
    if params is None:
        params = {}

    params["op"] = get_opstr(operation)
    if user:
        params["user.name"] = user

    with file(filename) as f:
        return requests.post(url, data=f.read(), 
                             auth=auth, verify=False, params=params)


def Delete(url, user=None, auth=None):
    params = {"op" : "DELETE"}

    if user:
        params["user.name"] = user

    return requests.delete(url, auth=auth, verify=False)


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
        resp = Put(self.weburl + filename, "cp", self.__user, 
                   filename=localfile, params={"overwrite" : "true"})
        if resp.status_code in (requests.codes.ok, requests.codes.created):
            return True
        else:
            if __debug__: print resp.status_code
            return False


    def append(self, localfile, filename) :
        resp = Post(self.weburl + filename, "append", localfile, self.__user)
        if resp.status_code == requests.codes.ok:
            return True
        else:
            if __debug__: print resp.status_code
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

        self.weburl = self.baseurl + "/gateway/" + schema + "/webhdfs/v1"

    @property
    def auth(self):
        return (self.__user, self.__password)

    def ls(self, dirname):
        resp = Get(self.weburl + dirname, "ls", auth=self.auth)

        if resp.status_code == requests.codes.ok:
            fs_list = resp.json()["FileStatuses"]["FileStatus"]
            return [self.gen_filespec(fs) for fs in fs_list]
        else:
            if __debug__: print resp.status_code
            return {}

    def cat(self, filename):
        resp = Get(self.weburl + filename, "cat", auth=self.auth)

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
    hdfs = Hdfs(host, "caiche")
    print hdfs.cat("/user/caiche/test.y")
    #print hdfs.mkdir("/user/caiche/test2")
    #print hdfs.stat("/user/caiche/test.1")
    #print hdfs.delete("/user/caiche/test.3")
    #print hdfs.rename("/user/caiche/test.4", "/user/caiche/test.t")
    print hdfs.append("run.cmd1", "/user/caiche/tttt.x")
    print "\n".join(hdfs.ls("/user/caiche"))

    #knox = Knox(host, "caiche", "caiche-password")
    #print "\n".join(knox.ls("/user/caiche"))
    #print knox.cat("/user/caiche/test.1")



#exports
__all__ = ('Hdfs', 'Knox')


import requests
import json
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


def Request(method, url, user=None, auth=None, params=None, 
                         data=None, headers=None, curl=False):
    if params is None:
        params = {}
    else:
        if isinstance(params, str):
            params = dict(p.split("=") for p in params.split("&"))

    if user:
        params["user.name"] = user

    paramstr = "&".join("%s=%s" % (k,v) for k,v in params.items())

    from urlparse import urlparse
    uri = urlparse(url)

    url = url + ("&" if len(uri.query) > 0 else "?") + paramstr

    if __debug__: print method, url, auth
    if curl:
        print "curl -X {method} {auth} {data} {url}".format(method=method,
               auth='' if auth is None else "-u '%s:%s'" % (auth),
               data='' if data is None else "-d '%s'" % data,
               url=url)

    return requests.request(method, url, auth=auth, verify=False, 
                            data=data, headers=headers)


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


    @staticmethod
    def Get(url, operation, user=None, auth=None, params={}):
        params["op"] = get_opstr(operation)

        return Request("GET", url, user, auth, params)


    @staticmethod
    def Delete(url, user=None, auth=None, params=None):
        params["op"] = "delete"
        return Request("DELETE", url, user, auth, params=params)


    @staticmethod
    def Put(url, operation, user=None, auth=None, params={}, data=None):
        params["op"] = get_opstr(operation)
        return Request("PUT", url, user, auth, params, data)


    @staticmethod
    def Post(url, operation, user=None, auth=None, params=None, data=None):
        params["op"] = get_opstr(operation)
        return Request("POST", url, user, auth, params, data)


    @property
    def weburl(self):
        return self.baseurl + "/webhdfs/v1"


    def ls(self, dirname):
        resp = Hdfs.Get(self.weburl + dirname, "ls", self.__user)
        if resp.status_code == requests.codes.ok:
            fs_list = resp.json()["FileStatuses"]["FileStatus"]
            return [self.gen_filespec(fs) for fs in fs_list]
        else:
            if __debug__: print resp.status_code


    def mkdir(self, dirname, perm="777") : 
        resp = Hdfs.Put(self.weburl + dirname, "mkdir", self.__user, 
                   params={"permission" : perm})
        if resp.status_code == requests.codes.ok:
            return resp.json()["boolean"]
        else:
            if __debug__: print resp.status_code


    def cp(self, localfile, filename) :
        with file(localfile) as f:
            resp = Hdfs.Put(self.weburl + filename, "cp", self.__user, 
                       params={"overwrite" : "true"}, data=f.read())

            if resp.status_code in (requests.codes.ok, requests.codes.created):
                return resp.json()
            else:
                if __debug__: print resp.status_code


    def append(self, localfile, filename) :
        with file(localfile) as f:
            resp = Hdfs.Post(self.weburl + filename, "append", self.__user,
                        data=f.read())

            if resp.status_code == requests.codes.ok:
                return resp.json()
            else:
                if __debug__: print resp.status_code, resp.text


    def delete(self, filename) : 
        resp = Hdfs.Delete(self.weburl + filename, self.__user)
        if resp.status_code == requests.codes.ok:
            return resp.json()
        else:
            if __debug__: print resp.status_code


    def stat(self, filename):
        resp = Hdfs.Get(self.weburl + filename, "stat", self.__user)
        if resp.status_code == requests.codes.ok:
            return self.gen_filespec(resp.json()["FileStatus"])
        else:
            if __debug__: print resp.status_code


    def cat(self, filename):
        resp = Hdfs.Get(self.weburl + filename, "cat", self.__user)
        if resp.status_code == requests.codes.ok:
            return resp.text
        else:
            if __debug__: print resp.status_code


    def rename(self, srcname, destname):
        resp = Hdfs.Put(self.weburl + srcname, "rename", self.__user,
                   params={"destination" : destname})
        if resp.status_code == requests.codes.ok:
            return resp.json()
        else:
            if __debug__: print resp.status_code


    def chmod(self, filename, perm="777"):
        resp = Hdfs.Put(self.weburl + filename, "chmod", self.__user,
                   params={"permission" : perm})
        if resp.status_code == requests.codes.ok:
            return resp.json()
        else:
            if __debug__: print resp.status_code


    def chown(self, filename, owner, group=None):
        resp = Hdfs.Put(self.weburl + filename, "chown", self.__user,
                   params={"owner" : owner,
                           "group" : group})
        if resp.status_code == requests.codes.ok:
            return resp.json()
        else:
            if __debug__: print resp.status_code


class HCat(HadoopUtil):
    def __init__(self, host, user):
        super(HCat, self).__init__("http", host, 50111)
        self.__user = user

    @staticmethod
    def Get(url, user=None, auth=None, params={}):
        return Request("GET", url, user, auth, params)


class Ranger(HadoopUtil):
    def __init__(self, host, user, password):
        super(Ranger, self).__init__("http", host, 6080)
        self.__user = user
        self.__password = password

        self.weburl = self.baseurl + "/service/public/api"
        self.repourl = self.weburl + "/repository"
        self.policyurl = self.weburl + "/policy"

    @property
    def auth(self):
        return (self.__user, self.__password)

    @staticmethod
    def Get(url, auth):
        return Request("GET", url, auth=auth)

    @staticmethod
    def Post(url, auth, data):
        return Request("POST", url, auth=auth, 
                       data=data, 
                       headers={"Content-Type" : "Application/json"})


    def get_repository(self, id=None):
        url = self.repourl if id is None else "%s/%d" % (self.repourl, id)

        resp = Ranger.Get(url, auth=self.auth)
        if resp.status_code == requests.codes.ok:
            return resp.json()
        else:
            if __debug__: print resp.status_code


    def get_policy(self, id=None):
        url = self.policyurl if id is None else "%s/%d" % (self.policyurl, id)

        resp = Ranger.Get(url, auth=self.auth)
        if resp.status_code == requests.codes.ok:
            return resp.json()
        else:
            if __debug__: print resp.status_code

    def create_hdfs_policy(self, repo_name, policy_name, filename, 
                           perm_map_list,
                           description="", is_enabled=True, 
                           is_recursive=True, is_audit_enabled=True):
        datadict = {
            "policyName"     : policy_name,
            "resourceName"   : filename,
            "description"    : description,
            "repositoryName" : repo_name,
            "repositoryType" : "hdfs",
            "isEnabled"      : is_enabled,
            "isRecursive"    : is_recursive,
            "isAuditEnabled" : is_audit_enabled,
            "permMapList"    : perm_map_list
        }
        resp = Ranger.Post(self.policyurl, 
                           auth=self.auth, 
                           data=json.dumps(datadict))
        if resp.status_code == requests.codes.ok:
            return resp.json()
        else:
            if __debug__: print resp.status_code, resp.text


class Knox(HadoopUtil):
    def __init__(self, host, user, password, topo="default"):
        super(Knox, self).__init__("https", host, 8443)
        self.__user = user
        self.__password = password

        self.weburl = self.baseurl + "/gateway/" + topo
        self.hdfsurl = self.weburl + "/webhdfs/v1"
        self.hcaturl = self.weburl + "/templeton/v1"

    @property
    def auth(self):
        return (self.__user, self.__password)

    def ls(self, dirname):
        resp = Hdfs.Get(self.hdfsurl + dirname, "ls", auth=self.auth)

        if resp.status_code == requests.codes.ok:
            fs_list = resp.json()["FileStatuses"]["FileStatus"]
            return [self.gen_filespec(fs) for fs in fs_list]
        else:
            if __debug__: print resp.status_code

    def cat(self, filename):
        resp = Hdfs.Get(self.hdfsurl + filename, "cat", auth=self.auth)

        if resp.status_code == requests.codes.ok:
            return resp.text
        else:
            if __debug__: print resp.status_code


    def get_database(self, dbname=None):
        url = self.hcaturl + "/ddl/database"
        if not dbname is None:
            url += "/" + dbname

        resp = HCat.Get(url, auth=self.auth)

        if resp.status_code == requests.codes.ok:
            return resp.json()
        else:
            if __debug__: print resp.status_code


    def get_table(self, dbname, tablename=None):
        url = self.hcaturl + "/ddl/database/%s/table" % dbname 
        if not tablename is None:
            url += "/" + tablename

        resp = HCat.Get(url, auth=self.auth)

        if resp.status_code == requests.codes.ok:
            return resp.json()
        else:
            if __debug__: print resp.status_code


#
# ---- main ----
if __name__ == "__main__":
    import sys

    host = sys.argv[1] if len(sys.argv) > 1 else "localhost"
    user = sys.argv[2] if len(sys.argv) > 2 else "caiche"
    user_pass = sys.argv[3] if len(sys.argv) > 3 else "caiche-password"
    #hdfs = Hdfs(host, user)
    #print "\n".join(hdfs.ls("/user/caiche"))
    #print hdfs.mkdir("/user/caiche/test3")
    #print hdfs.stat("/user/caiche/test.1")
    #print hdfs.delete("/user/caiche/test.3")
    #print hdfs.rename("/user/caiche/test.4", "/user/caiche/test.t")
    #print hdfs.append("run.cmd1", "/user/caiche/test.x")
    #print "\n".join(hdfs.ls("/user/caiche"))

    #knox = Knox(host, user, user_pass)
    #print "\n".join(knox.ls("/user/caiche"))
    #print knox.cat("/user/caiche/test.1")
    #print json.dumps(knox.get_database(), indent=4)
    #print json.dumps(knox.get_database("default"), indent=4)
    #print json.dumps(knox.get_table("default"), indent=4)
    #print json.dumps(knox.get_table("default", "u_data"), indent=4)

    ranger = Ranger(host, user, user_pass)
    print json.dumps(ranger.get_repository(), indent=4)
    #print json.dumps(ranger.get_repository(6), indent=4)
    #print json.dumps(ranger.get_policy(), indent=4)
    #print json.dumps(ranger.get_policy(13), indent=4)
    #print ranger.create_hdfs_policy("hdp230_2_hadoop", 
    #                                "hdfs-test-3", 
    #                                "/user/caiche",
    #                                [{"userList" : ["caiche"],
    #                                  "groupList" : [],
    #                                  "permList" : ["Read", "Write", "Execute"]
    #                                 },
    #                                 {"userList" : ["guest"],
    #                                  "groupList" : [],
    #                                  "permList" : ["Read", "Execute"]
    #                                 }])


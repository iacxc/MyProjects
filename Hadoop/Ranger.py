

#exports
__all__ = ('Ranger',)

from HadoopUtil import HadoopUtil, Request

STATUS_OK = 200
STATUS_NOCONTENT = 204

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
    def Delete(url, auth):
        return Request("DELETE", url, auth=auth)

    @staticmethod
    def Post(url, auth, data):
        print url, auth, data
        return Request("POST", url, auth=auth, 
                       data=data, 
                       headers={"Content-Type" : "Application/json"})


    def get_repository(self, id=None):
        url = self.repourl if id is None else "%s/%s" % (self.repourl, id)

        resp = Ranger.Get(url, auth=self.auth)
        if resp.status_code == STATUS_OK:
            return resp.json()
        else:
            if __debug__: print resp.status_code


    def get_policy(self, id=None):
        url = self.policyurl if id is None else "%s/%s" % (self.policyurl, id)

        resp = Ranger.Get(url, auth=self.auth)
        if resp.status_code == STATUS_OK:
            return resp.json()
        else:
            if __debug__: print resp.status_code

    def delete_policy(self, id):
        resp = Ranger.Delete("%s/%s" % (self.policyurl, id), auth=self.auth)
        if resp.status_code == STATUS_NOCONTENT:
            return {"status": "ok"}
        else:
            if __debug__: print resp.status_code


    def create_hdfs_policy(self, repo_name, policy_name, filename, 
                           perm_map_list,
                           description="", is_enabled=True, 
                           is_recursive=True, is_audit_enabled=True):
        if isinstance(perm_map_list, str):
            perm_map_list = json.loads(perm_map_list)

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
        data=json.dumps(datadict)
        resp = Ranger.Post(self.policyurl, 
                           auth=self.auth, 
                           data=data)
        if resp.status_code == STATUS_OK:
            return resp.json()
        else:
            if __debug__: print resp.status_code, resp.text


#
# ---- main ----
if __name__ == "__main__":
    import sys
    import json
    from optparse import OptionParser

    parser = OptionParser()
    parser.add_option("--host", default="localhost")
    parser.add_option("-u", "--user", default="caiche")
    parser.add_option("-p", "--password", default="caiche-password")

    opts, args = parser.parse_args()

    if len(args) < 1:
        print "Missing arguments"
        sys.exit(1)

    ranger = Ranger(opts.host, opts.user, opts.password)

    try:
        fun = getattr(ranger, args[0])
        print json.dumps(fun(*args[1:]), indent=4)

    except AttributeError:
        print "Unsupport method \"%s\"" % args[0]

#    print ranger.create_hdfs_policy("hdp230_2_hadoop", 
#                                    "hdfs-test-3", 
#                                    "/user/caiche",
#                                    [{"userList" : ["caiche"],
#                                      "groupList" : [],
#                                      "permList" : ["Read", "Write", "Execute"]
#                                     },
#                                     {"userList" : ["guest"],
#                                      "groupList" : [],
#                                      "permList" : ["Read", "Execute"]
#                                     }])




#exports
__all__ = ('Ranger',)

from HadoopUtil import HadoopUtil, Request, \
                       STATUS_OK, STATUS_NOCONTENT 

class Ranger(HadoopUtil):
    operations = ("get_repository",
                  "get_policy", "delete_policy", "create_policy",)
    rootpath = "/service/public/api"

    def __init__(self, host, user, password, curl_out):
        super(Ranger, self).__init__("http", host, 6080)
        self.__user = user
        self.__password = password
        self.__curl = curl_out

        self.weburl = self.baseurl + self.rootpath
        self.repourl = self.weburl + "/repository"
        self.policyurl = self.weburl + "/policy"

    def __iter__(self):
        return iter(self.operations)


    @property
    def auth(self):
        return (self.__user, self.__password)

    @staticmethod
    def Get(url, auth, params=None, curl=False):
        return Request("GET", url, auth=auth, params=params, curl=curl)

    @staticmethod
    def Delete(url, auth, curl=False):
        return Request("DELETE", url, auth=auth, curl=curl)

    @staticmethod
    def Post(url, auth, data, curl=False):
        return Request("POST", url, auth=auth, 
                       data=data, 
                       headers={"Content-Type" : "Application/json"}, curl=curl)

    @staticmethod
    def Put(url, auth, data, curl=False):
        return Request("PUT", url, auth=auth, 
                       data=data, 
                       headers={"Content-Type" : "Application/json"}, curl=curl)


    def get_repository(self, service_id=None):
        url = self.repourl if service_id is None \
                           else "%s/%s" % (self.repourl, service_id)

        resp = Ranger.Get(url, auth=self.auth, curl=self.__curl)
        if resp.status_code == STATUS_OK:
            return resp.json()
        else:
            if __debug__: print resp.status_code


    def get_policy(self, policy_id=None, params=None):
        url = self.policyurl if policy_id in (None, "None") \
                             else "%s/%s" % (self.policyurl, policy_id)

        resp = Ranger.Get(url, auth=self.auth, params=params, curl=self.__curl)
        if resp.status_code == STATUS_OK:
            return resp.json()
        else:
            if __debug__: print resp.status_code


    def delete_policy(self, policy_id):
        resp = Ranger.Delete("%s/%s" % (self.policyurl, policy_id), 
                             auth=self.auth, 
                             curl=self.__curl)
        if resp.status_code == STATUS_NOCONTENT:
            return {"status": "ok"}
        else:
            if __debug__: print resp.status_code


    def create_policy(self, service_type, service_name, policy_name, 
                            policy_data):
        dispatcher = getattr(self, "create_%s_policy" % service_type)
        return dispatcher(service_name, policy_name, policy_data)


    def update_policy(self, policy_id, policy_data):
        if isinstance(policy_data, str):
            policy_data = json.loads(policy_data)

        if __debug__:
            print json.dumps(policy_data, indent=4)

        resp = Ranger.Put("%s/%s" % (self.policyurl, policy_id), 
                           auth=self.auth, 
                           data=json.dumps(policy_data), 
                           curl=self.__curl)
        if resp.status_code == STATUS_OK:
            return resp.json()
        else:
            if __debug__: print resp.status_code, resp.text


    def create_hdfs_policy(self, service_name, policy_name, policy_data):
        if isinstance(policy_data, str):
            policy_data = json.loads(policy_data)

        policy_data.update(policyName=policy_name,
                           repositoryName=service_name,
                           repositoryType="hdfs")
        policy_data.setdefault("isEnabled"     , True)
        policy_data.setdefault("isRecursive"   , True)
        policy_data.setdefault("isAuditEnabled", True)
    
        resp = Ranger.Post(self.policyurl, 
                           auth=self.auth, 
                           data=json.dumps(policy_data),
                           curl=self.__curl)
        if resp.status_code == STATUS_OK:
            return resp.text
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
    parser.add_option("--curl", action="store_true", default=False)
    parser.add_option("-u", "--user", default="caiche")
    parser.add_option("-p", "--password", default="caiche-password")

    opts, args = parser.parse_args()

    if __debug__: print opts, args

    if len(args) < 1:
        print "Missing arguments, supported methods are", Ranger.operations
        sys.exit(1)

    ranger = Ranger(opts.host, opts.user, opts.password, opts.curl)

    method = args[0]
    if not method in ranger:
        print "Unsupported method '%s'" % method
        sys.exit(1)

    try:
        fun = getattr(ranger, method)
        print json.dumps(fun(*args[1:]), indent=4)

    except AttributeError as e:
        print e


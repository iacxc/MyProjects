

#exports
__all__ = ("HadoopUtil", "Request",
           "get_opstr", "gen_filespec",
           "STATUS_OK",
           "STATUS_CREATED",
           "STATUS_NOCONTENT",
           )


import requests
import json

from requests.packages.urllib3.exceptions import InsecureRequestWarning
requests.packages.urllib3.disable_warnings(InsecureRequestWarning)


STATUS_OK = requests.codes.ok
STATUS_CREATED = requests.codes.created
STATUS_NOCONTENT = requests.codes.no_content


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


def getpermission(permission):
    getbit = lambda bit: {'0' : '---',
                       '1' : '--x',
                       '2' : '-w-',
                       '3' : '-wx',
                       '4' : 'r--',
                       '5' : 'r-x',
                       '6' : 'rw-',
                       '7' : 'rwx'}.get(bit, '---')
    return ''.join(map(getbit, permission))


def gen_filespec(fs):
    from datetime import datetime
    ts2str = lambda timestamp: \
           datetime.fromtimestamp(timestamp * 0.001).strftime("%m %d %H:%M")

    return "%s%-10s  %-8s%-8s%-6s%-12s%-20s" % (
         'd' if fs.get("type", "FILE") == "DIRECTORY" else '-',
         getpermission(fs.get("permission", "000")),
         fs.get("owner", "<no user>"),
         fs.get("group", "<no group>"),
         fs.get("length", 0),
         ts2str(fs.get("modificationTime", 0)),
         fs.get("pathSuffix", "<no name>"))


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

    if curl:
        print "curl -X {method}{auth}{data}{url}".format(method=method,
               auth='' if auth is None else " -u '%s:%s'" % (auth),
               data='' if data is None else " -d '%s'" % data,
               url=" '%s'" % url)

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



#
# ---- main ----
if __name__ == "__main__":
    print "HadoopUtil"

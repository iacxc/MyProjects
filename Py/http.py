
import httplib, urllib

from urlparse import urlparse

def make_request(method, url, data=None, headers=None,
                timeout=60):
    """ called by get, post """
    o = urlparse(url)
    conn = httplib.HTTPConnection(o.netloc)

    if o.query:
        path = '%s?%s' % (o.path, o.query)
    else:
        path = '%s' % o.path

    if data is None:
        data = ''

    if headers is None:
        headers = {}

    conn.request(method.upper(), path, body=data, headers=headers)

    return conn.getresponse()


def http_get(url, headers=None):
    """ get """
    return make_request('GET', url, headers=headers)


def http_post(url, data=None, headers=None):
    """ post """
    if not data is None:
        data = urllib.urlencode(data)

    return make_request('POST', url, data=data, headers=headers)


# $Id: const.py 223 2013-05-29 11:14:36Z chengxin.cai $

""" a module provide the const """

class _const(object):
    class ConstError(TypeError): pass
    def __setattr__(self, name, value):
        if name in self.__dict__:
            raise self.ConstError, "Can't rebind const(%s)" % name
        self.__dict__[name] = value

    def __delattr__(self, name):
        if name in self.__dict__:
            raise self.ConstError, "Can't unbind const(%s)" % name
        raise NameError, name 

import sys
sys.modules[__name__] = _const()


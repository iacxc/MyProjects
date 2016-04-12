
import os, sys
import time

from seaquest import *

import const
const.SPROOT = os.environ.get('MY_SPROOT')

if const.SPROOT is None:
    raise EnvironmentError('MY_SPROOT not defined')

__all__ = ('TimedCheck', 'IdleCheck', 'AdaptiveCheck')


def get_default_log():
    basename = os.path.basename(sys.argv[0])

    return '%s/logs/%s.$pid.log' % (const.SPROOT,
                              os.path.splitext(basename)[0])


class ProActiveCheck(object):
    
    def __init__(self):
        from optparse import OptionParser

        self.__optparser = OptionParser()
        self.__optparser.add_option('-l', '--log-file', dest='log',
             action='store', default=get_default_log(),
             help='path for log file, default [%default]')

        self.__opts = None
        self.__args = None
        self.__logger = None

    def add_opt(self, *args, **kws):
        self.__optparser.add_option(*args, **kws)


    def get_opts(self):
        self.__opts, self.__args = self.__optparser.parse_args()


    def init_log(self):
        import logging

        #get the real log filename, replace the $pid with real pid
        if self.__opts is None:
            log_file = get_default_log().replace('$pid', str(os.getpid()))
        else:
            log_file = self.__opts.log.replace('$pid', str(os.getpid()))

        self.__logger = logging.getLogger()
        if __debug__:
            self.__logger.setLevel(logging.NOTSET)
        else:
            self.__logger.setLevel(logging.WARNING)

        handler = logging.FileHandler(log_file)
        formatter = logging.Formatter('%(asctime)s %(levelname)s %(message)s')

        handler.setFormatter(formatter)

        self.__logger.addHandler(handler)


    def log_debug(self, msg):
        if self.__logger is None: self.init_log()
        self.__logger.debug(msg)

    def log_info(self, msg):
        if self.__logger is None: self.init_log()
        self.__logger.info(msg)

    def log_warning(self, msg):
        if self.__logger is None: self.init_log()
        self.__logger.warning(msg)

    def log_error(self, msg):
        if self.__logger is None: self.init_log()
        self.__logger.error(msg)


class TimedCheck(ProActiveCheck):
    def __init__(self, interval):
        super(TimedCheck, self).__init__()

        self.__interval = interval


    def run(self, *args, **kws):
        t_start = time.time()
        while True:
            self.check(*args, **kws)

            t_end = time.time()
            t_delta = t_end - t_start
            t_start += self.__interval

            #remove the time drift, but if the check runs longer than the
            #predefined interval, just continue
            if t_delta < self.__interval:
                time.sleep(self.__interval - t_delta)


class IdleCheck(ProActiveCheck):
    def __init__(self, block_time):
        super(IdleCheck, self).__init__()

        self.__block_time = block_time


    def run(self, *args, **kws):
        while True:
            if self.is_systemidle():
                self.check(*args, **kws)
            else:
                if __debug__: print 'Blocked'
                time.sleep(self.__block_time)


class AdaptiveCheck(ProActiveCheck):
    def __init__(self, min_int=0, max_int=1800, speed=1.1,
                       init_interval=None, block_time=5):
        super(AdaptiveCheck, self).__init__()

        self.__min_int = min_int
        self.__max_int = max_int
        self.__speed = speed

        if init_interval:
            self.__interval = init_interval
        else:
            self.__interval = (min_int + max_int) / 2

        self.__block_time = block_time


    def run(self, *args, **kws):
        pre_elapsed = 0

        while True:
            if self.block_it():
                if __debug__: print 'Blocked'
                time.sleep(self.__block_time)

            else:
                rc, elapsed = time_it('AdaptiveCheck', self.check, *args, **kws)

                if elapsed > pre_elapsed: #slower than before
                    self.__interval *= self.__speed
                elif elapsed < pre_elapsed: # faster
                    self.__interval /= self.__speed

                pre_elapsed = elapsed

                #make sure the interval is still between min_int and max_int
                self.__interval = max( self.__min_int,
                                       min(self.__interval, self.__max_int) )

                if __debug__:
                    print 'new interval', self.__interval

                time.sleep(self.__interval)


#-------- main ---------
if __name__ == '__main__':

    import random
#    class Fcheck(AdaptiveCheck):
#        def __init__(self):
#            super(Fcheck, self).__init__(0.1, 100, 1.1, 3, 2)
#
#        def block_it(self):
#            return random.randint(1,5) == 3
#
#    class Fcheck(TimedCheck):
#        def __init__(self):
#            super(Fcheck, self).__init__(2)

    class Fcheck(IdleCheck):
        def __init__(self):
            super(Fcheck, self).__init__(5)

        def is_systemidle(self):
            return random.randint(1,4) != 3

        def check(self, *args, **kws):
            seconds = int(random.random() * 10)
            print 'Fcheck is running, sleep ', seconds
            print args, kws
            time.sleep( seconds )

    checker = Fcheck()
    checker.run('test', a=5, b=3)


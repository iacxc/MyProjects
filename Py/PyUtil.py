
# $Id: PyUtil.py 333 2013-08-19 02:12:49Z chengxin.cai $

''' Python utilities
'''
__all__ = ('time_it')

import datetime

def time_it(title_, sub, *args, **kws):
    """Calling a function and show the start time, end time 
       and elapsed time for running it"""

    def format_time (time_):
        """format a time value"""
        return time_.strftime('%Y-%m-%d %H:%M:%S.%f')

    start_t = datetime.datetime.now()

    ret = sub(*args, **kws)

    end_t = datetime.datetime.now()

    delta = end_t - start_t

    if __debug__:
        print '[%s] start at %s, end at %s' \
           % (title_, format_time(start_t), format_time(end_t))

    print '[%s] elapsed: %.4f seconds' \
           % (title_, delta.seconds + 1e-6 * delta.microseconds)

    return ret


if __name__ == '__main__':
    print('PyUtils')

    import time

    atitle = 'Sleep 3'
    print('Running "%s"' % atitle)
    time_it(atitle, time.sleep, 3)


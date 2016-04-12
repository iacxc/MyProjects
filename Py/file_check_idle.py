#!/usr/bin/python -O

import time
from seaquest import *
from proactive_chk import *

class Fcheck_item(object):
"""a helper class, to store the information of one file 
"""
    def __init__(self, name, ts_modified, full_path, size, ts_fcheck=0):
        self.__name = name
        self.__ts_modified = ts_modified
        self.__full_path = full_path
        self.__size = size
        self.__ts_fcheck = ts_fcheck


    def __repr__(self):
        return '%s: <%s, %s, %s, %s>' % (self.__name, self.__full_path, 
                                 self.__size, self.__ts_modified, 
                                 self.__ts_fcheck)


    @property
    def name(self):
        return self.__name

    @property
    def full_path(self):
        return self.__full_path

    @property
    def volume(self):
        return self.__name.split('.')[0]

    @property
    def subvolume(self):
        return self.__name.split('.')[1]

    @property
    def file(self):
        return self.__name.split('.')[2]

    @property
    def modified(self):
        return self.__ts_modified


    def is_empty(self):
        return self.__size == 0


    def is_dirty(self):
        return self.__ts_modified > self.__ts_fcheck


    def set_modified(self, ts_modified):
        self.__ts_modified = ts_modified

    def set_fcheck(self, ts_fcheck):
        self.__ts_fcheck = ts_fcheck




class File_Check(IdleCheck):
"""the check class to wrap fcheck
"""
    def __init__(self, block_time=1):
        super(File_Check, self).__init__(block_time)

        self.__files = {}
        self.__queue = []


    def is_systemidle(self):
        #calculate the load by average the load for last 1,5,15 minutes
        load_1, load_5, load_15 = get_loadavg()
        load = load_1 * 0.7 + load_5 * 0.2 + load_15 * 0.1

        #load equals the number of process running or waiting to run
        #if the number is less than cpu number, it means some cpus are free
        return load < get_cpu_number()


    def update_files(self):
        """this method maintains the file dict and the priority queue
        """
        import operator

        for file_item in reduce( operator.add,
                                 map(get_volume_files, get_local_volumes()) ):

            file_name = file_item[0]
            if file_name in self.__files:
                self.__files[file_name].set_modified( file_item[1] )
            else:
                self.__files[file_name] = Fcheck_item( *file_item )

        #get the list of file name needs to check(no empty, not checked yet)
        self.__queue = [ f for f in self.__files.keys()
                                 if not self.__files[f].is_empty()
                                    and self.__files[f].is_dirty() ]

        #sort the list by modified time, the latest to be the last,
        #since we will use pop to get a file to check
        self.__queue.sort( key=lambda fname: self.__files[fname].modified)

        if __debug__:
            for k in self.__queue:
                v = self.__files[k]
                print k, '\n    ', v


    def block_it(self):
        return False


    def check(self):
        from commands import getoutput
        import re

        rx_FatalError = re.compile('Fatal Error')

        if len(self.__queue) > 0:
            fname = self.__queue.pop()

            cmd = 'fcheck -file \\$%s'  % fname
            self.log_info(cmd)

            cmdouts = getoutput(cmd).split('\n')

            errorlines = filter(rx_FatalError.search, cmdouts)
            if len(errorlines) > 0:
                cmd = 'event_send --component-id %d --event-id %d --event-severity %d --event-text-string %s' % (cid, eid, esid, ''.join(errorlines))
                getoutput(cmd)
                self.log_error(''.join(errorlines))
                print 'Fatal Errors'

            #mark that file as checked
            self.__files[fname].set_fcheck(time.time())
        else:
            print 'All files are clean, refreshing'
            check.update_files()

        if __debug__:
            time.sleep(1)

#------------ main ---------------
if __name__ == '__main__':
    check = File_Check()
#    check.get_opts()
#    check.update_files()

    check.log_info('Started')

    try:
        check.run()
    except KeyboardInterrupt:
        print 'User interrupt, exit...'




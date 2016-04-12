
# $Id: dbcli.py 335 2013-08-19 03:08:36Z chengxin.cai $

from subprocess import Popen, PIPE, STDOUT
import time


class DBCli(object):

    def __init__(self, args, output=PIPE):
        if __debug__: print args
        self.cli = Popen(args, stdin=PIPE, stdout=output, stderr=STDOUT,
                         close_fds=True)


    @property
    def prompt(self):
        return 'Ready>'


    def execute(self, cmdlist, bPrompt=None):

        for cmd in cmdlist:
            if __debug__: print '   MYSQL:', cmd

            self.cli.stdin.write('%s;\n' % cmd)

        if bPrompt:
            self.cli.stdin.write('\\echo %s\n' % self.prompt)

        self.cli.stdin.flush()

        lines = []
        if self.cli.stdout:
            while True: 
                line = self.cli.stdout.readline().strip()
                if __debug__: print line

                if line == self.prompt: break
                lines.append(line)

        return lines

    def close(self):
        self.cli.stdin.write('\\q\n')

        return self.cli.communicate()


class VerticaCli(DBCli):

    def __init__(self, executable = '/opt/vertica/bin/vsql', 
                       host=None, uid=None, passwd=None):
        args = [executable]
        if host: args.extend(['-h', host])

        if uid: args.extend(['-U', uid])

        if passwd: args.extend(['-w', passwd])

        super(VerticaCli, self).__init__(args)


    def execute(self, cmdlist):
        return super(VerticaCli, self).execute(cmdlist, bPrompt=True)


class MySQLCli(DBCli):

    def __init__(self, executable='/usr/bin/mysql', 
                       host=None, uid=None, passwd=None, output=None):

        args = [executable]
        if host: args.append('--host=' + host)

        if uid: args.append('--user=' + uid)

        if passwd: args.append('--password=' + passwd)

        args.append('--prompt=' + self.prompt)

        super(MySQLCli, self).__init__(args, output)


    def execute(self, cmdlist):
        return super(MySQLCli, self).execute(cmdlist)


#-----------------------------------
if __name__ == '__main__':

    import sys
    if len(sys.argv) > 1:
        host = sys.argv[1]
    else:
        host = None

    mysql = MySQLCli(host=host, uid='seapilot', passwd='seapilot1')


    mysql.execute(['use mysql', 
                   'select user, host from user'])

    print mysql.close()

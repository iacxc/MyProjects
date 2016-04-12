#!/usr/bin/python

"""
   This script will setup the passwordless for ssh
"""

import pexpect
import getpass

def get_password(user, host):
    password = getpass.getpass('Please input password for %s: ' % user)

    host = host.replace(',', '')

    if __debug__: print 'Try using "%s" on host %s ...' % (password, host)

    prompt = '.*%s.*\$ ' % user
    cmd = 'ssh -o StrictHostKeyChecking=no %s@%s' % (user, host)

    handle = pexpect.spawn(cmd, timeout=5.0)
    index = handle.expect(['.*[pP]assword:\s*', prompt, pexpect.EOF,
                           pexpect.TIMEOUT])

    if index == 0: # wait for password
        if __debug__: print '\tSending password ...'
        handle.sendline(password)
        if handle.expect([prompt, pexpect.EOF, pexpect.TIMEOUT]) != 0:
            print 'Wrong password, please input again.'
            return get_password(user, host)

    elif index != 1:
        print 'Host %s unavailable' % host
        return None

    return password 


def ssh_keygen(user, password, hosts):
    prompt = '.*%s.*\$ ' % user
    remote_cmd = 'rm -rf ~/.ssh; ssh-keygen -t dsa'
    sshkeys = []

    for host in hosts:
        host = host.replace(',', '')

        if __debug__: print 'Generating ssh keys for %s ...' % host

        cmd = 'ssh -o StrictHostKeyChecking=no %s@%s' % (user, host)
        handle = pexpect.spawn(cmd, timeout=5.0)
        index = handle.expect(['.*[pP]assword:\s*', prompt, pexpect.EOF,
                               pexpect.TIMEOUT])

        if index == 0: # wait for password
            if __debug__: print '\tSending password ...'
            handle.sendline(password)

            if handle.expect([prompt, pexpect.EOF, pexpect.TIMEOUT]) != 0:
                print 'Wrong password'
                return []

        elif index != 1:
            print 'Host %s unavailable' % host
            return [] 

        if __debug__: print '\tRunning ssh-keygen ...'
        handle.sendline(remote_cmd)

        for i in range(3):
            if handle.expect(['Enter.*', pexpect.EOF, pexpect.TIMEOUT]) != 0:
                print 'Unknow error while running ssh-keygen'
                return []
            else:
                handle.sendline('\n')

        if handle.expect([prompt, pexpect.EOF, pexpect.TIMEOUT]) != 0:
            print 'Unknow error while running ssh-keygen'
            return []

        if __debug__: print '\tRetriving public key ...'
        handle.sendline('cat ~/.ssh/id_dsa.pub')
        handle.expect(['.*%s@%s' % (user, host)])

        sshkeys.append(handle.after.split('\n')[-1])

        handle.sendline('exit')
        handle.expect([pexpect.EOF, pexpect.TIMEOUT])

    return sshkeys


def copy_auth(user, password, hosts):
    for host in hosts:
        host = host.replace(',', '')

        cmd = 'scp %s %s@%s:~/.ssh' % ('authorized_keys', user, host)

        if __debug__: print 'Copy auth file to %s ...' % host
        handle = pexpect.spawn(cmd, timeout=5.0)
        index = handle.expect(['.*[pP]assword:\s*', pexpect.EOF,
                               pexpect.TIMEOUT])

        if index == 0: # wait for password
            if __debug__: print '\tSending password ...'
            handle.sendline(password)

        handle.expect([pexpect.EOF, pexpect.TIMEOUT])

def chmod_auth(user, password, hosts):
    prompt = '.*%s.*\$ ' % user

    remote_cmd = ';'.join(['chmod 700 ~/.ssh',
                           'chmod 600 ~/.ssh/authorized_keys'])

    for host in hosts:
        host = host.replace(',', '')

        if __debug__: print 'Change the mode of auth file for %s ...' % host

        cmd = 'ssh -o StrictHostKeyChecking=no %s@%s' % (user, host)
        handle = pexpect.spawn(cmd, timeout=5.0)
        index = handle.expect(['.*[pP]assword:\s*', prompt, pexpect.EOF,
                               pexpect.TIMEOUT])

        if index == 0: # wait for password
            if __debug__: print '\tSending password ...'
            handle.sendline(password)
            if handle.expect([prompt, pexpect.EOF, pexpect.TIMEOUT]) != 0:
                print 'Wrong password'
                return

        elif index > 1:
            print 'Host %s unavailable' % host
            return

        if __debug__: print '\tChanging mode ...'
        handle.sendline(remote_cmd)
        handle.expect(prompt)

        handle.sendline('exit')
        handle.expect([pexpect.EOF, pexpect.TIMEOUT])

if __name__ == '__main__':
    import sys,os
    from optparse import OptionParser

    usage = 'Usage: [options] host1 [host2 host3 ...]'
    parser = OptionParser(usage=usage)
    parser.add_option('-u', '--username',  dest = 'username',
           action = 'store',
           help='username')

    (opts, args) = parser.parse_args()

    if not opts.username:
        print 'Please provide input for --username'
        sys.exit(1)

    if len(args) == 0:
       print 'No hosts to setup, exit'
       sys.exit(0)

    if os.getcwd().endswith('/.ssh'):
        print 'You could not run this script under the .ssh directory'
        sys.exit(2)

    password = get_password(opts.username, args[0])

    if not password:
        print 'Something wrong when getting password, exit...'
        sys.exit(3)

    keys = ssh_keygen(opts.username, password, args)

    if len(keys) == 0:
        print 'Something wrong when generating ssh key, exit...'
        sys.exit(4)

    with open('authorized_keys', 'w') as fh:
        for key in keys:
            fh.write(key + '\n')

    copy_auth(opts.username, password, args)

    chmod_auth(opts.username, password, args)

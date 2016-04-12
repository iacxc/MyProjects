
import glob
import Common


def find_distro():
    """ find the distribution of hadoop """

    dirs_to_search = {'horton' : '/var/run/ambari*',
                      'cloudera' : '/var/run/cloudera*',
                      'mapr' : '/etc/rc*/init*/*mapr-warden*'}

    for distro, path in dirs_to_search.items():
        if glob.glob(path):
            return distro

    return 'apache'


def get_version():
    """ get the hadoop version using 'hadoop version' """
    rc, output = Common.run_cmd('hadoop version')

    if rc == Common.SUCCESS:
        version_line = output.split('\n')[0]
        return version_line.split()[-1]
        
    else:
        return '0.0.0'


if __name__ == '__main__':
    print 'Hadoop distribution is {0}'.format(find_distro())
    print 'Hadoop version is {0}'.format(get_version())

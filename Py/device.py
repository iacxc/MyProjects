
# $Id: device.py 223 2013-05-29 11:14:36Z chengxin.cai $

""" receive a disk/mpath/uuid name, get all the information for that disk """

import os
import re
from commands import getoutput

CMD = { 'multipath' : '/sbin/multipath',
        'lsscsi'    : '/usr/bin/lsscsi',
        'fdisk'     : '/sbin/fdisk',
        'sg_inq'    : '/usr/bin/sg_inq'
}

def get_disk_from_mpathlines (mpathlines):
    """ docstring """
    for line in mpathlines:
        match = re.search('sd[a-z]+', line)

        if match: 
            return '/dev/' +  match.group()

class Device:
    """ docstring """
    def __init__(self, param):

        cmdout = getoutput( CMD['multipath'] + ' -ll ' + param )
        mpathlines = cmdout.split('\n')
        fields = mpathlines[0].split()

        if len(fields) == 0:
            raise(Exception('Invalid parameter %s' % param))

        if param.startswith('/dev/'):
            self._disk = param
            self._mpath = fields[0]
            self._uuid = fields[1].replace('(', '').replace(')', '')
        elif param.startswith('mpath'):
            self._disk = get_disk_from_mpathlines(mpathlines)
            self._mpath = param
            self._uuid = fields[1].replace('(', '').replace(')', '')
        elif param.startswith('3600'):
            self._disk  = get_disk_from_mpathlines(mpathlines)
            self._mpath = fields[0]
            self._uuid  = param
        else:
            raise(Exception('Invalid parameter %s' % param))

        self._dm = fields[2]

        cmdout = getoutput( CMD['sg_inq'] + ' -p0x83 ' + self._disk )
        match = re.search( '\[0x([0-9a-f]{16})\]', cmdout )

        self._wwn = match.group(1) if match else ''

        lunline = getoutput( CMD['lsscsi'] + '| grep -w ' + self._disk )
        match = re.search('([0-9]+):([0-9]+):([0-9]+):([0-9]+)', lunline)
        self._lun = match.group(4) if match else 0

        if self._wwn:
            msafile = getoutput( 'grep -l ' + self._wwn + \
                                 ' /home/lunmgr/msa_xml/*' )
            self._msa = os.path.basename(msafile).replace('.xml', '')
        else:
            self._msa = 'Not found'

        #get partitions
        cmdouts = getoutput( CMD['fdisk'] + ' -l ' + self._disk \
                             + '| grep ^' + self._disk ).split('\n')

        self._parts = [ line.split()[0] for line in cmdouts if line ]

    def __repr__(self):
        return '{\n' + \
             '    "disk"  : "%s",\n' % self._disk + \
             '    "mpath" : "%s",\n' % self._mpath + \
             '    "uuid"  : "%s",\n' % self._uuid + \
             '    "dm"    : "%s",\n' % self._dm + \
             '    "wwn"   : "%s",\n' % self._wwn + \
             '    "lun"   : "%s",\n' % self._lun + \
             '    "msa"   : "%s",\n' % self._msa + \
             '    "parts" : ["%s"]\n' % ','.join(self._parts) + \
             '}\n'


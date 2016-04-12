
# $Id: dsaconf.py 337 2013-08-22 08:33:39Z chengxin.cai $

''' Classes and functions to represent the config file of DSA
'''

from Amqp.RoutingKey import RoutingKey
from SeaPilot import log_debug, log_info
from protomsg import GpbMessage


__all__ = ('PublicationMap',
           'ColumnMapping',
          )

class ColumnMapping(object):
    def __init__(self, elem_colmap):
        self.__amqp_colname = elem_colmap.findtext('amqpColumnName')
        self.__repo_colname = elem_colmap.findtext('repositoryColumnName')

        self.__transform_expr = elem_colmap.findtext('transformExpression')
        self.__function = elem_colmap.findtext('function')


    def __str__(self):
        return '{0}: {1}'.format(self.__repo_colname, self.value)


    @property
    def amqp_column(self):
        return self.__amqp_colname

    @property
    def repo_column(self):
        return self.__repo_colname

    @property
    def transform_expr(self):
        return self.__transform_expr

    @property
    def function(self):
        return self.__function

    @property
    def value(self):
        return self.__transform_expr if self.__transform_expr else '?'


class PublicationMap(object):
    def __init__(self, elem_root):
        self.__routing_key = RoutingKey(elem_root.findtext('amqpRoutingKey'))
        self.__table = elem_root.findtext('repositoryTableName').lower()

        self.__colmaps = [ColumnMapping(elem)
                             for elem in elem_root.findall('columnMapping')]

        self.__pmsg = None
        self.__insert_stmt = ''


    @property
    def routing_key(self):
        return str(self.__routing_key)

    @property
    def table(self):
        return self.__table

    @property
    def column_mappings(self):
        return self.__colmaps


    def preprocess(self):
        routing_key = self.__routing_key
        if __debug__:
            print 'Preprocess for', routing_key
        self.__pmsg = GpbMessage(routing_key.package, routing_key.publication)

        fields = []
        values = []
        for colmap in self.__colmaps:
            fields.append(colmap.repo_column)
            values.append(colmap.value)

        self.__insert_stmt = \
              'INSERT INTO {0} ({1}) VALUES ({2})'.format(self.__table,
                           ','.join(fields),
                           ','.join(values))
        log_debug('Generating insert statement: {0}'.format(self.__insert_stmt))


    def do_insert(self, message):
        from google.protobuf.message import DecodeError
        try:
            self.__pmsg.loads(message.content)
            import pdb
            pdb.set_trace()
#           log_debug('Got message: {0}'.format(self.__pmsg))
            log_debug(self.__pmsg.topo())
            log_debug(self.__pmsg.dumpjson())
            log_debug(self.__pmsg.dumptabs())

        except DecodeError:
            print 'Decode error for key: %s' % self.routing_key


def get_publication_maps(xml_file):
    import xml.etree.ElementTree as ET

    xml = ET.parse(xml_file)
    xml_root = xml.getroot()
    return [PublicationMap(elem)
                for elem in xml_root.findall('publicationMap')]



if __name__ == '__main__':
    import sys
    import xml.etree.ElementTree as ET

    if len(sys.argv) < 2: sys.exit(1)

    p_maps = get_publication_maps(sys.argv[1])

    for p_map in p_maps:
        print p_map.routing_key
        print p_map.table
        for colmap in p_map.column_mappings:
            print '   ', colmap


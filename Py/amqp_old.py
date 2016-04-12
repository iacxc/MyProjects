
# $Id: amqp.py 325 2013-08-15 05:44:10Z chengxin.cai $

__all__ = ('RoutingKey', 'Sender', 'Receiver')

class M(type):
    def __new__(mcl, cName, cBases, cDict):
        for attr in cDict.get('__slots__', ()):
            if attr.startswith('__'):
                def getter(self, attr=attr):
                    return getattr(self, ''.join(['_', cName, attr]))

                cDict[attr[2:]] = property(getter)

        return type.__new__(mcl, cName, cBases, cDict)

import os,sys

from qpid.connection import Connection
from qpid.datatypes import Message, RangedSet, uuid4
import qpid.util

class RoutingKey(object):
    __metaclass__ = M
    __slots__ = ( '__category', '__package', '__scope',
                  '__security', '__protocol', '__publication' )

    def __init__(self, key = None):
        if key:
           self._set(key)

    def _set(self, key):
        items = key.split('.')
        if len(items) == 6:
        # category, package, scope, security, protocol and publication
            self.__category = items[0]
            self.__package  = items[1]
            self.__scope    = items[2]
            self.__security = items[3]
            self.__protocol = items[4]
            self.__publication = items[5]

        elif len(items) == 5:
        # category, package, scope, security and publication
            self.__category = items[0]
            self.__package  = items[1]
            self.__scope    = items[2]
            self.__security = items[3]
            self.__protocol = 'gpb'
            self.__publication = items[4]

        elif len(items) == 4:
        # package, scope, security and publication
            self.__category = 'health_state'
            self.__package  = items[0]
            self.__scope    = items[1]
            self.__security = items[2]
            self.__protocol = 'gpb'
            self.__publication = items[3]

        elif len(items) == 3:
        # category, package and publication
            self.__category = items[0]
            self.__package  = items[1]
            self.__scope    = 'instance'
            self.__security = 'public'
            self.__protocol = 'gpb'
            self.__publication = items[2]

        elif len(items) == 2:
        # package and publication
            self.__category = 'health_state'
            self.__package  = items[0]
            self.__scope    = 'instance'
            self.__security = 'public'
            self.__protocol = 'gpb'
            self.__publication = items[1]

        else:
            raise TypeError('field number mismatch in %s' % key)

        if self.__protocol != 'gpb':
            raise TypeError('Protocol error %s' % key)

    @property
    def message(self):
        return '.'.join([self.__package, self.__publication])

    @property
    def protofile(self):
        return '.'.join([self.__package, self.__publication, 'proto'])

    @property
    def subscribe_str(self):
        return '#.%s.#.%s' % (self.__package, self.__publication)

    def __repr__(self):
        return '.'.join([self.__category, self.__package, self.__scope,
                       self.__security, self.__protocol, self.__publication])

class Sender(object):

    __slots__ = ('__conn', '__session')
    __exchange = 'amq.topic'

    def __init__(self, host='localhost', port=5672, timeout=5):

        self.__conn = None
        qpid.util.socket.setdefaulttimeout(timeout)
        
        if __debug__:
            print 'Connecting to %s:%d' % (host, port)

        sock = qpid.util.connect(host, port)
        self.__conn = Connection(sock=sock)
        self.__conn.start()
        self.__session = self.__conn.session(str(uuid4()))

    def __del__(self):
        if self.__conn: self.__conn.close()

    def send(self, routing_key, msg_text):

        propDelivery = self.__session.delivery_properties(
                       routing_key = routing_key.__str__(),
                       exchange = self.__exchange )
        contentType = 'application/x-protobuf'
        propMessage = self.__session.message_properties(
                          content_type = contentType)

        self.__session.message_transfer(destination = self.__exchange,
                  message = Message(propDelivery, propMessage, msg_text))

class Receiver(object):
    __slots__ = ('__conn', '__session', '__queue')
    __exchange = 'amq.topic'

    def __init__(self, host='localhost', port=5672, timeout=5):

        self.__conn = self.__session = None

        if __debug__:
            print 'Connecting to %s:%d' % (host, port)
            
        qpid.util.socket.setdefaulttimeout(timeout)
        sock = qpid.util.connect(host, port)
        self.__conn = Connection(sock=sock)
        self.__conn.start()
        self.__session = self.__conn.session(str(uuid4()))

    def __del__(self):
        if self.__session: self.__session.close()
        if self.__conn: self.__conn.close()

    def setup(self, binding_keys, queue_name = 'queue_%d' % os.getpid()):

        # Setup queue
        self.__session.queue_declare(queue = queue_name, auto_delete = True,
                                     exclusive = False)

        for k in binding_keys:
            if __debug__:
                print 'Binding', k

            self.__session.exchange_bind(exchange = self.__exchange,
                queue = queue_name, binding_key = k)

        localQueueName = 'local_queue_%d' % os.getpid()
        self.__queue = self.__session.incoming(localQueueName)

        # Route messages from message_queue to my_local_queue
        self.__session.message_subscribe(queue = queue_name,
                                        destination = localQueueName)
        if __debug__:
            print 'Queue info:', self.__session.queue_query(queue_name)

        self.__queue.start()
        return self

    def fetch(self, timeout=None):
        message = self.__queue.get(timeout=timeout)
        self.__session.message_accept(RangedSet(message.id))

        return message

if __name__ == '__main__':

    import sys

    key = RoutingKey('vertica.event_query')
    print key

    host = 'localhost' if len(sys.argv) < 2 else sys.argv[1]
    port = 5672 if len(sys.argv) < 3 else int(sys.argv[2])

    sender = Sender(host, port)
    receiver = Receiver(host, port).setup(binding_keys=[key.subscribe_str])

    msgstr = 'Hello World'
    print 'Sending message %s:(%s)' % (key, msgstr)
    sender.send(key, msgstr)

    import Queue
    try:
        msg = receiver.fetch(10)
    except Queue.Empty, e:
        print 'No message', e
    else:
        print 'Got message', msg

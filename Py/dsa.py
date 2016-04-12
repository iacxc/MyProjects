#!/usr/bin/python -O

import sys, os
import glob

import Amqp
from Amqp.QpidWrapper import ConsumerListener, Producer
from Amqp.RoutingKey import RoutingKey
from SeaPilot import init_ds_log, log_debug, log_info
from DSA.dsaconf import get_publication_maps


SEAPILOT_HOME = os.environ['SEAPILOT_HOME']


class Dsa(ConsumerListener):

    def __init__(self, sub_broker, pub_broker, exchange,
                       publication_map_list=[]):

        binding_keys = [pub_map.routing_key for pub_map in publication_map_list]

        super(Dsa, self).__init__(sub_broker, exchange, binding_keys)

        self.__producer = Producer(pub_broker, exchange)
        self.__publication_maps = dict((pub_map.routing_key, pub_map)
                                           for pub_map in publication_map_list)

    def received(self, message):
        if message.subject:
            keystr = message.subject
        else:
            keystr = message.properties.get('x-amqp-0-10.routing-key')

        keystr = str(RoutingKey(keystr))
        print 'Received event with routing key:', keystr

        pub_map = self.__publication_maps.get(keystr)
        if pub_map:
            pub_map.do_insert(message)
        else:
            print 'Unsupported message:', keystr


    def preprocess(self):
        for pub_map in self.__publication_maps.values():
            pub_map.preprocess()


# --main--
if __name__ == '__main__':
    import SeaPilot.OptParser
    from Amqp.RoutingKey import RoutingKey

    parser = SeaPilot.OptParser.OptParser()

    parser.add_option('--subscribe-broker-ip',  dest='subscribe_ip',
           default = '127.0.0.1',
           help='ip address of broker to subscribe, [default: %default]')
    parser.add_option('--subscribe-broker-port',  dest='subscribe_port',
           type = int, default = 5672,
           help='port of broker to subscribe, [default: %default]')

    parser.add_option('--publish-broker-ip', dest='publish_ip',
           default='127.0.0.1',
           help='ip address that send messages to, [default: %default]')
    parser.add_option('--publish-broker-port', dest='publish_port',
           type=int, default=int(os.getenv('SP_MESSAGE_PORT', 5672)),
           help='port number that send messages to, [default: %default]')

    parser.add_option('--proto-src',  dest='proto_src',
           default = os.path.join(SEAPILOT_HOME, 'lib', 'python',
                                 'seapilot_publications.zip'),
           help='directory to store publication files, [default: %default]')

    parser.add_option('--config-dir',  dest='config_dir',
          help='directory to store configuration files')

    parser.add_option('--preprocess',  dest='preprocess',
          action='store_true', default=False,
          help='whether doing preprocess only, [default: %default]')

    (opts, args) = parser.parse_args()

    init_ds_log(opts.debug, opts.verbose)
    Amqp.set_proto_src(opts.proto_src)
    exchange = 'amq.topic'

    publication_map_list = []

    for xml_file in glob.glob (opts.config_dir + '/*.xml'):
        publication_map_list.extend(get_publication_maps(xml_file))

    dsa = Dsa('{0}:{1}'.format(opts.subscribe_ip, opts.subscribe_port),
              '{0}:{1}'.format(opts.publish_ip, opts.publish_port),
              exchange,
              publication_map_list)

    dsa.preprocess()

    import qpid.messaging.exceptions
    try:
        dsa.run()

    except KeyboardInterrupt:
        print '\nControl-C pressed, exiting...'

    except qpid.messaging.exceptions.ConnectionError:
        print 'Lost connection, exit...'

    except qpid.messaging.exceptions.MalformedAddress:
        print 'Malformed routingkey'


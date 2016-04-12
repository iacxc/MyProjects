
# $Id: xml2obj.py 223 2013-05-29 11:14:36Z chengxin.cai $

from xml.parsers import expat

class Element(object):
    '''A parsed XML element'''
    __slots__ = ('__name', '__attrs', '__cdata', '__children')

    def __init__(self, name, attributes):
        # Record tagname and attributes dictionary
        self.__name = name
        self.__attrs = attributes

        # Initialize the element's cdata and children to empty
        self.__cdata = ''
        self.__children = []

    @property
    def name(self): return self.__name

    @property
    def cdata(self): return self.__cdata

    def topo(self):
        topo = {}
        if not (self.__attrs or self.__children):
            topo[self.__name] = self.__cdata
        else:
            topo['__tagname']  = self.__name

            for k, v in self.__attrs.iteritems():
                topo[k] = v

            if self.__cdata:
                topo['__cdata'] = self.__cdata

            elif self.__children:
                topo['__children'] = [ child.topo() for child in self.__children ]

        return topo

    def add_child(self, element):
        self.__children.append(element)

    def append_data(self, data):
        self.__cdata += data

    def getattr(self, key):
        return self.__attrs.get(key)

    def getelements(self, name=''):
        if name:
            return [c for c in self.children if c.name ==name]
        else:
            return list(self.__children)

class Xml2Obj(object):
    ''' XML to Object converter '''
    __slots__ = ('__root', '__nodes')

    def __init__(self):
        self.__root = None
        self.__nodes = []

    def start_element(self, name, attributes):
        'Expat start element event handler'
        # Instantiate an Element object
        element = Element(name.encode(), attributes)
        # Push element onto the stack and make it a child of parent
        if self.__nodes:
            self.__nodes[-1].add_child(element)
        else:
            self.__root = element

        self.__nodes.append(element)

    def end_element(self, name):
        'Expat end element event handler'
        self.__nodes.pop()

    def character_data(self, data):
        'Expat character data event handler'
        if data.strip():
            data = data.encode()
            self.__nodes[-1].append_data(data)

    def parse(self, filename):
        # Create an Expat parser
        parser = expat.ParserCreate()

        # Set the Expat event handlers to our methods
        parser.StartElementHandler = self.start_element
        parser.EndElementHandler = self.end_element
        parser.CharacterDataHandler = self.character_data

        # Parser the XML File
        status = parser.Parse(open(filename).read(), 1)
        return self.__root

if __name__ == '__main__':

    import sys, json
    if len(sys.argv) > 1:
        parser = Xml2Obj()
        root_elem = parser.parse(sys.argv[1])

        topo = root_elem.topo()
        json.dump(topo, sys.stdout, indent=4, sort_keys=True)
        print


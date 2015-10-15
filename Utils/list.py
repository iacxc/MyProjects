#!/usr/bin/python

class Cons(object):
    def __init__(self, head, tail):
        self.__head = head
        self.__tail = tail

    @property
    def head(self):
        return self.__head

    @property
    def tail(self):
        return self.__tail

    @property
    def isEmpty(self):
        return False

    def __str__(self):
        return "({0} {1})".format(self.head, self.tail)


class Nil(object):
    @property
    def head(self):
        raise RuntimeError("Empty list")

    @property
    def tail(self):
        raise RuntimeError("Empty list")

    @property
    def isEmpty(self):
        return True

    def __str__(self):
        return "()"


def List(*items):
    if len(items) == 0:
        return Nil()
    else:
        return Cons(items[0], List(*items[1:]))


l = List(1, 2, List("hello", "world"), "a", "b", List(List('aa', 'bb')))
print l
print l.head, type(l.head)
print l.tail, type(l.tail)

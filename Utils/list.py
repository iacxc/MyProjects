#!/usr/bin/python


__all__ = ('List',)


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


class Cons(object):
    def __init__(self, head, tail=Nil):
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


def List(*items):
    if len(items) == 0:
        return Nil()
    else:
        return Cons(items[0], List(*items[1:]))


def reverse(alist):
    def rec(lst, acc):
        if lst.isEmpty:
            return acc
        else:
            return rec(lst.tail, Cons(lst.head, acc))

    return rec(alist, Nil())

#    if list.isEmpty or list.tail.isEmpty:
#        return list
#
#    return Cons(reverse(list.tail), List(list.head))


l = List(1, 2, List("hello", "world"), "a", "b", List(List('aa', 'bb')))
print l
l2 = List(List(List('aa', 'bb')), "b", "a", List("hello", "world"), 2, 1) 
print l2
print reverse(l)

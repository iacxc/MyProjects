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


def List(*items):
    """ create a list """
    if len(items) == 0:
        return Nil()
    else:
        return Cons(items[0], List(*items[1:]))


def reverse(alist):
    """ reverse a list """
    def rec(lst, acc):
        if lst.isEmpty:
            return acc
        else:
            return rec(lst.tail, Cons(lst.head, acc))

    return rec(alist, Nil())


def concat(list1, list2):
    """ concat two lists """
    if list1.isEmpty:
        return list2
    else:
        return Cons(list1.head, concat(list1.tail, list2))


l1 = List(1, 2, List("hello", "world"), "a", "b", List(List('aa', 'bb')))
print l1
print reverse(l1)

l2 = List(1, 2, 3)
l3 = List("a", "b")
print concat(l2, l3)
print concat(l3, l2)
print reverse(concat(l2, l3))


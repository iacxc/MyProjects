#!/usr/bin/python -O
"""
   program to calculate days
"""

def print_cne(c_and_es):
    """ print chicken and eggs """
    formatter = '{0:<5}{1:<8}{2:<8}{3:<6}'
    print formatter.format('Day', 'Chicken', 'Eggs', 'Change')
    print formatter.format('=====', '========', '========', '======')

    for day, chick_and_egg in enumerate(c_and_es):
        print formatter.format(day, chick_and_egg[0],
                                    chick_and_egg[1],
                                    chick_and_egg[2])


def find_days(c_and_es, prof, total):
    """ find the days to generate total eggs """
    chicken, eggs, dummy_change = c_and_es[-1]

    if chicken * prof + eggs >= total:
        print_cne(c_and_es +
                  [(chicken, eggs + i * chicken, 0)
                      for i in range(1, prof + 1)])
        return

    eggs += chicken
    num = eggs / prof
    if num >= 1:
        find_days(c_and_es + [(chicken + num, eggs - num * prof, num * prof)],
                  prof, total)
    else:
        find_days(c_and_es + [(chicken, eggs, 0)], prof, total)


find_days([(1, 0, 0)], 50, 1000)

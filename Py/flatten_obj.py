#!/usr/bin/python -O

import json
from collections import OrderedDict

def flatten_object(obj, prefix='', header={}):

    if prefix != '': prefix += '.'

    if isinstance(obj, dict):
        result = OrderedDict(header)
        for key, val in obj.items():
            if isinstance(val, dict):
                t_result = flatten_object(val, prefix + key)
                if isinstance(t_result, dict):
                    result.update(t_result)
                else: #list
                    t_result = flatten_object(val, prefix + key, result)
                    return t_result
#                    return [OrderedDict(result.items() + d.items()) 
#                                      for d in t_result]
            elif isinstance(val, list):
                return [flatten_object(v, prefix + key, result) for v in val]
            else:
                result[prefix + key] = val

        return result
    else:
        return prefix + obj


def print_flat(obj, prefix=''):
    print json.dumps(flatten_object(obj, prefix), indent=4)
   

print_flat('hello')
print_flat({'a': 1, 'b': 2})
print_flat({'a': 1, 'b': 2}, prefix='aa')
print_flat(OrderedDict({'aa' : {'a' : 10, 'b': 20},
                        'a': 1, 
                        'b': 2,
                        'ccc' : { 'cc' : {'a' : 200, 'b' : 400}, 
                                  'd' : 1000,
                                },
                       }), prefix='header') 

print_flat(OrderedDict({'a' : 1, 
                        'c' : 20,
                        'b' : [{'a': 1}, 
                               {'a' : 2},
                               {'a' : 3}],
                        }))
print_flat(OrderedDict({ 'start' : 0,
                         'end' : OrderedDict({ 'h' : 'haha',
                                   'x' : OrderedDict({'a' : 1, 
                                          'c' : 20,
                                          'b' : [{'a': 1}, 
                                                 {'ab' : 2},
                                                 {'abc' : 3}],
                                          })
                                 })
                       }))

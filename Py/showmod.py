#!/usr/bin/python -O

import sys, os

def get_modules(root):
    for path, subdirs, files in os.walk(root):
        rel_path = path.replace(root, '').replace('/', '.')
        for f in files:
            if f == '__init__.py': 
                continue

            if f.endswith('.py'): 
                if rel_path:
                    yield '.'.join([rel_path[1:], f[:-3]]) + ' (%s)' % path
                else:
                    yield '%s (%s)' % (f[:-3], path)

if __name__ == '__main__':
    for path in sys.path[1:]: # skip the current directory
        modules = get_modules(path)
        for module in modules:
            print module


def getpermission(permission):
    getbit = lambda bit: {'0' : '---',
                       '1' : '--x',
                       '2' : '-w-',
                       '3' : '-wx',
                       '4' : 'r--',
                       '5' : 'r-x',
                       '6' : 'rw-',
                       '7' : 'rwx'}.get(bit, '---')
    return ''.join(map(getbit, permission))



def samerow(grid, idx):
    """ get all the values in the same row with idx """
    row_num = idx / 9
    return grid[row_num * 9 : (row_num+1) * 9]

def samecol(grid, idx):
    """ get all the values in the same col with idx """
    col_num = idx % 9
    indexes = [row*9 + col_num for row in range(9)]
    return tuple(grid[i] for i in indexes)

def samesquare(grid, idx):
    """ get all the values in the same square with idx """
    row_blk = idx / 9 / 3
    col_blk = idx % 9 / 3
    
    base_idx = 9 * 3 * row_blk + col_blk * 3

    return grid[base_idx : base_idx + 3] + \
           grid[base_idx + 9 : base_idx + 3 + 9] + \
           grid[base_idx + 18 : base_idx + 3 + 18] 

formats = ('\033[1;30m%s\033[0m',        #0
           '\033[1;31m%s\033[0m',        #1
           '\033[1;32m%s\033[0m',        #2
           '\033[1;33m%s\033[0m',        #3
           '\033[1;34m%s\033[0m',        #4
           '\033[1;35m%s\033[0m',        #5
           '\033[1;36m%s\033[0m',        #6
           '\033[1;37m%s\033[0m'         #7
)
        
def color_digit(number):
    if number > len(formats) - 1:
        format = formats[-1]
    else:
        format = formats[number]

    return format % number
    
def showgrid(grid):
    if len(grid) != 81: return
    
    lines = []
    for i in range(9):
        if i > 0 and i % 3 == 0:
            lines.append('+'.join(['-' * 9] * 3))

        blks = []
        for j in range(3):
            blks.append('  '.join(map(color_digit, grid[9*i+3*j:9*i+3*j+3])))

        lines.append(' ' + ' | '.join(blks))

    print '\n'.join(lines)
    print

def solvegrid(grid, idx=0):
    if idx == 81: 
        yield grid
    else:    
        if grid[idx] != 0: 
            for g in solvegrid(grid, idx+1):
               if g: yield g
        else:
            rowvals = samerow(grid, idx)
            colvals = samecol(grid, idx)
            squarevals = samesquare(grid, idx)

            for val in range(1, 10):
                if val in rowvals + colvals + squarevals: # an invalid choice
                    continue 

                for g in solvegrid(grid[:idx] + (val,) + grid[idx+1:], idx+1):
                    if g: yield g

        # if reach here, all choices has run out
        return

if __name__ == '__main__':

    grid = tuple( map(int, '000 070 001\
                            700 001 098\
                            060 809 000\
                            079 002 000\
                            210 000 607\
                            840 000 100\
                            000 060 800\
                            000 004 300\
                            000 237 000'\
        .replace(' ','')) )
    print 'Original'
    showgrid(grid)

    print 'After fill'
    count = 1
    for g in solvegrid(grid):
        print 'Solution No:', count
        showgrid(g)
        count += 1

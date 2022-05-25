#!/usr/bin/env python
import sys
# input comes from STDIN (standard input)
for line in sys.stdin:
    # remove leading and trailing whitespace
    line = line.strip()
    words = line.split(',')
    # make sure we skip header
    if 'timestamp' not in words:
        words = [word.strip() for word in words]
        # get the date
        date = words[0][:-9]
        id = words[2]
        if date == '2016-06-02':
            print ('%s\t%s' % (id, 1))
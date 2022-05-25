#!/usr/bin/env python
import sys
# input comes from STDIN (standard input)
for line in sys.stdin:
    # remove leading and trailing whitespace
    line = line.strip()
    words = line.split(',')
    # make sure we skip header
    if 'Date/Time' not in words:
        words = [word.strip() for word in words]
        # get the time
        time = words[0][11:13]
        # total electric demand power
        tedp = words[8]
        print ('%s\t%s' % (time, tedp))
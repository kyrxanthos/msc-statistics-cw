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
        floor = words[3]
        zone = words[4]
        key = floor + ' ' + zone
        print ('%s\t%s' % (key, 1))
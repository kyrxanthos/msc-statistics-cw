#!/usr/bin/env python
import sys

current_id = None
current_date = None
current_count = 0

for line in sys.stdin:
    line = line.strip()

    # parse the input we got from mapper.py
    date_id, count = line.split('\t')
    date , id = date_id.split()

    # convert count (currently a string) to int
    try:
            count = int(count)
    except ValueError:
            # count was not a number, so silently
            # ignore/discard this line
            continue


    if id != current_id:
        current_count += 1
        current_id = id


    if date != current_date:
        if current_date != None:
            print ('%s\t%s' % (current_date, current_count))
        current_date = date 
        # initialize all ids since we are in a diffferent date
        current_id = None
        current_count = 0
    
# do not forget to output the last word if needed!
if current_id == id:
    print ('%s\t%s' % (current_date, current_count))

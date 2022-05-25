#!/usr/bin/env python
import sys

current_id = None
current_floor_zone = None
current_count = 0
max_count = 0
max_fz = None

for line in sys.stdin:
    line = line.strip()

    # parse the input we got from mapper.py
    floor_zone, count = line.split('\t')

    # convert count (currently a string) to int
    try:
            count = int(count)
    except ValueError:
            # count was not a number, so silently
            # ignore/discard this line
            continue


    if current_floor_zone == floor_zone:
        # if the same fz, we increment count
        current_count += 1
    
    else:
        # make sure this is not none
        if current_floor_zone:
            # update max count if it is larger than the previous
            if current_count >= max_count:
                max_count = current_count
                max_fz = current_floor_zone
        current_count = count
        current_floor_zone = floor_zone

# print the final count
print('MAX FZ {} with count {}'.format(max_fz, max_count))


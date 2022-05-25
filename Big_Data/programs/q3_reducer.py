#!/usr/bin/env python
import sys

current_id = None
current_count = 0
max_count = 0
max_id = None


for line in sys.stdin:
    line = line.strip()

    # parse the input we got from mapper.py
    id, count = line.split('\t')

    # print('floor zone is {}'.format(floor_zone))

    # convert count (currently a string) to int
    try:
            count = int(count)
    except ValueError:
            # count was not a number, so silently
            # ignore/discard this line
            continue


    if current_id == id:
        # print('op')
        current_count += 1
    
    else:
        # print('ep')
        if current_id:
            # floor, zone = current_floor_zone.split(' ')
            # print(int(floor), int(zone), '\t', current_count)
            # print(current_id, current_count)
            if current_count >= max_count:
                max_count = current_count
                max_id = current_id
        current_count = count
        current_id = id

    
# do not forget to output the last word if needed!
if current_id == id:
    # print('pff')
    # print(current_id, current_count)
    pass

print('id with greatest number of prox-ID reading {} with count {}'.format(max_id, max_count))


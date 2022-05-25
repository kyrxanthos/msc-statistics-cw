#!/usr/bin/env python
import sys

current_id = None
current_floor_zone = None
current_count = 0

#print('FLOOR | ZONE | COUNT')

for line in sys.stdin:
    line = line.strip()

    # parse the input we got from mapper.py
    floor_zone, count = line.split('\t')

    # print('floor zone is {}'.format(floor_zone))

    # convert count (currently a string) to int
    try:
            count = int(count)
    except ValueError:
            # count was not a number, so silently
            # ignore/discard this line
            continue


    if current_floor_zone == floor_zone:
        # print('op')
        current_count += 1
    
    else:
        # print('ep')
        if current_floor_zone:
        	#floor, zone = current_floor_zone.split(' ')
    		#print('%s\t%s\t%s' % (int(floor), int(zone), current_count))
            print(current_floor_zone, current_count)
        current_count = count
        current_floor_zone = floor_zone

    
# do not forget to output the last word if needed!
if current_floor_zone == floor_zone:
    # print('pff')
    #floor, zone = current_floor_zone.split(' ')
    #print('%s\t%s\t%s' % (int(floor), int(zone), current_count))
	print(current_floor_zone, current_count)
#!/usr/bin/env python
import sys

current_id = None
current_brand_model = None
current_count = 0
max_count = 0
max_bm = None

for line in sys.stdin:
    line = line.strip()

    # parse the input we got from mapper.py
    brand_model, count = line.split('\t')

    # convert count (currently a string) to int
    try:
            count = int(count)
    except ValueError:
            # count was not a number, so silently
            # ignore/discard this line
            continue


    if current_brand_model == brand_model:
        current_count += 1
    
    else:
        if current_brand_model:
            if current_count >= max_count:
                # update max count if the current most popular car
                # is more popular than the previous one
                max_count = current_count
                max_bm = current_brand_model
        # initialise count again
        current_count = count
        current_brand_model = brand_model


print('Brand and Model of most popular car: {} with count {}'.format(max_bm,
     max_count))
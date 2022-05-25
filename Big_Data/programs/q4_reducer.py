#!/usr/bin/env python
import sys

current_time = None
current_count = 0
current_sum = 0.


for line in sys.stdin:
    line = line.strip()

    # parse the input we got from mapper.py
    time, tedp = line.split('\t')

    if current_time == time:
        current_count += 1
        current_sum += float(tedp)
    
    else:
        if (current_time) and (current_count!=0):
            print(time, current_sum / current_count)
        current_count = 0
        current_sum = 0.
        current_time = time

    
# # do not forget to output the last word if needed!
# if (current_time == time) and (current_count!=0):
#     # print('pff')
#     # print(time, current_sum / current_count)
#     pass



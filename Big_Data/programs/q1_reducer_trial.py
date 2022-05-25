#!/usr/bin/env python
import sys
current_date = None
current_id = None 
current_unique_ids = 0
print ('DATE N_UNIQUE_IDS') # Header

for line in sys.stdin:
    key, value = line.strip().split('\t') 
    date, id = key.split(' ')

    if date != current_date:
        if current_date != None:
            print (current_date + ' ' + str(current_unique_ids))
        current_date = date 
        current_id = None
        current_unique_ids = 0
    if id != current_id: 
        current_unique_ids += 1
        current_id = id
print (current_date + ' ' + str(current_unique_ids))
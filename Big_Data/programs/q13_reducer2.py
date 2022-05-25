import sys

current_date = None
current_count = 0

for line in sys.stdin:
    line = line.strip()

    # parse the input we got from mapper.py
    date, count = line.split('\t')
    # convert count (currently a string) to int
    try:
            count = int(count)
    except ValueError:
            # count was not a number, so silently
            # ignore/discard this line
            continue


    if date != current_date:
        if current_date != None:
            print ('%s\t%s' % (current_date, current_count))
        current_date = date 
        current_count = 1
    else:
        current_count += 1
    
# do not forget to output the last word if needed!
if current_date == date:
    print ('%s\t%s' % (current_date, current_count))
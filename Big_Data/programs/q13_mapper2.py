import sys
# input comes from STDIN (standard input)
for line in sys.stdin:
    # remove leading and trailing whitespace
    line = line.strip()
    words = line.split(',')
    # make sure we skip header
    if 'model' not in words:
        # get rid of all other models and outlier
        if ((' Fiesta' in words) and ('2060' not in words)):
            words = [word.strip() for word in words]
            # get the date
            year = words[2]
            key = year
            print ('%s\t%s' % (key, 1))



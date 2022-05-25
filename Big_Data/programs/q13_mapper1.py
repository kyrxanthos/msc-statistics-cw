import sys
# input comes from STDIN (standard input)
for line in sys.stdin:
    # remove leading and trailing whitespace
    line = line.strip()
    words = line.split(',')
    # make sure we skip header
    if 'model' not in words:
        words = [word.strip() for word in words]
        # get the model of the car
        model = words[1]
        # get the brand of the car
        brand = words[-1]
        key = brand + ' ' + model
        print ('%s\t%s' % (key, 1))
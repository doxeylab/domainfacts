import sys
from collections import defaultdict
from sets import Set

if len(sys.argv) != 2:
    print "python %s [architecture.txt]" %(sys.argv[0])
    sys.exit(0)

#domain: [number of times with others, number of times alone]
alone_count = defaultdict(lambda: [0,0])

with open(sys.argv[1],'r') as data:
    for line in data:
        items = line.split()[1].split("~")
        frequency = int(line.split()[3])
        if len(items) == 1:
            alone_count[items[0]][1] += frequency
        elif len(items) > 1:
            for entry in Set(items):
                alone_count[entry][0] += frequency

print "Domain\tWith_others\tWith_others_%\tAlone\tAlone_%"
for domain in alone_count:
    others = alone_count[domain][0]
    alone = alone_count[domain][1]
    total = others + alone
    perc_others = (float(others)/total)*100
    perc_alone = (float(alone)/total)*100
    print "%s\t%i\t%0.2f\t%i\t%0.2f" %(domain,others,perc_others,alone,perc_alone)
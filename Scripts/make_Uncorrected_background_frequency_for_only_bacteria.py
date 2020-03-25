import sys
from collections import defaultdict
from sets import Set

if len(sys.argv) != 3:
    print "python %s [pfamA_ncbi.txt] [ncbi_taxonomy.txt]" %(sys.argv[0])
    sys.exit(0)

with open(sys.argv[2],'r') as taxas:
    bacteria = Set(line.split()[0] for line in taxas if line.split("\t")[2].startswith("Bacteria"))

total = Set()
counts = defaultdict(int)
with open(sys.argv[1], 'r') as data:
    for line in data:
        tax = line.split()[2]
        if tax in bacteria:
            counts[line.split()[1]] += 1
            total.add(tax)

with open("Uncorrected_Pfam_background_pres_abs_bacteria.txt","w") as output:
    for item in counts:
        output.write("%i\t%s\n" %(counts[item],item))

print "Total bacterial proteomes: %i" %(len(total))

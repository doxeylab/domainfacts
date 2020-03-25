import sys,os
from sets import Set
from operator import itemgetter

if len(sys.argv) != 2:
    print "python %s [sample fasta file]"
    sys.exit(0)

all_seqs = {}
with open(sys.argv[1],'r') as fas:
    header = ""
    for line in fas:
        if line.startswith(">"):
            header = line
            pieces = header.split("_")
            start = [i for i,x in enumerate(pieces) if x == '-' or x == "+"]
            if len(start) > 1:
                print "WARNING: MORE THAN ONE - OR + ARGGGGG"
                sys.exit(0)
            domain = "_".join(pieces[(start[0]+1):-1])
            #sequence_name: [domain, fasta sequence]
            all_seqs[header] = [domain,""]
        else:
            all_seqs[header][1] += line

sample = sys.argv[1][:-3]
os.mkdir(sample)
all_files = Set()
domain = ""
for entry in sorted(all_seqs, key=itemgetter(0)):
    new_domain = all_seqs[entry][0] 
    if domain == new_domain:
        output.write("%s%s" %(entry,all_seqs[entry][1]))
    else:
        if domain != "":
            output.close()
        output = open("%s/%s.fa" %(sample,new_domain),'a')
        all_files.add("%s/%s.fa" %(sample,new_domain))
        output.write("%s%s" %(entry,all_seqs[entry][1]))
        domain = new_domain
output.close()
with open("%s.txt" %(sample),'w') as file_output:
    for item in all_files:
        file_output.write("%s\n" %(item))

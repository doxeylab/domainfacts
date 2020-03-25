import sys
from collections import defaultdict
from numpy import std
from sets import Set

if len(sys.argv) != 3:
    print "python %s [other_reg.txt] [Pfam-A.regions.uniprot_updated.tsv]" %(sys.argv[0])
    sys.exit(0)

seq_domains = defaultdict(list)
with open(sys.argv[2],'r') as domain_regions:
    for line in domain_regions:
        info = line.split()
        seq = info[0]
        if seq != "uniprot_acc":
            domain = info[4]
            domstart = int(info[5])
            domstop = int(info[6])
            #                    counter for residues that are disordered,total length of domain alignment
            seq_domains[seq] += [[domain,0.0,float((domstop-domstart)+1),domstart,domstop]]

domains_with_disorder = defaultdict(list)

with open(sys.argv[1],'r') as other_regions:
    for line in other_regions:
        info = line.split()
        seq = info[1]
        if info[4] == "disorder":
            disstart = int(info[2])
            disstop = int(info[3])
            disrange = set(range(disstart,(disstop+1)))
            if seq in seq_domains:
                for index,domain_alignment in enumerate(seq_domains[seq]):
                    domain = domain_alignment[0]
                    domrange = set(range(domain_alignment[3],(domain_alignment[4]+1)))
                    overlap = domrange.intersection(set(disrange))
                    seq_domains[seq][index][1] += len(overlap)

for sequence in seq_domains:
    for alignment_info in seq_domains[sequence]:
        domains_with_disorder[alignment_info[0]] += [(alignment_info[1]/alignment_info[2])*100]
        print "%s\t%s\t%f\t%f" %(sequence,alignment_info[0],alignment_info[1],alignment_info[2])

with open("Disorder_percentage.txt",'w') as output:
    for adomain in domains_with_disorder:
        disavg = sum(domains_with_disorder[adomain])/float(len(domains_with_disorder[adomain]))
        disstdv = std(domains_with_disorder[adomain])
        output.write("%f\t%f\t%s\n" %(disavg,disstdv,adomain))
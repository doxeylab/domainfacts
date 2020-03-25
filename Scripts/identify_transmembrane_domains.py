import sys
from collections import defaultdict

if len(sys.argv) != 3:
    print "python %s [other_reg.txt] [Pfam-A.regions.uniprot_updated.tsv]" %(sys.argv[0])
    sys.exit(0)

transmem = defaultdict(list)
with open(sys.argv[1],'r') as other_regions:
    for line in other_regions:
        if line.split()[4] == "transmembrane":
             transmem[line.split()[1]] += [[int(line.split()[2]),int(line.split()[3])]]

domains_with_transmem_overlap = defaultdict(lambda: [0.0,0.0,0.0])

with open(sys.argv[2],'r') as domain_regions:
    for line in domain_regions:
        info = line.split()
        seq = info[0]
        if seq != "uniprot_acc":
            domain = info[4]
            domains_with_transmem_overlap[domain][2] += 1
            if seq in transmem:
                start = int(info[5])
                stop = int(info[6])
                domains_with_transmem_overlap[domain][0] += 1
                for region in transmem[seq]:
                    if start <= region[1] and region[0] <= stop:
                        domains_with_transmem_overlap[domain][1] += 1
                        break

for adomain in domains_with_transmem_overlap:
    fam_with_overlap = domains_with_transmem_overlap[adomain][1]/domains_with_transmem_overlap[adomain][2]
    fam_with_transmem = domains_with_transmem_overlap[adomain][0]/domains_with_transmem_overlap[adomain][2]
    print "%f\t%f\t%s" %(fam_with_overlap,fam_with_transmem,adomain)
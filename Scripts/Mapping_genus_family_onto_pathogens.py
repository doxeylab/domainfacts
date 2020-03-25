import sys
from collections import defaultdict
from sets import Set
from collections import Counter

if len(sys.argv) != 4:
    print "python %s [Pathogens_pres_abs_hypergeometic_bacterial_domains.txt] [taxonomy.txt] [Domain_count_by_pathogen.txt]" %(sys.argv[0])
    sys.exit(0)

taxaidsdict = {}
with open(sys.argv[1],'r') as domain_list:
    pathdomains = Set(line.split()[0] for line in domain_list if not line.startswith("Pathogens"))
#    pathdomains = Set(line.split()[0] for line in domain_list if not line.startswith("Pathogens") and line.split()[0].startswith("DUF") or line.split()[0].startswith("UPF0"))

with open(sys.argv[2],'r') as taxonomy:
    #it's taxa id: [next taxa id in line,name of taxa, level of taxa]
    fam_gens = {line.split()[0]:[line.split("\t")[-4],line.split("\t")[2].split(";")[-2],line.split()[-1]] for line in taxonomy}

with open(sys.argv[3],'r') as species_mapping:
    for line in species_mapping:
        domain = line.split()[0]
        if domain in pathdomains:
            taxaid = line.split()[1]
            if taxaid in taxaidsdict:
                print "%s\t%s" %(taxaidsdict[taxaid][0],taxaidsdict[taxaid][1])
            else:
                family = "unclassified"
                genus = "unclassified"
#    kingdom = "unclassified"
#    phylum = "unclassified"
#    class_ = "unclassified"
#    order = "unclassified"
#    superkingdom = "unclassified"
                next_id = taxaid
                while next_id != "1" and next_id != "0":
                    info = fam_gens[next_id]
                    rank = info[2]
                    next_id = info[0]
                    taxaname = info[1]
                    if rank == "family":
                        family = taxaname
                    elif rank == "genus":
                        genus = taxaname
#        elif rank == "kingdom":
#            kingdom = taxaname
#        elif rank  == "phylum":
#            phylum = taxaname
#        elif rank == "class":
#            class_ = taxaname
#        elif rank == "order":
#            order = taxaname
#        elif rank == "superkingdom":
#            superkingdom = taxaname
#    taxaidsdict[id] = [superkingdom,kingdom,phylum,class_,order,family,genus]
                taxaidsdict[taxaid] = [family,genus]
                print "%s\t%s" %(taxaidsdict[taxaid][0],taxaidsdict[taxaid][1])

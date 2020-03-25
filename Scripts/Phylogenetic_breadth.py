import sys
from collections import defaultdict
from sets import Set

if len(sys.argv) != 3:
    print "python %s [pfamA_ncbi.txt] [taxonomy.txt]" %(sys.argv[0])
    sys.exit(0)

taxa = defaultdict(list)
taxaids_list = Set()
taxaidsdict = {}
superkingdom_count = Set()
kingdom_count = Set()
phylum_count = Set()
class_count = Set()
order_count = Set()
family_count = Set()
genus_count = Set()
species_count = Set()

with open(sys.argv[1],'r') as domains_taxa:
    for line in domains_taxa:
        domain = line.split()[1]
        taxaid = line.split()[2]
        taxa[domain] += [taxaid]
        taxaids_list.add(taxaid)

with open(sys.argv[2],'r') as taxonomy:
    #it's taxa id: [next taxa id in line,name of taxa, level of taxa]
    fam_gens = {line.split()[0]:[line.split("\t")[-4],line.split("\t")[2].split(";")[-2],line.split()[-1]] for line in taxonomy}

for id in taxaids_list:
    species = "unclassified"
    family = "unclassified"
    genus = "unclassified"
    kingdom = "unclassified"
    phylum = "unclassified"
    class_ = "unclassified"
    order = "unclassified"
    superkingdom = "unclassified"
    next_id = id
    while next_id != "1" and next_id != "0":
        info = fam_gens[next_id]
        rank = info[2]
        next_id = info[0]
        taxaname = info[1]
        if rank == "family":
            family = taxaname
        elif rank == "genus":
            genus = taxaname
        elif rank == "species":
            species = taxaname
        elif rank == "kingdom":
            kingdom = taxaname
        elif rank  == "phylum":
            phylum = taxaname
        elif rank == "class":
            class_ = taxaname
        elif rank == "order":
            order = taxaname
        elif rank == "superkingdom":
            superkingdom = taxaname
    taxaidsdict[id] = [superkingdom,kingdom,phylum,class_,order,family,genus,species]
    superkingdom_count.add(superkingdom)
    kingdom_count.add(kingdom)
    phylum_count.add(phylum)
    class_count.add(class_)
    order_count.add(order)
    family_count.add(family)
    genus_count.add(genus)
    species_count.add(species)

superkingdom_total = float(len([e for e in superkingdom_count if e != "unclassified"]))
kingdom_total = float(len([e for e in kingdom_count if e != "unclassified"]))
phylum_total = float(len([e for e in phylum_count if e != "unclassified"]))
class_total = float(len([e for e in class_count if e != "unclassified"]))
order_total = float(len([e for e in order_count if e != "unclassified"]))
family_total = float(len([e for e in family_count if e != "unclassified"]))
genus_total = float(len([e for e in genus_count if e != "unclassified"]))
species_total = float(len([e for e in species_count if e != "unclassified"]))

print "Domain\tSpecies\tGenus\tFamily\tOrder\tClass\tPhylum\tKingdom\tSuperkingdom"

for domain in taxa:
    temp_families = []
    temp_genera = []
    temp_species = []
    temp_kingdom = []
    temp_phylum = []
    temp_class = []
    temp_order = []
    temp_superkingdom = []
    for id in taxa[domain]:
        temp_superkingdom += [taxaidsdict[id][0]]
        temp_kingdom += [taxaidsdict[id][1]]
        temp_phylum += [taxaidsdict[id][2]]
        temp_class += [taxaidsdict[id][3]]
        temp_order += [taxaidsdict[id][4]]
        temp_families += [taxaidsdict[id][5]]
        temp_genera += [taxaidsdict[id][6]]
        temp_species += [taxaidsdict[id][7]]
    mc_superkingdom = (len(Set([e for e in temp_superkingdom if e != "unclassified"]))/superkingdom_total)*100
    mc_kingdom = (len(Set([e for e in temp_kingdom if e != "unclassified"]))/kingdom_total)*100
    mc_phylum = (len(Set([e for e in temp_phylum if e != "unclassified"]))/phylum_total)*100
    mc_class = (len(Set([e for e in temp_class if e != "unclassified"]))/class_total)*100
    mc_order = (len(Set([e for e in temp_order if e != "unclassified"]))/order_total)*100
    mc_families = (len(Set([e for e in temp_families if e != "unclassified"]))/family_total)*100
    mc_genera = (len(Set([e for e in temp_genera if e != "unclassified"]))/genus_total)*100
    mc_species = (len(Set([e for e in temp_species if e != "unclassified"]))/species_total)*100
    print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s" %(domain,mc_species,mc_genera,mc_families,mc_order,mc_class,mc_phylum,mc_kingdom,mc_superkingdom)
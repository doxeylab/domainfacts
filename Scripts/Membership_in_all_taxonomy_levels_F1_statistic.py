import sys
from collections import defaultdict
from sets import Set
from collections import Counter

if len(sys.argv) != 4:
    print "python %s [Uncorrected_Pfam_background_pres_abs.txt] [pfamA_ncbi.txt] [taxonomy.txt]" %(sys.argv[0])
    sys.exit(0)

taxa = defaultdict(list)
taxaids_list = Set()
taxaidsdict = {}
family_sizes = {}
superkingdom_count = defaultdict(int)
kingdom_count = defaultdict(int)
phylum_count = defaultdict(int)
class_count = defaultdict(int)
order_count = defaultdict(int)
family_count = defaultdict(int)
genus_count = defaultdict(int)

with open(sys.argv[1],'r') as famsizes:
    family_sizes = {line.split()[1]:line.split()[0] for line in famsizes}

with open(sys.argv[2],'r') as domains_taxa:
    for line in domains_taxa:
        domain = line.split()[1]
        taxaid = line.split()[2]
        taxa[domain] += [taxaid]
        taxaids_list.add(taxaid)

with open(sys.argv[3],'r') as taxonomy:
    #it's taxa id: [next taxa id in line,name of taxa, level of taxa]
    fam_gens = {line.split()[0]:[line.split("\t")[-4],line.split("\t")[2].split(";")[-2],line.split()[-1]] for line in taxonomy}

for id in taxaids_list:
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
    taxaidsdict[id] = [superkingdom,kingdom,phylum,class_,order,family,genus]
    superkingdom_count[superkingdom] += 1
    kingdom_count[kingdom] += 1
    phylum_count[phylum] += 1
    class_count[class_] += 1
    order_count[order] += 1
    family_count[family] += 1
    genus_count[genus] += 1

#print "Domain\tNum_species\tTop_superkingdom\tTop_superking_perc\tTop_superking_pene\tTop_kingdom\tTop_king_perc\tTop_king_pene\tTop_phylum\tTop_phy_perc\tTop_phy_pene\tTop_class\tTop_class_perc\tTop_class_pene\tTop_order\tTop_ord_perc\tTop_ord_pene\tTop_family\tTop_fam_perc\tTop_fam_pene\tTop_genus\tTop_gen_perc\tTop_gen_pene"
print "Domain\tNum_species\tBest_level\tBest_taxa\tBest_perc\tBest_pene\tSum\tSuperkingdom"

temp_families = []
temp_genera = []
temp_kingdom = []
temp_phylum = []
temp_class = []
temp_order = []
temp_superkingdom = []
for domain in taxa:
    famsize = family_sizes[domain]
    for id in taxa[domain]:
        temp_superkingdom += [taxaidsdict[id][0]]
        temp_kingdom += [taxaidsdict[id][1]]
        temp_phylum += [taxaidsdict[id][2]]
        temp_class += [taxaidsdict[id][3]]
        temp_order += [taxaidsdict[id][4]]
        temp_families += [taxaidsdict[id][5]]
        temp_genera += [taxaidsdict[id][6]]
    mc_superkingdom = Counter(temp_superkingdom).most_common(1)
    mc_kingdom = Counter(temp_kingdom).most_common(1)
    mc_phylum = Counter(temp_phylum).most_common(1)
    mc_class = Counter(temp_class).most_common(1)
    mc_order = Counter(temp_order).most_common(1)
    mc_families = Counter(temp_families).most_common(1)
    mc_genera = Counter(temp_genera).most_common(1)
#Superkingdoms
    top_superking = mc_superkingdom[0][0]
    top_superking_perc = (float(mc_superkingdom[0][1])/len(temp_superkingdom))*100
    if top_superking == "unclassified":
        top_superking_pene = 0.00
    else:
        top_superking_pene = (float(mc_superkingdom[0][1])/superkingdom_count[mc_superkingdom[0][0]])*100
#Kingdoms
    top_king = mc_kingdom[0][0]
    top_king_perc = (float(mc_kingdom[0][1])/len(temp_kingdom))*100
    if top_king == "unclassified":
        top_king_pene = 0.00
    else:
        top_king_pene = (float(mc_kingdom[0][1])/kingdom_count[mc_kingdom[0][0]])*100
#Phyla
    top_phyla = mc_phylum[0][0]
    top_phyla_perc = (float(mc_phylum[0][1])/len(temp_phylum))*100
    if top_phyla == "unclassified":
        top_phyla_pene = 0.00
    else:
        top_phyla_pene = (float(mc_phylum[0][1])/phylum_count[mc_phylum[0][0]])*100
#Classes
    top_class = mc_class[0][0]
    top_class_perc = (float(mc_class[0][1])/len(temp_class))*100
    if top_class == "unclassified":
        top_class_pene = 0.00
    else:
        top_class_pene = (float(mc_class[0][1])/class_count[mc_class[0][0]])*100
#Orders
    top_ord = mc_order[0][0]
    top_ord_perc = (float(mc_order[0][1])/len(temp_order))*100
    if top_ord == "unclassified":
        top_ord_pene = 0.00
    else:
        top_ord_pene = (float(mc_order[0][1])/order_count[mc_order[0][0]])*100
#Families
    top_fam = mc_families[0][0]
    top_fam_perc = (float(mc_families[0][1])/len(temp_families))*100
    if top_fam == "unclassified":
        top_fam_pene = 0.00
    else:
        top_fam_pene = (float(mc_families[0][1])/family_count[mc_families[0][0]])*100
#Genera
    top_genus = mc_genera[0][0]
    top_genus_perc = (float(mc_genera[0][1])/len(temp_genera))*100
    if top_genus == "unclassified":
        top_genus_pene = 0.00
    else:
        top_genus_pene = (float(mc_genera[0][1])/genus_count[mc_genera[0][0]])*100
#Finding best taxa level
    best_level = "NA"
    best_taxa = "NA"
    best_perc = 0.00
    best_pene = 0.00
    best_combo = 0.00
    if top_genus != "unclassified":
        best_level = "Genus"
        best_taxa = top_genus 
        best_perc = top_genus_perc
        best_pene = top_genus_pene
        best_combo = 2*(float(top_genus_perc*top_genus_pene)/float(top_genus_perc+top_genus_pene))
    if top_fam != "unclassified" and 2*(float(top_fam_perc*top_fam_pene)/float(top_fam_perc+top_fam_pene)) >= best_combo:
        best_level = "Family"
        best_taxa = top_fam 
        best_perc = top_fam_perc
        best_pene = top_fam_pene
        best_combo = 2*(float(top_fam_perc*top_fam_pene)/float(top_fam_perc+top_fam_pene))
    if top_ord != "unclassified" and 2*(float(top_ord_perc*top_ord_pene)/float(top_ord_perc+top_ord_pene)) >= best_combo:
        best_level = "Order"
        best_taxa = top_ord 
        best_perc = top_ord_perc
        best_pene = top_ord_pene
        best_combo = 2*(float(top_ord_perc*top_ord_pene)/float(top_ord_perc+top_ord_pene))
    if top_class != "unclassified" and 2*(float(top_class_perc*top_class_pene)/float(top_class_perc+top_class_pene)) >= best_combo:
        best_level = "Class"
        best_taxa = top_class 
        best_perc = top_class_perc
        best_pene = top_class_pene
        best_combo = 2*(float(top_class_perc*top_class_pene)/float(top_class_perc+top_class_pene))
    if top_phyla != "unclassified" and 2*(float(top_phyla_perc*top_phyla_pene)/float(top_phyla_perc+top_phyla_pene)) >= best_combo:
        best_level = "Phylum"
        best_taxa = top_phyla 
        best_perc = top_phyla_perc
        best_pene = top_phyla_pene
        best_combo = 2*(float(top_phyla_perc*top_phyla_pene)/float(top_phyla_perc+top_phyla_pene))
    if top_king != "unclassified" and 2*(float(top_king_perc*top_king_pene)/float(top_king_perc+top_king_pene)) >= best_combo:
        best_level = "Kingdom"
        best_taxa = top_king
        best_perc = top_king_perc
        best_pene = top_king_pene
        best_combo = 2*(float(top_king_perc*top_king_pene)/float(top_king_perc+top_king_pene))
    if top_superking != "other sequences" and top_superking != "unclassified sequences" and 2*(float(top_superking_perc*top_superking_pene)/float(top_superking_perc+top_superking_pene)) >= best_combo:
        best_level = "Superkingdom"
        best_taxa = top_superking
        best_perc = top_superking_perc
        best_pene = top_superking_pene
        best_combo = 2*(float(top_superking_perc*top_superking_pene)/float(top_superking_perc+top_superking_pene))
    print "%s\t%s\t%s\t%s\t%.2f\t%.2f\t%.2f\t%s" %(domain,famsize,best_level,best_taxa,best_perc,best_pene,best_combo,top_superking)

#    print "%s\t%s\t%s\t%.2f\t%.2f\t%s\t%.2f\t%.2f\t%s\t%.2f\t%.2f\t%s\t%.2f\t%.2f\t%s\t%.2f\t%.2f\t%s\t%.2f\t%.2f\t%s\t%.2f\t%.2f" %(domain,famsize,top_superking,top_superking_perc,top_superking_pene,top_king,top_king_perc,top_king_pene,top_phyla,top_phyla_perc,top_phyla_pene,top_class,top_class_perc,top_class_pene,top_ord,top_ord_perc,top_ord_pene,top_fam,top_fam_perc,top_fam_pene,top_genus,top_genus_perc,top_genus_pene)
    temp_families = []
    temp_genera = []
    temp_kingdom = []
    temp_phylum = []
    temp_class = []
    temp_order = []
    temp_superkingdom = []
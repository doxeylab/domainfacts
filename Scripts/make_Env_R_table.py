import sys
from collections import defaultdict
import cPickle as pickle

if len(sys.argv) != 6:
#    print "Usage: python %s Contig_lengths.p Project_nonrep_hmmsearch.out Project_domain_coverage_table.txt samples_meta.txt mapping.txt" %(sys.argv[0])
    print "Usage: python %s Contig_lengths.p Project_flattened_clusters Project_domain_coverage_table.txt samples_meta.txt mapping.txt" %(sys.argv[0])
    sys.exit(0)

#contig lengths are non-official contig names

#mapping to send samples to their official sample name
#iowa starts need different splicing
#JCVI and CAM starts need different splicing

#meta is official sample names to environment type

counts = defaultdict(lambda: defaultdict(int))

with open(sys.argv[4],'r') as metadata:
    meta = {line.split()[0] : line.split()[1] for line in metadata}

with open(sys.argv[5],'r') as mapping:
    mapp = {line.split()[0] : line.split()[1] for line in mapping}

with open(sys.argv[1],'r') as contig_pickle:
    conlen = pickle.load(contig_pickle)

samplelen = defaultdict(int)
for entry in conlen:
    contig = entry
    sample = ""
    if contig.startswith("JCVI") or contig.startswith("CAM"):
        sample = contig
    elif contig.startswith("iowa"):
        sample = contig.split(".")[0]
    elif contig.startswith("O") or contig.startswith("MW") or contig.startswith("A"):
        sample = contig[:5]
    else: 
        sample = contig.split("_")[-1]
    if sample in mapp:
        offsample = mapp[sample]
        samplelen[offsample] += conlen[contig]

# the id order is so each column in the output follows the same column order
Pfamidorder = set()
# counting the Pfam matches and adjusting based on coverage
with open(sys.argv[2], 'r') as pfams:
    for line in pfams:
        if (not line.startswith("#")) and line.strip():
            contig = "_".join(line.split()[0].split("_")[:3])
            if contig.startswith("iowa"):
                contig = contig.split("_")[0]+"_["+contig.split("_")[1]+"]"
            elif contig.startswith("O") or contig.startswith("MW") or contig.startswith("A"):
                contig = contig.split("_")[0]
            # remove any contigs in the extreme end of coverage ###used to be 1 but I let everything in
            if contig in conlen:
                sample = ""
                if contig.startswith("JCVI") or contig.startswith("CAM"):
                    sample = contig
                elif contig.startswith("iowa"):
                    sample = contig.split(".")[0]
                elif contig.startswith("O") or contig.startswith("MW") or contig.startswith("A"):
                    sample = contig[:5]
                else: 
                    sample = contig.split("_")[-1]
                if sample in mapp:
                    offsample = mapp[sample]
                    if offsample in meta:
#                        pfam_id = line.split()[6] -> for hmmsearch but switched to clusters
                        pfam_id = line.split()[1]
                        amount = 100000000.0/samplelen[offsample]
#                        amount = 1
                        counts[offsample][pfam_id] += amount
                        # build the column order
                        Pfamidorder.add(pfam_id)

with open(sys.argv[3],'w') as output:
    # write the header
    output.write("Sample\t")
    for item in Pfamidorder:
        output.write("%s\t" %(item))
    output.write("Env\n")
    # fill in the cells of the table with the coverage-adjusted counts
    for person in counts:
        list_terms = counts[person].keys()
        output.write("%s\t" %(person))
        for item in Pfamidorder:
            if item in list_terms:
                term_count = counts[person][item]
            else:
                term_count = 0.0
            output.write("%f\t" %(term_count))
        YN = meta[person]
        output.write("%s\n" %(YN))

import sys
from collections import defaultdict

if len(sys.argv) != 4:
    print "python %s [pfamscan output] [fasta file] [mapping.txt]" %(sys.argv[0])
    sys.exit(0)

with open(sys.argv[3],'r') as mapping:
    mapp = {line.split()[0] : line.split()[1] for line in mapping}

coordinates = defaultdict(list)

with open(sys.argv[1], 'r') as pfams:
    for line in pfams:
        if (not line.startswith("#")) and line.strip():
            contig = "_".join(line.split()[0].split("_")[:3])
            if contig.startswith("iowa"):
                contig = contig.split("_")[0]+"_["+contig.split("_")[1]+"]"
            elif contig.startswith("O") or contig.startswith("MW") or contig.startswith("A"):
                contig = contig.split("_")[0]
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
                off_sample = mapp[sample]
                enint = 0
                new_name = "%s_%s_%s" %(line.split()[0],line.split()[6],str(enint))
                while any(item[0] == new_name for item in coordinates[line.split()[0]]):
                    enint += 1
                    new_name = "_".join(new_name.split("_")[:-1]+[str(enint)])
                #coordinates for cutting out alignment (not envelope)
                coordinates[line.split()[0]] += [[new_name,off_sample,int(line.split()[1]),int(line.split()[2])]]

with open(sys.argv[2],'r') as fastas:
    header = ""
    fasta = ""
    sample = ""
    for line in fastas:
        if line.startswith(">"):
            new_header = line.split()[0][1:]
            if header in coordinates:
                curr_sample = coordinates[header][0][1]
                if curr_sample == sample:
                    for item in coordinates[header]:
                        output.write(">%s\n" %(item[0]))
                        output.write("%s\n" %(fasta[item[2]-1:item[3]-1]))
                else:
                   if sample != "":
                       print "closing %s" %(sample)
                       output.close()
                   output = open("%s.fa" %(curr_sample),'a')
                   print "opening %s" %(curr_sample)
                   for item in coordinates[header]:
                       output.write(">%s\n" %(item[0]))
                       output.write("%s\n" %(fasta[item[2]-1:item[3]-1]))
                   sample = curr_sample
            header = new_header
            fasta = ""  
        else:
            fasta += line[:-1]
    if header in coordinates:
        for item in coordinates[header]:
            output.write(">%s\n" %(item[0]))
            output.write("%s\n" %(fasta[item[2]-1:item[3]-1]))
    print "closing %s" %(sample)
    output.close()

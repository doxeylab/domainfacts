# domainfacts
Statistical detection of lineage, environment, and pathogen-association protein domain families

DomainFacts is a database of pre-computed analyses of Pfam domain families. The abundance of all 17,929 families in Pfam v. 32.0 was examined in:
* NCBI (through Pfam's databases)
* Environments (human gut, marine, soil)
* Taxonomic lineages
* Pathogens vs non-pathogens

Additionally, all Pfam families were analyzed to predict:
* Co-occurring gene families across the tree of life using phylocorrelate.uwaterloo.ca (bacterial families only)
* Feasibility for structure determination


## Web-server
An interactive database for exploring our predictions is available at: https://domainfacts.uwaterloo.ca/

## Analyses and source code
* Analysis scripts are available in /scripts/
* Source code for the RShinyApp web-server is available in /server/

## Authors
Briallen Lobb, Benjamin Tremblay, and Andrew Doxey (doxey.uwaterloo.ca)

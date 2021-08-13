#-------------------------------------------------------------------------------
# PathFams v1.0
# ui.R
# Last modified: 2021-03-04 23:57:30 (CET)
# BJM Tremblay

msg("Loading ui.R")
ui <- function(request) fluidPage(

  theme = shinytheme("lumen"),

  title = "PathFams",

  br(), br(), br(),

  navbarPage(
    tags$li(
      class = "dropdown",
      tags$style(".navbar {min-height:75px} .navbar-nav {float:right;height:75px;}")
    ),
    title = div(
      img(src = "logo2.png", height = 53, width = 50),
      HTML(" &nbsp;&nbsp; "),
      HTML("<a id=\"HOMEPAGE\" href=\"#\" class=\"action-button\" style=\"color:#000000\">PathFams</a>")
    ),
    id = "NAVBAR_PAGE",
    position = "fixed-top",
    collapsible = FALSE,
    selected = "SEARCH_TAB",

    tags$head(
      tags$style(HTML("hr {border-top: 1px solid #D3D3D3;} #NAVBAR_PAGE li a[data-value=SEARCH_TAB], #NAVBAR_PAGE li a[data-value=STATS_TAB], #NAVBAR_PAGE li a[data-value=TABLE_TAB] {display:none}"))
    ),

    tabPanel("", value = "SEARCH_TAB",
      fluidRow(
        br(),
        column(2),
        column(8,
          wellPanel(
            tags$h3("Sequence input"),
            tags$h4("Enter your protein sequence to predict pathogen-associated domains."),
            textAreaInput("HMMSCAN_INPUT",
              "Input a single amino acid sequence string:",
              value = "", placeholder = "", height = "150px"
            ),
            numericInput(
              "HMMSCAN_EVALUE", "E-value:", 0.001, width = "90px"
            ),
            bsTooltip(
              "HMMSCAN_EVALUE",
              paste0(
                "For hmmscan, the E-value is the value used by the --domE ",
                "parameter.",
                " For PfamScan, the E-value is used to filter the output based on ",
                "the E-value column."
              ),
              "right", options = list(container = "body")
            ),
            radioButtons("HMMSCAN_BUTTON_MODE",
              "Mode:",
              c("Strict mode (PfamScan)", "Sensitive mode (hmmscan)")
            ),
            actionLink("HMMSCAN_BUTTON_SUBMIT", "Submit"),
          )
        ),
        column(2)
      ),
      fluidRow(
        column(2),
        column(4,
          wellPanel(
            tags$h3("Domain database"),
            tags$h4("Explore individual Pfam domains in terms of pathogen-association and other statistics."),
            selectizeInput(
              "SIDE_PANEL_INPUT_SEARCH",
              "Keyword search (e.g. \"PF17323\" or \"ToxS\")",
              choices = NULL
            ),
            textOutput("SIDE_PANEL_INPUT_SEARCH_TEXT"),
            DT::dataTableOutput("SIDE_PANEL_INPUT_SEARCH_TABLE")
          ),
          wellPanel(
            tags$h3("Full dataset"),
            tags$h4("Download link for the collated information on all the domain families."),
            "Visualizations and rankings of this data in the Domain database section and Domain rankings section, respectively, above).",
            br(), br(),
            downloadLink("DOWNLOAD_ENTIRE_TABLE2", "Full dataset")
          )
        ),
        column(4,
          wellPanel(
            tags$h3("Domain rankings"),
            tags$h4("Explore lists of top-scoring Pfam domains according to pathogen-association and other criteria."),
            selectInput(
              "DOMAIN_RANKINGS_DROPDOWN",
              "Choose a list",
              c(
                Choose = "",
                "Abundance" = "BUTTON_ABUNDANCE",
                "Environment association" = "BUTTON_ENVIRONMENT",
                "Lineage specificity" = "BUTTON_LINEAGE",
                "Pathogen association" = "BUTTON_PATHOGEN",
                "Broadly distributed and pathogen-associated" = "BUTTON_BROAD_PATHOGEN",
                "Mimicry - pathogen-associated but most common in eukaryotes" = "BUTTON_MIMICRY",
                "Human-gut associated and pathogen-associated" = "BUTTON_GUT",
                "Structural characterization feasibility" = "BUTTON_STRUCTURAL"
              ),
              selectize = FALSE
            )
          )
        ),
        column(2)
      )
    ),

    tabPanel("", value = "STATS_TAB",
      uiOutput("TAB_DOMAIN_STATS")
    ),

    tabPanel("", value = "TABLE_TAB",
      br(),
      uiOutput("TAB_DOMAIN_TABLE")
    ),

    tabPanel("", value = "HMMSCAN_TAB",
      br(),
      uiOutput("TAB_HMMSCAN_RESULTS")
    ),

    tabPanel(div(img(src="logo2.png", height = 53, width = 0), "About"),
      tags$h2("About"),
      tags$h4("PathFams is a database of pre-computed analyses of Pfam domain families. The abundance of all 17,929 families in Pfam v. 32.0 was examined in:"),
      tags$h4(tags$ul(
        tags$li("Bioinformatic databases"),
        tags$li("Environments (human gut, marine, soil)"),
        tags$li("Taxonomic lineages"),
        tags$li("Pathogens vs non-pathogens")
      )),
      tags$h4("Additionally, all Pfam families were analyzed to predict:"),
      tags$h4(tags$ul(
        # tags$li("Co-occurring gene families across the tree of life using PhyloCorrelate [bacterial families only]"),
        HTML("<li>Co-occurring gene families across the bacterial tree of life using PhyloCorrelate [<a href=\"https://phylocorrelate.uwaterloo.ca\">link</a>]</li>"),
        tags$li("Feasibility for structure determination")
      )),
      tags$br(),
      tags$h4("The results of the above analyses and other collected information about the domain families (not including Phylocorrelate data) is available to browse in several different ways:"),
      tags$h4(tags$b("Sequence input")),
      tags$h4("The sequence input section performs a domain search on your protein sequence query, and displays pathogen-association statistics for each domain match. The displayed domain matches also link to their individual pages in the Domain database (see below)."),
      tags$h4(tags$b("Domain database")),
      tags$h4("Each page in this section contains visualizations of the data for a single domain family from the various analyses listed above, and how their rankings and statistics in the various categories compare to the other domain families."),
      tags$h4(tags$b("Domain rankings")),
      tags$h4("Here there is a selection of eight different lists based on filtering and ranking the collected domain information. Each list is displayed with only pertinent columns but the full dataset is available to download at the top of every list page, and on the main page of the website (see below)."),
      tags$h4(tags$b("Full dataset")),
      tags$h4("This section only contains a download link to the full dataset that is used for the Domain database and the Domain rankings. It is a copy of Supplemental Data S3 from our manuscript."),
      tags$br(),
      tags$h4("For more information, please see our manuscript."),
      tags$h3("Citation"),
      tags$h4(tags$ul(
        tags$li("Lobb B, Tremblay BJ, Moreno-Hagelsieb G, Doxey AC. PathFams: statistical detection of pathogen-associated protein domains. Forthcoming."),
        HTML("<li>Tremblay BJ, Lobb B, Doxey AC. PhyloCorrelate: inferring bacterial gene-gene functional associations through large-scale phylogenetic profiling. Bioinformatics. 2021;37(1):17-22. <a href=\"https://doi.org/10.1093/bioinformatics/btaa1105\">https://doi.org/10.1093/bioinformatics/btaa1105</a>.</li>")
        # tags$li("Tremblay et al. (2021) PhyloCorrelate: inferring bacterial gene-gene functional associations through large-scale phylogenetic profiling. Bioinformatics, ")
      )),
      tags$h3("Team"),
      tags$h4(tags$ul(
        tags$li("Data analysis: Briallen Lobb"),
        tags$li("Developer: Benjamin Tremblay"),
        tags$li("PI: Andrew Doxey")
      ))
    )


  )

)

msg("Finished loading ui.R")
ui

#-------------------------------------------------------------------------------
# VirFams v1.0
# ui.R
# Last modified: 2020-03-29 17:38:40 (CEST)
# BJM Tremblay

msg("Loading ui.R")
ui <- function(request) fluidPage(

  theme = shinytheme("lumen"),

  title = "VirFams",

  br(), br(), br(),

  navbarPage(
    tags$li(
      class = "dropdown",
      tags$style(".navbar {min-height:75px} .navbar-nav {float:right;height:75px;}")
    ),
    title = div(
      img(src = "logo2.png", height = 53, width = 50),
      HTML(" &nbsp;&nbsp; "),
      HTML("<a id=\"HOMEPAGE\" href=\"#\" class=\"action-button\" style=\"color:#000000\">VirFams</a>")
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
              "HMMSCAN_EVALUE", "E-value:", 0.01, width = "90px"
            ),
            br(),
            actionLink("HMMSCAN_BUTTON_STRICT", "Submit: Strict mode (PfamScan)"),
            br(), br(),
            actionLink("HMMSCAN_BUTTON_SENSITIVE", "Submit: Sensitive mode (hmmscan)")
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
              "Keyword search",
              choices = NULL
            ),
            textOutput("SIDE_PANEL_INPUT_SEARCH_TEXT"),
            DT::dataTableOutput("SIDE_PANEL_INPUT_SEARCH_TABLE")
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
      tags$h4("VirFams is a database of pre-computed analyses of Pfam domain families. The abundance of all 17,929 families in Pfam v. 32.0 was examined in:"),
      tags$h4(tags$ul(
        tags$li("Bioinformatic databases"),
        tags$li("Environments (human gut, marine, soil)"),
        tags$li("Taxonomic lineages"),
        tags$li("Pathogens vs non-pathogens")
      )),
      tags$h4("Additionally, all Pfam families were analyzed to predict:"),
      tags$h4(tags$ul(
        tags$li("Co-occurring gene families across the tree of life using PhyloCorrelate[link] [bacterial families only]"),
        tags$li("Feasibility for structure determination")
      )),
      tags$h4("For more information, please see our manuscript."),
      tags$h3("Citation"),
      tags$h4(tags$ul(
        tags$li("Lobb et al. VirFams: statistical analysis of protein domain families of unknown function using phylogenomic and metagenomic data. Forthcoming."),
        tags$li("Tremblay et al. PhyloCorrelate: large-scale gene co-occurrence patterns accross the bacterial tree of life. Forthcoming.")
      )),
      tags$h3("Team"),
      tags$h4(tags$ul(
        tags$li("Data analysis: Briallen Lobb"),
        tags$li("Developer: Benjamin Tremblay"),
        tags$li("PI: Andrew Doxey")
      )),
      tags$h3("Source data"),
      tags$h4("Weblink to data")
    )


  )

)

msg("Finished loading ui.R")
ui

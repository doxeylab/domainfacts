#-------------------------------------------------------------------------------
# DomainFacts v1.0
# ui.R
# Last modified: 2020-03-28 16:21:36 (CET)
# BJM Tremblay

msg("Loading ui.R")
ui <- function(request) fluidPage(

  theme = shinytheme("lumen"),

  title = "DomainFacts",

  br(), br(), br(),

  navbarPage(
    tags$li(
      class = "dropdown",
      tags$style(".navbar {min-height:75px} .navbar-nav {float:right;height:75px;}")
    ),
    title = div(
      img(src = "logo2.png", height = 53, width = 50),
      HTML(" &nbsp;&nbsp; "),
      HTML("<a id=\"HOMEPAGE\" href=\"#\" class=\"action-button\" style=\"color:#000000\">DomainFacts</a>")
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
        column(4,
          wellPanel(
            tags$h3("Individual domain search"),
            br(),
            checkboxInput(
              "SIDE_PANEL_INPUT_NONDUF_CHECKBOX",
              label = "Show non-DUFs",
              value = TRUE
            ),
            checkboxInput(
              "SIDE_PANEL_INPUT_DUF_CHECKBOX",
              label = "Show DUFs",
              value = TRUE
            ),
            selectizeInput(
              "SIDE_PANEL_INPUT_SEARCH",
              "Keyword search",
              choices = NULL
            ),
            # searchInput(
            #   "SIDE_PANEL_INPUT_SEARCH",
            #   "Keyword search",
            #   btnSearch = icon("search"),
            #   btnReset = icon("remove")
            # ),
            textOutput("SIDE_PANEL_INPUT_SEARCH_TEXT"),
            DT::dataTableOutput("SIDE_PANEL_INPUT_SEARCH_TABLE")
          )
        ),
        column(4,
          wellPanel(
            tags$h3("Domain table with preset filters"), br(),
            actionLink("BUTTON_ABUNDANCE", "Abundance"), br(), br(),
            actionLink("BUTTON_ENVIRONMENT", "Environment association"), br(), br(),
            actionLink("BUTTON_LINEAGE", "Lineage specificity"), br(), br(),
            actionLink("BUTTON_PATHOGEN", "Pathogen association"), br(), br(),
            actionLink("BUTTON_BROAD_PATHOGEN", "Broadly distributed and pathogen-associated"), br(), br(),
            actionLink("BUTTON_MIMICRY", "Mimicry - pathogen-associated but most common in eukaryotes"), br(), br(),
            actionLink("BUTTON_GUT", "Human-gut associated and pathogen-associated"), br(), br(),
            actionLink("BUTTON_STRUCTURAL", "Structural characterization feasibility")
          )
        ),
        column(2)
      ),
      fluidRow(
        column(2),
        column(8,
          wellPanel(
            tags$h3("hmmscan Input"),
            br(),
            textAreaInput("HMMSCAN_INPUT",
              "Input a single amino acid sequence string:",
              value = "", placeholder = "", height = "150px"
            ),
            numericInput(
              "HMMSCAN_EVALUE", "E-value:", 0.05, width = "90px"
            ),
            actionLink("HMMSCAN_BUTTON", "Submit")
          )
          # plotOutput("HMMSCAN_PLOT")
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
      tags$h4("DomainFacts is a database of pre-computed analyses of Pfam domain families. The abundance of all 17,929 families in Pfam v. 32.0 was examined in:"),
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
        tags$li("Lobb et al. DomainFacts: statistical analysis of protein domain families of unknown function using phylogenomic and metagenomic data. Forthcoming."),
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

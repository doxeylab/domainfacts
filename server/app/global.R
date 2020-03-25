#-------------------------------------------------------------------------------
# DomainFacts v1.0
# global.R
# Last modified: 2020-03-25 20:56:06 (CET)
# BJM Tremblay

msg <- function(...) {
  time <- format(as.POSIXlt(Sys.time(), tz = "America/Toronto"))
  message(paste0(c(time, ...), collapse = " "))
}
msg("Loading global.R")

msg("Loading packages")

msg("  shiny")
library(shiny)
msg("  DT")
suppressPackageStartupMessages(library(DT))
msg("  shinythemes")
library(shinythemes)
msg("  shinyWidgets")
library(shinyWidgets)
msg("  plotly")
suppressPackageStartupMessages(library(plotly))
msg("  shinycssloaders")
library(shinycssloaders)
# msg("  shinyjs")
# suppressPackageStartupMessages(library(shinyjs))

#-------------------------------------------------------------------------------
# Data - in memory

msg("Loading data")

DATA_ALL <- fst::read_fst("data/all.fst")
NAMES_OLD <- colnames(DATA_ALL)
colnames(DATA_ALL) <- c(LETTERS, paste0("A", LETTERS[1:15]))
names(NAMES_OLD) <- colnames(DATA_ALL)
rownames(DATA_ALL) <- DATA_ALL$B
DATA_ALL <- DATA_ALL[, colnames(DATA_ALL) != "B"]

DATA_ALL$H <- order(DATA_ALL$H)

FREQ_DAT <- fst::read_fst("data/DomainDiversityFrequencyTable.fst")

toround <- c(
  "H", "I", "J", "K", "L", "M", "N", "R", "S", "T", "U", "V", "Y", "Z", "AA",
  "AF", "AG", "AH", "AI", "AJ", "AM", "AN", "AO"
)
for (i in seq_along(toround)) {
  DATA_ALL[[toround[i]]] <- round(DATA_ALL[[toround[i]]], 3)
}

PFAMs <- structure(DATA_ALL[[1]], names = rownames(DATA_ALL))
PFAMsearchTable <- data.frame(
  row.names = rownames(DATA_ALL),
  Domain = DATA_ALL$A,
  Type = DATA_ALL$C,
  Status = DATA_ALL$D,
  stringsAsFactors = FALSE
)

GET_BUTTONS <- function() {
  c(
    "BUTTON_ABUNDANCE",
    "BUTTON_ENVIRONMENT",
    "BUTTON_LINEAGE",
    "BUTTON_PATHOGEN",
    "BUTTON_BROAD_PATHOGEN",
    "BUTTON_MIMICRY",
    "BUTTON_GUT",
    "BUTTON_STRUCTURAL"
  )
}

GET_FILTER_NAME <- function() {
  c(
    "BUTTON_ABUNDANCE" = "Abundance",
    "BUTTON_ENVIRONMENT" = "Environment association",
    "BUTTON_LINEAGE" = "Lineage specificity",
    "BUTTON_PATHOGEN" = "Pathogen association",
    "BUTTON_BROAD_PATHOGEN" = "Broadly distributed and pathogen-associated",
    "BUTTON_MIMICRY" = "Mimicry - pathogen-associated but most common in eukaryotes",
    "BUTTON_GUT" = "Human-gut associated and pathogen-associated",
    "BUTTON_STRUCTURAL" = "Structural characterization feasibility"
  )
}

PFAMsWithPMFs <- readRDS("data/PFAMsWithPMFs.RDS")

PFAMBestPMF <- function(j) {
  if (!j %in% PFAMsWithPMFs) {
    data.frame()
  } else {
    out <- data.frame(
      row.names = PFAMsWithPMFs,
      Probability = factor(
        fst::read_fst("data/PFAMBestPMF.fst", j)[[1]],
        levels = c("Very high", "High", "Low", "Very low")
      ),
      Description = PFAMDesc[PFAMsWithPMFs],
      stringsAsFactors = FALSE
    )
    out <- out[PFAMsWithPMFs != j, , drop = FALSE]
    if (!nrow(out)) return(out)
    out <- out[!out[[1]] %in% "Very low", , drop = FALSE]
    if (!nrow(out)) out <- out[order(out$Probability), ]
    out
  }
}

PFAMDesc <- readRDS("data/PFAMDescriptions.RDS")

#-------------------------------------------------------------------------------
# Functions

noNA <- function(x) x[!is.na(x)]

perc_rank <- function(x, x0, decreasing = FALSE) {
  if (!decreasing)
    (sum(x <= x0) / length(x)) * 100
  else
    (sum(x >= x0) / length(x)) * 100
}

searchPFAMs <- function(x, hideNonDUFs = TRUE, hideDUFs = FALSE) {
  res <- PFAMs[grepl(x, PFAMs, ignore.case = TRUE) | grepl(x, names(PFAMs), ignore.case = TRUE)]
  if (!length(res)) return(NULL)
  res <- PFAMsearchTable[names(res), ]
  if (!nrow(res)) return(NULL)
  if (hideDUFs) res <- res[res$Status != "DUF", ]
  if (!nrow(res)) return(NULL)
  if (hideNonDUFs) res <- res[res$Status != "non-DUF", ]
  if (!nrow(res)) return(NULL)
  res
}

make_search_table <- function(x) {
  if (is.null(x)) return()
  DT::datatable(x,
    options = list(dom = "pt", bSort = FALSE),
    selection = "none",
    # colnames = "",
    class = "cell-border strip hover",
    escape = FALSE
  ) %>% formatStyle(0, cursor = "pointer")
}

filter_duf <- function(x, hideDUFs, hideNonDUFs) {
  if (hideDUFs) x <- x[x$D != "DUF", ]
  if (hideNonDUFs) x <- x[x$D != "non-DUF", ]
  x
}

filter_table <- function(x, hideDUFs = FALSE, hideNonDUFs = TRUE, returnAll = FALSE) {
  switch(x,
    BUTTON_ABUNDANCE = {
      out <- DATA_ALL[, c("A", "C", "D", "H", "F", "AG")]
      if (returnAll) out <- cbind(out, DATA_ALL[, !colnames(DATA_ALL) %in% out])
      out <- out[order(out$H), ]
      out <- out[!is.na(out$H) & !is.na(out$F) & !is.na(out$AG), ]
      filter_duf(out, hideDUFs, hideNonDUFs)
    },
    BUTTON_ENVIRONMENT = {
      out <- DATA_ALL[, c("A", "C", "D", "AL", "AK")]
      if (returnAll) out <- cbind(out, DATA_ALL[, !colnames(DATA_ALL) %in% out])
      out <- out[order(out$AL), ]
      out <- out[!is.na(out$AL), ]
      out <- out[out$AL < 1e-15, ]
      filter_duf(out, hideDUFs, hideNonDUFs)
    },
    BUTTON_LINEAGE = {
      out <- DATA_ALL[, c("A", "C", "D", "AA", "G", "W", "X")]
      if (returnAll) out <- cbind(out, DATA_ALL[, !colnames(DATA_ALL) %in% out])
      out <- out[order(out$AA, decreasing = TRUE), ]
      out <- out[!is.na(out$AA) & !is.na(out$G), ]
      out <- out[out$G >= 20, ]
      filter_duf(out, hideDUFs, hideNonDUFs)
    },
    BUTTON_PATHOGEN = {
      out <- DATA_ALL[, c("A", "C", "D", "AF", "AE", "AC")]
      if (returnAll) out <- cbind(out, DATA_ALL[, !colnames(DATA_ALL) %in% out])
      out <- out[order(out$AF, decreasing = TRUE), ]
      out <- out[!is.na(out$AF) & !is.na(out$AE) & !is.na(out$AC), ]
      out <- out[out$AE < 0.05 & out$AC >= 5, ]
      filter_duf(out, hideDUFs, hideNonDUFs)
    },
    BUTTON_BROAD_PATHOGEN = {
      out <- DATA_ALL[, c("A", "C", "D", "Z", "X", "W", "AE")]
      if (returnAll) out <- cbind(out, DATA_ALL[, !colnames(DATA_ALL) %in% out])
      out <- out[order(out$Z), ]
      out <- out[!is.na(out$Z) & !is.na(out$W) & !is.na(out$X) & !is.na(out$AE), ]
      out <- out[out$W == "Superkingdom", ]
      out <- out[out$X == "Bacteria", ]
      out <- out[out$AE < 0.05, ]
      filter_duf(out, hideDUFs, hideNonDUFs)
    },
    BUTTON_MIMICRY = {
      out <- DATA_ALL[, c("A", "C", "D", "AB", "AE", "AF")]
      if (returnAll) out <- cbind(out, DATA_ALL[, !colnames(DATA_ALL) %in% out])
      out <- out[order(out$AE), ]
      out <- out[!is.na(out$AB) & !is.na(out$AE) & !is.na(out$AF), ]
      out <- out[out$AB == "Eukaryota", ]
      out <- out[out$AE < 0.05, ]
      filter_duf(out, hideDUFs, hideNonDUFs)
    },
    BUTTON_GUT = {
      out <- DATA_ALL[, c("A", "C", "D", "AF", "AE", "AK", "AL")]
      if (returnAll) out <- cbind(out, DATA_ALL[, !colnames(DATA_ALL) %in% out])
      out <- out[order(out$AF, decreasing = TRUE), ]
      out <- out[!is.na(out$AF) & !is.na(out$AE) & !is.na(out$AK) & !is.na(out$AL), ]
      out <- out[out$AE < 0.05, ]
      out <- out[out$AK == "HumanGut", ]
      out <- out[out$AL < 1e-15, ]
      filter_duf(out, hideDUFs, hideNonDUFs)
    },
    BUTTON_STRUCTURAL = {
      out <- DATA_ALL[, c("A", "C", "D", "Q", "S", "U", "V")]
      if (returnAll) out <- cbind(out, DATA_ALL[, !colnames(DATA_ALL) %in% out])
      out <- out[!is.na(out$Q) & !is.na(out$S) & !is.na(out$U) & !is.na(out$V), ]
      out <- out[out$Q == "not in PDB", ]
      out <- out[out$S < 10 & out$U < 10 & out$V < 10, ]
      filter_duf(out, hideDUFs, hideNonDUFs)
    }
  )
}

make_col_container <- function(x) {
  htmltools::withTags(table(
    class = "display",
    thead(
      switch(x,
        BUTTON_ABUNDANCE = tr(
          th(""),
          th("Domain"),
          th("Type of PFAM entry"),
          th("DUF status"),
          th("Avg abundance rank", title = "Average rank of column I (# of species in Pfam taxa ids (proteomes) with domain), column F (# of proteins with domain hits in NCBI sequence database), and column AG (Average of the normalized adjusted family size across all environmental samples)"),
          th("NCBI proteins w/ domain", title = "# of proteins with domain hits in NCBI sequence database"),
          th("Metagenomic abundance", title = "Average of the normalized adjusted family size across all environmental samples")
        ),
        BUTTON_ENVIRONMENT = tr(
          th(""),
          th("Domain"),
          th("Type of PFAM entry"),
          th("DUF status"),
          th("Environment enrichment Q-value", title = "Adjusted (BH) p-value from the Kruskall-Wallis test - difference in the normalized adjusted family size in at least one environment"),
          th("Environment w/ highest abundance", title = "Environment with the highest average")
        ),
        BUTTON_LINEAGE = tr(
          th(""),
          th("Domain"),
          th("Type of PFAM entry"),
          th("DUF status"),
          th("Highest F1 score", title = "Highest F1 score (only compared to the most common taxa for each domain at each taxonomic level)"),
          th("Proteomes w/ domain", title = "# of Pfam taxa ids (proteomes) with this domain"),
          th("Taxonomic level w/ highest F1 scores", title = "Taxonomic level with highest F1 score"),
          th("Taxon w/ highest F1", title = "Taxa with highest F1 score (only compared to the most common taxa for each domain at each taxonomic level)")
        ),
        BUTTON_PATHOGEN = tr(
          th(""),
          th("Domain"),
          th("Type of PFAM entry"),
          th("DUF status"),
          th("Fold change", title = "Fold change of the domain in pathogen bacterial Pfam taxa ids (proteomes) over non-pathogen bacterial Pfam taxa ids (proteomes)"),
          th("Pathogen enrichment Q-value", title = "Adjusted (BH) p-value from the hypergeometric test - enrichment of the domain in bacterial pathogens"),
          th("Pathogen frequency", title = "# of bacterial pathogen Pfam taxa ids (proteomes) containing this domain")
        ),
        BUTTON_BROAD_PATHOGEN = tr(
          th(""),
          th("Domain"),
          th("Type of PFAM entry"),
          th("DUF status"),
          th("Precision of taxa w/ highest F1", title = "Precision of the taxa with the highest F1 score (only compared to the most common taxa for each domain at each taxonomic level)"),
          th("Taxon w/ highest F1", title = "Taxa with highest F1 score (only compared to the most common taxa for each domain at each taxonomic level)"),
          th("Taxonomic level", title = "Taxonomic level with highest F1 score"),
          th("Pathogen enrichment Q-value", title = "Adjusted (BH) p-value from the hypergeometric test - enrichment of the domain in bacterial pathogens")
        ),
        BUTTON_MIMICRY = tr(
          th(""),
          th("Domain"),
          th("Type of PFAM entry"),
          th("DUF status"),
          th("Most common superkingdom", title = "Most common superkingdom for each domain"),
          th("Pathogen enrichment Q-value", title = "Adjusted (BH) p-value from the hypergeometric test - enrichment of the domain in bacterial pathogens"),
          th("Fold change", title = "Fold change of the domain in pathogen bacterial Pfam taxa ids (proteomes) over non-pathogen bacterial Pfam taxa ids (proteomes)")
        ),
        BUTTON_GUT = tr(
          th(""),
          th("Domain"),
          th("Type of PFAM entry"),
          th("DUF status"),
          th("Fold change", title = "Fold change of the domain in pathogen bacterial Pfam taxa ids (proteomes) over non-pathogen bacterial Pfam taxa ids (proteomes)"),
          th("Pathogen enrichment Q-value", title = "Adjusted (BH) p-value from the hypergeometric test - enrichment of the domain in bacterial pathogens"),
          th("Environment w/ highest abundance", title = "Environment with the highest average"),
          th("Environment enrichment Q-value", title = "Adjusted (BH) p-value from the Kruskall-Wallis test - difference in the normalized adjusted family size in at least one environment")
        ),
        BUTTON_STRUCTURAL = tr(
          th(""),
          th("Domain"),
          th("Type of PFAM entry"),
          th("DUF status"),
          th("In PDB?", title = "Are there members of this family with structures in PDB?"),
          th("% Disorder", title = "Average % of disordered residues across the domain region for the domain family"),
          th("% With TM in domain", title = "% of family members whose domains have a predicted transmembrane region overlapping them"),
          th("% With TM in protein", title = "% of family members whose proteins have a transmembrane region predicted")
        )
      )
    )
  ))
}

make_domain_download <- function(x, hideDUFs = FALSE, hideNonDUFs = TRUE) {
  x <- filter_table(x, hideDUFs, hideNonDUFs, TRUE)
  colnames(x) <- NAMES_OLD[colnames(x)]
  cbind(ID = rownames(x), x)
}

make_pmf_table <- function(x) {
  DT::datatable(
    x,
    options = list(
      pageLength = 10,
      dom = "tip"
    ),
    selection = "none"
  ) %>%
    formatStyle(0, cursor = "pointer")
}

make_domain_table <- function(x, hideDUFs = FALSE, hideNonDUFs = TRUE) {
  res <- DT::datatable(
    filter_table(x, hideDUFs, hideNonDUFs),
    options = list(
      pageLength = 25,
      scrollX = TRUE,
      dom = "tip"
    ),
    selection = "none",
    container = make_col_container(x)
  ) %>%
    formatStyle(0, cursor = "pointer")
  res
}

make_domain_table_tab <- function(x) {
  tagList(
    wellPanel(
      tags$h4(paste("Selected preset filter:", x)),
      checkboxInput(
        "DOMAIN_TABLE_NONDUF_CHECKBOX",
        label = "Show non-DUFs",
        value = TRUE
      ),
      checkboxInput(
        "DOMAIN_TABLE_DUF_CHECKBOX",
        label = "Show DUFs",
        value = TRUE
      ),
      "Click on a PFAM ID to view domain stats. Hover over column names for more details.",
      downloadLink("DOWNLOAD_TABLE", "Click to download table."),
      br(), br(),
      DT::dataTableOutput("DOMAIN_TABLE")
    )
  )
}

make_summary_plot <- function(x) {

  p_pathogen <- DATA_ALL[x, "AE"]
  p_pathogen_all <- noNA(DATA_ALL$AE)
  p_pathogen_all <- p_pathogen_all[p_pathogen_all < 0.05]
  if (is.na(p_pathogen)) {
    p_pathogen <- 0
  } else {
    p_pathogen <- perc_rank(p_pathogen_all, p_pathogen, TRUE)
  }

  p_abundance1 <- DATA_ALL[x, "F"]
  p_abundance2 <- DATA_ALL[x, "I"]
  if (is.na(p_abundance1) & is.na(p_abundance2)) {
    p_abundance <- 0
  } else {
    if (is.na(p_abundance1)) p_abundance1 <- 0
    if (is.na(p_abundance2)) p_abundance2 <- 0
    p_abundance1 <- perc_rank(noNA(DATA_ALL$F), p_abundance1)
    p_abundance2 <- perc_rank(noNA(DATA_ALL$I), p_abundance2)
    p_abundance <- mean(c(p_abundance1, p_abundance2))
  }

  p_environment <- DATA_ALL[x, "AL"]
  p_environment_all <- noNA(DATA_ALL$AL)
  p_environment_all <- p_environment_all[p_environment_all < 0.05]
  if (is.na(p_environment)) {
    p_environment <- 0
  } else {
    p_environment <- perc_rank(noNA(DATA_ALL$AL), p_environment, TRUE)
  }

  p_lineage <- DATA_ALL[x, "AA"]
  if (is.na(p_lineage)) p_lineage <- 0
  p_lineage <- perc_rank(noNA(DATA_ALL$AA), p_lineage)

  plot_ly(
    type = "scatterpolar",
    r = c(p_pathogen, p_abundance, p_environment, p_lineage),
    theta = c("Pathogen", "Abundance", "Environment", "Lineage"),
    fill = "toself",
    alpha = 0.6
    # mode = "lines+markers"
  ) %>%
    layout(
      polar = list(
        radialaxis = list(
          range = c(0, 100)
        ),
        visible = TRUE,
        range = c(0, 100)
      ),
      showlegend = FALSE
    )

}

make_abundance_plot_1 <- function(x) {
  dufs <- noNA(log10(DATA_ALL$F[DATA_ALL$D == "DUF"]))
  nondufs <- noNA(log10(DATA_ALL$F[DATA_ALL$D == "non-DUF"]))
  pall <- noNA(log10(DATA_ALL$F))
  xline <- log10(DATA_ALL$F[rownames(DATA_ALL) == x])
  p <- plot_ly(alpha = 0.6) %>%
    add_histogram(x = ~pall, name = "All") %>%
    add_histogram(x = ~dufs, name = "DUFs") %>%
    add_histogram(x = ~nondufs, name = "non-DUFs") %>%
    layout(
      barmode = "overlay",
      xaxis = list(title = "log10(Proteins with domain)"),
      yaxis = list(title = "Abudance")
    )
  if (is.na(xline))
    p
  else
    p %>% add_segments(x = xline, xend = xline, y = 0, yend = 600, name = x)
}

make_abundance_plot_2 <- function(x) {
  dufs <- noNA(log10(DATA_ALL$I[DATA_ALL$D == "DUF"]))
  nondufs <- noNA(log10(DATA_ALL$I[DATA_ALL$D == "non-DUF"]))
  pall <- noNA(log10(DATA_ALL$I))
  xline <- log10(DATA_ALL$I[rownames(DATA_ALL) == x])
  p <- plot_ly(alpha = 0.6) %>%
    add_histogram(x = ~pall, name = "All") %>%
    add_histogram(x = ~dufs, name = "DUFs") %>%
    add_histogram(x = ~nondufs, name = "non-DUFs") %>%
    layout(
      barmode = "overlay",
      xaxis = list(title = "log10(% species with domain)"),
      yaxis = list(title = "Abudance")
    )
  if (is.na(xline))
    p
  else
    p %>% add_segments(x = xline, xend = xline, y = 0, yend = 800, name = x)
}

make_lineage_plot <- function(x) {
  dufs <- noNA(DATA_ALL$AA[DATA_ALL$D == "DUF"])
  nondufs <- noNA(DATA_ALL$AA[DATA_ALL$D == "non-DUF"])
  pall <- noNA(DATA_ALL$AA)
  xline <- DATA_ALL$AA[rownames(DATA_ALL) == x]
  if (is.na(xline)) return(NULL)
  p <- plot_ly(alpha = 0.6) %>%
    add_histogram(x = ~pall, name = "All") %>%
    add_histogram(x = ~dufs, name = "DUFs") %>%
    add_histogram(x = ~nondufs, name = "non-DUFs") %>%
    layout(
      barmode = "overlay",
      xaxis = list(title = "Highest F1 score"),
      yaxis = list(title = "Abudance")
    )
  if (is.na(xline))
    p
  else
    p %>% add_segments(x = xline, xend = xline, y = 0, yend = 700, name = x)
}

make_environment_plot <- function(x) {
  x_name <- DATA_ALL[x, "A"]
  if (x_name %in% colnames(FREQ_DAT)) {
    whichcol <- which(colnames(FREQ_DAT) == x_name)
  } else {
    whichcol <- grep(
      gsub("-", ".", x_name, fixed = TRUE),
      colnames(FREQ_DAT),
      fixed = TRUE
    )
  }
  if (!length(whichcol)) return(NULL)
  p_HumanGut <- FREQ_DAT[[whichcol]][FREQ_DAT$Env == "HumanGut"]
  p_Marine <- FREQ_DAT[[whichcol]][FREQ_DAT$Env == "Marine"]
  p_Soil <- FREQ_DAT[[whichcol]][FREQ_DAT$Env == "Soil"]
  plot_ly(
    alpha = 0.6,
    y = ~p_HumanGut,
    name = "Human gut",
    type = "box"
  ) %>%
    add_trace(
      y = ~p_Soil,
      name = "Soil"
    ) %>%
    add_trace(
      y = ~p_Marine,
      name = "Marine"
    ) %>%
    layout(
      showlegend = FALSE,
      yaxis = list(title = "Adjusted family size")
    )
}

make_pathogen_plot <- function(x) {
  pp <- (DATA_ALL[x, "AC"] / 354) * 100
  pnp <- (DATA_ALL[x, "AD"] / 7897) * 100
  app <- (mean(DATA_ALL$AC, na.rm = TRUE) / 354) * 100
  apnp <- (mean(DATA_ALL$AD, na.rm = TRUE) / 7897) * 100
  dpp <- (mean(DATA_ALL$AC[DATA_ALL$D == "DUF"], na.rm = TRUE) / 354) * 100
  dpnp <- (mean(DATA_ALL$AD[DATA_ALL$D == "DUF"], na.rm = TRUE) / 7897) * 100
  ndpp <- (mean(DATA_ALL$AC[DATA_ALL$D == "non-DUF"], na.rm = TRUE) / 354) * 100
  ndpnp <- (mean(DATA_ALL$AD[DATA_ALL$D == "non-DUF"], na.rm = TRUE) / 7897) * 100
  if (is.na(pp)) pp <- 0
  if (is.na(pnp)) pnp <- 0
  d <- data.frame(
    What = c("Pathogen", "Non-pathogen"),
    PFAM = c(pp, pnp),
    Background = c(app, apnp),
    DUFs = c(dpp, dpnp),
    nonDUFs = c(ndpp, ndpnp)
  )
  plot_ly(
    d,
    alpha = 0.6,
    x = ~What,
    y = ~Background,
    type = "bar",
    name = "Average"
  ) %>%
    add_trace(
      y = ~DUFs,
      name = "DUF average"
    ) %>%
    add_trace(
      y = ~nonDUFs,
      name = "non-DUF average"
    ) %>%
    add_trace(
      y = ~PFAM,
      name = x
    ) %>%
    layout(
      barmode = "group",
      xaxis = list(title = ""),
      yaxis = list(range = c(0, 101), title = "Percent of proteomes")
    )
}

make_domain_info <- function(x) {
  y <- PFAMsearchTable[x, ]
  paste0(
    "<div\n",
    "  <p><B>ID:</B> ", x, "</p>\n",
    "  <p><B>Domain:</B> ", y$Domain, "</p>\n",
    "  <p><B>Type:</B> ", y$Type, "</p>\n",
    "  <p><B>Status:</B> ", y$Status, "</p>\n",
    "  <p><B>Links:</B> ",
    "<a href=https://pfam.xfam.org/family/", x,
    " target=_blank>PFAM</a> <a ",
    "href=http://annotree.uwaterloo.ca/app/#/?qtype=pfam&qstring=", x,
    "&eval=0.00001 target=_blank>AnnoTree</a>",
    if (x %in% PFAMsWithPMFs) {
      paste0(
        " <a href=https://phylocorrelate.uwaterloo.ca/app/?goto=", x,
        " target=_blank>PhyloCorrelate</a>"
      )
    } else {
      ""
    },
    "</div"
  )
}

make_domain_stats_tab <- function() {
  tagList(
    br(),
    fluidRow(
      column(1),
      column(5,
        wellPanel(
          tags$h3("Domain info"),
          htmlOutput("DOMAIN_INFO")
        ),
        wellPanel(
          tags$h3("Top co-occurring protein families from PhyloCorrelate"),
          "Click on a PFAM ID to load it.",
          DT::dataTableOutput("PMF_TABLE")
        )
      ),
      column(5,
        wellPanel(
          tags$h3("Summary statistics: inverse percentile ranks"),
          plotlyOutput("SUMMARY_PLOT")
        )
      ),
      column(1)
    ),
    fluidRow(
      column(1),
      column(5,
        wellPanel(
          tags$h3("Environmental association"),
          plotlyOutput("ENVIRONMENT_PLOT")
        )
      ),
      column(5,
        wellPanel(
          tags$h3("Pathogen association"),
          plotlyOutput("PATHOGEN_PLOT")
        )
      ),
      column(1)
    ),
    fluidRow(
      column(1),
      column(5,
        wellPanel(
          tags$h3("Abundance"),
          plotlyOutput("ABUNDANCE_PLOT_1"),
          plotlyOutput("ABUNDANCE_PLOT_2")
        )
      ),
      column(5,
        wellPanel(
          tags$h3("Lineage specificity"),
          plotlyOutput("LINEAGE_PLOT")
        )
      ),
      column(1)
    )
  )
}

#-------------------------------------------------------------------------------

msg("Finished loading global.R")
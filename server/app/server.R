#-------------------------------------------------------------------------------
# DomainFacts v1.0
# server.R
# Last modified: 2020-03-28 13:02:59 (CET)
# BJM Tremblay

msg("Loading server.R")
server <- function(input, output, session) {

  msg("Session start:", session$token)
  onSessionEnded({function() {
    msg("Session stop:", session$token)
  }})

  SelectedPFAM <- reactiveValues(Id = "PF06207", Success = NULL)
  SelectedFilter <- reactiveValues(Which = "BUTTON_ABUNDANCE")

  output$SIDE_PANEL_INPUT_SEARCH_TEXT <- renderText({
    if (isTRUE(SelectedPFAM$Success))
      "Click on a PFAM ID to view domain stats."
    else if (isFALSE(SelectedPFAM$Success))
      "No results, try again."
    else
      NULL
  })

  updateSelectizeInput(
    session,
    "SIDE_PANEL_INPUT_SEARCH",
    choices = rownames(DATA_ALL),
    selected = "",
    server = TRUE,
    options = list(
      maxOptions = 1000,
      create = TRUE,
      allowEmptyOption = TRUE,
      addPrecedence = TRUE
    )
  )

  output$SIDE_PANEL_INPUT_SEARCH_TABLE <- renderDataTable({
    req(input$SIDE_PANEL_INPUT_SEARCH)
    res <- searchPFAMs(
      input$SIDE_PANEL_INPUT_SEARCH,
      !input$SIDE_PANEL_INPUT_NONDUF_CHECKBOX,
      !input$SIDE_PANEL_INPUT_DUF_CHECKBOX
    )
    if (!is.null(res)) {
      SelectedPFAM$Success <- TRUE
      make_search_table(res)
    } else {
      SelectedPFAM$Success <- FALSE
      NULL
    }
  })

  observeEvent(input$SIDE_PANEL_INPUT_SEARCH_TABLE_cell_clicked, {
    what <- input$SIDE_PANEL_INPUT_SEARCH_TABLE_cell_clicked
    req(what$value)
    if (what$col != 0) return()
    SelectedPFAM$Id <- what$value
    updateNavbarPage(session, "NAVBAR_PAGE", "STATS_TAB")
  })

  output$TAB_DOMAIN_TABLE <- renderUI({
    make_domain_table_tab(GET_FILTER_NAME()[SelectedFilter$Which])
  })

  output$DOMAIN_TABLE <- DT::renderDataTable({
    make_domain_table(
      SelectedFilter$Which,
      !input$DOMAIN_TABLE_DUF_CHECKBOX,
      !input$DOMAIN_TABLE_NONDUF_CHECKBOX
    )
  })

  observeEvent(input$HOMEPAGE, {
    updateNavbarPage(session, "NAVBAR_PAGE", "SEARCH_TAB")
  })

  observe({
    lapply(GET_BUTTONS(), function(x) {
      observeEvent(input[[x]], {
        SelectedFilter$Which <- x
        updateNavbarPage(session, "NAVBAR_PAGE", "TABLE_TAB")
      })
    })
  })

  observeEvent(input$DOMAIN_TABLE_cell_clicked, {
    what <- input$DOMAIN_TABLE_cell_clicked
    req(what$value)
    if (what$col != 0) return()
    SelectedPFAM$Id <- what$value
    updateNavbarPage(session, "NAVBAR_PAGE", "STATS_TAB")
  })

  output$DOWNLOAD_TABLE <- downloadHandler(
    filename = paste0(gsub("BUTTON_", "", SelectedFilter$Which), "_table.tsv"),
    content = function(con) {
      readr::write_tsv(make_domain_download(
        SelectedFilter$Which,
        !input$DOMAIN_TABLE_DUF_CHECKBOX,
        !input$DOMAIN_TABLE_NONDUF_CHECKBOX
      ), con)
    }
  )

  output$TAB_DOMAIN_STATS <- renderUI({
    make_domain_stats_tab()
  })

  output$SUMMARY_PLOT <- renderPlotly({
    make_summary_plot(SelectedPFAM$Id)
  })

  output$ABUNDANCE_PLOT_1 <- renderPlotly({
    make_abundance_plot_1(SelectedPFAM$Id)
  })

  output$ABUNDANCE_PLOT_2 <- renderPlotly({
    make_abundance_plot_2(SelectedPFAM$Id)
  })

  output$LINEAGE_PLOT <- renderPlotly({
    make_lineage_plot(SelectedPFAM$Id)
  })

  output$ENVIRONMENT_PLOT <- renderPlotly({
    ep <- make_environment_plot(SelectedPFAM$Id)
    validate(need(
      !is.null(ep),
      "Not detected in metagenomic samples."
    ))
    ep
  })

  output$PATHOGEN_PLOT <- renderPlotly({
    make_pathogen_plot(SelectedPFAM$Id)
  })

  output$DOMAIN_INFO <- renderText({
    make_domain_info(SelectedPFAM$Id)
  })

  observeEvent(input$PMF_TABLE_cell_clicked, {
    what <- input$PMF_TABLE_cell_clicked
    req(what$value)
    if (what$col != 0) return()
    SelectedPFAM$Id <- what$value
  })

  output$PMF_TABLE <- DT::renderDataTable({
    validate(need(
      SelectedPFAM$Id %in% PFAMsWithPMFs,
      "Not present in PhyloCorrelate database."
    ))
    x <- PFAMBestPMF(SelectedPFAM$Id)
    validate(need(
      nrow(x) > 0,
      "Could not find any PFAMs with probable matching function."
    ))
    make_pmf_table(x)
  })

  HmmScanPlot <- reactiveValues(Plot = NULL)

  observeEvent(input$HMMSCAN_BUTTON, {
    req(input$HMMSCAN_INPUT)
    res <- run_hmm(input$HMMSCAN_INPUT)
    if (!is.null(res)) {
      HmmScanPlot$Plot <- plot_domains(res)
    }
  })

  output$HMMSCAN_PLOT <- renderPlot({
    req(HmmScanPlot$Plot)
    HmmScanPlot$Plot
  })

}

msg("Finished loading server.R")
server

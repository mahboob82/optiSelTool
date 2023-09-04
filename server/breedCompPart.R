
  # ********************************************************************************************************************
  # ===BREED COMPOSITION CALCULATION====================================================================================
  # ********************************************************************************************************************


  # ---- pedbreedcomp-breed
  updateSelectizeInput(
    session,
    'pedbreedcomp_breed',
    choices = NULL,
    #selected = selected_breed,
    # selected="Hinterwaelder",
    server = TRUE
  )

  #---- pedbreedcomp_btn events --------------------------------------------------------------------
  observeEvent(input$btn_pedbreedcomp, {
    if (!nrow(Pedig)) {
      ShowPedigError()
      return(NULL)
    }

    ## print(colnames(Pedig))
    rv <- btn_pedBreedcomp_onclick(
        Pedig,
        input$pedbreedcomp_breed
    )

    ## store modified dataframes for global access
    Pedig <<- rv[[1]]
    BreedComp <- rv[[2]]
    rv= NULL
    ## visual update
    output$BreedCompBreed = renderText(
      paste0("<h5> Breed: <span style=\"color:red; font-weight:bold;\"> ",
             input$pedbreedcomp_breed,
             " </span> (native) </h5>"))

    ## render table to dashboard
    output$BreedComp = renderDT15(BreedComp, 12)
    output$Pedig = renderDT15(Pedig, 12)

    WriteTextLogBreedComp(textLog, input)
    output$CallArgs = renderText(textLog())


  })

    # ---- BUTTON event ----------------------------------------------------------------------------
  observeEvent(input$btn_pedbreedcomp_view,{
      updateTabItems(session, "allTabs", selected = "TABLES")
      updateTabsetPanel(session, "tabchart1", selected = "BreedComp")
  })


  # ********************************************************************************************************************
  # ===INBREEDING CALCULATION====================================================================================
  # ********************************************************************************************************************


  #---- btn events
  observeEvent(input$btn_pedInbreeding, {

    if (!nrow(Pedig)) {
      ShowPedigError()
      return(NULL)
    }

    Inbreeding <<- btn_pedInbreeding_onclick(Pedig)

    output$Inbreeding = renderDT15(Inbreeding, 12)
    WriteTextLogInbreeding(textLog, input)
    output$CallArgs = renderText(textLog())
  })


  # ---- BUTTON event ----------------------------------------------------------------------------
  observeEvent(input$btn_pedInbreeding_view,{
      updateTabItems(session, "allTabs", selected = "TABLES")
      updateTabsetPanel(session, "tabchart1", selected = "Inbreeding")
  })

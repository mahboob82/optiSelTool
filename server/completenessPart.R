  # ********************************************************************************************************************
  # =========================================== COMPLETENESS CALCULATION ===============================================
  # ********************************************************************************************************************

  # # ---- completeness_keep
  # updateSelectizeInput(
  #     session,
  #     'completeness_keep',
  #     choices = "NULL", #, names(Phen)),
  #     server = TRUE
  # )

  # # ---- completeness_by ---------------------------------------------------------
  # updateSelectizeInput(
  #     session,
  #     'completeness_by',
  #     choices = NULL, # names(raw.ped),
  #     server = TRUE
  # )

  # ---- enable/disable pedigree completeness slider ---------------------------------
  observeEvent(input$completeness_keep, {
    if (input$completeness_keep != "NULL") {
      shinyjs::disable("completeness_slider")
      completeness_lastNativeActivated <<- TRUE
    }else{
      shinyjs::enable("completeness_slider")
      completeness_lastNativeActivated <<- FALSE
    }
  })


  #---- completeness_btn events
  observeEvent(
    input$btn_completeness,
    {
      if (nrow(Pedig)==0) {
        ShowPedigError()
        return(NULL)
      }
      run_flag = 0
      if (input$completeness_keep != "NULL"){
        Compl <<- btn_completeness_onclick(
          Pedig,
          raw.phen,
          input$completeness_keep,
          input$completeness_maxd,
          input$completeness_by)
        run_flag = 1
      }else{
        Compl <<- btn_completeness_onclick(
          Pedig,
          Pedig,
          input$completeness_slider,
          input$completeness_maxd,
          input$completeness_by)
        run_flag = 2
      }
      output$Compl = renderDT15(Compl, 12)
      #
      WriteTextLogCompletenesss(textLog, input, run_flag)
      output$CallArgs = renderText(textLog())
    }

  )


  # ---- BUTTON event ----------------------------------------------------------------------------
  observeEvent(input$btn_completeness_view,{
      updateTabItems(session, "allTabs", selected = "TABLES")
      updateTabsetPanel(session, "tabchart1", selected = "Compl")
  })







  # ********************************************************************************************************************
  # ===PED_SUMMARY CALCULATION=========================================================================================
  # ********************************************************************************************************************

  # ---- pedsummary_keep_only--------------------------------------------------------------------
  updateSelectizeInput(
      session,
      'pedsummary_keep_only',
      choices = c("NULL"),
      server = TRUE
  )

  # ---- enable/disable pedigree summary slider --------------------------------------------
  observeEvent(input$pedsummary_keep_only, {
      if (input$pedsummary_keep_only != "NULL") {
        shinyjs::disable("pedsummary_slider")
      }else{
        shinyjs::enable("pedsummary_slider")
      }
  })


  #---- pedsummary_btn events--------------------------------------------------------------------
  observeEvent(
    input$btn_pedsummary,
    {
      if (nrow(Pedig)==0) {
        ShowPedigError()
        return(NULL)
      }

      run_flag = 0
      if (input$pedsummary_keep_only != "NULL"){
        SummPedig <<- btn_pedSummary_onclick(
          Pedig,
          raw.phen,
          input$pedsummary_keep_only,
          input$pedsummary_maxd,
          input$pedsummary_d)
        run_flag = 1
      }else{
        SummPedig <<- btn_pedSummary_onclick(
          Pedig,
          Pedig,
          input$pedsummary_slider,
          input$pedsummary_maxd,
          input$pedsummary_d)
        run_flag=2
      }

      output$SummPedig = renderDT15(SummPedig, 12)
      WriteTextLogSummPed(textLog, input, run_flag)
      output$CallArgs = renderText(textLog())
    }

  )

  # ---- BUTTON event ----------------------------------------------------------------------------
  observeEvent(input$btn_pedsummary_view,{
      updateTabItems(session, "allTabs", selected = "TABLES")
      updateTabsetPanel(session, "tabchart1", selected = "SummPedig")
  })


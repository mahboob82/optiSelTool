

  # ====================================================================================================================
  # =====================================================FILTER_AGECONT_L ==============================================
  # ====================================================================================================================


  # ---- filter-breed --------------------------------------------------------------------------------
  updateSelectizeInput(
      session,
      'filter_breed',
      choices = "NULL",
      #selected="Hinterwaelder",     #selected = selected_breed,
      server = TRUE
  )

  # --------------------------------------------------------------------------------
  observeEvent(input$btn_filter_age_contribution,  {
      
    if (nrow(Pedig)==0) {
          ShowPedigError()
          return(NULL)
      }

      rv = btn_ageContribL_onclick(
          Pedig,
          input$filter_agecontrib_slider,
          input$filter_agecontrib_breed,
          input$filter_agecontrib_equiGen
      )

      flag = rv[1]

      if (flag == 1) {
          ShowAgeContError()
      }

      ## phen, cont, L
      PhenUpdated <- rv[[2]]
      AgeContrib <<- rv[[3]]
      GenInt <<- rv[4]
      PhenCopy <<- PhenUpdated


      output$AgeContrib = renderDT15(AgeContrib, 12)
      output$Phen = renderDT15(PhenUpdated, 12)


      # todo: set L on the screen
      #
      # WriteTextLogSummPed(textLog, input, run_flag)
      # output$CallArgs = renderText(textLog())

  })


  # ---- BUTTON event ----------------------------------------------------------------------------
  observeEvent(input$btn_filter_age_contribution_view,{
      updateTabItems(session, "allTabs", selected = "TABLES")
      updateTabsetPanel(session, "tabchart1", selected = "AgeContrib")
  })


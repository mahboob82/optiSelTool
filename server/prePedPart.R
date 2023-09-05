  # ********************************************************************************************************************
  # ============================================PREPARE PEDIGREE===============================================
  # ********************************************************************************************************************
  
  
  PedigData <- reactiveVal(NULL)
  
  # ---- preped_keep choices ------------------------------------------------------------------
  updateSelectizeInput(
    session,
    'preped_keep',
    #choices = c("NULL", names(raw.ped)),
    choices = c("NULL"),
    server = TRUE
  )

  #---- enable/disable pedigree keep slider ---------------------------------------------------------------------
  observeEvent(input$preped_keep, {
    if (input$preped_keep != "NULL") {
      shinyjs::disable("preped_slider")
    }else{
      shinyjs::enable("preped_slider")
    }
  })

  #---- enable/disable prePed::lastNative input and keep track of outcomes --------------------------------------
  observeEvent(input$enable_preped_lastNative, {
    if (input$enable_preped_lastNative) {
      shinyjs::enable("preped_lastNative")
      prePed_lastNativeActivated <<- TRUE
    }else{
      shinyjs::disable("preped_lastNative")
      prePed_lastNativeActivated <<- FALSE
    }
  })
  
  
  # ---- preped-breed choices ----------------------------------------------------------------------------
  updateSelectizeInput(
      session,
      'preped_breed',
      # choices = c("NA", breeds),
      # selected="Hinterwaelder",
      choices = c("NA"),
      server = TRUE
  )


  # ---- BUTTON event ----------------------------------------------------------------------------
  observeEvent(
    input$btn_preped,
    {
      if (nrow(raw.ped)==0) {
        ShowRawPedigUploadError()
        return(NULL)
      }
      run_flag = 0
      if (input$preped_keep != "NULL"){
        ## when keep is based on Phen Indiv column
        Pedig <<- btn_prePed_onclick(
            raw.ped,
            raw.phen,
            input$preped_keep, 
            input$preped_breed, 
            ifelse(prePed_lastNativeActivated, input$preped_lastNative, NA),
            input$preped_addnum
        )
        run_flag = 1
      }else{
        ## when keep is based on Born column
        Pedig <<- btn_prePed_onclick(
          raw.ped,
          data.frame(),
          input$preped_slider, 
          input$preped_breed, 
          ifelse(prePed_lastNativeActivated, input$preped_lastNative, NA), 
          input$preped_addnum
        )
        run_flag = 2
      }      
      
      ##___ render table to dashboard
      output$Pedig = renderDT15(Pedig, 12)
      WriteTextLogPrePed(textLog, input)
      output$CallArgs = renderText(textLog())
      
      
      if (!is.null(Pedig) && nrow(Pedig) > 0) {
        PedigData(Pedig)
      } else {
        PedigData(NULL)
      }
    }
  )
  
  # ---- BUTTON event ----------------------------------------------------------------------------
  observeEvent(input$btn_preped_view,{
      updateTabItems(session, "allTabs", selected = "TABLES")
      updateTabsetPanel(session, "tabchart1", selected = "Pedig")
  })
  
  
  # downloadPedig button with radiobuttons
  
  observe({
    
    if (!is.null(PedigData())) {
      enable("downloadPedig")
    }else{
      disable("downloadPedig")
    }
  })
    
  output$downloadPedig <- downloadHandler(
    
    filename = function() {
      #paste("Pedig.", tolower(input$PedigDownloadFormat))
      paste0("Pedig.", input$PedigDownloadFormat)
    },
    content = function(file) {
      if(input$PedigDownloadFormat == "txt") {
        write.table(PedigData(), file, row.names = FALSE, sep="\t", quote=FALSE, na = "NA")
      }else if(input$PedigDownloadFormat == "csv"){
        write.csv(PedigData(), file, row.names = FALSE)
      }else if(input$PedigDownloadFormat == "xlsx"){
        write.xlsx(PedigData(), file, rowNames = FALSE)
      }else if(input$PedigDownloadFormat == "rds"){
        saveRDS(PedigData(), file=file)
      }
    }
  )
  
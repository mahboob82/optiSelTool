

  # ===NATIVE KINSHIP/NATIVE IBD ================================================================================================

  # ----
  updateSelectizeInput(
    session,
    'pedKinatN_breed',
    choices = NULL,
    server = TRUE
  )

  # ----
  updateSelectizeInput(
    session,
    'pedKinatN_keeponly',
    choices =  NULL,
    server = TRUE
  )

  # ----
  updateSelectizeInput(
    session,
    'pedKinatN_keep',
    # choices = c("NULL", colnames(raw.ped)),
    choices = "NULL",
    server = TRUE
  )


 # -----
  observeEvent(input$btn_pedKinatN, {

    if (nrow(Pedig)==0) {
      ShowNativeKinshipError()
      return(NULL)
    }

    cols = c("Born","NC")
    if (!all(cols %in% colnames(Pedig))){
      msg= "Proabably Pedig missing NC column.\n Run Breed_Composition first! "
      ShowNativeKinshipExtraError(msg)
    }


    rv = btn_pedIBDatN_onclick(
      Pedig,
      Phen,
      input$pedKinatN_breed,
      input$pedKinatN_keeponly,
      input$pedKinatN_keep,
      input$pedKinatN_ngen) # returns (list(flag, pKin))

    msg = rv[[1]]
    if ( msg > "") {

      ShowNativeKinshipEmptyDFReturn(msg)
      return(NULL)
    }

    PkinatN <<- rv[[2]]

    PkinatN$of <<- PkinatN$Q1/PkinatN$Q2
    PkinatNCopyRender <<-PkinatN$of
    #
    output$PkinatN = renderDT15_PkinatN_dblClick(PkinatN$of, 12)
  })


  # ---- BUTTON event ----------------------------------------------------------------------------
  observeEvent(input$btn_pedKinatN_view,{
      updateTabItems(session, "allTabs", selected = "TABLES")
      updateTabsetPanel(session, "tabchart1", selected = "PkinatN")
  })

  # -----
  observeEvent(input$PkinatN_dt_dblclick, {

    r=input$PkinatN_dt_dblclick$dt_row
    c=input$PkinatN_dt_dblclick$dt_col-1
    p=input$PkinatN_dt_dblclick$dt_page
    l=input$PkinatN_dt_dblclick$dt_len
    #
    # print(c( input$PkinatN_dt_dblclick$dt_row,
    #          input$PkinatN_dt_dblclick$dt_page,
    #          input$PkinatN_dt_dblclick$dt_len) )


    columns = colnames(PkinatNCopyRender)
    rows = rownames(PkinatNCopyRender)
    animal1 = rows[r+p*l]
    animal2 = columns[c]
    if (length(animal2)==0)
      animal2 = columns
    #animal2= ifelse(is.na(columns[c])==FALSE, columns[c], columns)
    #animal2 = cols[c] | cols
    print(animal2)
    #print(class(columns[c]))
    #print(columns[c])
    #print(c(animal1, animal2))

    cols = c("Born","NC")
    # datatmp = Pedig[c("276000891862786", "276000812497659"),cols]
    # vectortmp = PkinatN$of["276000891862786","276000812497659"]
    datatmp = Pedig[c(animal1, animal2),cols]
    vectortmp = PkinatN$of[animal1,animal2]
    if (length(vectortmp)>1)
      vectortmp= vectortmp[-(r+p*l)]
      vectortmp = paste(round(mean(vectortmp),5), "  [avg.]")

    #
    output$PkinatNwithNC <- renderDT(datatmp)
    output$NativeKinship <- renderText(vectortmp)
    #output$NativeKinship = renderDT(vectortmp)

  })

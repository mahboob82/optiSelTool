

  # ===NATIVE KINSHIP/NATIVE IBD ================================================================================================

  # ----
  updateSelectizeInput(
    session,
    'pedKinatN_breed',
    choices = c(breeds),
    selected="Hinterwaelder",
    server = TRUE
  )

  # ----
  updateSelectizeInput(
    session,
    'pedKinatN_keeponly',
    choices =  c(colnames(Phen)),
    selected = "Indiv",
    server = TRUE
  )

  # ----
  updateSelectizeInput(
    session,
    'pedKinatN_keep',
    choices = c("NULL", colnames(raw.ped)),
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

    msg = rv[1]
    if ( msg > "") {
      ShowNativeKinshipError(msg)
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
    print(c( input$PkinatN_dt_dblclick$dt_row,
             input$PkinatN_dt_dblclick$dt_page,
             input$PkinatN_dt_dblclick$dt_len) )


    cols = colnames(PkinatNCopyRender)
    rows = rownames(PkinatNCopyRender)
    animal2 = cols[c]
    animal1 = rows[r+p*l]
    print(c(animal1, animal2))

    cols = c("Born","NC")
    datatmp = Pedig[c("276000891862786", "276000812497659"),cols]
    vectortmp = PkinatN$of["276000891862786","276000812497659"]

    #
    output$PkinatNwithNC <- renderDT(datatmp)
    output$NativeKinship = renderText(vectortmp)

  })

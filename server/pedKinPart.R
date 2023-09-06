  # ===KINSHIP/IBD ================================================================================================



  # # ---- pedkinNat-breed --------------------------------------------------------------------------------
  # updateSelectizeInput(
  #     session,
  #     'pedkinNat_breed',
  #     choices = c("NA", breeds),
  #     selected="Hinterwaelder",     #selected = "NA",
  #     server = TRUE
  # )
  #
# 
#   # ----
#   updateSelectizeInput(
#     session,
#     'pedkin_keeponly',
#     #choices = c(colnames(Phen)),
#     choices = c(),
#     server = TRUE
#   )
# 
#   # ----
#   updateSelectizeInput(
#     session,
#     'pedkin_keep',
#     #choices = c("NULL", colnames(raw.ped)),
#     choices = c("NULL"),
#     server = TRUE
#   )

  # ----
  updateSelectizeInput(
    session,
    'pedkin_founder',
    choices = c("NULL", "kinmat-founder"),
    server = TRUE
  )

  observeEvent(input$btn_pedIBD, {

    if (nrow(Pedig)==0) {
      ShowPedigError()
      return(NULL)
    }

    rv = btn_pedIBD_onclick(
      Pedig,
      raw.phen,
      input$pedkin_keeponly,
      input$pedkin_keep,
      input$pedkin_founder) # returns (list(flag, pKin))


    if (rv[[1]] == 1) {
      ShowPedigError()
      return(NULL)
    }

    Pkin <<- rv[[2]]
    # print(class(Pkin))
    print("IT WAS HERE")
    
    PkinCopy <<- as.data.frame(Pkin) %>% 
      tibble::rownames_to_column(var = "Indiv") #%>% 
      #mutate(Indiv=as.numeric(Indiv))
    
    #print(str(PkinCopy))
    #print(str(Phen))
    #Phen <<- as.data.frame(Phen)
    tmpdf = dplyr::select(raw.phen, Indiv, Sex)
    tmpdf$Indiv = as.character(tmpdf$Indiv)
    PkinCopy <<- dplyr::left_join(PkinCopy, tmpdf, by="Indiv")
    PkinCopy <<- PkinCopy %>% dplyr::select(c(Indiv, Sex), everything())
    PkinCopyRender <<- PkinCopy

    output$Pkin = renderDT15_Pkin_dblClick(PkinCopy, 10)
    #output$Pkin = renderDT15(PkinCopy)
  })

    # ---- BUTTON event ----------------------------------------------------------------------------
  observeEvent(input$btn_pedIBD_view,{
      updateTabItems(session, "allTabs", selected = "TABLES")
      updateTabsetPanel(session, "tabchart1", selected = "Pkin")
  })

  observeEvent(input$si_sexinfo, {
    if(input$si_sexinfo=="all"){
      output$Pkin <- renderDT15_Pkin_dblClick(PkinCopy, 10)
      PkinCopyRender <<- PkinCopy
    }else{
      PkinCopyRender <<- PkinCopy %>%
        filter(Sex==input$si_sexinfo)

      output$Pkin <- renderDT15_Pkin_dblClick(PkinCopyRender,10)
    }

  })



  observeEvent(input$Pkin_dt_dblclick, {

    r=input$Pkin_dt_dblclick$dt_row
    c=input$Pkin_dt_dblclick$dt_col
    p=input$Pkin_dt_dblclick$dt_page
    l=input$Pkin_dt_dblclick$dt_len
    #
    print(c( input$Pkin_dt_dblclick$dt_row,
             input$Pkin_dt_dblclick$dt_page,
             input$Pkin_dt_dblclick$dt_len) )

    animal = as.character(PkinCopyRender[(r+p*l),1])
    sex    = PkinCopyRender[(r+p*l),2]

    print(animal)

    if (sex=="female"){
      isMale <- Pedig$Sex=="male" & (Pedig$Indiv %in% raw.phen$Indiv[raw.phen$BV> as.numeric(input$Pkinship_BV)])
      males  <- Pedig$Indiv[isMale & summary(Pedig)$equiGen>input$Pkinship_equiGen]
      print("It came here0")
      SireKinshipRank <<- Pkin[males, animal, drop=FALSE]
      print("It came here1")
      colnames(SireKinshipRank) <-c("Kinship")
      output$FemaleIDwithKinship <- renderText(paste("Female ID:", animal))
      SireKinship <- tibble::rownames_to_column( as.data.frame(SireKinshipRank), "Sire" )
      SireKinship <- dplyr::arrange(SireKinship, Kinship) %>% mutate(Kinship = round(Kinship, 5))
      #print(SireKinship)
      output$SireKinshipRank <- renderDT15_Pkin_dblClick(SireKinship)

    }else{
      print("It came here2")

      SireKinshipRank <<- data.frame('SexError'=c("Sorry, Male is selected"))
      output$FemaleIDwithKinship <- renderText(paste("Female ID:", NULL))
      output$SireKinshipRank <- renderDT15_Pkin_dblClick(SireKinshipRank)
    }


#    print(PkinCopy[r,1])
    #print(paste(rownames(global_vars$Pkin[r])), global_vars$Pkin[r,c])
  })


  #######################################################################

  raw.ped <<- vroom("PedigreeWithErrors.csv", delim = ",")
  raw.phen <<- vroom("Phen.csv", delim = ",")

  observeEvent(input$RawPedUpload, {

    req(input$RawPedUpload)
    file <- input$RawPedUpload
    if (is.null(file)) return(NULL)
    ext <- tools::file_ext(file$name)

    if (ext=="csv")
      raw.ped <<- vroom(file$datapath, delim = ",")
    else if (ext=="txt")
      raw.ped <- vroom(file$datapath, delim = " ")
    else if (ext=="rds")
      raw.ped <- readRDS(file=file$datapath)
    else
      print("ERROR while file upload")


    # for tables on left
    raw.ped <<- as.data.frame(raw.ped)
    raw.ped <- raw.ped %>%
      mutate(Indiv= as.character(Indiv),
             Sire= as.character(Sire),
             Dam= as.character(Dam))

    rownames(raw.ped) <- raw.ped$Indiv
    output$RawPed <- renderDT15(raw.ped, 12)

    # for tables on right
    column_info = data.frame(
      Column = colnames(raw.ped),
      DataType = sapply(raw.ped, class),
      stringsAsFactors = FALSE
    )
    rownames(column_info) <- NULL
    output$RawPedColumnInfo <- renderDTminimal(column_info)

    #global_vars$raw_ped_cols = colnames(raw.ped)

    born_minmax <- c( min(raw.ped$Born, na.rm=TRUE), max(raw.ped$Born, na.rm=TRUE) )
    yr_diff= max(raw.ped$Born, na.rm=TRUE) - min(raw.ped$Born, na.rm=TRUE)
    born_slider_init_range <- c(
      round(born_minmax[1]+ yr_diff*.85, digits=0),
      round(born_minmax[1]+ yr_diff*.95, digits=0)
    )

    # assume_candes_t <<- max(floor(na.omit(raw.ped$Born)))
    #
    updateSelectizeInput(session,'preped_keep',choices = c("NULL", names(raw.ped)))
    updateSliderInput(session, "preped_slider",value=born_slider_init_range, min=born_minmax[1], max=born_minmax[2])
    updateSelectizeInput(session,'preped_breed',choices = c("NA", unique(raw.ped$Breed), selected="Hinterwaelder"))
    updateSelectizeInput(session,'completeness_by',choices=colnames(raw.ped))
    updateSliderInput(session, "completeness_slider", value=born_slider_init_range, min=born_minmax[1], max=born_minmax[2])

      # ---- pedbreedcomp-breed
    updateSelectizeInput(
      session,'pedbreedcomp_breed',choices = unique(raw.ped$Breed))
    updateSliderInput(session, "pedsummary_slider", value=born_slider_init_range, min=born_minmax[1], max=born_minmax[2])
    updateSliderInput(session, "filter_agecontrib_slider", value=born_slider_init_range, min=born_minmax[1], max =born_minmax[2])

    updateSelectizeInput(session, 'filter_agecontrib_breed', choices=unique(raw.ped$Breed))
    updateSelectizeInput(session, 'pedkin_keep', choices = c("NULL", colnames(raw.ped)))

    updateSelectizeInput(session, 'pedKinatN_breed', choices = unique(raw.ped$Breed))
    updateSelectizeInput(session, 'pedKinatN_keep', choices = c("NULL", colnames(raw.ped)))


  })


  ######################################## RawPHEN UPLOAD #################################
  observeEvent(input$RawPhenUpload, {

    req(input$RawPhenUpload)
    file <- input$RawPhenUpload
    if (is.null(file)) return(NULL)
    ext <- tools::file_ext(file$name)

    if (ext=="csv")
      raw.phen <<- vroom(file$datapath, delim = ",")
    else if (ext=="txt")
      raw.phen <- vroom(file$datapath, delim = " ")
    else if (ext=="tsv")
      raw.phen <- vroom(file$datapath, delim = "\t")
    else if (ext=="RData")
      raw.phen <- load(file=file$datapath)
    else if (ext=="Rda")
      raw.phen <- load(file=file$datapath)
    else
      print("ERROR while file upload")

    raw.phen <<- as.data.frame(raw.phen)
    rownames(raw.phen) <- raw.phen$Indiv

    raw.phen <- raw.phen %>%
      mutate(Indiv= as.character(Indiv))

    RawPhen <<- raw.phen
    output$RawPhen <- renderDT15(raw.phen,12)

    column_info = data.frame(
      Column = colnames(raw.phen),
      DataType = sapply(raw.phen, class),
      stringsAsFactors = FALSE
    )

    rownames(column_info) <- NULL
    output$RawPhenColumnInfo <- renderDTminimal(column_info)

    updateSelectizeInput(session,'completeness_keep', choices = c("NULL", names(raw.phen)))
    updateSelectizeInput(session, 'pedsummary_keep_only', choices = c("NULL", names(raw.phen)))
    updateSelectizeInput(session, 'pedkin_keeponly', choices = c(colnames(raw.phen)))
    updateSelectizeInput(session, 'pedKinatN_keeponly', choices =  c(colnames(raw.phen)), selected = "Indiv")
  })


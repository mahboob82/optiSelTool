
# SERVERE SIDE SCRIPT OF THE APP
source('global.R')
source('libs/ocslib.R')


global.dfs = reactiveValues()
#textLog = reactiveVal("")

ShowEntryRowsList = list(
  c(5, 10, 12, 15, 30, 50, 100, 200, 500, 1000,-1),
  c('5', '10', '12', '15', '30', '50', "100", "200", "500", "1000", 'All')
)




ShowRawPedigUploadError <- function (){
  showModal(modalDialog(
    title = "Pedigree Upload Error",

    div(tags$b("Original pedigree", style = "color: red;"),
        tags$b("was not uploaded.", style = "color: black;"),
        tags$hr(),
        tags$b("Tips: Try uploading using left side upload options", 
               style = "color: blue;")
        ),
    easyClose = TRUE,
    footer = tagList(modalButton("Cancel")),
    size="s"
  ))
}



ShowPedigError <- function (){
  showModal(modalDialog(
    title = "Pedig Object Error",
    #paste("'Pedig' dataframe is not created! ", "Try prePed first!"),
    div(tags$b("Pedig", style = "color: red;"),
        tags$b("dataframe not found.", style = "color: black;"),
        tags$hr(),
        tags$b("Tips: Try ", style = "color: black;"),
        tags$b("prePed ", style = "color: blue;"),
        tags$b(" step at first.", style = "color: black;")
        ),
    easyClose = TRUE,
    footer = tagList(modalButton("Cancel")),
    size="s"
  ))
}

ShowNativeKinshipError <- function (){
  showModal(modalDialog(
    title =  "Pedig Object Error",
    div(
      tags$b("Pedig", style = "color: red;"),
      tags$b("object not found.", style = "color: black;"),
      tags$hr(),
      tags$b("Tips: Try ", style = "color: black;"),      
      tags$b(" prePed()", style = "color: blue;"),
      tags$b("&"),
      tags$b(" pedBreedComp().", style = "color: blue;"),
      tags$b("  steps in order.")
      ),
    easyClose = TRUE,
    footer = tagList(modalButton("Cancel")),
    size="s"
  ))
}

ShowNativeKinshipExtraError <- function (msg){
  showModal(modalDialog(
    title = "Pedig column error:",
    div(
      tags$b("Check"),
      tags$b("Pedig", style = "color: red;"),
      tags$b("object for NC column.", style = "color: black;"),
      tags$hr(),
      tags$b("Tips: Try ", style = "color: black;"),
      tags$b(" prePed()", style = "color: blue;"),
      tags$b(" &"),
      tags$b(" pedBreedComp()", style = "color: blue;"),
      tags$b(" steps in order.")
      ),
    easyClose = TRUE,
    footer = tagList(modalButton("Cancel")),
    size="s"
  ))
}


ShowAgeContError <- function (){
  showModal(modalDialog(
    title = "Error",
    #paste("Empty Dataframe output!", "\nTry different options!"),
    div(tags$b("agecont", style = "color: red;"),
        tags$b("returned empty dataframe.", style = "color: black;"),
        tags$br(),
        tags$b("Pleast try different options.", style = "color: black;"),
    ),
    easyClose = TRUE,
    footer = tagList(modalButton("Cancel")),
    size="s"
  ))
}



WriteTextLogPrePed <- function(textLog, input){
  textLog(
    paste(
      textLog(),
      div(tags$b("[Pedigree Prepare]", style="color:blue;font-size:12pt")),
      div(tags$text(
        paste0(
          "optiSel::prePed( keep=",ifelse(input$preped_keep=="NULL","","RawPed$"),          
          input$preped_keep,
          ", thisBreed=",
          input$preped_breed,
          ", lastNative=",
          input$preped_lastnative,
          ", addNum=",
          input$preped_addnum,
          ")\n")
        ,style="color:teal;font-size:14pt")
      ),
      sep = ""
    )
  )
}

WriteTextLogCompletenesss <- function(textLog, input, run_flag){
  textLog(
    paste(
      textLog(),
      div(tags$b("[Pedigree Completeness]", style="color:blue;font-size:12pt")),
      div(
        tags$text(
          paste0(
            "optiSel::completeness( keep=",ifelse(run_flag==1 ,"Phen$", ""),
            input$completeness_keep,
            ", born=(",
            ifelse(run_flag==2, input$completeness_slider[1], 0),
            " to ",
            #input$completeness_slider[2],
            ifelse(run_flag==2, input$completeness_slider[2], 0),
            "), maxd=",
            input$completeness_maxd,
            ", by=",
            input$completeness_by,
            " )\n"
          ), 
          style="color:teal;font-size:14pt"
        )
      ),
      sep = ""
    )
  )
}


WriteTextLogInbreeding <- function(textLog, input){
  textLog(
    paste(
      textLog(),
      div(tags$b("[Inbreeding Coeff]", style="color:blue;font-size:12pt")),
      div(
        tags$text(
          paste0(
            "optiSel::pedInbreeding( thisBreed=\"", input$pedbreedcomp_breed,"\" )\n"
          ), 
          style="color:teal;font-size:14pt"
        )
      ),
      sep = ""
    )
  )
  
}


WriteTextLogBreedComp <- function(textLog, input){
  textLog(
    paste(
      textLog(),
      div(tags$b("[Breed Composition]", style="color:blue;font-size:12pt")),
      div(
        tags$text(
          paste0(
            "optiSel::pedBreedComp( thisBreed=\"", input$pedbreedcomp_breed,"\" )\n"
          ), 
          style="color:teal;font-size:14pt"
        )
      ),
      sep = ""
    )
  )
  
}


WriteTextLogSummPed <- function(textLog, input, run_flag){
  textLog(
    paste(
      textLog(),
      div(tags$b("[Ped Summary Stats]", style="color:blue;font-size:12pt")),
      div(
        tags$text(
          paste0(
            "optiSel::summary.Pedig( keep.only=",
            ifelse(
              run_flag==1,
              paste0("Phen$",input$pedsummary_keep_only),
              paste0(
                "Pedig$Born %in% (",
                ifelse(run_flag==2, input$pedsummary_slider[1], 0),
                ":",
                ifelse(run_flag==2, input$pedsummary_slider[2], 0),
                ")"
              )
            ),
            ", maxd=",
            input$pedsummary_maxd,
            ", d=",
            input$pedsummary_d,
            " )\n"
          ), 
          style="color:teal;font-size:14pt"
        )
      ),
      sep = ""
    )
  )
}


WriteTextLogFilterPed <- function(textLog, input, run_flag){

}



renderDT15 <- function(df, row=15){
  renderDT(
    df,
    filter = 'top',
    options = list(
      scrollX = TRUE,
      paging = TRUE,
      pageLength = row,
      lengthMenu = ShowEntryRowsList,
      #dom = 'Blfrtip' # 'tip'
      dom='Blfrtip'
    )
  )
}

renderDTminimal <- function(df, row=15){
  renderDT(
    df,
    filter = 'top',
    options = list(
      scrollX = TRUE,
      paging = TRUE,
      pageLength = row,
      lengthMenu = ShowEntryRowsList,
      dom = 'tp' # 'tip'
    )
  )
}

renderDT15special <- function(df, row=15){
  renderDataTable(
    df,
    filter = 'top',
    options = list(
      searchCols = list(NULL, NULL, NULL,NULL, NULL, NULL,NULL), 
      # scrollX = TRUE,
      # paging = TRUE,
      # pageLength = row,
      # lengthMenu = ShowEntryRowsList,
      # dom = 't', # 'tip',Blfrtip
      initComplete = JS(
          "function(settings, json) {",
          "  var table = this.api();",
          "  table.columns().every(function() {",
          "    var that = this;",
          "    $('input', this.header()).on('keyup', function() {",
          "      if (that.search() !== this.value) {",
          "        that.search(this.value).draw();",
          "      }",
          "    });",
          "  });",
          "}"
      ),
      #dom = 'Blfrtip',
      rowCallback = JS(
        "function(row, data, index) {",
        "  var cells = $('td', row);",
        "  cells.eq(0).addClass('highlight');",  # Highlight the first column
        "}"
      ),
      class = "display cell-border stripe"
    )
  )
}



renderDT15_Pkin_dblClick <- function(df, rows=15){
  renderDT(
    df,
    filter = 'top',
    options = list(
      scrollX = TRUE,
      paging = TRUE,
      pageLength = rows,
      lengthMenu = ShowEntryRowsList,
      dom = 'Blfrtip' # 'tip'
    ),
    callback = htmlwidgets::JS(
      "table.on('dblclick', 'td',",
      "  function() {",
      "    var row = table.row(this).index();",
      "    var col = table.column(this).index();",
      "    var info = table.page.info()",
      "    var pagelen = table.page.len()",
      "    Shiny.setInputValue('Pkin_dt_dblclick', {dt_row: row+1, dt_col: col+1, dt_page: info.page, dt_len: pagelen} );",
      "  }",
      ");"
    )
  )
}

renderDT15_PkinatN_dblClick <- function(df, rows=15){
  renderDT(
    df,
    filter = 'top',
    options = list(
      scrollX = TRUE,
      paging = TRUE,
      pageLength = rows,
      lengthMenu = ShowEntryRowsList,
      dom = 'Blfrtip' # 'tip'
    ),
    callback = htmlwidgets::JS(
      "table.on('dblclick', 'td',",
      "  function() {",
      "    var row = table.row(this).index();",
      "    var col = table.column(this).index();",
      "    var info = table.page.info()",
      "    var pagelen = table.page.len()",
      "    Shiny.setInputValue('PkinatN_dt_dblclick', {dt_row: row+1, dt_col: col+1, dt_page: info.page, dt_len: pagelen} );",
      "  }",
      ");"
    )
  )
}


# 
# renderDTPkinatNdblClick <- function(df, rows=15){
#   renderDT(
#     df,
#     filter = 'top',
#     options = list(
#       scrollX = TRUE,
#       paging = TRUE,
#       pageLength = rows,
#       lengthMenu = ShowEntryRowsList,
#       dom = 'Blfrtip' # 'tip'
#     ),
#     callback = htmlwidgets::JS(
#       "table.on('dblclick', 'td',",
#       "  function() {",
#       "    var tbl_name = 'PKinatN;'",
#       "    var row = table.row(this).index();",
#       "    var col = table.column(this).index();",
#       "    var info = table.page.info()",
#       "    var pagelen = table.page.len()",
#       "    Shiny.setInputValue('dt_dblclick', {dt_tbl_name: tbl_name, dt_row: row+1, dt_col: col+1, dt_page: info.page, dt_len: pagelen} );",
#       "  }",
#       ");"
#     )
#   )
# }


######################################################################################################
    ################################## SERVER CODES #############################################
######################################################################################################

# Declare some global intermediate dataframes for future references 
# <<- assignment meaning assignment is a global declaration
Pedig <<- data.frame()
Compl <<- data.frame()
Inbreeding <<- data.frame()
SummPedig <<- data.frame()
BreedComp <<- data.frame()
AgeContrib <<- data.frame()
Pkin <<- data.frame()
PkinCopy <<- data.frame()
PkinCopyRender <<- data.frame()
SireKinshipRank <<- data.frame()

assume_candes_t <<- 0


appdfs <<- c('Pedig','Compl','Inbreeding', 'SummPedig', 'BreedComp', 'AgeContrib', 'Pkin', 'PkinatN')
divFormat1 = "
display: contents; 
margin: 0px;
padding: 2px;
"



# ********************************************************************************************************************
# *************************** SEVER SIDE CODES (START) ****************************************************************
# ********************************************************************************************************************
# ********************************************************************************************************************



server <- function(input, output, session) {
  
  ## some tracking variables
  textLog = reactiveVal("")
  # breeds = sort(unique(raw.ped$Breed))
  # selected_breed <- breeds[order(nchar(breeds), breeds)][1]
  breeds = c()
  selected_breed <- "NA"

  final_tables = c()
  global_vars <- reactiveValues()
  global_vars$raw_ped_cols <- c()

  raw.ped <<- data.frame()  

  #######################################################################

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
    
  
    raw.ped <<- as.data.frame(raw.ped)
    raw.ped <- raw.ped %>% 
      mutate(Indiv= as.character(Indiv),
             Sire= as.character(Sire),
             Dam= as.character(Dam),
             )
    
    rownames(raw.ped) <- raw.ped$Indiv
    output$RawPed <- renderDT15(raw.ped, 12)
    # output$RawPed <- renderDataTable({
    #   datatable(raw.ped, 
    #             options = list(
    #               searchCols = list(NULL, NULL, NULL,NULL, NULL, NULL,NULL)  # Disable individual column searching
    #             ))
    # })
    # output$RawPed <- renderDT15special(raw.ped, 12)
    
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
    updateSelectizeInput(session, 'pedkin_keep', choices = c("NULL", colnames(raw.ped)), server = TRUE )    
        
  })

  
  ######################################## PHEN UPLOAD #################################
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
    
    Phen <<- raw.phen
    output$Phen <- renderDT15(raw.phen,12)
    
    column_info = data.frame(
      Column = colnames(raw.phen),
      DataType = sapply(raw.phen, class),
      stringsAsFactors = FALSE
    )
    
    rownames(column_info) <- NULL
    output$RawPhenColumnInfo <- renderDTminimal(column_info)
    
    updateSelectizeInput(session,'completeness_keep', choices = c("NULL", names(raw.phen)),server = TRUE)
    updateSelectizeInput(session, 'pedsummary_keep_only', choices = c("NULL", names(raw.phen)), server = TRUE )
    updateSelectizeInput(session, 'pedkin_keeponly', choices = c(colnames(raw.phen)), server = TRUE)
        

  })
  
  #######################################################################  
  
  
  # ===DISPLAYING RAWPED=========================================================================================
  #---- Showing raw pedigree to dashboard ----
  
  # Update dashboard during the loading first time of the app
  # output$RawPed = renderDT15(raw.ped, 12)
  # output$Phen = renderDT15(Phen,12)
  # print(head(raw.ped))

  # ********************************************************************************************************************
  # ============================================PREPARE PEDIGREE===============================================
  # ********************************************************************************************************************
  
    
  source(file="./server/prePedPart.R", local = TRUE)
  

  # ********************************************************************************************************************
  # =========================================== COMPLETENESS CALCULATION ===============================================
  # ********************************************************************************************************************

  source(file="./server/completenessPart.R", local = TRUE)
    
  

  # ********************************************************************************************************************
  # ===INBREEDING CALCULATION====================================================================================
  # ********************************************************************************************************************


  source(file="./server/inbreedingPart.R", local=TRUE)



  # ********************************************************************************************************************
  # ===BREED COMPOSITION CALCULATION====================================================================================
  # ********************************************************************************************************************


  source(file="./server/breedCompPart.R", local=TRUE)


  # ********************************************************************************************************************
  # ===PED_SUMMARY CALCULATION=========================================================================================
  # ********************************************************************************************************************

  source(file="./server/pedSummaryPart.R", local=TRUE)

     
  # ====================================================================================================================
  # =====================================================FILTER_AGECONT_L ==============================================
  # ====================================================================================================================

  source(file="./server/filterAgeContLPart.R", local=TRUE)
  
  
  # ====================================================================================================================
  # ===================================================== PED-KINSHIP ==============================================
  # ====================================================================================================================

  source(file="./server/pedKinPart.R", local=TRUE)
  

  # ====================================================================================================================
  # ===================================================== PED-KINSHIP-NATIVE ===========================================
  # ====================================================================================================================

  # source(file="./server/pedKinAtNPart.R", local=TRUE)
  


#   
#   # # ---- 
#   # updateSelectizeInput(
#   #   session,
#   #   'pedKinatN_keeponly',
#   #   choices =  c(colnames(Phen)),
#   #   selected = "Indiv",
#   #   server = TRUE
#   # )
#   # 
#   # # ---- 
#   # updateSelectizeInput(
#   #   session,
#   #   'pedKinatN_keep',
#   #   choices = c("NULL", colnames(raw.ped)),
#   #   server = TRUE
#   # )
#   # 
#  # # ----- 
#  #  observeEvent(input$btn_pedKinatN, {
#  # 
#  #    if (nrow(Pedig)==0) {
#  #      ShowNativeKinshipError()
#  #      return(NULL)
#  #    }    
#  #    
#  #    cols = c("Born","NC")
#  #    if (!all(cols %in% colnames(Pedig))){
#  #      msg= "Proabably Pedig missing NC column.\n Run Breed_Composition first! "
#  #      ShowNativeKinshipExtraError(msg)
#  #    }
#  #    
#  #    
#  #    rv = btn_pedIBDatN_onclick(
#  #      Pedig,
#  #      Phen,
#  #      input$pedKinatN_breed,
#  #      input$pedKinatN_keeponly,
#  #      input$pedKinatN_keep,
#  #      input$pedKinatN_ngen) # returns (list(flag, pKin))
#  # 
#  #    msg = rv[1]
#  #    if ( msg > "") {
#  #      ShowNativeKinshipError(msg)
#  #      return(NULL)
#  #    }
#  # 
#  #    PkinatN <<- rv[[2]]
#  #    
#  #    PkinatN$of <<- PkinatN$Q1/PkinatN$Q2
#  #    PkinatNCopyRender <<-PkinatN$of
#  #    # 
#  #    output$PkinatN = renderDT15_PkinatN_dblClick(PkinatN$of, 15)
#  #  })
# 
#   # # ----- 
#   # observeEvent(input$PkinatN_dt_dblclick, {
#   #   
#   #   r=input$PkinatN_dt_dblclick$dt_row
#   #   c=input$PkinatN_dt_dblclick$dt_col-1
#   #   p=input$PkinatN_dt_dblclick$dt_page
#   #   l=input$PkinatN_dt_dblclick$dt_len
#   #   # 
#   #   print(c( input$PkinatN_dt_dblclick$dt_row,
#   #            input$PkinatN_dt_dblclick$dt_page, 
#   #            input$PkinatN_dt_dblclick$dt_len) )
#   #   
#   #   
#   #   cols = colnames(PkinatNCopyRender)
#   #   rows = rownames(PkinatNCopyRender)
#   #   animal2 = cols[c]
#   #   animal1 = rows[r+p*l]
#   #   print(c(animal1, animal2))
#   #   
#   # 
#   # 
#   # })  
#   # 
#   
#   
  
###################################
  
  observeEvent(input$RawPedToHome|input$PhenToHome|input$PedigToHome|
                 input$InbreedingToHome|input$ComplToHome|input$SummPedigToHome|
                 input$BreedCompToHome|input$AgeContribToHome|input$PkinToHome|
                 input$PkinatNtoHome, {
      updateTabItems(session, "allTabs", selected = "STEPS")
  })
  
  observeEvent(input$refresh, {
    #shinyjs::reset("Workspace")
    Pedig <<- data.frame()
    Compl <<- data.frame()
    Inbreeding <<- data.frame()
    SummPedig <<- data.frame()
    BreedComp <<- data.frame()
    #
    output$Pedig = renderDT15(Pedig)
    output$Compl = renderDT15(Compl,12)
    output$Inbreeding = renderDT15(Inbreeding,12)
    output$SummPedig = renderDT15(SummPedig,12)
    output$BreedComp = renderDT15(BreedComp,12)
    
  })
  
  
    # ---- BUTTON event ----------------------------------------------------------------------------
  observeEvent(input$btnPlanA,{
      updateTabItems(session, "allPlanTabs", selected = "Plan A")
      #updateTabsetPanel(session, "tabchart1", selected = "Pedig")
  })
  
  observeEvent(input$btnPlanB,{
      updateTabItems(session, "allPlanTabs", selected = "Plan B")
      #updateTabsetPanel(session, "tabchart1", selected = "Pedig")
  })
  observeEvent(input$btnPlanC,{
      updateTabItems(session, "allPlanTabs", selected = "Plan C")
      #updateTabsetPanel(session, "tabchart1", selected = "Pedig")
  })
  
  
  # output$ocs_original_html = renderUI(
  #   #includeHTML("https://cran.r-project.org/web/packages/optiSel/vignettes/ped-vignette.html")
  # )
  
  
  
  
  
  observeEvent(input$goToTop, {
    session$sendCustomMessage(type = "scroll", message = "top")
    
  })
  
  observeEvent(input$goToBottom, {
    session$sendCustomMessage(type = "scroll", message = "bottom")
  })
  
  

  
}

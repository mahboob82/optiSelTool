
# SERVERE SIDE SCRIPT OF THE APP
source('global.R')
source('libs/ocslib.R')


global.dfs = reactiveValues()
#textLog = reactiveVal("")

ShowEntryRowsList = list(
  c(5, 10, 12, 15, 30, 50, 100, 200, 500, 1000,-1),
  c('5', '10', '12', '15', '30', '50', "100", "200", "500", "1000", 'All')
)


# IMPORT error and logging codes
source('server/error_and_logsPart.R', local=TRUE)


# IMPORT table rendering codes
source('server/renderTablesPart.R', local=TRUE)



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

  # raw.ped <<- vroom("PedigreeWithErrors.csv", delim = ",")
  # raw.phen <<- vroom("Phen.csv", delim = ",")
  # 
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
  
  
  # observeEvent(input$btn_filter_age_contribution,{
  # 
  # print(ls()[sapply(ls(), function(x) is.data.frame(get(x)))])
  # # updateSelectizeInput(
  # #   session,
  # #   'candes_cont',
  # #   choices = appdfs, #"AgeContrib", #ls()[sapply(ls(), function(x) is.data.frame(get(x)))],
  # #   selected="AgeContrib",
  # #   server = TRUE
  # # )
  #   
  # })
  
  
  
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

  source(file="./server/pedKinAtNPart.R", local=TRUE)
  

  # ====================================================================================================================
  # ===================================================== PED-KINSHIP-NATIVE ===========================================
  # ====================================================================================================================

  source(file="./server/candesPart.R", local=TRUE)


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
  
############################################################################################################
  
  observeEvent(input$RawPedToHome|
                 input$RawPhenToHome|
                 input$PedigToHome|
                 input$InbreedingToHome|
                 input$ComplToHome|
                 input$SummPedigToHome|
                 input$BreedCompToHome|
                 input$AgeContribToHome|
                 input$PhenModifiedToHome|
                 input$PkinToHome|
                 input$PkinatNtoHome, {
                   
      updateTabItems(session, "allTabs", selected = "STEPS")
  })
  
  
  
  
  ############################################################################################################
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
  
  ############################################################################################################
  
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
  
  
  
  ############################################################################################################
  
  observeEvent(input$goToTop, {
    session$sendCustomMessage(type = "scroll", message = "top")
    
  })
  
  observeEvent(input$goToBottom, {
    session$sendCustomMessage(type = "scroll", message = "bottom")
  })
  
  ############################################################################################################

  
}

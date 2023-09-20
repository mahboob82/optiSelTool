########################################################################################################################
###################################### SERVER SIDE Main Script##########################################################
########################################################################################################################

# import necessary app libararies and codes
source('global.R')
source('libs/ocslib.R')

global.dfs = reactiveValues()

ShowEntryRowsList = list(
  c(5, 10, 12, 15, 30, 50, 100, 200, 500, 1000,-1),
  c('5', '10', '12', '15', '30', '50', "100", "200", "500", "1000", 'All')
)

# IMPORT error and logging codes
source('server/error_and_logsPart.R', local=TRUE)


# IMPORT table rendering codes
source('server/renderTablesPart.R', local=TRUE)




# Declare some global intermediate dataframes for future references 
# <<- assignment meaning assignment is a global declaration
Pedig      <<- data.frame()
Compl      <<- data.frame()
Inbreeding <<- data.frame()
SummPedig  <<- data.frame()
BreedComp  <<- data.frame()
AgeContrib <<- data.frame()
Pkin       <<- data.frame()
PkinCopy   <<- data.frame()
PkinCopyRender  <<- data.frame()
SireKinshipRank <<- data.frame()
Phen       <<- data.frame()
PkinatN    <<- NULL
PkinatNof  <<- data.frame()

assume_candes_t <<- 0

appdfs <<- c('Pedig',
             'Compl',
             'Inbreeding', 
             'SummPedig', 
             'BreedComp', 
             'AgeContrib',
             'Phen',
             'Pkin', 
             'PkinatN')


# this format code is required.
divFormat1 = "
display: contents; 
margin: 0px;
padding: 2px;
"


# **********************************************************************************************************************
# *************************** SEVER SIDE CODES (START) *****************************************************************
# **********************************************************************************************************************
# **********************************************************************************************************************


server <- function(input, output, session) {
  
  ## some tracking variables
  textLog = reactiveVal("")
  breeds = c()
  selected_breed <- "NA"
  final_tables = c()
  global_vars <- reactiveValues()
  global_vars$raw_ped_cols <- c()

  raw.ped <<- data.frame()  

  ## Manual upload gui
  ##-----------------
  source("manualUpload.R", local=TRUE)

  
  
  ################################################
  ## Auto upload gui
  ##-----------------
  source("autoUpload.R", local=TRUE)
  auto_upload_pedigree()
  auto_upload_phen()
  ###############################################
  
  
  
  # ********************************************************************************************************************
  # =================================== PREPARE PEDIGREE================================================================
  
    
  source(file="./server/prePedPart.R", local = TRUE)
  

  # ********************************************************************************************************************
  # ================================== COMPLETENESS CALCULATION ========================================================
  
  source(file="./server/completenessPart.R", local = TRUE)
    
  

  # ********************************************************************************************************************
  # ================================ INBREEDING CALCULATION=============================================================
  source(file="./server/inbreedingPart.R", local=TRUE)



  # ********************************************************************************************************************
  # =============================== BREED COMPOSITION CALCULATION=======================================================
  source(file="./server/breedCompPart.R", local=TRUE)


  # ********************************************************************************************************************
  # =============================== PED_SUMMARY CALCULATION=============================================================
  source(file="./server/pedSummaryPart.R", local=TRUE)

     
  # ********************************************************************************************************************
  # =============================== FILTER_AGECONT_L ===================================================================
  source(file="./server/filterAgeContLPart.R", local=TRUE)
  
  
  # ********************************************************************************************************************
  # =============================== PED-KINSHIP ========================================================================
  source(file="./server/pedKinPart.R", local=TRUE)
  

  # ********************************************************************************************************************
  # ================================== PED-KINSHIP-NATIVE ==============================================================
  source(file="./server/pedKinAtNPart.R", local=TRUE)
  

  # ********************************************************************************************************************
  # ================================== PED-KINSHIP-NATIVE ==============================================================
  source(file="./server/candesPart.R", local=TRUE)


# ********************************************************************************************************************
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
  observeEvent(input$btnPlanA,{
      updateTabItems(session, "allPlanTabs", selected = "Plan A")
  })
  
  observeEvent(input$btnPlanB,{
      updateTabItems(session, "allPlanTabs", selected = "Plan B")
  })
  observeEvent(input$btnPlanC,{
      updateTabItems(session, "allPlanTabs", selected = "Plan C")
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

  
  # IMPORT breedplan rendering codes
  source('server/breedpanAPart.R', local=TRUE)
  
  
}

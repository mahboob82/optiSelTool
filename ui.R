
source('global.R')
source('server.R')
source('libs/ocslib.R')
source('ui/ui_lib.R', local = TRUE)


# UI SCRIPT



#--------------HEADER SECTION-------------------------------------------------------------------------------------------
appHeader <- dashboardHeader(title='OptSel-Breeding')

#--------------SIDEBAR SECTION------------------------------------------------------------------------------------------
source('ui/ui_sidebar.R', local=TRUE)

#--------------BODY SECTION---------------------------------------------------------------------------------------------
appBody <- dashboardBody(
  shinyjs::useShinyjs(),
  includeCSS("www/custom.css"),
  tags$link(rel="stylesheet", type="text/css", href="www/custom.css"),
  tags$script(
    HTML('
      Shiny.addCustomMessageHandler("scroll", function(message) {
        if (message === "top") {
          window.scrollTo(0, 0); // Scroll to the top
        } else if (message === "bottom") {
          window.scrollTo(0, document.body.scrollHeight); // Scroll to the bottom
        }
      });
    ')
  ),
  tags$style(HTML("
      /* Custom CSS for checkbox input */
      .custom-checkbox label {
        content: '';
        display: inline-block;
        width: 16px;
        height: 16px;
        padding: 100px;
        border: 12px solid #999;
        background-color: white;
        margin-right: 118px;
        vertical-align: middle;
        position: relative;
      }
    ")),
  
  tabItems(
    tabItem(
      tabName = "Workspace",
      tabsetPanel(
        id="allTabs",
        type = "tabs",
        tabPanel(
            #---- First-Tab UI components --------------------------------------------------------------------------------
            "STEPS",
            source("ui/ui_stepstab_top.R", local=TRUE),
            
            #---- Middle section ----
            tags$hr(style = "border: 1px solid #eafefe; margin: 20px 0;"),
            source("ui/ui_stepstab_bottom.R", local=TRUE)

        ), # tabpanel
        
        
        
        #---- TABLES-Tab UI components --------------------------------------------------------------------------------
        
        tabPanel(
          "TABLES",
          fluidRow(
            column(
              12,
              tabBox(
                id='tabchart1',
                width = 12,
                tabPanel("RawPed",
                    fluidRow(
                      column(9,
                         #div( style="inline",
                         actionButton("RawPedToHome", label=NULL, icon=icon("home")),
                         #actionButton("saveRawPed", label='Download', icon=icon("download")),
                         # actionButton("dataTypesRawPed", label="types", icon=icon("info")),
                         # actionButton("convertRawPed", label="Convert", icon=icon("cog")),
                         # actionButton("convertRawPed", label="Colnames", icon=icon("cog")),
                         actionButton("deleteRawPed", label=NULL, icon=icon("trash")),
                         tags$h4("Pedigree data [original]") ,
                         
                         DTOutput("RawPed")
                      ),
                      column(3,
                             br(),
                             hr(),
                             tags$h4(HTML(paste(icon('book'), "Data types"))),
                             hr(),

                             DTOutput("RawPedColumnInfo")
                      )
                    )
                ),
                tabPanel("Phen",
                         fluidRow(
                           column(9,
                                  #div( style="inline",
                                  actionButton("PhenToHome", label=NULL, icon=icon("home")),
                                  #actionButton("saveRawPed", label='Download', icon=icon("download")),
                                  # actionButton("dataTypesRawPed", label="types", icon=icon("info")),
                                  # actionButton("convertRawPed", label="Convert", icon=icon("cog")),
                                  # actionButton("convertRawPed", label="Colnames", icon=icon("cog")),
                                  actionButton("deletePhen", label=NULL, icon=icon("trash")),
                                  tags$h4("Phenotype data [original]") ,
                                  
                                  DTOutput("Phen")
                           ),
                           column(3,
                                  br(),
                                  hr(),
                                  tags$h4(HTML(paste(icon('book'), "Data types"))),
                                  hr(),
                                  
                                  DTOutput("RawPhenColumnInfo")
                           )
                         )
                #),
                        #div( style="inline",
                        #       actionButton("PhenToHome", label=NULL, icon=icon("home")),
                        #       actionButton("savePhen", label='Download', icon=icon("download")),
                        #       actionButton("deletePhen", label=NULL, icon=icon("trash")),
                        #       tags$h4("Phenotype data [original]")
                        # ),
                        # DTOutput("Phen")
                ),
                tabPanel(
                         "Pedig",
                        fluidRow(
                           column(9,
                            #     div( style="inline",
                            actionButton("PedigToHome", label=NULL, icon=icon("home")),
                            actionButton("deletePedig", label=NULL, icon=icon("trash")),
                            tags$h4("Cleaned Pedigree [processed]"),
                            DTOutput("Pedig")                           
                           ),
                           column(3,
                                  radioButtons("PedigDownloadFormat", label=NULL, 
                                               choices = c("txt", "csv", "xlsx", "rds"), inline = TRUE),
                                  downloadButton("downloadPedig", label='Download', icon=icon("download"))
                         
                          )
                        )
                ),
                tabPanel("Compl", 
                         div( style="inline",
                              actionButton("ComplToHome", label=NULL, icon=icon("home")),
                              actionButton("saveCompl", label="Download", icon=icon("download")),
                              actionButton("deleteCompl", label=NULL, icon=icon("trash")),
                              tags$h4("Completeness [processed]")),
                          DTOutput("Compl")
                        ),
                tabPanel("Inbreeding",
                         div( style="inline",
                              actionButton("InbreedingToHome", label=NULL, icon=icon("home")),
                              actionButton("saveInbreeding", label="Download", icon=icon("download")),
                              actionButton("deleteInbreeding", label=NULL, icon=icon("trash")),
                              tags$h4("Inbreeding Coefficient [processed]")),
                         DTOutput("Inbreeding")
                        ),
                tabPanel("BreedComp",
                         div( style="inline",
                              actionButton("BreedCompToHome", label=NULL, icon=icon("home")),
                              actionButton("saveBreedComp", label="Download", icon=icon("download")),
                              actionButton("deleteBreedComp", label=NULL, icon=icon("trash")),
                              tags$h4("Breed composition of Animals [processed]"),
                              htmlOutput("BreedCompBreed")),
                         DTOutput("BreedComp")
                        ),
                tabPanel("SummPedig",
                         
                         div( style="inline",
                              actionButton("SummPedigToHome", label=NULL, icon=icon("home")),
                              actionButton("saveSummPedig", label="Download", icon=icon("download")),
                              actionButton("deleteSummPedig", label=NULL, icon=icon("trash")),
                              tags$h4("Summary of Pedigree [processed]")),
                         
                         DTOutput("SummPedig")
                        ),
                tabPanel("AgeContrib",
                         div( style="inline",
                              actionButton("AgeContribToHome", label=NULL, icon=icon("home")),
                              actionButton("saveAgeContrib", label="Download", icon=icon("download")),
                              actionButton("deleteAgeContrib", label=NULL, icon=icon("trash")),
                              tags$h4("Age contribution of population [processed]")),
                          DTOutput("AgeContrib")
                        ),
                tabPanel("Pkin",
                    fluidRow(
                         column(9,
                         div( style="inline",
                              actionButton("PkinToHome", label=NULL, icon=icon("home")),
                              actionButton("savePkin", label="Download", icon=icon("download")),
                              actionButton("deletePkin", label=NULL, icon=icon("trash")),
                              tags$h4("Kinship of Individuals (Popln)"),                                
                              selectInput("si_sexinfo", "Filter by Sex", choices=(c("all", "male", "female")))),
                         DTOutput("Pkin")
                         ),
                        column(3,
                          numericInput("Pkinship_equiGen", "equiGen", value=5, step=1, min = 0, max=100),
                          textInput("Pkinship_BV", "BV", value="1.0"),
                          br(),
                          h4("Female's Kinship with Sires"),
                          tags$h4(textOutput("FemaleIDwithKinship")),
                          br(),
                          DTOutput("SireKinshipRank")
                         )
                    )

                ),
                tabPanel("PkinatN",
                    fluidRow(
                        
                         # column(9,
                         #  selectInput("si_sexinfo", "Filter by Sex", choices=(c("all", "male", "female"))),
                         #  DTOutput("Pkin")
                         # ),
                        column(9,
                         div( style="inline",
                              actionButton("PkinatNtoHome", label=NULL, icon=icon("home")),
                              actionButton("savePkinatN", label="Download", icon=icon("download", style="align-right")),
                              actionButton("deletePkinatN", label=NULL, icon=icon("trash", style="align-right")),
                              tags$h4("Kinship at Native Alleles")),                               
                         DTOutput("PkinatN")
                        ),
                        column(3,
                               tags$h4("Native Contribution"),
                               DTOutput("PkinatNwithNC"),
                               br(),
                               tags$h4("Native Kinship"),
                               textOutput("NativeKinship")
                        )
                        
                    )

                ),
              )
            )
            
          )
          #selectizeInput("Tables", "Select ", choices = NULL),
          #DTOutput("Pedig"),DTOutput("BreedComp")
          #DT::dataTableOutput("BreedComp", width=400)
        ),
        
        
        #---- FIGURES-Tab UI components ----------------------------------------------------------------------------------
        tabPanel(
          "FIGURES",
          fluidRow(
            column(
              12,
              tabBox(
                id='tabGraph1',
                width = 12,
                tabPanel("PedigreeRaw", h2("figure1")),
                tabPanel("Pedig", h2("figure2"))
                
              )
            )
          )
          #selectizeInput("Tables", "Select ", choices = NULL),
          #DTOutput("Pedig"),DTOutput("BreedComp")
          #DT::dataTableOutput("BreedComp", width=400)
        ),
        
        
        #---- LOG-Tab UI components --------------------------------------------------------------------------------
        
        # tabPanel("LOG","Last Args:", textOutput("CallArgs"))
        
        #tabPanel("LOG","Last Args:", verbatimTextOutput("CallArgs", placeholder = TRUE))
        tabPanel("LOG","Last Args:", htmlOutput("CallArgs"))
        #tabPanel("HelpInfo","Last Args:", htmlOutput("ocs_original_html"))
        
        #actionButton("refresh", "Refresh")
        

      ), # tabsetpanel ends
      
      
    ), # tabitem ends
    tabItem(
      tabName = "BreedingPlan",
      tabsetPanel(
        id="allPlanTabs",
        type = "tabs",
        tabPanel(
          "Plan A",
          h2("UI for plan A")
        ),

        tabPanel(
          "Plan B",
          h2("UI for plan B")
        ),
        tabPanel(
          "Plan C",
          h2("UI for plan C")
        )

      )
    )


  ),
  
  fluidRow(
    column(2,
           div(class = "floating-panel",
               actionButton("goToTop", "Top",width='60px',class='floating-button'),br(),
               actionButton("goToBottom", "Bottom",width='60px',class='floating-button')
               
           )
    )
  )

  #strong('Relation between Sepal Length and Petal Length for different species:'),
  #plotOutput('correlation')
  # tableOutput("PedigWithErrors")
  #tableOutput("iris")
  #DTOutput('iris')
  # DTOutput('PedigWithErrors'),
  
  # prePed(Pedig, keep=NULL, thisBreed=NA, lastNative=NA, addNum=FALSE)
  # completeness(Pedig, keep=NULL, maxd=50, by="Indiv")
  # pedBreedComp(Pedig, thisBreed="Hinterwaelder")
  
  
  
  
)

#-----------------------------------------------------------------------------------------------------------------------
# App 
ui <- shinyUI(
    dashboardPage(
    appHeader,
    appSidebar,
    appBody,
    skin = "red")
)


shinyApp(ui=ui, server=server)

# if (interactive()) {
#   options(device.ask.default = FALSE)
#   
#   app=  shinyApp(ui=ui, server=server, 
#          options = list(
#            width=1200,
#            launch.browser = TRUE,
#            browser = "C:/Program Files/Google/Chrome/Application/chrome.exe"))
# 
#   #runApp(app)
# }
# runApp(list(ui = ui, server = server),host="127.0.0.1",port=8888, launch.browser = TRUE)


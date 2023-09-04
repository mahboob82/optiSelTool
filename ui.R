
source('global.R')
source('server.R')
source('ocslib.R')


# UI SCRIPT



#--------------HEADER SECTION-------------------------------------------------------------------------------------------
appHeader <- dashboardHeader(title='OptSel-Breeding')


#--------------SIDEBAR SECTION------------------------------------------------------------------------------------------
appSidebar <- dashboardSidebar(
  width="300px",
  sidebarMenu(
    menuItem(tabName="Workspace", text="Workspace",  icon=icon("home")),
    div(style="padding-left:20px;",
        hr(),
        fileInput("RawPedUpload", "Upload Pedigree file:",accept=c(".csv", ".txt", ".rds")),
        fileInput("RawPhenUpload", "Upload Phenotype file:",accept=c(".csv", ".txt", ".rds")),
        actionButton("clearDB", "Clear database", width='100px', class='floating-button'),
    ),
  
    menuItem(tabName="BreedingPlan", text="Breeding Plans", icon=icon("th")),
    div(
        hr(),
        style="padding-left:20px",
        actionButton("btnPlanA", "Plan A", width='100px', class='floating-button'),
        actionButton("btnPlanB", "Plan B", width='100px', class='floating-button'),
        actionButton("btnPlanC", "Plan C", width='100px', class='floating-button')
    ),

    hr(),
    div(
      style="padding-left:20px",
      h4("OPTISEL official doc."),
      actionButton(inputId='PedOCSdoc',label="1. Pedigree OCS",icon=icon("book"), onclick=PedOCSdoc)),
    div(style="padding-left:20px",
      actionButton(inputId='MarkerOCSdoc',label="2. Marker OCS",icon=icon("book"), onclick=MarkerOCSdoc)),
    div(style="padding-left:20px",
        actionButton(inputId='OCSdoc',label="3. OCS",icon=icon("book"), onclick=OCSdoc))


  # actionButton(inputId='ab2', label='Upload'),
  # actionButton(
  #   inputId='Work',
  #   label="Learn More",
  #   icon=icon("th"),
  #   onclick=helplink
  # )
  ))



# grid2cols = "
# display:grid; 
# grid-template-columns: 50% 50%; 
# grid-gap: 0px; 
# background-color=#ccc; 
# padding: 0px;
# margin: 0px;
# text-align: left;
# "

divFormat0 = "
display: contents; 
margin: 0px;
padding: 2px;
min-height: 60px;
font-weight: bold;
text-align: center;
border: 3px solid green;
"
# color:#fff;
# background: #014d4e;

divFormat1 = "
display: contents; 
margin: 0px;
padding: 2px;
"
# color: white;
# background: #019799;

  
divFormat2 = "
display: inline-block; 
min-width: 7%; 
max-width: 10.0%;
max-height: 60px;
margin: 0px;
padding: 0px;
"
# background: lightblue;
# min-width: 25%; 
# max-width: 25.0%;


custom_box_style="width: 100%; background-color:#13395d;padding:4px;color:white"


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
          
          #---- Top section ----
          fluidRow(
            column(
              2,
              div(style=divFormat0, h4("Prepare pedigree")),
              div(width=NULL, style=divFormat1, selectizeInput("preped_keep", "keep", choices=NULL, width = '95%')),
              div(style=divFormat1, sliderInput("preped_slider","born [RawPed] ", min = born_minmax[1], max = born_minmax[2],
                                                value = born_slider_init_range, step = 1, width="95%", sep="")),
              
              div(style=divFormat1, selectizeInput("preped_breed", "thisBreed", choices=NULL, width = '95%')),
              
              fluidRow(
                column(6,
                  checkboxInput("enable_preped_lastNative", tags$b("lastNative"), width="100%")),
                column(6,
                  numericInput("preped_lastNative", "Born", value=1970, width="85%")),
              ),
              div(style=divFormat1, selectInput("preped_addnum", "addNum", choices=c("FALSE", "TRUE"), width = '95%')),
              div(style=divFormat1, 
                  actionButton(inputId = "btn_preped", label = "Prepare", class = "custom-button"),
                  actionButton(inputId = "btn_preped_view", label = "", icon=icon('search'),class = "custom-button"))
            ),

            column(
              2,
              div(style=divFormat0,h4("Ped Completeness")),
              div(style=divFormat1, selectizeInput("completeness_keep", "keep [Phen]", choices=NULL, width = '95%')),
              div(style=divFormat1, sliderInput("completeness_slider","born [RawPed] ", min = born_minmax[1], max = born_minmax[2],
                                                value = born_slider_init_range, step = 1, width="95%", sep="")),
              div(style=divFormat1, numericInput("completeness_maxd", "maxd", value=50, width = '95%')),
              div(style=divFormat1, selectizeInput("completeness_by", "by", choices=NULL, width = '95%')),
              div(style=divFormat1, 
                  actionButton(inputId = "btn_completeness", label="Calculate", class = "custom-button"),
                  actionButton(inputId = "btn_completeness_view", label = "", icon=icon('search'),class = "custom-button"))
            ),
            column(
              2,
              div(style=divFormat0,h4("Pedigree Inbreeding")),
              div(style=divFormat1, 
                  actionButton(inputId = "btn_pedInbreeding", label="Calculate", class = "custom-button"),
                  actionButton(inputId = "btn_pedInbreeding_view", label = "", icon=icon('search'),class = "custom-button")),
              br(),
              br(),

              div(style=divFormat0,h4("Breed Composition")),
              div(style=divFormat1, selectizeInput("pedbreedcomp_breed", "thisBreed", choices=NULL, width = '95%')),
              div(style=divFormat1, 
                  actionButton(inputId = "btn_pedbreedcomp", label="Calculate", class = "custom-button"),
                  actionButton(inputId = "btn_pedbreedcomp_view",label = "", icon=icon('search'),class = "custom-button"))
            ),


          column(
            2,
            div(style=divFormat0, h4('Pedigree Summary')),
            div(style=divFormat1, selectizeInput("pedsummary_keep_only", "keep.only [Phen]", choices=NULL, width = '95%')),
            div(style=divFormat1, sliderInput("pedsummary_slider", "keep.only [Pedig] ", min=born_minmax[1], max=born_minmax[2],
                                                value= born_slider_init_range, step= 1, width="95%",sep="")),
            div(style=divFormat1, numericInput("pedsummary_maxd", "maxd", value=50, width = '95%')),
            div(style=divFormat1, numericInput("pedsummary_d", "d", value=4, width = '95%')),
            div(style=divFormat1, 
                actionButton(inputId = "btn_pedsummary", label="Calculate", class = "custom-button"),
                actionButton(inputId = "btn_pedsummary_view", label = "", icon=icon('search'),class = "custom-button"))
          ),

          column(
            2,
            div(style=divFormat0, h4('Filter/AgeContrib/L')),
            div(style=divFormat1, sliderInput("filter_agecontrib_slider","born ", min=born_minmax[1], max=born_minmax[2],
                value = born_slider_init_range, step = 1, sep="",width = '95%')),
            div(style=divFormat1, selectizeInput("filter_agecontrib_breed", "thisBreed", choices=NULL, width = '95%')),
            div(style=divFormat1, numericInput("filter_agecontrib_equiGen", label="equiGen > ", value=4, width = '95%')),
            div(style=divFormat1, 
                actionButton(inputId = "btn_filter_age_contribution", label = "Calculate", class = "custom-button"),
                actionButton(inputId = "btn_filter_age_contribution_view", label = "",icon=icon("search"), class = "custom-button"))
          )
        ),

          #---- Middle section ----
          tags$hr(style = "border: 1px solid #eafefe; margin: 20px 0;"),
          fluidRow(
            column(
              width=2,
              div(style=divFormat0, h4('Pedigree Kinship for All')),
              div(style=divFormat1, selectizeInput("pedkin_keeponly", "keep.only", choices=NULL, width = '95%')),
              div(style=divFormat1, selectizeInput("pedkin_keep", "keep", choices=NULL, width = '95%')),
              div(style=divFormat1, selectizeInput("pedkin_founder", "kinFounder", choices=NULL, width = '95%')),
              div(style=divFormat1, 
                  actionButton(inputId = "btn_pedIBD", label = "Calculate", class = "custom-button"),
                  actionButton(inputId = "btn_pedIBD_view", label = "",icon = icon("search"), class = "custom-button"))
            ),

            column(
              width=2,
              div(style=divFormat0, h4('Pedigree Kinship for Natives')),
              div(style=divFormat1, selectizeInput("pedKinatN_breed", "thisBreed", choices=NULL, width = '95%')),
              div(style=divFormat1, selectizeInput("pedKinatN_keeponly", "keep.only", choices=NULL, width = '95%')),
              div(style=divFormat1, selectizeInput("pedKinatN_keep", "keep", choices=NULL, width = '95%')),
              div(style=divFormat1, textInput("pedKinatN_ngen", label="nGen", value="NA", width = '95%')),
              div(style=divFormat1, 
                  actionButton(inputId = "btn_pedKinatN", label = "Calculate", class = "custom-button"),
                  actionButton(inputId = "btn_pedKinatN_view", label = "", icon=icon('search'),class = "custom-button"))
            ),

            column(
              width=2,
              div(style=divFormat0, h4('Candidates Description')),
              div(style=divFormat1, selectizeInput("candes_cont", "cont", choices=NULL, width = '95%')),
              div(style=divFormat1, numericInput("candes_n", "N", value=1000, width = '95%')),
              div(style=divFormat1, selectInput("candes_quiet", "quiet", choices=c("FALSE","TRUE"), selected="FALSE", width = '95%')),
              div(style=divFormat1, numericInput("candes_t", label="t", value=assume_candes_t, width = '95%')),
              div(style=divFormat1, numericInput("candes_bc", label="bc", value="NULL", width = '95%')),
              div(style=divFormat1, selectInput("candes_reduce", "reduce.data", choices=c("TRUE","FALSE"), selected='TRUE', width = '95%')),
              div(style=divFormat1, 
                  actionButton(inputId = "btn_candes", label = "Combine", class = "custom-button"),
                  actionButton(inputId = "btn_candes_view", label = "", icon=icon('search'),class = "custom-button"))

            ),

            column(
              width=2,
              div(style=divFormat0, h4('Optimum Contribution')),
              div(style=divFormat1, selectizeInput("opticont_method", "method",choices=c("max.BV", "min.BV", "max.pKin", "min.pKin", "max.pKinatN","min.pKinatN"), width = '95%')),
              div(style=divFormat1, checkboxGroupInput(inputId="selected_con_opt", "Select:",choices=c("uniform=female", "ub.pKin", "ub.pKinatN", "lb.NC"),selected=c("uniform=female", "ub.pKin"), width = '95%')),
              div(style=divFormat1, selectInput("opticont_bc", "bc", choices=c("NULL"), width = '95%')),
              div(style=divFormat1, selectInput("opticont_solver", "solver", choices=c("alabama","cccp","cccp2","slsqp", "default"),selected="default", width = '95%')),
              div(style=divFormat1, selectInput("opticont_quiet", "quiet", choices=c("FALSE","TRUE"), width = '95%')),
              div(style=divFormat1, selectInput("opticont_make.definite", "make.definite", choices=c("FALSE","TRUE"), width = '95%')),
              div(style=divFormat1, 
                  actionButton("btn_opticont_method", "Calculate", class = "custom-button"),
                  actionButton(inputId = "btn_opticont_method_view", label = "", icon=icon('search'),class = "custom-button"))

            ),

            column(
              width=2,
              div(style=divFormat0, h4('Optimization policies')),
              div(style=divFormat1,tags$hr(style = "border: 1px solid #ccc; margin: 20px 0;")),
              div(style=divFormat1, actionButton("opticont_method_btn1", "MaxBV", class = "custom-button2")),
              div(style=divFormat1, actionButton("opticont_method_btn2", "MinF",class = "custom-button2")),
              div(style=divFormat1,actionButton("opticont_method_btn3", "MaxNC",class = "custom-button2")),
              div(style=divFormat1,actionButton("opticont_method_btn4", "MinNatC",class = "custom-button2")),
              div(style=divFormat1,actionButton("opticont_method_btn5", "MinNC_MaxBV",class = "custom-button2")),
              div(style=divFormat1,tags$hr(style = "border: 1px solid #ccc; margin: 20px 0;")),
              div(style=divFormat1,actionButton("opticont_method_btn5", "RUN",class = "custom-button"))
            )
          ),
        #  tags$hr(style = "border: 1px red #ccc; margin: 20px 0;")
          # #---- Bottom section ----  
          # # 
          # # fluidRow(
          # #   column(2,box(title = "Example BOX", height="130px", width='100%', background = 'red')),
          # #   column(width = 8,
          # #     box(
          # #         width=NULL,
          # #         column(3,height="60px",selectizeInput("opticont_method", "method", choices=c("max.BV1", "min.BV"))),
          # #         column(3,height="60px",selectizeInput("opticont_method", "method", choices=c("max.BV2", "min.BV"))),
          # #         column(3,height="60px",selectizeInput("opticont_method", "method", choices=c("max.BV3", "min.BV"))),
          # #         column(3,height="60px",selectizeInput("opticont_method", "method", choices=c("max.BV4", "min.BV"))),
          # #         column(3,height="60px",selectizeInput("opticont_method", "method", choices=c("max.BV5", "min.BV"))),
          # #         column(3,height="60px",selectizeInput("opticont_method", "method", choices=c("max.BV6", "min.BV"))),
          # #         column(3,height="60px",actionButton(inputId = "candes_btn", label = "Combine")),
          # #         column(3,height="60px",selectizeInput("opticont_method", "method", choices=c("max.BV8", "min.BV")))
          # #     ),
          # # 
          # #         
          # #         
          # #         h5("hello world4")
          # #   )
          # # )
          
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


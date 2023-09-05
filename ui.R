
source('global.R')
source('server.R')
source('libs/ocslib.R')
source('ui/ui_lib.R', local = TRUE)


ui_stepstab_top    = source("ui/ui_stepstab_top.R", local=TRUE)
ui_stepstab_bottom = source("ui/ui_stepstab_bottom.R", local=TRUE)

ui_tables_Rawped_to_PkinatN = source("ui/ui_tables_Rawped_to_PkinatN.R", local=TRUE)
ui_figures = source("ui/ui_figures.R")


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
            #---- First-Tab UI components ------------------------------------------------------------------------------
            "STEPS",
            ui_stepstab_top$value,
            
            #---- Middle section ----
            tags$hr(style = "border: 1px solid #eafefe; margin: 20px 0;"),
            
            ui_stepstab_bottom$value
            

        ), 
        
        #---- TABLES-Tab UI components ---------------------------------------------------------------------------------
        
        tabPanel(
          "TABLES",
          ui_tables_Rawped_to_PkinatN$value
        ),
        
        
        #---- FIGURES-Tab UI components --------------------------------------------------------------------------------
        tabPanel(
          "FIGURES",
          
          ui_figures$value
        ),
        
        
        #---- LOG-Tab UI components ------------------------------------------------------------------------------------
        tabPanel("LOG","Last Args:", htmlOutput("CallArgs"))


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

)

#-----------------------------------------------------------------------------------------------------------------------
# App STRUCTURE  AND RUN
#-----------------------------------------------------------------------------------------------------------------------

ui <- shinyUI(
    dashboardPage(
    appHeader,
    appSidebar,
    appBody,
    skin = "red")
)



# shinyApp(ui=ui, server=server)










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


        fluidRow(
          column(
            width=2,
            div(style=divFormat0, h4('Pedigree Kinship for All')),
            div(style=divFormat1, selectizeInput("pedkin_keeponly", "keep.only [Phen]", choices=NULL, width = '95%')),
            div(style=divFormat1, selectizeInput("pedkin_keep", "keep [Pedig]", choices=NULL, width = '95%')),
            div(style=divFormat1, selectizeInput("pedkin_founder", "kinFounder", choices=NULL, width = '95%')),
            div(style=divFormat1, 
                actionButton(inputId = "btn_pedIBD", label = "Calculate", class = "custom-button"),
                actionButton(inputId = "btn_pedIBD_view", label = "",icon = icon("search"), class = "custom-button"))
          ),
          
          column(
            width=2,
            div(style=divFormat0, h4('Pedigree Kinship for Natives')),
            div(style=divFormat1, selectizeInput("pedKinatN_breed", "thisBreed [Pedig]", choices=NULL, width = '95%')),
            div(style=divFormat1, selectizeInput("pedKinatN_keeponly", "keep.only [Phen]", choices=NULL, width = '95%')),
            div(style=divFormat1, selectizeInput("pedKinatN_keep", "keep [Pedig]", choices=NULL, width = '95%')),
            div(style=divFormat1, textInput("pedKinatN_ngen", label="nGen", value="NA", width = '95%')),
            div(style=divFormat1, 
                actionButton(inputId = "btn_pedKinatN", label = "Calculate", class = "custom-button"),
                actionButton(inputId = "btn_pedKinatN_view", label = "", icon=icon('search'),class = "custom-button"))
          ),
          
          column(
            width=2,
            div(style=divFormat0, h4('Candidates Description')),
            div(style=divFormat1, selectizeInput("candes_cont", "cont [df]", choices=NULL, width = '95%')),
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
        )
        
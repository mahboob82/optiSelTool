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
    div(style=divFormat1, selectizeInput("completeness_keep", "keep [RawPhen]", choices=NULL, width = '95%')),
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
  div(style=divFormat1, selectizeInput("pedsummary_keep_only", "keep.only [RawPhen]", choices=NULL, width = '95%')),
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
)

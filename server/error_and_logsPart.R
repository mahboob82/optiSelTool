

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

ShowNativeKinshipEmptyDFReturn <- function (msg){
  showModal(modalDialog(
    title = "Pedig return value error:",
    div(
      tags$b(msg),
      tags$hr(),
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

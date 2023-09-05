          fluidRow(
            column(
              12,
              tabBox(
                id='tabchart1',
                width = 12,
                tabPanel("RawPed",
                    fluidRow(
                      column(9,
                         actionButton("RawPedToHome", label=NULL, icon=icon("home")),
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
                tabPanel("RawPhen",
                         fluidRow(
                           column(9,
                                  #div( style="inline",
                                  actionButton("RawPhenToHome", label=NULL, icon=icon("home")),
                                  actionButton("deleteRawPhen", label=NULL, icon=icon("trash")),
                                  tags$h4("Phenotype data [original]") ,
                                  
                                  DTOutput("RawPhen")
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
                         fluidRow(
                           column(9,
                                  actionButton("ComplToHome", label=NULL, icon=icon("home")),
                                  #actionButton("saveCompl", label="Download", icon=icon("download")),
                                  actionButton("deleteCompl", label=NULL, icon=icon("trash")),
                                  tags$h4("Completeness [processed]"),
                                  DTOutput("Compl")
                           ),
                           column(3,
                                  radioButtons("ComplDownloadFormat", label=NULL, 
                                               choices = c("txt", "csv", "xlsx", "rds"), inline = TRUE),
                                  downloadButton("downloadCompl", label='Download', icon=icon("download"))
                                  
                           )
                         )
                ),
                tabPanel("Inbreeding",
                         fluidRow(
                           column(9,
                              actionButton("InbreedingToHome", label=NULL, icon=icon("home")),
                              #actionButton("saveInbreeding", label="Download", icon=icon("download")),
                              actionButton("deleteInbreeding", label=NULL, icon=icon("trash")),
                              tags$h4("Inbreeding Coefficient [processed]"),
                              DTOutput("Inbreeding")
                           ),
                           column(3,
                                  radioButtons("InbreedingDownloadFormat", label=NULL, 
                                               choices = c("txt", "csv", "xlsx", "rds"), inline = TRUE),
                                  downloadButton("downloadInbreeding", label='Download', icon=icon("download"))
                                  
                           )                           
                        )
                ),
                tabPanel("BreedComp",
                         fluidRow(
                           column(9,
                                  actionButton("BreedCompToHome", label=NULL, icon=icon("home")),
                                  #actionButton("saveBreedComp", label="Download", icon=icon("download")),
                                  actionButton("deleteBreedComp", label=NULL, icon=icon("trash")),
                                  tags$h4("Breed composition of Animals [processed]"),
                                  htmlOutput("BreedCompBreed"),
                                  DTOutput("BreedComp")
                           ),
                           column(3,
                                  radioButtons("BreedCompDownloadFormat", label=NULL, 
                                               choices = c("txt", "csv", "xlsx", "rds"), inline = TRUE),
                                  downloadButton("downloadBreedComp", label='Download', icon=icon("download"))
                                  
                           )                
                         )
                ),
                tabPanel("SummPedig",
                         fluidRow(
                           column(9,
                                  actionButton("SummPedigToHome", label=NULL, icon=icon("home")),
                                  #actionButton("saveSummPedig", label="Download", icon=icon("download")),
                                  actionButton("deleteSummPedig", label=NULL, icon=icon("trash")),
                                  tags$h4("Summary of Pedigree [processed]"),
                                  DTOutput("SummPedig")
                           ),
                           column(3,
                                  radioButtons("SummPedigDownloadFormat", label=NULL, 
                                               choices = c("txt", "csv", "xlsx", "rds"), inline = TRUE),
                                  downloadButton("downloadSummPedig", label='Download', icon=icon("download"))
                                  
                           )                
                         )
                ),                         
                         
                tabPanel("AgeContrib",
                    fluidRow(
                      column(6,
                             div( style="inline",
                                  actionButton("AgeContribToHome", label=NULL, icon=icon("home")),
                                  #actionButton("saveAgeContrib", label="Download", icon=icon("download")),
                                  actionButton("deleteAgeContrib", label=NULL, icon=icon("trash")),
                                  tags$h4("Age contribution of population [processed]")),
                             DTOutput("AgeContrib")
                      )
                    )
                ),
                tabPanel("Phen",
                    fluidRow(
                      column(6,
                             div( style="inline",
                                  actionButton("PhenModifiedToHome", label=NULL, icon=icon("home")),
                                  #actionButton("savePhenModified", label="Download", icon=icon("download")),
                                  actionButton("deletePhenModified", label=NULL, icon=icon("trash")),
                                  tags$h4("Phen Modified [processed]")),
                             DTOutput("Phen")
                      )
                    )
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
                               tags$b(textOutput("NativeKinship"))
                        )
                        
                    )

                ),
              )
            )
            
          )
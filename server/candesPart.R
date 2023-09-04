
# ===CANDIDATES DESCRIPTION ================================================================================================


  # ----
  updateSelectizeInput(
    session,
    'candes_cont',
    choices = appdfs, #"AgeContrib", #ls()[sapply(ls(), function(x) is.data.frame(get(x)))],
    selected="AgeContrib",
    server = TRUE
  )


  observeEvent(input$btn_candes,{

    data(PedigWithErrors)
    # pedig step
    Pedig      <- prePed(PedigWithErrors, thisBreed="Hinterwaelder", lastNative=1970,
                       keep=PedigWithErrors$Born%in%1992)
    # breed composition step, add NC
    Pedig$NC   <- pedBreedComp(Pedig, thisBreed="Hinterwaelder")$native
    # pedigree kinship and pedigree native kinship
    use        <- Pedig$Born %in% (1980:1990) & Pedig$Breed=="Hinterwaelder"
    Population <- Pedig$Indiv[use]
    pKin       <- pedIBD(Pedig, keep.only=Population)
    # kinship at native alleles only
    pKinatN    <- pedIBDatN(Pedig, thisBreed="Hinterwaelder",  keep.only=Population)
    Phen       <- Pedig[Population, ]

    ### Situation 1: Having Overlapping Generations (cont != NULL)
                    ### Old individuals contribute only little to the means:
    cont <- agecont(Pedig, Population, maxAge=10)

    candOG <- candes(phen=Phen, pKin=pKin, pKinatN=pKinatN, cont=cont, quiet=as.logical(input$candes_quiet))

    print(candOG$current[,c("Name", "Type", "Breed", "Val", "Var")])
    print(candOG$mean)
    print(candOG$mean$pKin)  #[1] 0.01979228


    ### Situation 2: Discrete Generations (cont=NULL).
                  ### Old individuals and young individuals contribute equally to the means:

    Phen$Born <- 1  # change all Born year to a constant 1
    cont <- NULL
    candDG <- candes(phen=Phen, pKin=pKin, pKinatN=pKinatN, cont=cont, quiet=as.logical(input$candes_quiet))
    print(candDG$current[,c("Name", "Type", "Breed", "Val", "Var")])
    print(candDG$mean)
    print(candDG$mean$pKin)



    })

        # ---- BUTTON event ----------------------------------------------------------------------------
  observeEvent(input$btn_candes_view,{
      updateTabItems(session, "allTabs", selected = "TABLES")
      updateTabsetPanel(session, "tabchart1", selected = "PkinatN")
  })


# Load global variables and objects
source('global.R')

#=======================================================================================================================
# preped_btn_onclick() processes inputs and passes them to optiSel::prePed() function
#-----------------------------------------------------------------------------------------------------------------------
btn_prePed_onclick <- function(df, refdf, keep, breed, lastnative, addnum){
  # process raw args
  print(paste( "LastNative: ", lastnative))
  #return (NULL)
  # 'keep' as NULL or a vector
  if (class(keep) == "character" & nrow(refdf) != 0 ){

    keeplist=as.vector(unlist(refdf[keep]))
  }else{

    keeplist = df$Born %in% (keep[1]:keep[2])
  }
  print(length(keeplist))
  # 
  # if (keep=="NULL")
  #   keeplist=NULL
  # else
  #   keeplist=as.vector(unlist(df[keep]))
  # 
  # 'addnum' as TRUE/FALSE
  addnum= as.logical(addnum)
  
  # 
  if (!is.na(lastnative)) {
    lastnative= as.numeric(lastnative)
  }else{
    lastnative = NA
  }

  # optiSel::prePed() is used to process pedigree
  # Usage:   prePed(Pedig, keep=NULL, thisBreed=NA, lastNative=NA, addNum=FALSE)
  
  # ---- call to optiSel
  Pedig <- optiSel::prePed(
      raw.ped,
      keep=keeplist, 
      thisBreed=breed, 
      lastNative=lastnative, 
      addNum=addnum
    )
  
  return(Pedig)
  
}


#=======================================================================================================================
# completeness_btn_onclick() processes inputs and passes them to optiSel::prePed() function
# usage: completeness_btn_onclick("Indiv", "Jersey", "1970", "FALSE")
#-----------------------------------------------------------------------------------------------------------------------
btn_completeness_onclick <- function(df, refdf, keep,maxd,by ){
  
  if (class(keep) == "character"){
    # get a list of reference from small df
    keeplist=as.vector(unlist(refdf[keep]))
    
  }else{
    # get a list of ids from cleaned, large df
    keeplist = df$Born %in% (keep[1]:keep[2])
  }
  
  #print(keeplist)
  # 'maxd' as TRUE/FALSE
  ifelse (maxd=="NA", NA, as.numeric(maxd))
   
  # print(paste(keep, by, class(maxd), maxd) )
  # stop("DDDDDDDDDDDDDDDDDDDD")
   
  # # optiSel::prePed() is used to process pedigree
  # # Usage:   
  # 
  # ---- call to optiSel
  #print(head(raw.ped))
  
  print(names(df))
  print(names(refdf))
  
  compl <- optiSel::completeness(
     df,
     keep=keeplist, 
     maxd=maxd,
     by=by)

  #print(head(compl))
  #print(dim(compl))
  # # validate
  # # taildf = tail(Pedig[-1])
  # # print(taildf)
  # 
  # # save into disk as R object
  # save(Pedig, file="Pedig.tmp")
  rownames(compl) <- seq(1,nrow(compl))
  return(compl)
}
# completeness(Pedig, keep=Pedig$Born %in% (2006:2007), maxd=50, by="Indiv")
# completeness(Pedig, keep=Phen$Indiv, maxd=50,by="Indiv")


#print(paste("Calculate breed composition within animal", breed))
# load(file="Pedig.tmp")
#=======================================================================================================================
# pedinbreed_btn_onclick() processes inputs and passes them to optiSel::prePed() function
#-----------------------------------------------------------------------------------------------------------------------
btn_pedInbreeding_onclick <- function(df, breed){
  inb_df <- pedInbreeding(df)
  # print(head(inb_df))
  return (inb_df)
}

#=======================================================================================================================
# pedbreedcomp_btn_onclick() processes inputs and passes them to optiSel::prePed() function
#-----------------------------------------------------------------------------------------------------------------------
# pedbreedcomp_btn_onclick("Indiv", "Jersey", "1970", "FALSE")

btn_pedBreedcomp_onclick <- function(df, breed){
  #print(paste("Calculate breed composition within animal", breed))
  # load(file="Pedig.tmp")

  cont <- pedBreedComp(df, thisBreed=breed)
  df$NC <- cont$native
  #df = tail(cont[,2:5])
  print(head(df))
  #save(cont, file="Cont.tmp")
  #save(Pedig, file="Pedig.tmp")
  ##load(file="Cont.tmp")
  
  return (list(df, cont))
    
}



#=======================================================================================================================
# pedsummary_btn_onclick() processes inputs and passes them to optiSel::prePed() function
#-----------------------------------------------------------------------------------------------------------------------
# pedsummary_btn_onclick("Indiv", "Jersey", "1970", "FALSE")
btn_pedSummary_onclick <- function(df, refdf, keep_only, maxd, d,... ){
  

  if (class(keep_only) == "character"){
    # get a list of reference from small df
    keeplist=as.vector(unlist(refdf[keep_only]))
    
  }else{
    # get a list of ids from cleaned, large df
    keeplist = df$Born %in% (keep_only[1]:keep_only[2])
  }
  
  print(keeplist)
  # 'maxd' as TRUE/FALSE
  ifelse (maxd=="NA", NA, as.numeric(maxd))
  
  # print(paste(keep, by, class(maxd), maxd) )
  # stop("DDDDDDDDDDDDDDDDDDDD")
  
  # # optiSel::prePed() is used to process pedigree
  # # Usage:   
  # 
  # ---- call to optiSel
  #print(head(raw.ped))
  
  # print(colnames(df))
  # print(colnames(refdf))
  
  summPed <- optiSel::summary.Pedig(
    df,
    keep=keeplist, 
    maxd=maxd,
    d=d)
  
  # print(head(summPed))
  # print(dim(summPed))
  # # validate
  # # taildf = tail(Pedig[-1])
  # # print(taildf)
  # 
  # # save into disk as R object
  # save(Pedig, file="Pedig.tmp")
  rownames(summPed) <- seq(1,nrow(summPed))
  return(summPed)
}




#=======================================================================================================================
# btn_ageContribL_onclick() processes inputs and passes them to optiSel::prePed() function
#-----------------------------------------------------------------------------------------------------------------------
# btn_ageContribL_onclick("Indiv", "Jersey", "1970", "FALSE")

btn_ageContribL_onclick <- function(df, sld, breed, equiGen){
  # filter data
  #print(head(df))
  #print(sld)

  use = df$Born %in% (sld[1]:sld[2]) & (df$Breed==breed)
  
  if (equiGen>0)
    use = use & summary(df)$equiGen>=equiGen

  
  #print(c("Indiv", "Sex", "Breed", "Born", "BV", "NC") %in% colnames(df))
  target_cols = c("Indiv", "Sex", "Breed", "Born", "BV", "NC")
  phen= df[use, intersect(colnames(df), target_cols)]
  
  # add a column 
  phen$isCandidate= phen$Born<=sld[2] + 1
  flag = 0
  #print(breed)  
  
  if (nrow(phen) == 0){
    flag = 1
    return(list(flag,phen,0,0))
  }
  
  # age contribution from optiSel
  cont = agecont(df, phen$Indiv)
  L = 1/(4*cont$male[1]) + 1/(4*cont$female[1])
  
  #print(head(cont))
  #print(L)
  
  return (list(flag, phen, cont, L))

}




#=======================================================================================================================
# pedkin_btn_onclick() processes inputs and passes them to optiSel::prePed() function
#-----------------------------------------------------------------------------------------------------------------------
# filter_btn_onclick("Indiv", "Jersey", "1970", "FALSE")
btn_pedIBD_onclick <- function(df, refdf, keep_only, keep, kin_founder){
  # filter data
  print(head(df))
  print(keep_only)
  
  flag = 0
  if (class(keep_only) == "character" & keep_only != "NULL"){
    keep_only_list=as.vector(unlist(refdf[keep_only]))
    keeplist = keep_only_list
    
  }else{
    flag = 1
    # get a list of ids from cleaned, large df
    keep_only_list=NULL
    keeplist = keep_only_list
  }  

 # ---- kinship ----

  if (flag)
    return (list(flag, data.frame()))
  else
    pKin = pedIBD(df, keep.only = keep_only_list)
  
  # mean(pKin$Inbr[pKin$Indiv %in% keep])
  return (list(flag, pKin))
}



#=======================================================================================================================
# btn_pedIBDatN_onclick() processes inputs and passes them to optiSel::prePed() function
#-----------------------------------------------------------------------------------------------------------------------
# filter_btn_onclick("Indiv", "Jersey", "1970", "FALSE")

get_sex_validaton <- function(df, df2, breed){
    x = df[df$Breed==breed,]
    y = x[x$Indiv %in% df2$Indiv,]
    sexes = unique(y$Sex)
    if(nrow(y)>0 & all(c("male", "female") %in% sexes))
      return (TRUE)
    else
      return (FALSE)
}


btn_pedIBDatN_onclick <- function(df, refdf, breed, keep_only, keep, ngen){
      # Pedig,
      # Phen, 
      # input$pedKinatN_breed,
      # input$pedKinatN_keeponly,
      # input$pedKinatN_keep,
      # input$pedKinatN_ngen
  
  # breed = ifelse(breed=="NA", NA, breed)
  # if (is.na(breed)){
  #   return (NULL)
  # }
  
  flag = 0
  msg = ""
  
  ngen = ifelse (is.numeric(ngen)> 0, is.numeric(ngen), NA)
  
  if (!get_sex_validaton(df, refdf, breed)){
    flag = 1
    print(paste("Possible missing of females. Aborting. "))
    msg="Possible missing of females. Try other breeds etc."
    return (list(msg, data.frame()))
  }
  
  #pKinatN <- pedIBDatN(df, thisBreed=breed, keep.only=refdf$Indiv)
  
  if (class(keep_only) == "character" & keep_only != "NULL"){
    keep_only_list=as.vector(unlist(refdf[keep_only]))
    keep_only_list = keep_only_list[keep_only_list>""]
    keeplist = keep_only_list
  }else{
    flag = 1
    keep_only_list=NULL
    keeplist = keep_only_list
  }
 # ---- kinship ----
  if (length(keep_only_list) < 1){
    flag = 0
    msg= "No valid animals with selected column."
  }
  
  if (flag)
    return (list(flag, msg, data.frame()))
  else
    pKinatN <- pedIBDatN(
      df,
      thisBreed=breed,
      keep.only=keep_only_list,
      nGen=ngen)

  #print(head(pKinatN))
  return (list(msg, pKinatN))
}


# pKinatN <- pedIBDatN(Pedig, thisBreed="Hinterwaelder", keep.only=Phen$Indiv)
# pKinatN
#pKinatN$of <- pKinatN$Q1/pKinatN$Q2
# pKinatN$of["276000891862786","276000812497659"]



# 
# breeds = c("Hinterwaelder",       "unbek0",              "Vorderwaelder",
# "Fleckvieh",           "Sonstige_Kreuzungen", "Holstein_Rbt"  ,
# "Pedigree Error",      "Holstein_Sbt",        "Limousin",
# "Braunvieh",           "Gelbvieh",            "Jersey",
# "Galloway",            "Blonde_d_Aquitaine" )
# 
# 
# "Hinterwaelder"
# "Vorderwaelder"
# 
# for (b in breeds){
#   print(b)
#   x = Pedig[Pedig$Breed==b,]
#   y = x[x$Indiv %in% Phen$Indiv,]
#   print(nrow(y))
#   print(unique(y$Sex))
#   #pKinatN <- pedIBDatN(Pedig, thisBreed="Vorderwaelder", keep.only=Phen$Indiv)
# }




#=======================================================================================================================
# filter_btn_onclick() processes inputs and passes them to optiSel::prePed() function
#-----------------------------------------------------------------------------------------------------------------------
# filter_btn_onclick("Indiv", "Jersey", "1970", "FALSE")




#=======================================================================================================================
# filter_btn_onclick() processes inputs and passes them to optiSel::prePed() function
#-----------------------------------------------------------------------------------------------------------------------
# filter_btn_onclick("Indiv", "Jersey", "1970", "FALSE")



#=======================================================================================================================
# filter_btn_onclick() processes inputs and passes them to optiSel::prePed() function
#-----------------------------------------------------------------------------------------------------------------------
# filter_btn_onclick("Indiv", "Jersey", "1970", "FALSE")



#=======================================================================================================================
# filter_btn_onclick() processes inputs and passes them to optiSel::prePed() function
#-----------------------------------------------------------------------------------------------------------------------
# filter_btn_onclick("Indiv", "Jersey", "1970", "FALSE")














# # LOAD the folllowing packages
# 
# for (package in c('shiny', 'dplyr', 'datasets', 'shinydashboard', 'ggplot2')){
#   if (!require(package, character.only = T, quietly = T)){
#     install.packages(package, repos="http://cran.us.r-project.org")
#     library(package,character.only = T)
#   }
# }



# Load the following packages.

# shinyjs

for (package in c('shiny',
                  'shinyjs',
                  'shinythemes',
                  'dplyr',
                  'vroom',
                  'tibble',
                  'datasets',
                  'shinydashboard',
                  'ggplot2',
                  'optiSel',
                  'DT',
                  'magrittr',
                  'readr',
                  "openxlsx",
                  'shinyalert',
                  'zeallot')) {
  
  # if above packages do not exist, then download them from the web.
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package,repos="http://cran.us.r-project.org")
    library(package, character.only=T)
  }
}



#uidf = data.frame(c(osctype))

optSelChoice = c("Pedig-OCS","Marker-OCS")

PedOCSdoc = "window.open('https://cran.r-project.org/web/packages/optiSel/vignettes/ped-vignette.html', '_blank')"
MarkerOCSdoc = "window.open('https://cran.r-project.org/web/packages/optiSel/vignettes/seg-vignette.html', '_blank')"
OCSdoc = "window.open('https://cran.r-project.org/web/packages/optiSel/vignettes/ocs-vignette.html', '_blank')"

#rv = browseVignettes('optiSel')
#str(rv)

############################
# data("PedigWithErrors")
# data("Phen")



# raw.ped <- PedigWithErrors
raw.ped <- data.frame()

#born_minmax <- c( min(raw.ped$Born, na.rm=TRUE), max(raw.ped$Born, na.rm=TRUE) )
born_minmax <- c( 1900, 2023 )

#yr_diff= max(raw.ped$Born, na.rm=TRUE) - min(raw.ped$Born, na.rm=TRUE)
yr_diff = born_minmax[2]-born_minmax[1]
born_slider_init_range <- c(
  round(born_minmax[1]+ yr_diff*.85, digits=0),
  round(born_minmax[1]+ yr_diff*.95, digits=0)
)


#born_minmax <- c(1900,2023)
#yr_diff= 0
#born_slider_init_range <- c(born_minmax,0)


#print(born_minmax)
#print(born_slider_init_range)





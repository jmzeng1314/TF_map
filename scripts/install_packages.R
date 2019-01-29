
# Version: 1.30.4	Depends: R (≥ 3.5.0)
# CRAN checks: BiocManager results	Imports: utils
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("Sushi", version = "3.8")

BiocManager::install(c('shiny','shinydashboard','stringr','DT','ggplot2'),ask = F,update = F)
BiocManager::install('shinyBS',ask = F,update = F)
BiocManager::install('shinyAce',ask = F,update = F)
BiocManager::install(c('knitr','rmarkdown','rmarkdown','cowplot','dplyr'),ask = F,update = F)
BiocManager::install('RMySQL',ask = F,update = F) 
BiocManager::install(c('tm','SnowballC','wordcloud','RColorBrewer'),ask = F,update = F)
BiocManager::install(c('remotes','devtools'),ask = F,update = F) 
BiocManager::install('nik01010/dashboardthemes') 
BiocManager::install('ggpubr',ask = F,update = F)
BiocManager::install('shinyjs',ask = F,update = F)

## if the R version is lower than 3.5, one should use the code below:

if(F){
  library(devtools)
  install_github('nik01010/dashboardthemes' )
}



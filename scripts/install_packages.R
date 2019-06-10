if(T){
  suppressPackageStartupMessages(library(shiny))
  suppressPackageStartupMessages(library(shinydashboard))
  suppressPackageStartupMessages(library(stringr))
  suppressPackageStartupMessages(library(DT))
  suppressPackageStartupMessages(library(ggplot2))
  
  suppressPackageStartupMessages(library(shinyBS))
  suppressPackageStartupMessages(library(shinyAce))
  
  suppressPackageStartupMessages(library(knitr))
  suppressPackageStartupMessages(library(rmarkdown))
  suppressPackageStartupMessages(library(shinyjs))
  
  suppressPackageStartupMessages(library(Sushi))  
  suppressPackageStartupMessages(library(RMySQL))
  
  suppressPackageStartupMessages(library(cowplot))
  suppressPackageStartupMessages(library(dplyr)) 
  
  suppressPackageStartupMessages(library(tm))
  suppressPackageStartupMessages(library(SnowballC))
  suppressPackageStartupMessages(library(wordcloud))
  suppressPackageStartupMessages(library(RColorBrewer))
  suppressPackageStartupMessages(library(ggpubr))
}


#  R version 3.5.3 (2019-03-11) -- "Great Truth"
#  Bioconductor version 3.8 (BiocManager 1.30.4), ?BiocManager::install 
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



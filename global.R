suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(shinyBS))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(shinyAce))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(rmarkdown))
suppressPackageStartupMessages(library(RMySQL))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(Sushi))
#suppressPackageStartupMessages(library(plotly))


suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(cowplot))
suppressPackageStartupMessages(library(dplyr))


suppressPackageStartupMessages(library(tm))
suppressPackageStartupMessages(library(SnowballC))
suppressPackageStartupMessages(library(wordcloud))
suppressPackageStartupMessages(library(RColorBrewer))



createLink <- function(base,val) {
  sprintf('<a href="%s" class="btn btn-link" target="_blank" >%s</a>',base,val) ##target="_blank" 
}

log_cat <- function(info='hello world~',file='log.txt'){
  cat(as.character(Sys.time()),info ,"\n",file=file,append=TRUE)
}

mysql_getData <- function(sql="select * from cistrome_metadata limit 10;"){
  host <<- "127.0.0.1"
  port <<- 3306
  user <<- "root" 
  password <<- ifelse(.Platform$OS.type == 'unix','ganglijimmy','11111111')
  library(RMySQL)
  con <- dbConnect(MySQL(), host=host, port=port, user=user, password=password) 
  dbSendQuery(con, "USE TF_map") 
  dat=dbGetQuery(con,sql ) 
  dbDisconnect(con)
  cat(as.character(Sys.time()),sql, file=stderr());cat("\n",file=stderr())
  return(dat)
}


tmp2bed <- function(tmp){
  #tmp="chr1:523613,523636\nchr2:562316,562335"
  tmp=strsplit(tmp,'\n')[[1]]
  n_tmp=length(tmp)
  if( grepl('[:,-]',tmp[1])){
    tmp <- lapply(tmp, function(x){ 
      return( strsplit(x,'[:,-]')[[1]] )
    })
  }else{
    tmp <- lapply(tmp, function(x){ 
      return( strsplit(x,'\\s+')[[1]] )
    })
  }
  
  tmp<- do.call(rbind.data.frame, tmp)
  colnames(tmp)[1:3]=c('chrom','start','end')
  tmp=tmp[,1:3]
  tmp$chrom=as.character(tmp$chrom)
  tmp$start=as.numeric(as.character(tmp$start))
  tmp$end=as.numeric(as.character(tmp$end))
  return(tmp)
}

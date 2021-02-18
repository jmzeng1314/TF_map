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

createLink <- function(base,val) {
  sprintf('<a href="%s" class="btn btn-link" target="_blank" >%s</a>',base,val) ##target="_blank"
}

log_cat <- function(info='hello world~',file='log.txt'){
  if(!file.exists(file)){file.create(file)}
  cat(as.character(Sys.time()),info ,"\n",file=file,append=TRUE)
}

mysql_getData <- function(sql="select * from cistrome_metadata limit 10;"){
  source(".KEY.r")
  host <<- getOption("database_host")
  port <<- getOption("database_port")
  user <<- getOption("database_userid")
  password <<-  getOption("database_password")
  library(RMySQL)
  con <- dbConnect(MySQL(), host=host, port=port, user=user, password=password)
  dbSendQuery(con, "USE tfmapperdb")
  dat=dbGetQuery(con,sql )
  dbDisconnect(con)
  cat(as.character(Sys.time()),sql, file=stderr());cat("\n",file=stderr())
  return(dat)
}


text2bed <- function(text){
  # text="chr1:523613,523636\nchr2:562316,562335"
  text=strsplit(text,'\n')[[1]]
  n_text=length(text)
  if( grepl('[:,-]',text[1])){
    text <- lapply(text, function(x){
      return( strsplit(x,'[:,-]')[[1]] )
    })
  }else{
    text <- lapply(text, function(x){
      return( strsplit(x,'\\s+')[[1]] )
    })
  }
  
  text<- do.call(rbind.data.frame, text)
  colnames(text)[1:3]=c('chrom','start','end')
  text=text[,1:3]
  text$chrom=gsub('chr','',as.character(text$chrom))
  text$start=as.numeric(as.character(text$start))
  text$end=as.numeric(as.character(text$end))
  bed=text
  return(bed)
}





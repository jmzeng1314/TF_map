library(shiny)
library(shinydashboard) 
library(dplyr)
library(tidyr)
library(stringr) 
library(DT)    
library(shinyBS)
library(GGally)
library(ggplot2)
library(shinyAce)
library(knitr)
library(rmarkdown) 
library(shinyjs)
## input values : species/database/input_gene/cellline/genomic_feature

createLink <- function(base,val) {
  sprintf('<a href="%s" class="btn btn-link" target="_blank" >%s</a>',base,val) ##target="_blank" 
}

host <<- "127.0.0.1"
port <<- 3306
user <<- "root"
if( .Platform$OS.type == 'unix')
password <<- ifelse(.Platform$OS.type == 'unix','ganglijimmy','11111111')

 

dbDisconnect(con)

shinyServer(
  function(input,output,session){
    
    glob_values <- reactiveValues(
      results=NULL, ## the search results based on gene,species,db,ip,cellline
      input_gene=NULL,
      species=NULL,
      database=NULL,
      IP=NULL,
      cellline=NULL,
	  input_gene_choices=NULL, ## depends on species!
      tmp=NULL
    )
    
    reactiveValues.reset <-function(){
      glob_values$results=NULL
      glob_values$input_gene=NULL
	  glob_values$input_gene_choices=NULL
      glob_values$tmp=NULL
	  
    }
	
	
	## the gene choices  depends on the species user choosed
    observe({
    x <- input$species

    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character(0)

	db=paste(x,'gene_mapping',sep='_')
	library(RMySQL)
	con <- dbConnect(MySQL(), host=host, port=port, user=user, password=password) 
	dbSendQuery(con, "USE TF_map") 
	gene_mapping=dbGetQuery(con,paste0("select symbols,geneNames  from ",db)) 
	dbDisconnect(con)

  
    # Can also set the label and select items
    updateSelectizeInput(
      session, inputId='input_gene', label = "Type a gene name", server = TRUE,
      choices =  data.frame(label = gene_mapping$symbols, value = gene_mapping$symbols, name = gene_mapping$geneNames),
      options = list(
        #create = TRUE, persist = FALSE,
        render = I(
          "{
          option: function(item, escape) {
          return '<div> <strong>' + item.label + '</strong> - ' +
          escape(item.name) + '</div>';
          }
          }"
        ))## end for options
      
        )## end for updateSelectizeInput 
	
  }) ## end for observe 
    
   
 
	## the cellLine choices  depends on the database user choosed
    observe({
    x <- input$database

    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character(0)

	db=paste(x,'metadata',sep='_')
	library(RMySQL)
	if(x='cistrome'){
	  cellline_info=dbGetQuery(con,paste0("select cellline,tissue,organ  from ",db))
	}else{
	  cellline_info=dbGetQuery(con,paste0("select cellline,celltype,tissue  from ",db))
	}

	cellline_info=unique(cellline_info)
	cellline_info=rbind(c("ALL","ALL","ALL"),cellline_info)
	dbDisconnect(con)

  
    # Can also set the label and select items
	
    updateSelectizeInput(
      session, inputId='cellline', label = "Choose a cell line", server = TRUE,
      choices =  data.frame(label = cellline_info$cellline, value = cellline_info$cellline, name = cellline_info$tissue),
      options = list(
        #create = TRUE, persist = FALSE,
        render = I(
          "{
          option: function(item, escape) {
          return '<div> <strong>' + item.label + '</strong> - ' +
          escape(item.name) + '</div>';
          }
          }"
        ))## end for options
      
    )## end for updateSelectizeInput 
	
  }) ## end for observe 
  
  

    

    
    
    observeEvent(input$do, {
      glob_values$input_gene=input$input_gene
      glob_values$species=input$species
      glob_values$database=input$database
      glob_values$IP=input$IP
      glob_values$cellline=input$cellline
      
	  
	  
      gene=input$input_gene
      cellline=input$cellline
      genomic_feature=input$genomic_feature
      
      
      library(RMySQL)
      con <- dbConnect(MySQL(), host=host, port=port, user=user, password=password)
      dbSendQuery(con, "USE TF_map")
      
      dbListTables(con)
      
      
      ## Firstly get the peaks:
      
      if(genomic_feature != 'ALL'){
        peaks_tab=paste(genomic_feature, glob_values$species,glob_values$IP,glob_values$database,sep="_")
        sql=paste0(" select * from ",peaks_tab," where symbol=",shQuote(gene))
        peaks_tb <- dbGetQuery(con,sql)
        peaks_tb$genomic_feature=genomic_feature
        
      }else{
        
        peaks_tb=c()
        for(genomic_feature in c('promoter_tss','tts','utr_5','utr_3','intron','exon','intergenic','non_coding') ){
          peaks_tab=paste(genomic_feature, glob_values$species,glob_values$IP,glob_values$database,sep="_")
          sql=paste0(" select * from ",peaks_tab," where symbol=",shQuote(gene))
          tmp <- dbGetQuery(con,sql)
          if(nrow(tmp)>0){
            tmp$genomic_feature=genomic_feature
            peaks_tb=rbind(peaks_tb,tmp)
          }
          
          
        }
      }
      #peaks_tb=dbGetQuery(con," select * from promoter_tss_human_tf_cistrome where symbol='EZH2' ")
      if(nrow(peaks_tb) >0){
        attribute <- strsplit(peaks_tb$attribute,';')[[1]]
        tmp=as.data.frame(str_split_fixed(attribute, ":", 5),stringsAsFactors=F)
        colnames(tmp)=c('sampleID','dis_start','width','score','dis_tss')
        tmp$sampleID=as.numeric(lapply(tmp$sampleID,function(x){strsplit(x,"_")[[1]][1]}))
        peaks_tb=tmp
        
        search_gene_info=dbGetQuery(con,paste0("select * from hg38_position where symbol=",shQuote(gene)))[1,]
        peaks_tb$chrom=search_gene_info[1,1]
        peaks_tb$start=as.numeric(peaks_tb$dis_start)+as.numeric(search_gene_info[1,2])
        peaks_tb$end=as.numeric(peaks_tb$start)+as.numeric(peaks_tb$width) 
      }

      
      # Then get the meta information:
  
      metadata_tab='cistrome_metadata'
      if(cellline != 'ALL'){
        sql=paste0("select * from ",metadata_tab," where cellline = ",shQuote(cellline))
        metadata <- dbGetQuery(con,sql)
      }else{
        sql=paste0("select * from ",metadata_tab )
        metadata <- dbGetQuery(con,sql)
      }

      dbDisconnect(con)

      ## can't find peak for some genomic features

      if(nrow(peaks_tb) >0 && nrow(metadata)>0 ){
        tmp=merge(peaks_tb,metadata,by='sampleID')
        ## It counld be NULL
        if(nrow(tmp)>0){ 
          glob_values$results=tmp
        }else{
          glob_values$results=NULL
        }

      }else{

        glob_values$results=NULL
      }


     

    })
    output$cellline <- DT::renderDataTable( {
      cellline_info
    })

    output$results <- DT::renderDataTable( {
      tmp1=glob_values$results
      
      # https://www.encodeproject.org/experiments/ENCSR541AOQ/
      # https://www.encodeproject.org/files/ENCFF132KNA/
      # https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSM530178
      
      if(! is.null(tmp1)){
        if(glob_values$database == 'cistrome'){
          ## if choose the cistrome database:
          
          tmp1$GSM=createLink(paste0("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=",tmp1$GSM),tmp1$GSM)
          ##   http://epigenomegateway.wustl.edu/browser/?genome=hg38&
          ##  datahub=http://dc2.cistrome.org/api/datahub/9216&gftk=refGene,full&coordinate=chr6:26099558-26299988
          genome='hg38'
          
          WashU_link=createLink(paste0("http://epigenomegateway.wustl.edu/browser/?genome=",genome,
                                       "&datahub=http://dc2.cistrome.org/api/datahub/",
                                       tmp1$sampleID,"&gftk=refGene,full&coordinate=",
                                       tmp1$chrom,":",tmp1$start,"-",tmp1$end
          )
          
          ,'WashU')
          UCSC_link=createLink(paste0( "http://dc2.cistrome.org/api/hgtext/", tmp1$sampleID,
                                       "/?db=",genome,
                                       "&position=",  tmp1$chrom,":",tmp1$start,"-",tmp1$end
          )
          
          ,'UCSC')
          tmp1$Visualize=paste(WashU_link,UCSC_link)
          tmp1<-tmp1[,c(9,6:8,1,10:13,16)]
          return(tmp1)
          
        }else{
          ## if choose the encode database:
          return(NULL)
        }
       
        
      }else{
        return(NULL)
      }

      
      
      
    }
    , options = list(
      dom = 'Bfrtip', 
      buttons = c('copy', 'excel', 'pdf', 'print', 'colvis'),
      lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
      pageLength = -1
    ),
    extensions = 'Buttons',
    filter = 'top',
    escape = FALSE)## end for results;
    
})

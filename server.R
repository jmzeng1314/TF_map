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


## input values : species/database/input_gene/cellline/genomic_feature

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

all_human_gene<<- mysql_getData("select distinct V7,gene_name from human_N_MRP_gene_info ;")
cat(as.character(Sys.time()),"Search all of the human gene \n",file=stderr())
cat(dim(all_human_gene), file=stderr());cat("\n",file=stderr())
all_mouse_gene<<- mysql_getData("select distinct V7,gene_name from mouse_N_MRP_gene_info ;")
cat(as.character(Sys.time()),"Search all of the mouse gene \n",file=stderr())
cat(dim(all_mouse_gene), file=stderr());cat("\n",file=stderr())

colnames(all_human_gene)=c('symbols','geneNames')
colnames(all_mouse_gene)=c('symbols','geneNames')

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



shinyServer(
  function(input,output,session){

    host <<- "127.0.0.1"
    port <<- 3306
    user <<- "root" 
    password <<- ifelse(.Platform$OS.type == 'unix','ganglijimmy','11111111')
    
    IP <- reactive({ 
      tmp<-input$getIP 
      as.character(tmp$ip)
      })
    
    glob_values <- reactiveValues(
      results=NULL, ## the search results based on gene,species,db,ip,cellline
      input_gene=NULL,
      species=NULL,
      sushi_dat=NULL, ## depends on peaks results
      database=NULL,
      IP=NULL,
      cellline=NULL,
      input_gene_choices=NULL, ## depends on species!
      chromosome=NULL,
      bed_filename=NULL,
      gene_position=NULL,
      tmp=NULL
    )
    
    reactiveValues.reset <-function(){
      glob_values$results=NULL
      glob_values$input_gene=NULL
      glob_values$input_gene_choices=NULL
      glob_values$tmp=NULL
      glob_values$sushi_dat=NULL
      glob_values$chromosome=NULL
      glob_values$bed_filename=NULL
      glob_values$gene_position=NULL
    }
    
    
    ## the gene choices  depends on the species user choosed
    observe({
      x <- as.character(input$species)
      cat(x, file=stderr());cat("\n",file=stderr())
      
      if(x=='human'){
        gene_mapping <<- all_human_gene
      }else{
        gene_mapping <<-  all_mouse_gene
      }
      #gene_mapping <<- ifelse( x=='human',all_human_gene,all_mouse_gene) 
      cat(dim(gene_mapping), file=stderr());cat("\n",file=stderr())
      
      # Can also set the label and select items
      updateSelectizeInput(
        session, inputId='input_gene', label = "Please type a gene name (HUGO symbol)", server = TRUE, 
        choices =  data.frame(label = gene_mapping$symbols, value = gene_mapping$symbols, name = gene_mapping$geneNames),
        options = list(
          
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
    
    observe({
      shinyjs::toggleState("do", !input$cellline ==""  && !input$input_gene =="")
    })
    
    observe({
      shinyjs::toggleState("do_position", !input$cellline ==""  && !input$position =="")
    })
    
    ## the cellLine choices  depends on the database and species and IP  user choosed
    observe({
      x <- input$database
      
      # Can use character(0) to remove all choices
      if (is.null(x))
        x <- character(0)
      
      db=paste(x,'metadata',sep='_')
      sql=paste0("select distinct bs1,bs2,bs3  from ",db,
                    " where species = ",shQuote(input$species)," and type=",shQuote(input$IP)
      )
        
      library(RMySQL) 
      cellline_info=mysql_getData( sql ) 
      cellline_info=unique( apply(cellline_info,1,function(x) paste(x,collapse = ';')))
      cellline_info=c( "ALL" ,cellline_info)
      updateSelectizeInput(
        session, inputId='cellline', label = "Biological source", server = TRUE, 
        choices = cellline_info 
          )## end for updateSelectizeInput 
      
      }) ## end for observe 
    
    get_gene_position = reactive({
      input_gene = input$input_gene 
      if(nchar(input_gene)>1){
        genome=ifelse(input$species=='human','hg38','mm10')
        chrom_db=paste0('CCDS_current_',genome)
        chrom_sql=paste0('select chromosome,cds_from,cds_to  from ',chrom_db,' where gene=',shQuote( input_gene))
        tmp_gene_info=mysql_getData(chrom_sql)
        gene_position=paste0(tmp_gene_info[1,1],":",tmp_gene_info[1,2],"-",tmp_gene_info[1,3])
        gene_position
      }
      
    })
    observe({
      updateTextInput(session, 'position', label = 'Please input the coordinates', value = get_gene_position())
    })
    
    observeEvent(input$zoom_in, {
      tmp=tmp2bed(input$position)[1,]
      chrom=tmp[1]
      start=as.numeric(tmp[2])
      end=as.numeric(tmp[3])
      if((end-start)>2000){
        start=start+1000
        end=end-1000
      }
      tmp=paste0(chrom,":",start,",",end)
      updateTextInput(session, 'position', label = 'Please input a position', value = tmp)
    })
    observeEvent(input$zoom_out, {
      tmp=tmp2bed(input$position)[1,]
      chrom=tmp[1]
      start=as.numeric(tmp[2])
      end=as.numeric(tmp[3])
      start=start-1000
      end=end+1000
      tmp=paste0(chrom,":",start,",",end)
      updateTextInput(session, 'position', label = 'Please input a position', value = tmp)
    })
    
    observeEvent(input$do, {
      reactiveValues.reset()
      glob_values$results=NULL
      glob_values$input_gene=input$input_gene
      glob_values$species=input$species
      glob_values$database=input$database
      glob_values$IP=input$IP
      glob_values$cellline=input$cellline
      
      
      ## firstly search the exactly chromosome for the input_gene .
      genome=ifelse(glob_values$species=='human','hg38','mm10')
      chrom_db=paste0('CCDS_current_',genome)
      chrom_sql=paste0('select chromosome,cds_from,cds_to  from ',chrom_db,' where gene=',shQuote(glob_values$input_gene))
      tmp_gene_info=mysql_getData(chrom_sql)
      glob_values$chromosome=tmp_gene_info[1,1]
      glob_values$gene_position=paste0(tmp_gene_info[1,1],":",tmp_gene_info[1,2],"-",tmp_gene_info[1,3])
      
      log_cat(paste0(IP(),"    ",chrom_sql))
      
      gene=input$input_gene
      cellline=input$cellline
      genomic_feature=input$genomic_feature
       
      
      ## secendly meta information 
      
      metadata_tab=paste0(input$database,'_metadata')
      cat( cellline , file=stderr());cat("\n",file=stderr())
      if(cellline != 'ALL'){
        tmp=strsplit(cellline,';')[[1]]
        
        meta_sql <<- paste0("select * from ",metadata_tab," where bs1 = ",shQuote(tmp[1])," and bs2 = ",shQuote(tmp[2])
                            ," and bs3 = ",shQuote(tmp[3]),
                   " and type=",shQuote(input$IP) ," and species=",shQuote(input$species) 
        )
       
      }else{
        meta_sql  <<-  paste0("select * from ",metadata_tab," where species = ",shQuote(input$species),
                              " and type=",shQuote(input$IP) )
      } 
      metadata <- mysql_getData(meta_sql)
      log_cat(paste0(IP(),"    ",meta_sql))
      
      if(input$database=='cistrome'){
        gsm_sql=paste0("select title,source_name_ch1,GSM  from  cistrome_GSM_metadata ")
        gsm_data <- mysql_getData(gsm_sql)
        log_cat(paste0(IP(),"    ",gsm_sql))
        metadata<-merge(metadata,gsm_data,by='GSM')
      }
      
      ## lastly get the peaks table:
      chromosome_db=''
      if( ! glob_values$chromosome %in%  1:22 ){
         chromosome_db ='chr_other'
      }else{
        chromosome_db=paste0('chr',glob_values$chromosome)
      }
      glob_values$chromosome=paste0('chr',glob_values$chromosome)
      
      peaks_tab=paste(glob_values$database,glob_values$species,glob_values$IP,chromosome_db,sep="_")
      
      if(genomic_feature != 'ALL'){
        peaks_sql <<- paste0(" select * from ",peaks_tab," where symbol=",shQuote(gene),' and feature_type=',genomic_feature)
       
        
      }else{
        peaks_sql <<- paste0(" select * from ",peaks_tab," where symbol=",shQuote(gene) ) 
      } 
      peaks_tb <- mysql_getData(peaks_sql) 
      log_cat(paste0(IP(),"    ",peaks_sql) )
      
      if(nrow(peaks_tb)>0)
          peaks_tb$chrom=as.character(glob_values$chromosome)
 
      

      ## can't find peak for some genomic features
      if(nrow(peaks_tb) >0 ){
        if(nrow(metadata)>0 ){
          tmp=merge(peaks_tb,metadata,by='sampleID')
          ## It counld be NULL
          if(nrow(tmp)>0){ 
            closeAlert(session, "exampleAlert")
            tmp$dis=as.numeric(tmp$dis)
            tmp=tmp[order(tmp$dis),]
            glob_values$results=tmp
            
            tmp1=glob_values$results
            tmp1=tmp1[,c('chrom'	,'start'	,'end'	,'IP'	,'score')]
            tmp1$strand='+'
            glob_values$bed_filename <- paste(glob_values$input_gene ,glob_values$cellline,glob_values$genomic_feature,
                                              'data', Sys.Date(), 'peaks.bed', sep='-')
            FH=file.path('/var/www/html/igv', glob_values$bed_filename)
            write.table( tmp1 ,FH,sep = '\t',quote = F,row.names = F,col.names = F)
            
            
          }else{
            createAlert(session, "alert_search_results_anchorId", "exampleAlert", title = "Oops",
                        content = " very strange, cell-line didn't match~~~", append = FALSE)
            glob_values$results=NULL
          }
          
        }else{
          createAlert(session, "alert_search_results_anchorId", "exampleAlert", title = "Oops",
                      content = " It should happen", append = FALSE)
          glob_values$results=NULL
        }
        
      }else{
        createAlert(session, "alert_search_results_anchorId", "exampleAlert", title = "Oops",
                    content = " we can't find results for this gene", append = FALSE)
        glob_values$results=NULL
      }
    
      
      
    }) ## end for observe searching!!!
    
    observeEvent(input$do_position, {
      reactiveValues.reset()
      
      glob_values$input_gene=input$input_gene
      glob_values$species=input$species
      glob_values$database=input$database
      glob_values$IP=input$IP
      glob_values$cellline=input$cellline
      glob_values$gene_position=input$position
      tmp=tmp2bed(input$position)[1,]
      chrom=tmp[1]
      start=as.numeric(tmp[2])
      end=as.numeric(tmp[3])
      glob_values$chromosome=chrom
      
      
      ## firstly get peaks table
      chromosome_db=''
      if( ! glob_values$chromosome %in%  1:22 ){
        chromosome_db ='chr_other'
      }else{
        chromosome_db=paste0('chr',glob_values$chromosome)
      }
      glob_values$chromosome=paste0('chr',glob_values$chromosome)
      
      peaks_tab=paste(glob_values$database,glob_values$species,glob_values$IP,chromosome_db,sep="_")
       
      peaks_sql <<- paste0("select * from ",peaks_tab," where start > ",start," and end < ",end)
  
      peaks_tb <- mysql_getData(peaks_sql) 
      log_cat(paste0(IP(),"    ",peaks_sql) )
      if(nrow(peaks_tb)>0)
         peaks_tb$chrom=as.character(glob_values$chromosome)
      
      ## secendly meta information 
      
      metadata_tab=paste0(input$database,'_metadata')
      cellline=input$cellline
      
      if(cellline != 'ALL'){
        tmp=strsplit(cellline,';')[[1]]
        
        meta_sql <<- paste0("select * from ",metadata_tab," where bs1 = ",shQuote(tmp[1])," and bs2 = ",shQuote(tmp[2])," and bs3 = ",
                            shQuote(tmp[3]),
                            " and type=",shQuote(input$IP) ," and species=",shQuote(input$species) 
        )
        
      }else{
        meta_sql  <<-  paste0("select * from ",metadata_tab," where species = ",shQuote(input$species),
                              " and type=",shQuote(input$IP) )
      } 
      metadata <- mysql_getData(meta_sql)
      log_cat(paste0(IP(),"    ",meta_sql))
      
      
      if(input$database=='cistrome'){
        gsm_sql=paste0("select title,source_name_ch1,GSM  from  cistrome_GSM_metadata ")
        gsm_data <- mysql_getData(gsm_sql)
        log_cat(paste0(IP(),"    ",gsm_sql))
        metadata<-merge(metadata,gsm_data,by='GSM')
      }
      
      ## lastly merge the peaks_table and metadata information
      
      ## can't find peak for some genomic features
      if(nrow(peaks_tb) >0 ){
        if(nrow(metadata)>0 ){
          tmp=merge(peaks_tb,metadata,by='sampleID')
          ## It counld be NULL
          if(nrow(tmp)>0){ 
            closeAlert(session, "exampleAlert")
            tmp$dis=as.numeric(tmp$dis)
            tmp=tmp[order(tmp$dis),]
            glob_values$results=tmp
            
            tmp1=glob_values$results
            tmp1=tmp1[,c('chrom'	,'start'	,'end'	,'IP'	,'score')]
            tmp1$strand='+'
            glob_values$bed_filename <- paste(glob_values$input_gene ,glob_values$cellline,glob_values$genomic_feature,
                                              'data', Sys.Date(), 'peaks.bed', sep='-')
            FH=file.path('/var/www/html/igv', glob_values$bed_filename)
            write.table( tmp1 ,FH,sep = '\t',quote = F,row.names = F,col.names = F)
            
            
          }else{
            createAlert(session, "alert_search_results_anchorId", "exampleAlert", title = "Oops",
                        content = " very strange, cell-line didn't match~~~", append = FALSE)
            glob_values$results=NULL
          }
          
        }else{
          createAlert(session, "alert_search_results_anchorId", "exampleAlert", title = "Oops",
                      content = " May be there's no cell line choosed!", append = FALSE)
          glob_values$results=NULL
        }
        
      }else{
        createAlert(session, "alert_search_results_anchorId", "exampleAlert", title = "Oops",
                    content = "There's no peak for this parameters", append = FALSE)
        glob_values$results=NULL
      }
      
      
      
    })
    
    output$results <- DT::renderDataTable( {
      tmp1=glob_values$results
      cat(as.character(Sys.time()),"Check the peaks table column!\n",file=stderr())
      cat(colnames(tmp1),'\n\n', file=stderr());
      log_cat(paste0(IP(),"    Dim for the results table:",nrow(tmp1),"\t",ncol(tmp1),'\n'))
      
      # https://www.encodeproject.org/experiments/ENCSR541AOQ/
      # https://www.encodeproject.org/files/ENCFF132KNA/
      # https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSM530178
      
      if(! is.null(tmp1)){
        if(glob_values$database == 'cistrome'){
          ## if choose the cistrome database:
          
          tmp1$GSM=createLink(paste0("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=",tmp1$GSM),tmp1$GSM)
          ##   http://epigenomegateway.wustl.edu/browser/?genome=hg38&
          ##  datahub=http://dc2.cistrome.org/api/datahub/9216&gftk=refGene,full&coordinate=chr6:26099558-26299988
          genome=ifelse(glob_values$species=='human','hg38','mm10')
          
          WashU_link=createLink(paste0("http://epigenomegateway.wustl.edu/browser/?genome=",genome,
                                       "&datahub=http://dc2.cistrome.org/api/datahub/",
                                       tmp1$sampleID,"&gftk=refGene,full&coordinate=",
                                       tmp1$chrom,":",tmp1$start-1000,"-",tmp1$end+1000
          )
          
          ,'WashU')
          UCSC_link=createLink(paste0( "http://dc2.cistrome.org/api/hgtext/", tmp1$sampleID,
                                       "/?db=",genome,
                                       "&position=",  tmp1$chrom,":",tmp1$start-1000,"-",tmp1$end+1000
          )
          
          ,'UCSC')
          tmp1$Visualize=paste(WashU_link,UCSC_link)
          tmp1$sequence=createLink(paste0(
            "http://genome.ucsc.edu/cgi-bin/das/",genome,"/dna?segment=",
            paste0(tmp1$chrom,':',tmp1$start,',',tmp1$end)
          )
          , paste0(tmp1$chrom,':',tmp1$start,',',tmp1$end) )
         
          #tmp1<-tmp1[, c('GSM','IP','Visualize','sequence','dis','score','attri','bs1','bs2','bs3')]
          tmp1<-tmp1[, c('GSM','IP','Visualize','sequence','dis','score','attri','title','source_name_ch1')]
          names(tmp1)=c('GSM','IP','Visualization','Sequence','Distance','Score','Attribute','Title','Source Name')
          return(tmp1)
          
        }else{
          ## if choose the encode database:
          #return(NULL)
          genome=ifelse(glob_values$species=='human','hg38','mm10')
          
          tmp1$Visualize=createLink(paste0( "http://genome.ucsc.edu/cgi-bin/hgTracks?hubClear=https://www.encodeproject.org/", 
                                                tmp1$uniqID,
                                                "/@@hub/hub.txt&db=",genome,
                                             "&position=",  tmp1$chrom,":",tmp1$start-1000,"-",tmp1$end+1000
          )
          ,'go to UCSC')
          tmp1$sampleID=createLink(paste0("https://www.encodeproject.org/files/",tmp1$sampleID),tmp1$sampleID)
          tmp1$uniqID=createLink(paste0("https://www.encodeproject.org/experiments/",tmp1$uniqID),tmp1$uniqID)
      
          tmp1$sequence=createLink(paste0(
            "http://genome.ucsc.edu/cgi-bin/das/",genome,"/dna?segment=",
            paste0(tmp1$chrom,':',tmp1$start,',',tmp1$end)
          )
          , paste0(tmp1$chrom,':',tmp1$start,',',tmp1$end) )
          tmp1<-tmp1[, c('uniqID','sampleID','IP','Visualize','sequence','dis','score','attri','bs1','bs2','bs3')]
          names(tmp1)=c('UniqID','SampleID','IP','Visualize','Sequence','Distance','Score','Attribute','Bs1','Bs2','Bs3')
          return(tmp1)
         
        }
        
        
      }else{
        return(NULL)
      }
      
      
      
      
    }
    , rownames= FALSE,extensions = c('Scroller'), options = list(  
      pageLength = 50, 
      lengthMenu = list(c(10, 50, 100,-1), c('10', '50','100', 'All')),
     
      scrollX = TRUE,
      fixedHeader = TRUE,
      fixedColumns = TRUE ,
      deferRender = TRUE 
    ),
    filter = 'top',
    escape = FALSE
    )
    
    output$results_stat <- renderPlot({
      tmp1=glob_values$results
      if(! is.null(tmp1)){
        tmp=unique(tmp1[,c('sampleID','IP')])
        tmp=sort( table( tmp$IP ),decreasing = T)
        #barplot(tmp,las=2)
        library(ggplot2)
        dat=data.frame(IP=names(tmp),number= as.numeric(tmp),stringsAsFactors = F)
        dat$IP <- factor( dat$IP,  levels=dat$IP )
        
       p <-  ggplot(dat, aes(x = IP, y = number , label = number,theme_set(theme_bw()) )) +
          geom_bar(stat = "identity",fill='steelblue',color='red') +
          geom_text(size = 6,color='white', position = position_stack(vjust = 0.5))+
           theme_set(theme_set(theme_bw(base_size=20)))+
           theme(text=element_text(face='bold'),
                  axis.text.x=element_text(angle=30,hjust=1,size =15),
                  plot.title = element_text(hjust = 0.5) ,
                  panel.grid = element_blank() 
        )
        
       #ggplotly(p) 
       p
      }
    })
    
    output$washUlink <- renderUI({
      
      tmp1=glob_values$results 
      ## http://dc2.cistrome.org/api/batchview/h/2816_3584_4608_8960_33792/w/

      if(! is.null(tmp1)){
        if(glob_values$database == 'cistrome'){
          ## if choose the cistrome database:
          s = input$results_rows_selected
          if (length(s)) {
            tmp1=tmp1[s, , drop = FALSE]
            genome=ifelse(glob_values$species=='human','h','m')
            
            url=paste0("http://dc2.cistrome.org/api/batchview/",genome,"/",
                       paste(unique(as.character(tmp1$sampleID)),collapse  = '_'),
                       "/w/")
            cat(as.character(Sys.time()),"Check the URL:",url,'\n',file=stderr())
            log_cat(paste0(IP(),"    Check the URL:",url,'\n'))
            return(a('WashU Epigenome Browser',href=url,target="_blank"))
            
          }
          
          
        }
      } 
      
     
      
      
    })
    
    
    output$downloadData_csv <- downloadHandler(
      filename = function() {
        paste(glob_values$species ,glob_values$IP,glob_values$database,
              glob_values$input_gene ,glob_values$cellline,glob_values$genomic_feature,
              'data', Sys.Date(), 'peaks.csv', sep='-')
      },
      content = function(con) {
        tmp1=glob_values$results
        write.csv( tmp1 , con)
      }
    ) 
    
    
    output$downloadData_bed <- downloadHandler(
      filename = function() {
        paste(glob_values$species ,glob_values$IP,glob_values$database,
              glob_values$input_gene ,glob_values$cellline,glob_values$genomic_feature,
              'data', Sys.Date(), 'peaks.bed', sep='-')
      },
      content = function(con) {
        tmp1=glob_values$results
        tmp1=tmp1[,c('chrom'	,'start'	,'end'	,'IP'	,'score')]
        tmp1$strand='+'
        write.table( tmp1 , con,sep = '\t',quote = F,row.names = F,col.names = F)
      }
    ) 
    

    
    
## for visulization :    
if(F){
  output$chooseIP_checkbox <- renderUI({
    tmp1=glob_values$results
    if(! is.null(tmp1)){ 
      currentIPs=sort(unique(tmp1$IP))
      checkboxGroupInput("choosed_IPs","Choose IP(s):",currentIPs,inline = T) 
    }else{
      return(NULL)
    }
    
  })
  
  observe({
    tmp1=glob_values$results
    if(! is.null(tmp1)){ 
      currentIPs=sort(unique(tmp1$IP))
      if(input$selectALL_button == 0) return(NULL)
      else if (input$selectALL_button%%2 == 0)
      {
        updateCheckboxGroupInput(session,"choosed_IPs","Choose IP(s):",choices=currentIPs,inline = T)     
      }
      else
      {
        updateCheckboxGroupInput(session,"choosed_IPs","Choose IP(s):",choices=currentIPs,selected=currentIPs,inline = T)
      }
    }else{
      return(NULL)
    }
  })
  
  observeEvent(input$drawSushi,{
    tmp1=glob_values$results
    if(! is.null(tmp1)){ 
      if( ! is.null(input$choosed_IPs)){
        a=tmp1[ tmp1$IP %in% input$choosed_IPs,]
        
        dat=a[,c('chrom' ,'start','end')]
        dat$name=paste(a$cellline,a$IP,a$GSM,sep = '_')
        dat$score=0
        dat$strand='.'
        dat= dat[order(dat$name),]
        dat$row=as.numeric(factor(dat$name))
      }else{ ## if there's no IPs
        return(NULL)
      }
    }else{ ## if there's no peaks
      return(NULL)
    }
    glob_values$sushi_dat =dat
  })
  
  
  output$sushi_peaks <-renderPlot( {
    
    dat=glob_values$sushi_dat 
    if( ! is.null( dat )){
      
      chrom=dat$chrom[1] 
      chromstart=min(c(dat$start,dat$end))-500
      chromend=max(c(dat$start,dat$end))+500
      par(mar=c(5,15,5,5))
      plotBed(beddata    = dat,chrom = chrom,
              chromstart = chromstart,chromend =chromend,
              rownumber  = dat$row, type = "region",
              color=dat$color,row="given",
              plotbg="grey95",rowlabels=unique(dat$name),
              rowlabelcol=unique(dat$color),rowlabelcex=0.75)
      labelgenome(chrom,chromstart,chromend,n=3,scale="Kb")
      mtext("ChIP-seq",side=3, adj=-0.065,line=0.5,font=2)
      
    }else{ ## if there's no IPs
      return(NULL)
    } 
  },
  height = function() {
    #session$clientData$output_plot1_width 
    400+10*nrow(glob_values$sushi_dat) }
  
  )  ## end for sushi_peaks
  
  
}

    
    
    ## page for statistics
    
    output$stat_table <-  DT::renderDataTable({
      metadata_tab=paste0(input$stat_database,'_metadata')
      sql=paste0("select * from ",metadata_tab," where species=",shQuote(input$stat_species)," and type=",shQuote(input$stat_IP))
      dat <- mysql_getData(sql)
      if(input$stat_database == 'cistrome'){
        dat=dat[,-ncol(dat)];dat=dat[,-ncol(dat)];
        
        dat$GSM=createLink(paste0("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=",dat$GSM),dat$GSM )
        dat$sampleID=createLink(paste0("http://epigenomegateway.wustl.edu/browser/?genome=",'hg38',
                                       "&datahub=http://dc2.cistrome.org/api/datahub/",
                                       dat$sampleID,"&gftk=refGene,full " 
        )
        ,dat$sampleID) 
        colnames(dat)=c('Sample ID','GSM ID','Biological Source 1','Biological Source 2','Biological Source 3','IP')
      }else{
        dat=dat[,-ncol(dat)];dat=dat[,-ncol(dat)];dat=dat[,-ncol(dat)]; 
        dat$sampleID=createLink(paste0("https://www.encodeproject.org/files/",dat$sampleID),dat$sampleID)
        dat$uniqID=createLink(paste0("https://www.encodeproject.org/experiments/",dat$uniqID),dat$uniqID)
        colnames(dat)=c('Sample ID','Unique ID','Biological Source 1','Biological Source 2','Biological Source 3',"Gender","Age",'IP')
      }
      
     
      dat
    }
    ,rownames = FALSE, options = list( 
      pageLength = 10, 
      lengthMenu = list(c(10, 50, 100,-1), c('10', '50','100', 'All')) 
    ), 
    class = 'cell-border stripe',
    filter = 'top',
    escape = FALSE
    )
    
    
    output$stat_figure <- renderPlot({
      metadata_tab=paste0(input$stat_database,'_metadata')
      sql=paste0(" select * from ",metadata_tab," where species=",shQuote(input$stat_species)," and type=",shQuote(input$stat_IP))
      dat <- mysql_getData(sql)
      
      tmp=sort(table(dat$bs1),decreasing = T);tmp=tmp[tmp>20] 
    
      dat1=data.frame(name=names(tmp),number= as.numeric(tmp),stringsAsFactors = F)
      dat1$name <- factor( dat1$name,  levels=dat1$name )
      dat1$type='cellline'
      
      tmp=sort(table(dat$IP),decreasing = T);tmp=tmp[tmp>10]
      dat2=data.frame(name=names(tmp),number= as.numeric(tmp),stringsAsFactors = F)
      dat2$name <- factor( dat2$name,  levels=dat2$name )
      dat2$type='IP'
      
      get_a <- function(){
        dat=dat1
        p <- ggplot(dat, aes(x = name, y = number , label = number,theme_set(theme_bw()) )) +
          geom_bar(stat = "identity",fill='steelblue') +
          geom_text(size = 4,color='white', position = position_stack(vjust = 0.5))+coord_flip()+
          theme_set(theme_set(theme_bw(base_size=20)))+
          theme(text=element_text(face='bold'),
                axis.text.x=element_text(angle=30,hjust=1,size =15),
                plot.title = element_text(hjust = 0.5) ,
                panel.grid = element_blank() 
          )
        return(p)
      }
      
      get_b <- function(){
        dat=dat2
        p <- ggplot(dat, aes(x = name, y = number , label = number,theme_set(theme_bw()) )) +
          geom_bar(stat = "identity",fill='steelblue') +
          geom_text(size = 3,color='white', position = position_stack(vjust = 0.5))+coord_flip()+
          theme_set(theme_set(theme_bw(base_size=10)))+
          theme(text=element_text(face='bold'),
                axis.text.x=element_text(angle=30,hjust=1,size =8),
                plot.title = element_text(hjust = 0.5) ,
                panel.grid = element_blank() 
          )
        return(p)
      }
      
      first_row <- plot_grid(get_a(), ncol = 1, labels = c("cell-line" ))
      second_row <- plot_grid(get_b(), ncol = 1, labels = c("IP"))
      plot_grid(first_row, second_row, ncol = 2,nrow = 1)
      
    
    })
    
    
    
}) ## END FOR shinyServer




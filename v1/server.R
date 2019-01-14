library(shiny)
library(shinydashboard) 
library(DT)    
library(shinyBS)
library(GGally)
library(ggplot2)
library(shinyAce)
library(knitr)
library(rmarkdown)
library(RCurl)
library(shinyjs)
## input values : species/database/input_gene/cellline/genomic_feature

createLink <- function(base,val) {
  sprintf('<a href="%s" class="btn btn-link" target="_blank" >%s</a>',base,val) ##target="_blank" 
}


library(RMySQL)
con <- dbConnect(MySQL(), host="127.0.0.1", port=3306, user="qqqq", password="111") 
dbSendQuery(con, "USE TF_map") 
gene_mapping=dbGetQuery(con,"select symbols,geneNames  from gene_mapping")
dbDisconnect(con)

shinyServer(
  function(input,output,session){
    
    glob_values <- reactiveValues(
      results=NULL,
      input_gene=NULL,
      tmp=NULL
    )
    
    reactiveValues.reset <-function(){
      glob_values$results=NULL
      glob_values$input_gene=NULL
      glob_values$tmp=NULL
    }
    
    
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
    
    
    observeEvent(input$do, {
      glob_values$input_gene=input$input_gene
      gene=input$input_gene
      cellline=input$cellline
      genomic_feature=input$genomic_feature
      
      library(RMySQL)
      con <- dbConnect(MySQL(), host="127.0.0.1", port=3306, user="root", password="11111111") 
      dbSendQuery(con, "USE encode_hg19")
      
      dbListTables(con)
      
      if(genomic_feature != 'ALL'){
        sql=paste0(" select * from ",genomic_feature," where symbol=",shQuote(gene))
        peaks_tb <- dbGetQuery(con,sql)
        
      }else{
        
        peaks_tb=c()
        for(tab in c('promoter_tss','tts','utr_5','utr_3','intron','exon','intergenic','noc_coding') ){
          sql=paste0(" select * from ",tab," where symbol=",shQuote(gene))
          tmp <- dbGetQuery(con,sql)
          if(nrow(tmp)>0){
            tmp$genomic_feature=tab
            peaks_tb=rbind(peaks_tb,tmp)
          }
          
          
        }
      }
      
      
      if(cellline != 'ALL'){
        sql=paste0("select * from metadata where cellline = ",shQuote(cellline))
        metadata <- dbGetQuery(con,sql)
      }else{
        sql=paste0("select * from metadata   " )
        metadata <- dbGetQuery(con,sql)
      }
      
      dbDisconnect(con)
      
      ## can't find peak for some genomic features 
      
      if(nrow(peaks_tb) >0 && nrow(metadata)>0 ){
        tmp=merge(peaks_tb,metadata,by='sampleID')
        ## It counld be NULL
        if(nrow(tmp)>0){
          tmp1=tmp[,c(1:9,14)]
          glob_values$results=tmp1   
        }else{
          glob_values$results=NULL
        }
           
      }else{

        glob_values$results=NULL
      }
      

     

    })
    
    output$peaks_sushi <- renderPlot({
      tmp=glob_values$results
      gene=glob_values$input_gene
      
      if(! is.null(tmp)){
        ## firstly draw gene model:
        
        library(RMySQL)
        con <- dbConnect(MySQL(), host="127.0.0.1", port=3306, user="root", password="11111111") 
        dbSendQuery(con, "USE TF_map") 
        gene_structure=dbGetQuery(con,paste0("select *  from ccds_current where gene=",shQuote(gene)))
        dbDisconnect(con)
        
        gene_exon_list=c()
        apply(gene_structure, 1, function(x){
          #x=gene_structure[1,]
          chr=x[1]
          ccds_id=x[5]
          exon_list=x[10]
          strand=x[7]
          exon_list=substr(exon_list,2,nchar(exon_list)-1)
          tmp=strsplit(exon_list,',')[[1]]
          start=unlist(lapply(tmp, function(x) as.numeric(strsplit(x,'-')[[1]][1])))
          end=unlist(lapply(tmp, function(x) as.numeric(strsplit(x,'-')[[1]][2])))
          this_ccds=data.frame(chrom=chr,start=start,stop=end,gene=ccds_id,score=0,strand=strand,type='exon',stringsAsFactors = F)
          gene_exon_list <<- rbind(gene_exon_list,this_ccds)
        }) 
        colnames(gene_exon_list)=c( 'chrom','start' ,'stop' ,'gene' ,'score' ,'strand' ,'type')
        
        layout(matrix(c(1,1,2,2),2, 2, byrow = TRUE))
        par(mar=c(3,4,1,1))
        ## firstly draw gene model:
        
        chrom            =  gene_exon_list[1,1]
        chromstart       = min(c(gene_exon_list$start,gene_exon_list$stop))-20000
        chromend         = max(c(gene_exon_list$start,gene_exon_list$stop))+20000
        pg = plotGenes(geneinfo = gene_exon_list, chrom,chromstart,chromend ,
                       types = gene_exon_list$type, 
                       colorby=log10(gene_exon_list$score+0.001),
                       colorbycol= SushiColors(5),colorbyrange=c(0,1.0),
                       labeltext=TRUE,maxrows=50,height=0.4,plotgenetype="box")
        labelgenome( chrom, chromstart,chromend,n=3,scale="Mb") 
        

        ## Then draw TF matrix figures
        tmp1=glob_values$results[,c(2:4,10,5)]## column 10 is the TF name.
        colnames(tmp1)=c('chrom','start','end' ,'name', 'score')
        tmp1$strand='.'
        tmp1$row=as.numeric(as.factor(tmp1$name))
        par(mar=c(4,8,4,4))
        chrom            = tmp1[1,1]
        zoomregion1_chromstart      = min(c(tmp$start,tmp$end))-50
        zoomregion1_chromend         = max(c(tmp$start,tmp$end))+50
        tmp1$color =maptocolors(tmp1$row,col=SushiColors(5))
        
        zoomregion1      = c(zoomregion1_chromstart,zoomregion1_chromend)
        zoomsregion(zoomregion1,extend=c(0.01,0.13),wideextend=0.05,
                    offsets=c(0,0.580))
        plotBed(beddata    = tmp1,chrom = chrom,
                chromstart=zoomregion1[1],chromend=zoomregion1[2],
                rownumber  = tmp1$row, type = "circles",
                color=tmp1$color,row="given",
                plotbg="grey95",rowlabels=unique(tmp1$name),
                rowlabelcol=unique(tmp1$color),rowlabelcex=0.75)
        labelgenome(chrom,chromstart=zoomregion1[1],chromend=zoomregion1[2],n=3,scale="Mb")
        mtext("ChIP-seq",side=3, adj=-0.065,line=0.5,font=2)
        zoombox()

      
      }else{
        return(NULL)
      }
      
      
      
    })
    
    output$results <- DT::renderDataTable( {
      tmp1=glob_values$results
      
      # https://www.encodeproject.org/experiments/ENCSR541AOQ/
      # https://www.encodeproject.org/files/ENCFF132KNA/
      
      if(! is.null(tmp1)){
        tmp1$sampleID=createLink(paste0("https://www.encodeproject.org/files/",tmp1$sampleID),tmp1$sampleID)
        tmp1$uniqID=createLink(paste0("https://www.encodeproject.org/experiments/",tmp1$uniqID),tmp1$uniqID)
        return(tmp1)
      }else{
        return(NULL)
      }

      
    }
    , escape = FALSE)## end for results;
    
})

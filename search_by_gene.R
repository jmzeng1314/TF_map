observeEvent(input$do_gene, { 
  glob_values$results=NULL
  glob_values$input_gene=input$input_gene
  glob_values$species=input$species
  glob_values$database=input$database
  glob_values$IP=input$IP
  glob_values$cellline=input$cellline
  
  log_cat(paste0("Now the user:",getIP(),":search the gene for ",glob_values$input_gene))
  
  
  ## firstly search the exactly chromosome for the input_gene . 
  gene_pos=get_gene_position() ## this function come from scripts: position.R
  glob_values$chromosome=strsplit(gene_pos,':')[[1]][1]
  glob_values$gene_position=gene_pos 
  gene=input$input_gene
  cellline=input$cellline
  genomic_feature=input$genomic_feature
  
  ## secendly meta information based on species/IP/database/cellline
  # there are two metadata (cistrome and ENCODE)
  metadata_tab=paste0(input$database,'_metadata')
  cat( cellline , file=stderr());cat("\n",file=stderr())
  if(cellline != 'ALL'){
    
    if(  grepl(';',cellline)  ){
      ## there should be multiple cellline been choosed.
      tmp=strsplit(cellline,';')[[1]]
      
      meta_sql <<- paste0("select * from ",metadata_tab," where bs1 = ",shQuote(tmp[1])," and bs2 = ",shQuote(tmp[2])
                          ," and bs3 = ",shQuote(tmp[3]),
                          " and type=",shQuote(input$IP) ," and species=",shQuote(input$species) 
      )
    }else{
      # just one cellline been choosed.
      meta_sql <<- paste0("select * from ",metadata_tab,
                          " where bs2 = ",shQuote( cellline ) 
                          ," or bs3 = ",shQuote( cellline ),
                          " and type=",shQuote(input$IP) ," and species=",shQuote(input$species) 
      )
    }
    
    
  }else{
    meta_sql  <<-  paste0("select * from ",metadata_tab,
                          " where species = ",shQuote(input$species),
                          " and type=",shQuote(input$IP) )
  } 
  metadata <- mysql_getData(meta_sql) 
  
  ## for GSE study in cistrome, we can add more information based on GEOmetadb
  if(input$database=='cistrome'){
    gsm_sql=paste0("select title,source_name_ch1,GSM  from  cistrome_GSM_metadata ")
    gsm_data <- mysql_getData(gsm_sql) 
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
    peaks_sql <<- paste0(" select * from ",peaks_tab,
                         " where symbol=",shQuote(gene),
                         ' and feature_type=',genomic_feature)
    
    
  }else{
    peaks_sql <<- paste0(" select * from ",peaks_tab," where symbol=",shQuote(gene) ) 
  } 
  peaks_tb <- mysql_getData(peaks_sql)  
  
  if(nrow(peaks_tb)>0)
    peaks_tb$chrom=as.character(glob_values$chromosome)
   
  ## can't find peak for some genomic features
  if(nrow(peaks_tb) >0 ){
    # we can find peaks for a specific gene when we choose species/IP/database/cellline
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
        outdir='/var/www/html/igv'
        FH=file.path(outdir, glob_values$bed_filename)
        if(file.exists(outdir)){ 
          write.table( tmp1 ,FH,sep = '\t',quote = F,row.names = F,col.names = F)
          }
        
      }else{
        createAlert(session, "alert_search_results_anchorId", "exampleAlert", title = "Oops",
                    content = "we can't find results for this criteria", append = FALSE)
        glob_values$results=NULL
      }
      
    }else{
    createAlert(session, "alert_search_results_anchorId", "exampleAlert", title = "Oops",
                  content = "funny", append = FALSE)
      glob_values$results=NULL
    }
    
  }else{
    # if we didn't find any peaks for a specific gene when we choose species/IP/database/cellline
    # Then we report the message below:
    
    createAlert(session, "alert_search_results_anchorId", "exampleAlert", title = "Oops",
                content = paste0("There's no peak annotated to this gene by HOMER\n" ,
"you can still try to search it by coordinates",gene_pos), append = FALSE)
    glob_values$results=NULL
  }
  
})
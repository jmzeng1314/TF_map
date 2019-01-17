
output$washUlink <- renderUI({
  ## the glob_values$results  will return by two search buttones.
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
        log_cat(paste0(getIP(),"    Check the URL:",url,'\n'))
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

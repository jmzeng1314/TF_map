get_gene_position = reactive({
  input_gene = input$input_gene 
  if(nchar(input_gene)>1){
    chrom_db=ifelse(input$species=='human',
                    'HAVANA_v36_human_gene_info',
                    'HAVANA_vM25_mouse_gene_info') 
    # c('symbol'   ,  'type' ,   'ensembl'   , 'chr' ,'start', 'end' )
    chrom_sql=paste0('select  chr , start ,  end  from ',chrom_db,' where symbol=',shQuote( input_gene))
    tmp_gene_info=mysql_getData(chrom_sql)
    ## we should make sure that there's no "chr" as prefix for chromosome.
    gene_position=paste0(gsub('chr','',tmp_gene_info[1,1]),":",tmp_gene_info[1,2],"-",tmp_gene_info[1,3])
    return(gene_position)
  }
  
})
observe({
  updateTextInput(session, 'position', 
                  label = NULL, 
                  value = get_gene_position())
})

observeEvent(input$zoom_in, {
  tmp=text2bed(input$position)[1,]
  chrom=tmp[1]
  start=as.numeric(tmp[2])
  end=as.numeric(tmp[3])
  if((end-start)>2000){
    start=start+1000
    end=end-1000
  }
  tmp=paste0(chrom,":",start,",",end)
  updateTextInput(session, 'position', label = NULL, value = tmp)
})
observeEvent(input$zoom_out, {
  tmp=text2bed(input$position)[1,]
  chrom=tmp[1]
  start=as.numeric(tmp[2])
  end=as.numeric(tmp[3])
  start=start-1000
  end=end+1000
  tmp=paste0(chrom,":",start,",",end)
  updateTextInput(session, 'position', label = NULL, value = tmp)
})
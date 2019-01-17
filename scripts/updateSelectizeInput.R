
## the gene choices  depends on the species user choosed
observe({
  x <- as.character(input$species)
  cat(x, file=stderr());cat("\n",file=stderr())
  
  if(x=='human'){
    gene_mapping <<- all_human_gene
  }else{
    gene_mapping <<-  all_mouse_gene
  } 
  
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
  shinyjs::toggleState("do_gene", !input$cellline ==""  && !input$input_gene =="")
})

observe({
  shinyjs::toggleState("do_position", !input$cellline ==""  && !input$position =="")
})

## the cellLine choices  depends on the database and species and IP  user choosed
observe({
  x <- input$database
  # input$database, only for  "cistrome" or "ENCODE" 
  ## input$IP, only for "TF" or "histone" 
  
  # Can use character(0) to remove all choices
  if (is.null(x))
    x <- character(0)
  
  
  ## colnames for cistrome_metadata 
  # sampleID       GSM    bs1                 bs2      bs3       IP species    type
  ## TOdo: colnames for ENCODE_metadata 
  db=paste(x,'metadata',sep='_')
  sql=paste0("select distinct bs1,bs2,bs3  from ",db,
             " where species = ",shQuote(input$species)," and type=",shQuote(input$IP)
  )
  
  library(RMySQL) 
  cellline_info=mysql_getData( sql )
  cellline_info=unique(cellline_info)
  
  cellline_input=unique( apply(cellline_info,1,function(x) paste(x,collapse = ';')))
  cellline_input=c( "ALL" ,unique(cellline_info[,2]),unique(cellline_info[,3]),cellline_input)
  
  
  updateSelectizeInput(
    session, inputId='cellline', label = "Biological source", server = TRUE, 
    choices = cellline_input 
  )## end for updateSelectizeInput 
  
}) ## end for observe 




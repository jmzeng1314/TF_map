library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(knitr)
library(rmarkdown)
library(DT)     

############# input values #############
## species/IP/database/cellline
## input_gene/genomic_feature
## position, such as '18:28176327,28178670'
############# input values #############

############# output values #############
##    DT::dataTableOutput('results') 
##    plotOutput('results_stat')
##    DT::dataTableOutput('stat_table')
############# output values #############

############# actionButton #############
## do_gene
## do_position/zoom_in/zoom_out
############# actionButton #############

all_human_gene<<- mysql_getData("select distinct symbol,gene_name from human_genename ;") 
all_mouse_gene<<- mysql_getData("select distinct symbol,gene_name from mouse_genename ;") 
colnames(all_human_gene)=c('symbols','geneNames')
colnames(all_mouse_gene)=c('symbols','geneNames')



shinyServer(
  function(input,output,session){
    
    getIP <- reactive({ 
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
    ####################################################
    # update   inputId='cellline' #
    # update    inputId='input_gene' #
    source('updateSelectizeInput.R', local = TRUE)$value
    ## user can only choose the gene which come from org.Hs.eg.db or org.Mm.eg.db 
    ## human_genename and mouse_genename 
    ###################################################  
    
    ####################################################
    # Get the position of choosed gene according to GENCODE database.
    # gencode_v29_human_gene_info  and  gencode_vM20_mouse_gene_info 
    source('positions.R', local = TRUE)$value
    # first the choosed gene will change the positon.
    ## the zoom_in and zoom_out will also change the position.
    ###################################################      
     
    
    ####################################################
    # once the user click the button for searching by gene.
    source('search_by_gene.R', local = TRUE)$value
    # 
    ###################################################  
    
    ####################################################
    # The table show in home page, return all the peaks found based on user.
    source('output_main_result_table.R', local = TRUE)$value
    # 
    ###################################################  
    
})



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
    
    ## once the user click the button for searching by gene.
    observeEvent(input$do_gene, { 
      glob_values$results=NULL
      glob_values$input_gene=input$input_gene
      glob_values$species=input$species
      glob_values$database=input$database
      glob_values$IP=input$IP
      glob_values$cellline=input$cellline
      
      log_cat(paste0("Now the user:",getIP(),":search the gene for ",glob_values$input_gene))
      
    })
    
})



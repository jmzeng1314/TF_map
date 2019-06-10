## there are 3 boxes in home page:
## which are Parameters, searching, results.

## home page: choose Parameters 
box1 <- box(status = "success",height = "350px",width = NULL,
            style="background-color: #fff",
            p(style = "font-size: 20px;", "Parameters:"),
            selectInput("species", "Select species:",
                        c("Human(GRCh38)" = "human",
                          "Mouse(mm10)" = "mouse")
            ),
            selectInput("IP", "Select IP:",
                        c("Trans-acting factors" = "TF",
                          "Histone marks" = "histone"
                        )
            ),
            selectInput("database", "Select database:",
                        c("GEO" = "cistrome",
                          "ENCODE" = "ENCODE" 
                        )
            ),
            
            selectizeInput('cellline', label = "Biological source",
                           choices = NULL,# width = 275,
                           options = list(placeholder = "Search by Cell Line; Tissue; Organ,Example: MCF-7;Epithelium;Mammary Gland",
                                          maxOptions = 1000)
            )
)
## home page: Search by gene or genomic coordinates
box2 <- tabBox(
  height = "350px",width = NULL, selected = "Search by gene",
  tabPanel("Search by gene",
           selectizeInput('input_gene', 
                          label = "Please type a gene name (HUGO symbol)", 
                          choices = NULL,# width = 275,
                          options = list(placeholder = "eg: EZH2#",
                                         maxOptions = 1000)
           ),
           selectInput("genomic_feature", "Which portion of the gene to query?",
                       c("ALL" = "ALL" ,
                         "Promoter"=1 ,
                         "TTS"=2 ,
                         "5'UTR"= 7 ,
                         "3'UTR"= 8 ,
                         "Intron"=5 ,
                         "Exon" = 4 , 
                         "Intergenic"=3 
                       )),
           
           
           actionButton("do_gene","Search",icon("search"),
                        style="color: #fff; background-color: #18bc9c; border-color: #18bc9c"),
           width = NULL,status = "success"
           
  ),
  tabPanel("Search by genomic coordinates",
           fluidRow(column(9,p("Please input the coordinates:"),
                           textInput('position',label = NULL,'18:28176327,28178670',width = '100%'),
                           column(6,
                                  actionButton("zoom_in",  "Zoom in ",icon("search-minus"),width = '100%',
                                               style="color: #18bc9c; background-color: #fff; border-color: #18bc9c")),
                           column(6,
                                  
                                  actionButton("zoom_out", "Zoom out ",icon("search-plus"),width = '100%',
                                               style="color: #18bc9c; background-color: #fff; border-color: #18bc9c"))),
                    column(3,p(HTML("&nbsp;")),
                           actionButton("do_position", "Search",icon("search"),
                                        style="color: #fff; background-color: #18bc9c; border-color: #18bc9c"),
                           br(),br(),
                           p(" (by 1 kb)"))),
           #h3("you can ajust the postion by increase or decrease 1 kb at the region in box "),
           
           width = NULL,status = "success"))


box3 <-  box( 
  status = "success",width = NULL,height = "800px",
  title = "Results:",
  hr(),
  conditionalPanel( ## only show results after user click the search button !
    condition = ("input.do_gene>0 | input.do_position>0"),
    downloadLink('downloadData_csv', 'Download the CSV file'),br(),
    downloadLink('downloadData_bed', 'Download the BED file'),br(),
    ## we should change the link behind this UI in server client.
    uiOutput('washUlink'),
    
    br(),
    br(),
    br(),
    DT::dataTableOutput('results') ,
    br(),
    plotOutput('results_stat',height = "800px"),
    # plotlyOutput('results_stat' ),
    br()
  )## end for conditionalPanel
)



library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(knitr)
library(rmarkdown)
library(DT)     

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

page_Home <-fluidRow(
  shinyjs::useShinyjs(), 
  tags$script(src="www/getIP.js"),
  column(width = 4,box1,box2
  ),
  column(width = 8,box3
  )
)
page_statistics<- fluidRow(
  
  box(p(style = "font-size: 30px;", "Statistics for the metadata in GEO/ENCODE database"),
      # title = "Statistics for the metadata in GEO/ENCODE database", 
      
      status = "success",width=12,style="background-color: #fff",
      fluidRow(
        column(4,selectInput("stat_species", "Select species:",
                             c("Human(GRCh38)" = "human",
                               "Mouse(mm10)" = "mouse") )
        ),
        column(4,selectInput("stat_IP", "Select IP:",
                             c("Trans-acting factors" = "TF",
                               "Histone marks" = "histone"
                             ) )
        ),
        column(4, selectInput("stat_database", "Select database:",
                              c("GEO" = "cistrome",
                                "ENCODE" = "ENCODE" 
                              ) )
        )
      ) ## end for fluidRow
  ),## end for box 
  wellPanel(DT::dataTableOutput('stat_table')) 
  #plotOutput('stat_figure',height = 1000)
)

page_more <- fluidRow(
  # column(8, align="center", offset = 2,
  #        includeMarkdown("contact.Rmarkdown"),
  #        tags$style(type="text/css", "#string { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}")
  # )
  
  box(p(style = "font-size: 20px;", "Email:"),
      status = "success",style="background-color: #fff",width = 12,
      p("jianmingzeng@umac.mo"),
      p("gangli@umac.mo")
  ),
  box(p(style = "font-size: 20px;", "Acknowlagement:"),
      status = "success",style="background-color: #fff",width = 12,
      HTML("<p>
        TFmapper was built using the <a href='https://www.encodeproject.org/'>ENCODE</a> datasets and the   <a href='https://www.ncbi.nlm.nih.gov/geo/'>GEO</a> datasets curated by <a href='http://cistrome.org/db/#/'>CistromeDB</a>
    </p>")
  )
)

page_help <- fluidRow(
  column(8, offset = 2, # align="center", 
         includeMarkdown("help.Rmarkdown") 
         #tags$style(type="text/css", "#string { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}")
  )
)

body <- dashboardBody(
  shinyDashboardThemes(
    theme = "poor_mans_flatly"
    #blue_gradient,boe_website,grey_light,grey_dark,onenote,poor_mans_flatly,purple_gradient
  ),
   fluidRow(
    tabBox(
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1", height = "12000px",width = 12,
      tabPanel("Home", icon = icon("home"),page_Home
      ),
      tabPanel("statistics" ,icon = icon("table"), page_statistics
      ),
      tabPanel("Help",icon = icon('book'),"page_help"),
      tabPanel("more", icon = icon('info-circle'),page_more
      )
    )
    
  )
)
dashboardPage(
  dashboardHeader(
    title = "TFmapper"
  ),
  dashboardSidebar(
    disable = TRUE
  ),body
)


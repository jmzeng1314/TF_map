
header = dashboardHeader(title = "TFmapper"
                         ,titleWidth=150
)


sidebar = dashboardSidebar(
  width=150,  
  sidebarMenu(id='sidebarMenu',
              hr(),
              menuItem("Home", tabName = "Home", icon = icon("home")),
              menuItem("Statistics", tabName = "statistics", icon = icon("table")), 
              hr(), 
              menuItem("Contact us",tabName = 'contact',icon = icon('address-card-o')),
              menuItem("Help",tabName = 'help',icon = icon('book')) 
              
  )## end for sidebarMenu 
  
  
) 


page_Home<- fluidRow(
  shinyjs::useShinyjs(),
  #tags$script(src="igv.js"),
  tags$script(src="getIP.js"),
  box(title = "Parameters:", status = "success",width=12,solidHeader = TRUE,
      radioButtons("species", "Select species:",
                   c("Human(GRCh38)" = "human",
                     "Mouse(mm10)" = "mouse"),
                   inline=T
      ),
      radioButtons("IP", "Select IP:",
                   c("Trans-acting factors" = "TF",
                     "Histone marks" = "histone"
                     ),
                   inline=T
      ),
      radioButtons("database", "Select database:",
                   c("GEO" = "cistrome",
                     "ENCODE" = "ENCODE" 
                      ),
                   inline=T
      ),

      selectizeInput('cellline', label = "Biological source",
                     choices = NULL,# width = 275,
                     options = list(placeholder = "Search by “Cell Line”; “Tissue”; “Organ”_Example: MCF-7;Epithelium;Mammary Gland",
                                    maxOptions = 1000)
      )
  ),
  box(title = "Search by gene", status = "success",solidHeader = TRUE,
      selectizeInput('input_gene', label = "Please type a gene name (HUGO symbol)", choices = NULL,# width = 275,
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
     
      
        actionButton("do", "Search by gene symbol", icon("paper-plane"), 
                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
  ),
  box(title = "Search by position", status = "success",solidHeader = TRUE,
      textInput('position','Please input the coordinates','18:28176327,28178670'),
      #h3("you can ajust the postion by increase or decrease 1 kb at the region in box "),
      actionButton("zoom_in",  "Zoom in (by 1 kb)"),br(),br(),
      actionButton("zoom_out", "Zoom out (by 1 kb)"),  br(),br(),
      actionButton("do_position", "Search by position", icon("paper-plane"), 
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
  ),
  
  box(title = "Results:", status = "success",width=12,solidHeader = TRUE,
      hr(),
      conditionalPanel( ## only show results after user click the search button !
          condition = ("input.do>0 | input.do_position>0"),
          downloadLink('downloadData_csv', 'Download the CSV file'),br(),
          downloadLink('downloadData_bed', 'Download the BED file'),br(),
          uiOutput('washUlink'),
          
          
          br(),
          bsAlert("alert_search_results_anchorId"),
          br(),
          br(),
          DT::dataTableOutput('results') ,
          br(),
          plotOutput('results_stat',height = "800px"),
         # plotlyOutput('results_stat' ),
          br()
      ) ## end for conditionalPanel
      ) ## end for the box 
)
page_contact <- fluidRow(
  # column(8, align="center", offset = 2,
  #        includeMarkdown("contact.Rmarkdown"),
  #        tags$style(type="text/css", "#string { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}")
  # )
  
  box(title = "Email", status = "success",solidHeader = TRUE, 
      p("jianmingzeng@umac.mo"),
      p("gangli@umac.mo")
  ) 
)
page_help <- fluidRow(
  column(8, offset = 2, # align="center", 
         includeMarkdown("help.Rmarkdown") 
         #tags$style(type="text/css", "#string { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}")
  )
)

page_statistics<- fluidRow(
  
  box(title =  tags$p(style = "font-size: 30px;", "Statistics for the metadata in GEO/ENCODE database"),
    # title = "Statistics for the metadata in GEO/ENCODE database", 
      
      status = "info",width=12,solidHeader = TRUE,
      fluidRow(
      column(4,radioButtons("stat_species", "Select species:",
                   c("Human(GRCh38)" = "human",
                     "Mouse(mm10)" = "mouse") )
      ),
      column(4,radioButtons("stat_IP", "Select IP:",
                   c("Trans-acting factors" = "TF",
                     "Histone marks" = "histone"
                   ) )
      ),
      column(4, radioButtons("stat_database", "Select database:",
                   c("GEO" = "cistrome",
                     "ENCODE" = "ENCODE" 
                   ) )
      )
      ) ## end for fluidRow
    ),## end for box 
  wellPanel(DT::dataTableOutput('stat_table')) 
  #plotOutput('stat_figure',height = 1000)
)



body = dashboardBody(
  shinyjs::useShinyjs(),
  includeScript("google-analytics.js"),
  #tags$style(type="text/css", ".content-wrapper { background-color: write;font-size: 20px; }"),
  
  bsAlert("alert1"), 
  tabItems(
    tabItem(tabName = 'Home' ,page_Home),
    tabItem(tabName = 'statistics',page_statistics),
    tabItem(tabName = 'contact',page_contact),
    tabItem(tabName = 'help',page_help) 
  )
  
)
 
dashboardPage(skin = "blue", 
              header,sidebar,body,title = "TFmapper")




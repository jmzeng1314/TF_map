library(shiny)
library(shinydashboard)
library(DT)
library(Biobase)
library(reshape2) 
library(shinyBS)
library(GGally)
library(shinyAce)
library(knitr)
library(rmarkdown) 
library(shinyjs)
library(RMySQL)
library(Sushi)

header = dashboardHeader(title = "TF map"
                         ,titleWidth=250
)


sidebar = dashboardSidebar(
  width=350, 
  sidebarMenu(id='sidebarMenu',
              hr(),
              menuItem("Home", tabName = "Home", icon = icon("home")),
              menuItem("statistics", tabName = "statistics", icon = icon("refresh")), 
              hr(), 
              menuItem("Contact Us",tabName = 'contact',icon = icon('code')),
              menuItem("Help",tabName = 'help',icon = icon('info-circle')) 
              
  )## end for sidebarMenu 
  
  
) 


page_Home<- fluidRow(
  box(title = "search a gene ", status = "info",width=12,
      radioButtons("species", "Select species:",
                   c("Human(GRCh38)" = "human",
                     "Mouse(mm10)" = "mouse"),
                   inline=T
      ),
      radioButtons("IP", "Select IP:",
                   c("transcription factor" = "TF",
                     "histone" = "histone"
                     ),
                   inline=T
      ),
      radioButtons("database", "Select database:",
                   c("GEO" = "cistrome",
                     "ENCODE" = "encode" 
                      ),
                   inline=T
      ),

      selectizeInput('cellline', label = "Choose a cell line",
                     choices = NULL,# width = 275,
                     options = list(placeholder = "Please Type a  cell line#",
                                    maxOptions = 1000)
      ),
      selectizeInput('input_gene', label = "Type a gene name", choices = NULL,# width = 275,
                     options = list(placeholder = "Please Type a gene name #",
                                    maxOptions = 1000)
      ),
      selectInput("genomic_feature", "Which portion of the gene to query?",
                  c("Promoter"="promoter_TSS" ,
                    "TTS"="TTS" ,
                   "5'UTR"= "UTR_5" ,
                   "3'UTR"= "UTR_3" ,
                    "Intron"="intron" ,
                    "Exon" = "exon" , 
                    "Intergenic"="Intergenic" ,
                    "ncRNA"="non_coding", 
                    "ALL" = "ALL"
                    #"rRNA" = "rRNA",
                    #"snoRNA" = "snoRNA",
                    #"pseudo" = "pseudo"
                    )),
     
      
        actionButton("do", "Search", icon("paper-plane"), 
                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      hr(),br(),
      conditionalPanel( ## only show results after user click the search button !
          condition = ("input.do>0"),
          h3(actionLink("multiple_visualization","Choose IPs to visualize",width='150%')),

          bsModal("modalExample", "Choose IPs", "multiple_visualization", size = "large",
                  wellPanel(
            
            actionButton("selectALL_button", "select ALL"),
            uiOutput('chooseIP_checkbox'),
            actionButton("drawSushi", "Draw figure"),
            conditionalPanel(
              condition = ("input.drawSushi>0"),
              plotOutput('sushi_peaks',  height = 1000)
            )
            
          )), ## end for bsModal
          br(),
          bsAlert("alert_search_results_anchorId"),
          br(),
          br(),
          DT::dataTableOutput('results') 
      ) ## end for conditionalPanel
      ) ## end for the box 
)
page_contact <- fluidRow(
  column(8, align="center", offset = 2,
        # includeMarkdown("contact.Rmarkdown"),
         tags$style(type="text/css", "#string { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}")
  )
)
page_help <- fluidRow(
  column(8, align="center", offset = 2,
         #includeMarkdown("help.Rmarkdown"),
         tags$style(type="text/css", "#string { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}")
  )
)

page_statistics<- fluidRow(
  column(8, align="center", offset = 2,
         h1('statistics for the metadata in GEO/ENCODE database!'),
         tags$style(type="text/css", "#string { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}")
  ),
  hr(),
  box(title = "choice for statistics", status = "info",width=12,
      fluidRow(
      column(4,radioButtons("stat_species", "Select species:",
                   c("Human(GRCh38)" = "human",
                     "Mouse(mm10)" = "mouse") )
      ),
      column(4,radioButtons("stat_IP", "Select IP:",
                   c("transcription factor" = "TF",
                     "histone" = "histone"
                   ) )
      ),
      column(4, radioButtons("stat_database", "Select database:",
                   c("GEO" = "cistrome",
                     "ENCODE" = "encode" 
                   ) )
      )
      ) ## end for fluidRow
    ),## end for box 
  DT::dataTableOutput('stat_table'),
  plotOutput('stat_figure',height = 800)
)



body = dashboardBody(
  bsAlert("alert1"), 
  tabItems(
    tabItem(tabName = 'Home' ,page_Home),
    tabItem(tabName = 'statistics',page_statistics),
    tabItem(tabName = 'contact',page_contact),
    tabItem(tabName = 'help',page_help) 
  )
  
)
 
dashboardPage(skin = "black", header,sidebar,body,title = "Transcription factor map")




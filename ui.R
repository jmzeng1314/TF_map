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

header = dashboardHeader(title = "TF map"
                         ,titleWidth=350
)


sidebar = dashboardSidebar(
  width=350, 
  sidebarMenu(id='sidebarMenu',
              hr(),
              menuItem("Home", tabName = "Home", icon = icon("home")),
              menuItem("New Analysis", tabName = "NewAnalysis", icon = icon("refresh")), 
              hr(), 
              menuItem("Contact Us",tabName = 'contact',icon = icon('code')),
              menuItem("Help",tabName = 'help',icon = icon('info-circle')) 
              
  )## end for sidebarMenu 
  
  
) 

page_Home <- fluidRow(
  column(8, align="center", offset = 2,
         DT::dataTableOutput('cellline'),
         includeMarkdown("home.Rmarkdown"),
         tags$style(type="text/css", "#string { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}")
  )
  
)
page_NewAnalysis <- fluidRow(
  box(title = "search a gene ", status = "info",width=12,
      radioButtons("species", "Select species:",
                   c("Human" = "human",
                     "Mouse" = "mouse"),
                   inline=T
      ),
      radioButtons("IP", "Select IP:",
                   c("tf" = "TF",
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
      h2("OR"),
      textInput('input_region',"type a genomic region" ,placeholder="Chromosomal region (chrN:start-end)"),
      hr(),
      actionButton("do", "Search"),
      br(),
      br(),
      br(),
      DT::dataTableOutput('results'),
      hr(),
      column(8, align="center", offset = 2,plotOutput('peaks_sushi',height = "800px")),
      hr()
      ) ## end for the box 
)
page_contact <- fluidRow(
  column(8, align="center", offset = 2,
         includeMarkdown("contact.Rmarkdown"),
         tags$style(type="text/css", "#string { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}")
  )
)
page_help <- fluidRow(
  column(8, align="center", offset = 2,
         includeMarkdown("help.Rmarkdown"),
         tags$style(type="text/css", "#string { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}")
  )
)


body = dashboardBody(
  bsAlert("alert1"), 
  tabItems(
    tabItem(tabName = 'Home' ,page_NewAnalysis),
    tabItem(tabName = 'NewAnalysis',page_Home),
    tabItem(tabName = 'contact',page_contact),
    tabItem(tabName = 'help',page_help) 
  )
  
)
 
dashboardPage(skin = "black", header,sidebar,body,title = "Transcription factor map")




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
      conditionalPanel(
          condition = ("input.do>0"),
          actionLink("multiple_visualization","Choose IPs to visualize by WashU browser",width='150%'),
    
          bsModal("modalExample", "Choose IPs", "multiple_visualization", size = "small",
                  wellPanel(
            
            actionButton("selectALL_button", "select ALL"),
            uiOutput('chooseIP_checkbox'),
            actionButton("generateLink_button", "generate Link"),
            conditionalPanel(
              condition = ("input.generateLink_button>0"),
              uiOutput('multiple_visualization_links')
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
  h1('statistics for the metadata in GEO/ENCODE database!'),
  hr(),
  tabBox( width=12,  
          tabPanel("GEO", 
                   tabBox(  width=12, 
                            tabPanel("human",  tabBox(  width=12, 
                                                        tabPanel("TF", box(title = "GEO human TF stat ", status = "primary",width=12,
                                                                           DT::dataTableOutput('GEO_human_TF_stat_table'),
                                                                           plotOutput('GEO_human_TF_stat_plot')
                                                        ) 
                                                        
                                                        ),
                                                        tabPanel("histone",   box(title = "GEO human histone stat ", status = "primary",width=12,
                                                                                  DT::dataTableOutput('GEO_human_histone_stat_table'),
                                                                                  plotOutput('GEO_human_histone_stat_plot')
                                                        )
                                                        ) 
                            )
                            
                            ),
                            tabPanel("mouse",
                                     tabBox(   width=12,
                                               tabPanel("TF",    box(title = "GEO mouse TF stat ", status = "primary",width=12,
                                                                     DT::dataTableOutput('GEO_mouse_TF_stat_table'),
                                                                     plotOutput('GEO_mouse_TF_stat_plot')
                                               )    
                                               ),
                                               tabPanel("histone",   box(title = "GEO mouse histone stat ", status = "primary",width=12,
                                                                         DT::dataTableOutput('GEO_mouse_histone_stat_table'),
                                                                         plotOutput('GEO_mouse_histone_stat_plot')
                                               )
                                               ) 
                                     )
                            ) 
                   )
          ),
          tabPanel("ENCODE",
                   tabBox(   width=12,
                             tabPanel("human", 
                                      tabBox(  width=12, 
                                               tabPanel("TF",    box(title = "ENCODE human TF stat ", status = "primary",width=12,
                                                                     DT::dataTableOutput('ENCODE_human_TF_stat_table'),
                                                                     plotOutput('ENCODE_human_TF_stat_plot')
                                               ) 
                                               
                                               ),
                                               tabPanel("histone",  box(title = "ENCODE human histone stat ", status = "primary",width=12,
                                                                        DT::dataTableOutput('ENCODE_human_histone_stat_table'),
                                                                        plotOutput('ENCODE_human_histone_stat_plot')
                                               )
                                               
                                               ) 
                                      )
                                      
                             ),
                             tabPanel("mouse",
                                      tabBox(   width=12,
                                                tabPanel("TF",    box(title = "ENCODE mouse TF stat ", status = "primary",width=12,
                                                                      DT::dataTableOutput('ENCODE_mouse_TF_stat_table'),
                                                                      plotOutput('ENCODE_mouse_TF_stat_plot')
                                                )
                                                
                                                ),
                                                tabPanel("histone",   box(title = "ENCODE mouse histone stat ", status = "primary",width=12,
                                                                          DT::dataTableOutput('ENCODE_mouse_histone_stat_table'),
                                                                          plotOutput('ENCODE_mouse_histone_stat_plot')
                                                )
                                                
                                                ) 
                                      )
                             ) 
                   )
          ) 
  )## end for out tabBox  
  
)



page_statistics_1 <- fluidRow(

  box(title = "GEO human TF stat ", status = "primary",width=12,
      DT::dataTableOutput('GEO_human_TF_stat_table'),
      plotOutput('GEO_human_TF_stat_plot')
  ),
  box(title = "GEO mouse TF stat ", status = "primary",width=12,
      DT::dataTableOutput('GEO_mouse_TF_stat_table'),
      plotOutput('GEO_mouse_TF_stat_plot')
  ),
  box(title = "GEO human histone stat ", status = "primary",width=12,
      DT::dataTableOutput('GEO_human_histone_stat_table'),
      plotOutput('GEO_human_histone_stat_plot')
  ),
  box(title = "GEO mouse histone stat ", status = "primary",width=12,
      DT::dataTableOutput('GEO_mouse_histone_stat_table'),
      plotOutput('GEO_mouse_histone_stat_plot')
  ),
  box(title = "ENCODE human TF stat ", status = "primary",width=12,
      DT::dataTableOutput('ENCODE_human_TF_stat_table'),
      plotOutput('ENCODE_human_TF_stat_plot')
  ),
  box(title = "ENCODE mouse TF stat ", status = "primary",width=12,
      DT::dataTableOutput('ENCODE_mouse_TF_stat_table'),
      plotOutput('ENCODE_mouse_TF_stat_plot')
  ),
  box(title = "ENCODE human histone stat ", status = "primary",width=12,
      DT::dataTableOutput('ENCODE_human_histone_stat_table'),
      plotOutput('ENCODE_human_histone_stat_plot')
  ),
  box(title = "ENCODE mouse histone stat ", status = "primary",width=12,
      DT::dataTableOutput('ENCODE_mouse_histone_stat_table'),
      plotOutput('ENCODE_mouse_histone_stat_plot')
  )
  
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




library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(knitr)
library(rmarkdown)
library(DT)     

### there are 4 pages in this website:
## Home, Statistics, Help, About

## there are 3 boxes in home page:
## which are Parameters, searching, results.
source('scripts/ui-box-homepage.R', local = TRUE)$value
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

page_about <- fluidRow( 
  column(8, offset = 2,  
         includeMarkdown("about.Rmarkdown")   
         )
)

page_help <- fluidRow(
  column(8, offset = 2, 
         includeMarkdown("help.Rmarkdown") 
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
      tabPanel("Home", icon = icon("Home"),page_Home
      ),
      tabPanel("Statistics" ,icon = icon("table"), page_statistics
      ),
      tabPanel("Help",icon = icon('book'),page_help),
      tabPanel("About", icon = icon('info-circle'),page_about
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


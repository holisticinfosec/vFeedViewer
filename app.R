## vFeed Viewer ##
## Russ McRee, @HolisticInfoSec, May 2016
## 0.1 alpha

## app.R ##
library(shinydashboard)
library(RSQLite)
library(ggplot2)
library(reshape2)

databasefile <- "vfeed.db"
dbcon <- dbConnect(dbDriver("SQLite"), dbname = databasefile)
CWEDB <- dbReadTable(dbcon, "cwe_db")
NVDDB <- dbReadTable(dbcon, "nvd_db", select.cols="cveid,summary")
LATEST <- dbReadTable(dbcon, "stat_new_cve")
STATS <- dbReadTable(dbcon, "stat_vfeed_kpi")
dbDisconnect(dbcon)

options(warn=-1)

ui <- dashboardPage(skin = "blue",
  dashboardHeader(title = "vFeed Viewer 0.1 alpha", titleWidth = 350),
  dashboardSidebar(
    sidebarMenu(
      menuItem(tags$div("vFeed Overview"), tabName = "summary", icon = icon("info-circle")),
      menuItem(tags$div("vFeed NVD Search"), tabName = "nvdDb", icon = icon("search")),
      menuItem(tags$div("vFeed CWE Search"), tabName = "cweDb", icon = icon("search")),
      menuItem(tags$div("vFeed Latest Added CVEs"), tabName = "latest", icon = icon("list")),
      menuItem(tags$div("vFeed Statistics"), tabName = "stats", icon = icon("calculator"))
      
    )
  ),
  
  dashboardBody(
    img(src = "vFeed.png"),
    tabItems(
            # Data table
      tabItem(tabName = "nvdDb",
              h1("National Vulnerability Database Search"),
              h3("NVD is the U.S. government repository of standards based vulnerability management data represented using the Security Content Automation Protocol (SCAP)."),
              fluidRow(
                column(width=2, 
                       selectInput("cveid", 
                                   "CVE ID:", 
                                   c("All", 
                                     unique(as.character(NVDDB$cveid))))
                ),
                column(width=2, 
                       selectInput("summary", 
                                   "Summary:", 
                                   c("All", 
                                     unique(as.character(NVDDB$summary))))
                       
                )
              ),
              # Create a new row for the table.
              fluidRow(
                dataTableOutput(outputId="table")
              )    
      ),
        
      # Data table
      tabItem(tabName = "cweDb",
              h1("Common Weaknesses Enumeration Search"),
              h3("A Community-Developed Dictionary of Software Weakness Types."),
              fluidRow(
                column(width=2, 
                       selectInput("cweid", 
                                   "CWE ID:", 
                                   c("All", 
                                     unique(as.character(CWEDB$cweid))))
                ),
                column(width=2, 
                       selectInput("cwetitle", 
                                   "CWE Title:", 
                                   c("All", 
                                     unique(as.character(CWEDB$cwetitle))))
             
                )
              ),
              # Create a new row for the table.
              fluidRow(
                dataTableOutput(outputId="table2")
              )    
      ),

      # Data table
      tabItem(tabName = "latest",
              h1("Latest added CVEs"),
              fluidRow(
                column(width=2, 
                       selectInput("new_cve_id", 
                                   "New CVE ID:", 
                                   c("All", 
                                     unique(as.character(LATEST$new_cve_id))))
                ),
                column(width=2, 
                       selectInput("new_cve_summary", 
                                   "New CVE Summary:", 
                                   c("All", 
                                     unique(as.character(LATEST$new_cve_summary))))
                       
                )
              ),
              # Create a new row for the table.
              fluidRow(
                dataTableOutput(outputId="table3")
              )    
      ),
      
      # Stats plot
      tabItem(tabName = "stats",
              fluidRow(
                plotOutput("plotType")
              )
      ),
            
      
      # Print Summary
      tabItem(tabName = "summary",
              h1("The Correlated Community Vulnerability and Threat Database"),
              p(strong("vFeed Framework"), "is a CVE, CWE and OVAL Compatible naming scheme concept that provides extra structured detailed 
                third-party references",br(), "and technical characteristics for a CVE entry through an extensible XML/JSON schema.",br(), 
                "It also improves the reliability of CVEs by providing a flexible and comprehensive vocabulary for describing the relationship with other standards and security references."),
              p(strong("vFeed"), "utilizes XML-based / JSON-based format outputs to describe in detail vulnerabilities.",br(), 
                "They can be leveraged as input by security researchers, practitioners and tools as part of their vulnerability description.",br(),
                "In fact, the standard syntax is easy to interpret by humans and systems.")
              
              )    
              
              )
              
              )
      )
    
 
server <- function(input, output) {
  
    output$table <- renderDataTable({
    
      withProgress(message = 'Calculation in progress',
                   detail = 'Standby...', value = 0, {
                     for (i in 1:10) {
                       incProgress(1/10)
                       Sys.sleep(0.1)  }})
      data <- NVDDB
    if (input$cveid != "All"){
      data <- data[data$cveid == input$cveid,]
    }
    if (input$summary != "All"){
      data <- data[data$summary == input$summary,]
    }
    data
  })
    output$table2 <- renderDataTable({
    
      withProgress(message = 'Calculation in progress',
                   detail = 'Standby...', value = 0, {
                     for (i in 1:10) {
                       incProgress(1/10)
                       Sys.sleep(0.1)  }})
      data <- CWEDB
    if (input$cweid != "All"){
      data <- data[data$cweid == input$cweid,]
    }
    if (input$cwetitle != "All"){
      data <- data[data$cwetitle == input$cwetitle,]
    }
    data
  })
  
    output$table3 <- renderDataTable({
      
      withProgress(message = 'Calculation in progress',
                   detail = 'standby...', value = 0, {
                     for (i in 1:10) {
                       incProgress(1/10)
                       Sys.sleep(0.1)  }})
      
      data <- LATEST
      if (input$new_cve_id != "All"){
        data <- data[data$new_cve_id == input$new_cve_id,]
      }
      if (input$new_cve_summary != "All"){
        data <- data[data$new_cve_summary == input$new_cve_summary,]
      }
      data
    })
    
    vfeedStats <- melt(STATS)
    vfeedStats <- vfeedStats[-c(1,3),]
    x=vfeedStats$variable
    y=vfeedStats$value
    output$plotType <- renderPlot({
      
      withProgress(message = 'Calculation in progress',
                   detail = 'Standby...', value = 0, {
                     for (i in 1:10) {
                       incProgress(1/10)
                       Sys.sleep(0.1)  }})
      
      ggplot(data=vfeedStats, aes(x,y, fill=variable)) + 
        geom_bar(stat="identity") + 
        theme(text = element_text(size=20), axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + 
        scale_y_continuous(breaks = c(0,25000,50000,75000,100000)) +
        labs(title = "vFeed Statistics (-CPE)", x = "vFeed Category Entities", y = "vFeed Entries") +
        scale_colour_gradientn(colours=rainbow(8)) +
        theme(legend.position="none")
    })
  
  }

shinyApp(ui, server)
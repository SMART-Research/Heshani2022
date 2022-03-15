library(shiny)
library(tidyverse)
library(DT)
library(visdat)
library(GGally)
library(skimr)
library(tidyverse)
library(plotrix)
library(VIM)
library(mice)
thematic::thematic_shiny(font = "auto")


# Define UI for application that draws a histogram
ui <- fluidPage( 

  titlePanel("KIT Tool"),
  tabsetPanel(
    tabPanel("Overview", fluidRow(
               column(2,
                      fileInput(
                 "df",
                 label = "choose the file:  csv. tx, excel or rds",
                 multiple = FALSE,
                 accept = c("text/csv/Excel",
                            "text/comma-separated-values,text/plain/excel",
                            ".csv",".txt",".xls",".xlsx",".rds")),
                 # Horizontal line ----
                 tags$hr(),
                 column(
                   width = 4,
                   align = "left",
                   fluidRow(
                     # Input: Checkbox if file has header ----
                     checkboxInput("header", "Header", TRUE),
                     
                     # Input: Select separator ----
                     radioButtons("sep", "Separator",
                                  choices = c(Comma = ",",
                                              Semicolon = ";",
                                              Tab = "\t"),
                                  selected = ","),
                   )
                 ),
                 column(
                   width = 4,
                   align = "left",
                   fluidRow(
                     br(),
                     br(),
                   
                     # Input: Select quotes ----
                     radioButtons("quote", "Quote",
                                  choices = c(None = "",
                                              "Double Qot." = '"',
                                              "Single Qot." = "'"),
                                  selected = '"')
                     
                   )
                 ),
                 
                 fluidRow(uiOutput("varSelect"))
                
               ),
               column(6,dataTableOutput("dataSet")),
               column(4,plotOutput("structure"))),
             
             fluidRow(
               column(6,verbatimTextOutput("summary")),
               column(6,plotOutput("correlation"))),
             ),
    
    
  tabPanel("Outlier Analysis",
             
            fluidRow( plotOutput("aggr")),
            fluidRow(sidebarLayout(
              sidebarPanel(
                uiOutput("var6"),
                uiOutput("var7"),
                code(em(h3(textOutput("msg")))),
                
                ),
             
              mainPanel(plotOutput("mp")),
            )),
             
    ), 
    
    
    tabPanel("Univariate Analysis",
             sidebarLayout(
               
               sidebarPanel(
                 
                  uiOutput("var1"),
                  uiOutput("var2"),
                  sliderInput(inputId = "bins",
                              label = "Number of bins in histogram:",
                              min = 1,
                              max = 50,
                              value = 15),
                  sliderInput("x_range", "Range in histogram:",
                              min = 0, max = 1000, value = c(0, 50), step = 10)
                  
               ),
                            
               mainPanel(
                         fluidRow(
                           column(5,plotOutput("his1")),
                           column(5,plotOutput("box1")), ),
                           
                         
                         fluidRow(
                           column(5,offset = 3,plotOutput("bar1") ),
                          )
             
               ),        
    ) ), 
    
    tabPanel("Multivariate Analysis",
             sidebarLayout(
               
               sidebarPanel(
                 
                 uiOutput("var3"),
                 uiOutput("var4"),
                 uiOutput("var5"),
                 
                 sliderInput("x1_range", "x limit:",
                             min = 0, max = 1000, value = c(0, 100), step = 10),
                 sliderInput("x2_range", "y limit:",
                             min = 0, max = 1000, value = c(0,20), step = 10)
               ),
               
               mainPanel(
                 fluidRow(
                   column(5,offset=3,plotOutput("sct")), 
                  ),
                 
                 fluidRow(
                 
                   column(5, plotOutput("bar2") ),
                   column(5,plotOutput("bar3")), ),
                 ),
                 
               ),        
            
           
    ),
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  data <- eventReactive(input$df,{
    
    ext <- tools::file_ext(input$df$name)
    
    if (ext == "rds"){
      data <- as.data.frame(readRDS(input$df$datapath))  
    }
    else if (ext == "xls" || ext == 'xlsx'){
      data <- as.data.frame(readxl::read_excel(input$df$datapath))  
    }
    else{
      tryCatch(
        {
          data <- read.csv(input$df$datapath,
                                         header = input$header,
                                         sep = input$sep,
                                         quote = input$quote)
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )
    }
   
  })
 
  #---------------Overview-----------------
  
  output$varSelect <- renderUI({
    data <- data()
    checkboxGroupInput("show_vars", "Columns in Data set to show:",
                       names(data), selected = names(data))
  }) 
  
 output$dataSet<- renderDataTable({
   data <- data()
  datatable(data[, input$show_vars, drop = FALSE])
  })
    
    output$summary <- renderPrint({
     skim(data())
   })
    
    output$structure <- renderPlot({
      vis_dat(data())
    })
    
    output$correlation <- renderPlot({
      ggpairs(data())
    })
 
#-----------uni variate  ------------------------  
  output$var1 <- renderUI({
    data <- data()
    numericVar <- select_if(data,is.numeric)
    selectInput("x","Select quantitaive variable:",names(numericVar) )
  }) 
  output$var2 <- renderUI({
    data <- data()
    chaVar <- select_if(data,is.character)
    selectInput("y","Select qualitative variable:",names(chaVar) )
  }) 
  
  
  output$his1 <- renderPlot({
    
    data <- data()
    data <- data[,input$x]
    bins <-  seq(min(data),max(data),length.out = input$bins+1)
    hist(data,breaks = bins, xlim = c(input$x_range[1], input$x_range[2]),col="#75AADB",border = "white",main = input$x)
  })

  output$box1 <- renderPlot({
    
    data <- data()
    data <- data[,input$x]
    
    plot(density(data))
    
  })
  
  output$bar1 <- renderPlot({
    
    data <- data()
    data <- data[,input$y]
    df<- data.frame(table(data))
  ggplot(data=df, aes(x=df[,1], y=Freq)) +
    geom_bar(stat="identity",color="blue", fill="white")+
    xlab(input$y) +
    ylab("Frequency") +
    geom_text(aes(label=Freq))+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
  })
  

  #------------Mutivariate ----------------------
  
  output$var3 <- renderUI({
    data <- data()
    numericVar <- select_if(data,is.numeric)
    selectInput("k","Select 1st quantitaive variable:",names(numericVar) )
  })
  
  output$var4 <- renderUI({
    data <- data()
    numericVar <- select_if(data,is.numeric)
    selectInput("l","Select 2nd quantitaive variable:",names(numericVar) )
  })
  
  output$var5 <- renderUI({
    data <- data()
    chaVar <- select_if(data,is.character)
    selectInput("m","Select qualitative variable:",names(chaVar) )
  }) 
  
 
  
  output$sct <- renderPlot({
    data <- data()
    data <- data[,c(input$k,input$l,input$m)]
  
    ggplot(data=data,aes(x=data[,1],y=data[,2],col=data[,3]) )+geom_point() 
    
  })
  output$bar2 <- renderPlot({
    data <- data()
    data <- data[,c(input$k,input$l,input$m)]
    
    ggplot(data,aes_string(x=colnames(data)[colnames(data)==input$m],y=colnames(data)[colnames(data)==input$k])) +geom_col(fill="dark blue")
    
  })
  output$bar3 <- renderPlot({
    data <- data()
    data <- data[,c(input$k,input$l,input$m)]
    
    ggplot(data,aes_string(x=colnames(data)[colnames(data)==input$m],y=colnames(data)[colnames(data)==input$l])) +geom_col(fill="dark green")
    
  })
  
  
  
  #------------- Outliers analysis -----------
  output$aggr <- renderPlot({
    data <- data()
    aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
    
  })
 
  output$var6 <- renderUI({
    data <- data()
    numericVar <- select_if(data,is.numeric)
    selectInput("o","Select 1st quantitaive variable:",names(numericVar) )
  })
  output$var7 <- renderUI({
    data <- data()
    numericVar <- select_if(data,is.numeric)
    selectInput("p","Select 2st quantitaive variable:",names(numericVar) )
  })
  
  output$mp <- renderPlot({
    data <- data()
    data <- data[,c(input$o,input$p)]
    marginplot(data)
  }
  )
  
  output$msg <- renderText({
    data<- data()
    ifelse( sum(is.na(data))>0,"",
    "There is no any missing values in the data set. ")
  })
 
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

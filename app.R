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


# Define UI for application that draws a histogram
ui <- fluidPage( 

  titlePanel("Tool Kit"),
  tabsetPanel(
    tabPanel("Overview", fluidRow(
               column(2,
                      fileInput(
                 "df",
                 label = "Choose the file:  csv. tx, excel or rds",
                 multiple = FALSE,
                 accept = c("text/csv/Excel",
                            "text/comma-separated-values,text/plain/excel",
                            ".csv",".txt",".xls",".xlsx",".rds")),
                 # Horizontal line ----
                 tags$hr(),
                 column(
                   width = 5,
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
                   width = 5,
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
               column(width = 6,em(h4("Data Set")),
                   
                   dataTableOutput("dataSet")),
               column(4,em(h4(" Struacture of the Data Set ")),plotOutput("structure"))),
             
             fluidRow(em(h4("Summary of the Data Set")),
               column(6,verbatimTextOutput("summary")),
               column(6,plotOutput("correlation"))),
             ),
    
    
  tabPanel(" Outlier Analysis",
             
            fluidRow(em(h4("         Aggregations for missing values")) ,column(10,offset = 1,plotOutput("aggr")) ),
           
            fluidRow(sidebarLayout(
              sidebarPanel(em(h4("Select appropriate variables ")),
                
                       uiOutput("var6"),
                       uiOutput("var7"),
                
                
                code(em(h3(textOutput("msg")))),
                
                ),
             
              mainPanel(
                        column(width=10,em(h4("Marginal plot")),plotOutput("mp"))),
            ),
             
    )), 
   
    tabPanel("Univariate Analysis",
             sidebarLayout(
               
               sidebarPanel(
                  uiOutput("var2"),
                  uiOutput("var1"),
                  
                  sliderInput(inputId = "bins",
                              label = em("Number of bins :"),
                              min = 1,
                              max = 50,
                              value = 15),
                  uiOutput("k1"),
                  
               ),
                            
               mainPanel(
                 
                          fluidRow(
                            column(8,offset = 2,plotOutput("bar1") ),
                         fluidRow(
                           column(6,plotOutput("his1")),
                           column(6,plotOutput("box1")), ),
                           
                         
                          )
             
               ),        
    ) ), 
    
    tabPanel("Multivariate Analysis",
             sidebarLayout(
               
               sidebarPanel(
                 
                 uiOutput("var3"),
                 uiOutput("var4"),
                 uiOutput("var5"),
                 
                 uiOutput("k2"),
                 uiOutput("k3")
               ),
               
               mainPanel(
                 fluidRow(
                   column(10,em(h5("Colored Scatter Plot According to Qualitative Variable")),plotOutput("sct")), 
                  ),
                 
                 fluidRow(
                   tabsetPanel(
                     tabPanel("Bar Plots",
                              column(5,
                                     em(h5("Bar for 1st variable")), plotOutput("bar2") ),
                              column(5,em(h5("Bar for 2nd variable")),plotOutput("bar3")), ),
                     tabPanel("Box Plots",
                              column(5,
                                     em(h5("Box for 1st variable")), plotOutput("box2") ),
                              column(5,em(h5("Box for 2nd variable")),plotOutput("box3")),
                              ),
                              )
                   )
                 
                   
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
    checkboxGroupInput("show_vars", "Select Colums in Data Set",
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
  
  output$k1 <- renderUI({
    data <- data()
    df <- data[,input$x]
    max1 <- max(df,na.rm = T) + 3
    min1 <- min(df,na.rm = T) - 3
    
    sliderInput("x_range", em("Select the range to visualize :"),
                min = min1, max = max1, value = c(0, 10), step = 5)
  })
  
  output$his1 <- renderPlot({
    
    data <- data()
    df <- data[,input$x]
    
    ggplot(data,aes(x=df)) + 
      geom_histogram(bins=input$bins,aes(y=..density..),fill="dodgerblue4",color="white") + 
      geom_density(lwd=1.2,linetype=2)+coord_cartesian(xlim = c(input$x_range[1], input$x_range[2]))+labs(title="Histogram with density plot", x=input$x)
  })

  output$box1 <- renderPlot({
    
    data <- data()
    df <- data[,input$x]
    
    ggplot(data,aes(x="",y=df)) + 
      geom_boxplot()+coord_cartesian(ylim = c(input$x_range[1], input$x_range[2]))+labs(title="Box plot", x=input$x)+geom_jitter(color="#56B4E9")
    
  })
  
  output$bar1 <- renderPlot({
    
    data <- data()
    data <- data[,input$y]
    df<- data.frame(table(data))
  ggplot(data=df, aes(x=df[,1], y=Freq,fill=df[,1])) +
    geom_bar(stat="identity")+labs(title="Bar plot", x=input$y,y="Frequency")+guides(col=guide_legend(input$y))+
    geom_text(aes(label=Freq))#+theme(axis.text.x = element_text( hjust = 1, vjust = .5))
  })
  

  #------------Multivariate ----------------------
  
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
  
    ggplot(data=data,aes(x=data[,1],y=data[,2],col=data[,3]) )+geom_point()+labs(title ="",x=input$k,y=input$l) +guides(col=guide_legend(input$m)) +coord_cartesian(xlim = c(input$x1_range[1], input$x1_range[2]),ylim = c(input$x2_range[1], input$x2_range[2]) )
    
  }) 
  
  output$k2 <- renderUI({
    data <- data()
    df <- data[,input$k]
    max2 <- max(df,na.rm = T) + 3
    min2 <- min(df,na.rm = T) - 3
    
    sliderInput("x1_range", "limits of 1st variable:",
                min = min2, max = max2, value = c(0, 10), step = 2)
  })
  
  output$k3 <- renderUI({
    data <- data()
    df <- data[,input$l]
    max3 <- max(df,na.rm = T) + 3
    min3 <- min(df,na.rm = T) - 3
    
    sliderInput("x2_range", "limits of 2nd variable:",
                min = min3, max = max3, value = c(0, 10), step = 2)
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
  
  output$box2 <- renderPlot({
    data <- data()
    df <- data[,c(input$k,input$l,input$m)]
    
    ggplot(data,aes_string(x=input$m,y=input$k)) + 
      geom_boxplot()+coord_cartesian(ylim = c(input$x1_range[1], input$x1_range[2]))+labs( y=input$k,x=input$m)+geom_jitter(color="#56B4E9")
    
  })
  output$box3 <- renderPlot({
    data <- data()
    data <- data[,c(input$k,input$l,input$m)]
    
    ggplot(data,aes_string(x=input$m,y=input$l)) + 
      geom_boxplot()+coord_cartesian(ylim = c(input$x2_range[1], input$x2_range[2]))+labs( y=input$l,x=input$m)+geom_jitter(color=3)
    
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
    selectInput("p","Select 2nd quantitaive variable:",names(numericVar) )
  })
  
  output$mp <- renderPlot({
    data <- data()
    data <- data[,c(input$o,input$p)]
    marginplot(data,alpha = 0.8)
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

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Linear Regression in R"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Data Options",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            actionButton("lm", "Linear Fit"),
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),
            plotOutput("fitPlot"),
            htmlOutput("RSquared"),
            htmlOutput("Slope"),
            htmlOutput("Intercept"),
            tableOutput("contents")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    dataInput <- reactive({
        req(input$file1)
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })
    
    Line <- eventReactive(input$lm,{lm(dataInput()$y ~ dataInput()$x)})
    
    output$distPlot <- renderPlot({
        plot(dataInput()$x, dataInput()$y, main="Scatter Plot",xlab="X", ylab="Y", pch=19)
    })
    
    output$fitPlot <- renderPlot({
        plot(dataInput()$x, dataInput()$y, main="Linear Regression",xlab="X", ylab="Y", pch=19)
        abline(Line())
    })
   
    output$RSquared <- renderUI({
        str1 <- paste("R", tags$sup(2),"-", sep = "")
        str2 <- paste(summary(Line())$coefficients[1])
        HTML(paste(str1, str2))
    })
    output$Slope <- renderUI({
        str1 <- paste("Slope-")
        str2 <- paste(summary(Line())$coefficients[2])
        HTML(paste(str1, str2))
    })
    
    output$Intercept <- renderUI({
        str1 <- paste("Intercept-")
        str2 <- paste(summary(Line())$coefficients[1])
        HTML(paste(str1, str2))
    })
    
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        if(input$disp == "head") {
            return(head(dataInput()))
        }
        else {
            return(dataInput())
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


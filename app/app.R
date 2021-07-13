# Create Shiny app ----
library(shiny)
library(ggplot2)
# Defining UI for the application 
ui <- fluidPage(
    titlePanel("Linear Regression"),      # Title of the app output
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            sliderInput("Poly",
                        "Polynomial Fitting Order:",
                        min = 1,
                        max = 10,
                        value = 1),
            
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
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head")
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            # Output scatter Plot with regression
            plotOutput("scatterPlot"),
            tableOutput("contents")
        )
    )
)

# Define server logic to read selected file
# Logic for output of a scatter plot with linear regression from a csv input

server <- function(input, output) {
    output$contents <- renderTable({
        req(input$file1)
        
        # when reading semicolon separated files,
        # having a comma separator causes `read.csv` to error
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep = input$sep,
                               quote = input$quote)
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        
        if(input$disp == "head") {
            return(head(df))
        }
        else {
            return(df)
        }
    })
    
    dat <- reactive({
        df <- read.csv(input$file1$datapath,header = input$header,sep = input$sep,quote = input$quote)
        print(df)
        df
    })
    
    output$scatterPlot <-renderPlot({
        ggplot(dat(),aes(x=x,y=y))+geom_point(colour='red')})
}
# Run the application 
shinyApp(ui = ui, server = server)

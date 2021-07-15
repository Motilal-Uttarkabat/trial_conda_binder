# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(ggpmisc)
library(plotly)

# Defining UI for the application
# The app uses code (slightly modified) for reading a csv to shiny app from Source- https://shiny.rstudio.com/gallery/file-upload.html

ui <- fluidPage(
    titlePanel("Polynomial Regression"),      # Title of the app output
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            sliderInput("Poly",
                        "Order of polynomial model:",
                        min = 0,
                        max = 10,
                        value = 0),
            
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
            checkboxInput("se", "Confidence Interval", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            tags$hr(),
            textOutput("Export"),
            numericInput("width", "Width (inches)", value = 7, min = 1, max = 20),
            numericInput("height", "Height (inches)", value = 7, min = 1, max = 20),
            numericInput("dpi", "DPI", value = 150, min = 72, max = 300),
            downloadButton('Download'),
            actionButton("regex", "Regression"),
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            # Output scatter Plot with regression
            plotlyOutput("scatterPlot"),
            htmlOutput("RSquared"),
            htmlOutput("Slope"),
            htmlOutput("Intercept"),
            tableOutput("contents")
        )
    )
)


# Define server logic to read selected file
# Logic for output of a scatter plot with linear regression from a csv input
server <- function(input, output) {

    # Parsing data from csv to a Reactive dat() for plotting
    dat <- reactive({
        req(input$file1)
        df <- read.csv(input$file1$datapath,header = input$header,sep = input$sep)
        df
    })
    
    # Event reactive dataframe from csv input
    Line <- eventReactive(input$regex,{lm(dat()$y ~ poly(dat()$x, input$Poly, raw = T))})
    

    # Scatter plot with regressional analysis using stat_smooth (ggplot2) and stat_poly_eq (from ggpmisc)
    output$scatterPlot <- renderPlotly(
        ggplotly(figure())
    )
    
    figure <- reactive ({
        ggplot(dat(),aes(x=x,y=y))+geom_point(colour='red')+stat_smooth(                                    # Scatter Plot 
            method = "lm",                                                                                  # Fitting/smoothing to a polynomial fit
            formula = y ~ poly(x, input$Poly, raw = T),                                                     # Formula for the polynomial fit from input
            size = 1,                                                                                       # Line width of the fit
            se=input$se)
    })
    

    output$RSquared <- renderUI({
        str1 <- paste("R", tags$sup(2),"-", sep = "")
        str2 <- paste(format(round(summary(Line())$r.squared, 3)))
        HTML(paste(str1, str2))
    })
    output$Slope <- renderUI({
        str1 <- paste("Slope-")
        str2 <- paste(format(round(summary(Line())$coefficients[2], 3)))
        HTML(paste(str1, str2))
    })
    
    output$Intercept <- renderUI({
        str1 <- paste("Intercept-")
        str2 <- paste(format(round(summary(Line())$coefficients[1], 3)))
        HTML(paste(str1, str2))
    })
    
    output$Download <- downloadHandler(
        filename = function() { paste(tools::file_path_sans_ext(input$file1), 'order', input$Poly, '.png', sep='_') },
        content = function(file) {
            ggsave(file, plot = figure(), width = input$width, height = input$height, dpi = input$dpi, units = "in", device = "png")
        })
    
    output$Export <- renderText(
        paste("Download")
    )
    
    output$contents <- renderTable({
        if(input$disp == "head") {
            return(head(dat()))
        }
        else {
            return(dat())
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
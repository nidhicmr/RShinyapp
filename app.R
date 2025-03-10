# Load required libraries
library(shiny)
library(ggplot2)
library(forecast)
library(tseries)

# Define UI with multiple tabs using navbarPage
ui <- navbarPage("Time Series Analysis App - Shinyway",
                 
                 # 1. Data Upload & Preprocessing tab
                 tabPanel("Data Upload",
                          sidebarLayout(
                            sidebarPanel(
                              fileInput("file", "Upload CSV File", accept = c(".csv")),
                              uiOutput("col_select"),  # Dynamically generated column selection
                              numericInput("start_year", "Start Year:", value = 2000),
                              numericInput("frequency", "Frequency:", value = 12)  # e.g., monthly data
                            ),
                            mainPanel(
                              h4("Data Preview"),
                              tableOutput("data_head"),
                              verbatimTextOutput("data_summary")
                            )
                          )
                 ),
                 
                 # 2. Graphical Analysis tab
                 tabPanel("Graphical Analysis",
                          sidebarLayout(
                            sidebarPanel(
                              checkboxInput("show_trend", "Show Trend", value = TRUE),
                              checkboxInput("show_season", "Show Seasonality", value = TRUE)
                            ),
                            mainPanel(
                              plotOutput("tsPlot"),
                              plotOutput("acfPlot"),
                              plotOutput("pacfPlot")
                            )
                          )
                 ),
                 
                 # 3. Model Implementation tab
                 tabPanel("Model Implementation",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("model_type", "Model Type:", 
                                           choices = c("Auto ARIMA" = "auto", "Manual ARIMA" = "manual")),
                              # Only show manual options when Manual ARIMA is selected
                              conditionalPanel(
                                condition = "input.model_type == 'manual'",
                                numericInput("p", "AR order (p):", value = 1, min = 0),
                                numericInput("d", "Differencing (d):", value = 0, min = 0),
                                numericInput("q", "MA order (q):", value = 1, min = 0)
                              ),
                              actionButton("fit_model", "Fit Model")
                            ),
                            mainPanel(
                              h4("Model Summary"),
                              verbatimTextOutput("model_summary")
                            )
                          )
                 ),
                 
                 # 4. Diagnostics & Download tab
                 tabPanel("Diagnostics & Download",
                          sidebarLayout(
                            sidebarPanel(
                              downloadButton("downloadModel", "Download Model Summary")
                            ),
                            mainPanel(
                              h4("Residuals Diagnostic"),
                              plotOutput("residualPlot"),
                              plotOutput("residualAcfPlot")
                            )
                          )
                 ),
                 
                 # 5. User Guide tab
                 tabPanel("User Guide",
                          fluidPage(
                            h3("User Guide"),
                            p("1. Data Upload & Preprocessing: Upload your CSV file and select the time series column. The app omits missing values and converts the data into a time series object."),
                            p("2. Graphical Analysis: Visualize your data using a time series plot along with ACF and PACF plots. Use the checkboxes to explore trend and seasonality options."),
                            p("3. Model Implementation: Fit an ARIMA model. Choose 'Auto ARIMA' for automatic selection or 'Manual ARIMA' to set parameters (p, d, q) yourself."),
                            p("4. Diagnostics & Download: View residual diagnostic plots and download the model summary if needed."),
                            p("5. Final Integration: All these modules are integrated into a single working app for streamlined analysis.")
                          )
                 )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive expression to read the uploaded CSV file
  dataInput <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath, header = TRUE, stringsAsFactors = FALSE)
    df
  })
  
  # Dynamically generate the column selection input after data is uploaded
  output$col_select <- renderUI({
    df <- dataInput()
    selectInput("selected_col", "Select Time Series Column", choices = names(df))
  })
  
  # Show the first few rows of the data
  output$data_head <- renderTable({
    head(dataInput())
  })
  
  # Show a summary of the data
  output$data_summary <- renderPrint({
    summary(dataInput())
  })
  
  # Create a reactive time series object from the selected column,
  # handling missing values by omitting them
  tsData <- reactive({
    req(input$selected_col)
    df <- dataInput()
    ts_vector <- na.omit(df[[input$selected_col]])
    # Create a time series object using user-specified start year and frequency
    ts_obj <- ts(ts_vector, start = input$start_year, frequency = input$frequency)
    ts_obj
  })
  
  # Plot the time series using autoplot (from the forecast package)
  output$tsPlot <- renderPlot({
    req(tsData())
    autoplot(tsData()) +
      ggtitle("Time Series Plot") +
      xlab("Time") +
      ylab(input$selected_col)
  })
  
  # Plot the Autocorrelation Function (ACF)
  output$acfPlot <- renderPlot({
    req(tsData())
    acf(tsData(), main = "ACF Plot")
  })
  
  # Plot the Partial Autocorrelation Function (PACF)
  output$pacfPlot <- renderPlot({
    req(tsData())
    pacf(tsData(), main = "PACF Plot")
  })
  
  # Reactive value to store the fitted ARIMA model
  fittedModel <- reactiveVal(NULL)
  
  # Fit the model when the "Fit Model" button is pressed
  observeEvent(input$fit_model, {
    req(tsData())
    if (input$model_type == "auto") {
      model <- auto.arima(tsData())
    } else {
      model <- tryCatch({
        arima(tsData(), order = c(input$p, input$d, input$q))
      }, error = function(e) {
        showNotification("Error fitting model: Please check your parameters.", type = "error")
        return(NULL)
      })
    }
    fittedModel(model)
  })
  
  # Display the summary of the fitted model
  output$model_summary <- renderPrint({
    req(fittedModel())
    summary(fittedModel())
  })
  
  # Plot the residuals of the fitted model
  output$residualPlot <- renderPlot({
    req(fittedModel())
    plot(residuals(fittedModel()), main = "Residuals", ylab = "Residuals", xlab = "Time")
    abline(h = 0, col = "red", lty = 2)
  })
  
  # Plot the ACF of the model residuals
  output$residualAcfPlot <- renderPlot({
    req(fittedModel())
    acf(residuals(fittedModel()), main = "ACF of Residuals")
  })
  
  # Download handler to export the model summary as a text file
  output$downloadModel <- downloadHandler(
    filename = function() {
      paste("model_summary", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      req(fittedModel())
      sink(file)
      print(summary(fittedModel()))
      sink()
    }
  )
}

# Run the Shiny app
shinyApp(ui, server)


# Run the application 
shinyApp(ui = ui, server = server)

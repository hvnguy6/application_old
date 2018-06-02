source("global.R")
source("volcharting.R")

library(shiny)

############################################################
### UI
############################################################


titleText <- "Stock Dashboard Demo"

welcomeText <- "Welcome! Please select the dates and stock you want to track"

ui <- fluidPage(
        tags$head(
        tags$style(HTML('#update{background-color:#4885ed; color:white; font-weight: bold}',
                      '#downloadRaw {background-color:#db3236; color:white; font-weight: bold;}')
                )
        ),
   
        titlePanel(titleText),
        
        sidebarLayout(
         sidebarPanel(width = 3,
                     welcomeText,
                     br(),
                     br(),
                     uiOutput("DateSelector"),
                     br(),
                     uiOutput("StockIndicator"),
                     actionButton(inputId = "update", label = "Load Data",
                                icon = icon("paper-plane"), width = '178'),
                     br(),
                     "to be added ,25 days, 90 days, summary table..",
                     br(),
                     br(),
                     downloadButton('downloadRaw', 'Download Raw Data', width = '185'),
                     br()
           
                     ),
        
        # Show a plot of the generated distribution
        mainPanel(width = 9, 
                  tabsetPanel(
                          tabPanel("Selected days", 
                                   splitLayout(cellWidths = c("50%", "50%"), 
                                               plotOutput(outputId = "distPlot_daily_return", height = "200px"),
                                               plotOutput(outputId = "distPlot_daily_volume", height = "200px")
                                               ),
                                   br(),
                                   highchartOutput(outputId = 'Plot_volume_adjustedPrice', height = "350px"),
                                   br(),
                                   verbatimTextOutput(outputId = 'summary')
                                   ),
                          
                           
                        
                          tabPanel("90 days", 
                             plotOutput("distPlot_daily_return1"),
                             plotOutput("distPlot_daily_volume1"),
                              br()
                                  ),
                          
                          tabPanel("25 days", 
                                   plotOutput("distPlot_daily_return2"),
                                   plotOutput("distPlot_daily_volume2"),
                                   br()
                          ),
                          
                          tabPanel("All time trend", 
                                   highchartOutput("Plot_all_time", height = "550px")
                          ),
                          
                          tabPanel("Options", 
                                   verbatimTextOutput(outputId = 'summary_options'),
                                   highchartOutput("distPlot_option_1"),
                                   br(),
                                   highchartOutput("distPlot_option_2"),
                                   #highchartOutput("distPlot_option_3"),
                                   br()
                          ),
                          
                          tabPanel("UVXY", 
                                   dygraphOutput("SPX_VS_graph", height = "300px"),
                                   br(),
                                   dygraphOutput("UVXY_graph", height = "250px")
                          )
                          
                             )
                  
      )
   )
)



############################################################
### Server
############################################################

server <- function(input, output) {
        
        # Select Date Range
        output$DateSelector <- renderUI({
                dateRangeInput(label = 'Date Range:',
                               inputId = 'MySelectedDateRange',
                               start = Sys.Date() - 365, end = Sys.Date(),
                               min = '2016-01-01', max = Sys.Date()
                               )
        })
        
        # Select Stock
        output$StockIndicator <- renderUI({
                selectInput(inputId = 'SelectedStock', label = 'Select Stock', choices = SymbolLists,
                            selected = 'WB', multiple = F )
        })
        
        
        my.current.raw.data <- eventReactive(
                input$update, {stock.raw.data(
                        some.stock = input$SelectedStock,
                        some.start.date = input$MySelectedDateRange[1],
                        some.end.date = input$MySelectedDateRange[2])
                              }
        )
        
        
        # Daily Return
        my.current.dist_daily_return.data <- reactive({
                data.dist_daily_return(some.raw.data = my.current.raw.data())
        })
        
        
        my.current.dist_daily_return.graph <- reactive({
                graph.dist_daily_return(some.graph.data = my.current.dist_daily_return.data())
        })
        
        
        output$distPlot_daily_return <- renderPlot({
                my.current.dist_daily_return.graph()
        })
        
        
        # Daily Volume
        my.current.dist_daily_volume.data <- reactive({
                data.dist_daily_volume(some.raw.data = my.current.raw.data())
        })
        
        
        my.current.dist_daily_volume.graph <- reactive({
                graph.dist_daily_volume(some.graph.data = my.current.dist_daily_volume.data())
        })
        
        
        output$distPlot_daily_volume <- renderPlot({
                my.current.dist_daily_volume.graph()
        })
        
        
        # Volume and Adjusted price
        my.current.volume_adjustedPrice.graph <- reactive({
                graph.volume_adjustedPrice(some.raw.data = my.current.raw.data())
        })
        
        
        output$Plot_volume_adjustedPrice <- renderHighchart({
                my.current.volume_adjustedPrice.graph()
        })
        
        
        # All time
        my.current.all_time.graph <- reactive({
                graph.all_time(some.stock = input$SelectedStock)
        })
        
        output$Plot_all_time <- renderHighchart({
                my.current.all_time.graph()
        })
        
        
        
        # Summary Table on Selected days
        my.curent.summary.data <- reactive({
                data.stats_summary(some.raw.data = my.current.raw.data())
        })
        
        output$summary <- renderPrint({
                my.curent.summary.data()
        })
        
        
        
        # Download
        output$downloadRaw <- downloadHandler(
                filename = function() {paste0(my.stock, ' Raw Data', '.csv')},
                content = function(file) {write.csv(my.current.raw.data(), file)}
        )
        
        
        
        
        # Options - Summary
        my.curent.options.summary.data <- reactive({
                data.options_summary(my.current.raw.data())
        })
        
        output$summary_options <- renderPrint({
                summary(my.curent.options.summary.data())
        })
        
        
        # Options - Plot 1
        my.current.options_distPlot1.graph <- reactive({
                graph.options_distPlot1(some.raw.data = my.current.raw.data())
        })
        
        
        output$distPlot_option_1 <- renderHighchart({
                my.current.options_distPlot1.graph()
        })
        
        
        # Options - Plot 2
        my.current.options_distPlot2.graph <- reactive({
                graph.options_distPlot1(some.raw.data = my.current.raw.data())
        })
        
        
        output$distPlot_option_2 <- renderHighchart({
                my.current.options_distPlot1.graph()
        })
        
        

        # UVXY -- SPX_VS_graph
        my.current.SPX_VS_graph <- reactive({
                SPX_VS_graph()
        })
        
        output$SPX_VS_graph <- renderDygraph({
                my.current.SPX_VS_graph()
        })
        
        # UVXY -- UVXY_graph
        my.current.UVXY_graph <- reactive({
                UVXY_graph()
        })
        
        output$UVXY_graph <-  renderDygraph({
                my.current.UVXY_graph()
        })
        
}

# Run the application 
shinyApp(ui = ui, server = server)


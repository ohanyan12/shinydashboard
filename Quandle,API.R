
library("ggplot2")
library("shiny")
library("shinythemes")
library("plotly")
library("Quandl")
library("shinydashboard")

#Task 1 & 2


tickr_list <- list("Agilent " = "A", "Apple" = "AAPL","Cisco "="CSCO")
time_series_freq <- list("daily", "weekly", "monthly")

Line_plot <- function(ticker_l, collapse_f_l, date_l){
  Quandl_input_l <- paste0("WIKI/",ticker_l)
  df_l <- Quandl(Quandl_input_l, collapse = collapse_f_l)
  df_l <- subset(df_l, Date >= date_l, select = c("Date", "Open"))
  plot_l <- ggplot(data = df_l, aes(x = Date, y = Open, color = "red"))+geom_line()+theme_classic()+
    labs(x= "Date", y="Stock opening price", title = "Stock opening price over time")
  plotly_l <- ggplotly(plot_l)
  return(plotly_l)
}


Scatter_plot <- function(ticker_s, collapse_f_s, date_s){
  Quandl_input_s <- paste0("WIKI/",ticker_s)
  df_s <- Quandl(Quandl_input_s, collapse = collapse_f_s)
  df_s <- subset(df_s, Date >= date_s, select = c("Date", "Open"))
  plot_s <- ggplot(data = df_s, aes(x=Date, y = Open))+geom_point(shape = 16, color = "red")+
    geom_smooth(method=lm, color = "grey")+theme_gray()+
    labs(x= "Date", y="Stock opening price", title = "Stock opening price over time")
  plotly_s <- ggplotly(plot_s)
  return(plotly_s)
}

Area_plot <- function(ticker_a, collapse_f_a, date_a){
  Quandl_input_a <- paste0("WIKI/",ticker_a)
  df_a <- Quandl(Quandl_input_a, collapse = collapse_f_a)
  df_a <- subset(df_a, Date >= date_a, select = c("Date", "Open"))
  plot_a <- ggplot(data = df_a, aes(x = Date, y = Open))+geom_area(fill = "lightgrey", color = "darkblue")+theme_classic()+
    labs(x= "Date", y="Stock opening price", title = "Stock opening price over time")
  plotly_a <- ggplotly(plot_a)
  return(plotly_a)
}



ui <- navbarPage("Shiny practice by Ohanyan",theme=shinytheme("united"),
                 tabPanel("Question 1",
                          sidebarLayout(
                            sidebarPanel(
                              "Question 1:Plot & Boxplot,"
                            ), 
                            mainPanel("Violin plot is a powerful data visualization technique since it allows to compare both the ranking of several groups and their distribution. Violin plot are made vertically most of the time. In terms of long labels, building an horizontal version make the labels more readable. We can build a grouped violin. We can order groups by median value, which will make the chart more insightful. Violins are particularly adapted when the amount of data is huge and showing individual observations gets impossible. On the other hand for small datasets, a boxplot with jitter is better option since it really shows all the information. A boxplot gives a nice summary of one or several numeric variables. Boxplot can summarize only the distribution of a numeric variable for several groups. Here, the problem is that summarizing also means losing information, and that can be a pitfall.")),
                          
                          
                          sidebarLayout(
                            sidebarPanel(
                              "Question 2:Error bars,main applications"
                            ),
                            mainPanel("Error Bars help to indicate estimated error or uncertainty to give a general sense of how precise a measurement is. This is done through the use of markers drawn over the original graph and its data points. Typically, Error bars are used to display either the standard deviation, standard error, confidence intervals or the minimum and maximum values in a ranged dataset.")),
                          
                          sidebarLayout(
                            sidebarPanel(
                              "Question 3 : Error bars,main applications"
                            ),
                            mainPanel("Parallel Coordinates Plot is used for plotting multivariate, numerical data. Parallel Coordinates Plots are ideal for comparing many variables together and seeing the relationships between them. For example, if we had to compare an array of products with the same attributes.  In a Parallel Coordinates Plot, each variable is given its own axis and all the axes are placed in parallel to each other. Each axis can have a different scale, as each variable works off a different unit of measurement, or all the axes can be normalized to keep all the scales uniform. Values are plotted as a series of lines that connected across all the axes. This means that each line is a collection of points placed on each axis, that have all been connected together."))
                 ),
                 tabPanel("Question 2",
                          
                          fluidRow(
                            column(4,
                                   wellPanel(selectInput("stock_list", h4("Please, choose the stock name"),choices = tickr_list)
                                   )),
                            column(4,
                                   wellPanel(dateInput("start_date", h4("Select the date from which the data will be provided"), 
                                                       format = "yyyy-mm-dd", value = "2016-01-01")
                                   )),
                            column(4,
                                   wellPanel(radioButtons("freq", h4("Choose the frequency of time series"), choices = time_series_freq, 
                                                          selected = "weekly")
                                   ))
                          ),
                          fluidPage(
                            fluidRow(
                              column(4,
                                     wellPanel(plotlyOutput("Stock_opening_price_line"))
                              ),
                              column(4,
                                     wellPanel(plotlyOutput("Stock_opening_price_scatter"))
                              ),
                              column(4,
                                     wellPanel(plotlyOutput("Stock_opening_price_area"))
                              )
                            )
                          )
                 )
                 )

 
server <- function(input, output){
  output$Stock_opening_price_line <- renderPlotly(Line_plot(input$stock_list, input$freq, input$start_date))
  output$Stock_opening_price_scatter <- renderPlotly(Scatter_plot(input$stock_list, input$freq, input$start_date))
  output$Stock_opening_price_area <- renderPlotly(Area_plot(input$stock_list, input$freq, input$start_date))
}


shinyApp(ui = ui, server = server)



#Task 3
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      selectInput("stock_list", h4("Please, choose the stock name"),choices = tickr_list),
      dateInput("start_date", h4("Select the date from which the data will be provided"), format = "yyyy-mm-dd", value = "2016-01-01"),
      radioButtons("freq", h4("Choose the frequency of time series"), choices = time_series_freq, selected = "weekly")
    )
  ),
  dashboardBody(
    box(collapsible = TRUE, plotlyOutput("Stock_opening_price_line")),
    box(collapsible = TRUE, plotlyOutput("Stock_opening_price_scatter")),
    box(collapsible = TRUE, plotlyOutput("Stock_opening_price_area"))
  )
)


server <- function(input,output){
  output$Stock_opening_price_line <- renderPlotly(Line_plot(input$stock_list, input$freq, input$start_date))
  output$Stock_opening_price_scatter <- renderPlotly(Scatter_plot(input$stock_list, input$freq, input$start_date))
  output$Stock_opening_price_area <- renderPlotly(Area_plot(input$stock_list, input$freq, input$start_date))
}


shinyApp(ui=ui, server = server)



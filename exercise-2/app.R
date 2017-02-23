# Load the shiny, ggplot2, and dplyr libraries
library(shiny)
library(ggplot2)
library(dplyr)
# You will once again be working with the `diamonds` data set provided by ggplot2
# Use dplyr's `sample_n()` function to get a random 3000 rows from the data set
# Store this sample in a variable `diamonds.sample`
diamonds.sample <- sample_n(diamonds, 3000)

# For convenience store the `range()` of values for the `price` and `carat` values
# for the ENTIRE diamonds dataset.
price.range <- range(diamonds$price)
carat.range <- range(diamonds$carat)

ui <- fluidPage(
  titlePanel("Diamond Viewer"),
    sidebarLayout(
      sidebarPanel(
        sliderInput('price', label = "Price (in dollars)", min = price.range[1], max = price.range[2], value = price.range),
        sliderInput('carat', label = "Carats", min = carat.range[1], max = carat.range[2], value = carat.range ),
        checkboxInput('trendLine', label = "Show Trendline", value = TRUE),
        selectInput('facet_by', label = "Facet by", choices = c("cut", "clarity", "color"))
      ),
      mainPanel(
        plotOutput('plot'),
        dataTableOutput('table')
      )
    )
)

server <- function(input, output) {
  
  filtered <- reactive({
    new.data <- diamonds.sample %>% 
      filter(price > input$price[1] & price < input$price[2]) %>% 
      filter(carat > input$carat[1] & carat < input$carat[2])
    return(new.data)
  })
  # Assign a reactive `renderPlot()` function to the outputted `plot`
  output$plot <- renderPlot({
    
    plot <- ggplot(data = filtered(), mapping = aes(x = carat, y = price, color = cut)) +
      geom_point() +
      facet_wrap(input$facet_by)
      
    if(input$trendLine) {
      plot <- plot + geom_smooth(se= FALSE)
    }
    return(plot)
  })
   
  output$table <- renderDataTable({
    return(filtered())
  })
}

shinyApp(ui, server)
  # Bonus: Assign a reactive `renderDataTable()` function to the outputted table
  # You may want to use a `reactive()` variable to avoid needing to filter the data twice!


# Create a new `shinyApp()` using the above ui and server


## Double Bonus: For fun, can you make a similar browser for the `mpg` data set?
## it makes the bonus data table a lot more useful

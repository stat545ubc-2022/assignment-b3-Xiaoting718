library(shiny)
library(ggplot2)
library(dplyr)

# load data
bcl <- read.csv("www/bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("BC Liquor Store prices"),
  sidebarLayout(
    sidebarPanel(

      # Add new UI widget to display BC Liquor Logo
      img(src = "BCLiqourStores.jpg", width = "100%"),

      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      radioButtons("typeInput", "Product type",
                   choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                   selected = "WINE"),
      uiOutput("countryOutput"),

      # Add new UI widget to sort result by price
      checkboxInput("sortByPrice", "Sort By Price(ascending)")
    ),
    mainPanel(

      # add a tab selection panel for plot and table
      tabsetPanel(
        type = "tabs",
        tabPanel("Alcohol Count Hist Plot",
                 plotOutput("coolplot")),

        # Use DT to enable interactive table
        tabPanel("Data Details",
                 DT::dataTableOutput("results"))
      )
    )
  )
)

server <- function(input, output) {
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })

  filtered <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }

    temp <- bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput
      )

    # functional change for sorting by price
    if (input$sortByPrice) {
      temp %>% arrange(Price)
    } else {
      temp
    }

  })

  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(Alcohol_Content)) +
      geom_histogram()
  })

  # functional change for interactive table
  output$results <- DT::renderDataTable({
    filtered()
  })
}

shinyApp(ui = ui, server = server)

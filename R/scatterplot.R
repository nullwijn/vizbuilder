#' Visual Builder: Scatter plot
#'
#' This function takes a data frame and creates a shiny web app
#' for you to create a scatter plot with.
#' There are four customizable elements: x-axis, y-axis, color, and size
#' @param df The data frame
#' @keywords scatter plot
#' @export

scatterplot <- function(df){
  #Create the UI
  ui <- shiny::fluidPage(
    # Create a basic panel and sidebar with dropdown input selections
    shiny::titlePanel("Customize a scatterplot based on the data"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput(inputId = "x", label = "X-axis", choices = colnames(df)),
        shiny::selectInput(inputId = "y", label = "Y-axis", choices = colnames(df)),
        shiny::selectInput(inputId = "color", label = "Color", choices = colnames(df)),
        shiny::selectInput(inputId = "size", label = "Size", choices = colnames(df))
      ),
      shiny::mainPanel(
        plotly::plotlyOutput(outputId = "scatterplot")
      )
    )
  )
  #Create the server
  server <- function(input, output) {
    # Create a reactive data frame to catch the inputs and put them into the plot
    data <- reactive({
      df[, c(input$x, input$y, input$color, input$size)]
    })
    # Create the scatterplot using ggplot and plotly
    # Input variables are passed to the server as strings in Shiny, so I need to use !!as.name to grab the actual value
    output$scatterplot <- plotly::renderPlotly({
      ggplot2::ggplot(data(), ggplot2::aes(x = !!as.name(input$x), y = !!as.name(input$y), color = !!as.name(input$color), size = !!as.name(input$size))) +
        ggplot2::geom_point() +
        ggplot2::labs(x = input$x, y = input$y, color = input$color, size = input$size) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) #Make x-axis values vertical so that they are legible
    })
  }
  #Finally, deploy the app
  shiny::shinyApp(ui = ui, server = server)
}

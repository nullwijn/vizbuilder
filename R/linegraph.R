#' Visual Builder: Line Graph
#'
#' This function takes a data frame and creates a shiny web app
#' for you to create a lien graph with.
#' There are three customizable elements: x-axis, y-axis, and 2 colors
#' In this function, colors are predetermined for a gradient
#' @param df The data frame
#' @keywords scatter plot
#' @export

linegraph <- function(df){
  #Create the UI
  ui <- shiny::fluidPage(
    # Create a basic panel and sidebar with dropdown input selections
    shiny::titlePanel("Customize a scatterplot based on the data"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput(inputId = "x", label = "X-axis", choices = colnames(df)),
        shiny::selectInput(inputId = "y", label = "Y-axis", choices = colnames(df)),
        shiny::selectInput(inputId = "color1", label = "Color", choices = c("green","red","blue","black","grey"))
      ),
      shiny::mainPanel(
        plotly::plotlyOutput(outputId = "linegraph")
      )
    )
  )
  #Create the server
  server <- function(input, output) {
    # Create a reactive data frame to catch the inputs and put them into the plot
    data <- reactive({
      df[, c(input$x, input$y, input$color)]
    })
    # Create the scatterplot using ggplot and plotly
    # Input variables are passed to the server as strings in Shiny, so I need to use !!as.name to grab the actual value
    output$linegraph <- plotly::renderPlotly({
      ggplot2::ggplot(data(), ggplot2::aes(x = !!as.name(input$x), y = !!as.name(input$y), color = input$y)) +
        ggplot2::geom_line() +
        ggplot2::labs(x = input$x, y = input$y) +
        ggplot2::scale_color_manual(values = input$color1) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) #Make x-axis values vertical so that they are legible
    })
  }
  #Finally, deploy the app
  shiny::shinyApp(ui = ui, server = server)
}

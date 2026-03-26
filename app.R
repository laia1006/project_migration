library(shiny)
library(ggplot2)
library(plotly)

ui <- fluidPage(
  
  titlePanel("Heatmap of mtDNA haplogroups per location"),
  
  mainPanel(
    plotlyOutput("heatmap_plot")
  )
)

server <- function(input, output) {
  
  output$heatmap_plot <- renderPlotly({
    
    p <- ggplot(data_without_na, 
                aes(Location, `mtDNA haplogroup`, fill = Culture)) +
      geom_tile(color = "white") +
      labs(title = "Heatmap of mtDNA haplogroup per location",
           x = "Location",
           y = "mtDNA haplogroup",
           fill = "Culture") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_viridis_d(option = "turbo")
    
    ggplotly(p)
  })
}

shinyApp(ui, server)

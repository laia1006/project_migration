library(shiny)
library(ggplot2)
library(plotly)
library(readr)
library(dplyr)

data <- read_csv2(
  "table_s1.csv",      
  skip = 1,           # skip the first line which is a description
  locale = locale(decimal_mark = ",") # interpret comma as decimal
)
data_subset <- data %>%
  select(
    Culture,
    Location,
    `mtDNA haplogroup`,
    `Equivalent mg bone powder used for library preparation`,
    `Shotgun raw sequences`,
    `mtDNA coverage`,
    `mtDNA fraction damaged in last base`
  )
data_without_na <- na.omit(data_subset)
data_without_na$`Equivalent mg bone powder used for library preparation` <- as.numeric(as.character(data_without_na$'Equivalent mg bone powder used for library preparation'))

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

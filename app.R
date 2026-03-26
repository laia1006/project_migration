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


# Bone Powder distribution by location
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)

data <- read_csv2(
  "table_s1.csv",
  skip = 1,
  locale = locale(decimal_mark = ",")
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

data_without_na$`Equivalent mg bone powder used for library preparation` <- 
  as.numeric(as.character(
    data_without_na$`Equivalent mg bone powder used for library preparation`
  ))


ui <- fluidPage(
  
  titlePanel("Bone Powder distribution by location"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        "location_select",
        "Select Location(s):",
        choices = unique(data_without_na$Location),
        selected = unique(data_without_na$Location),
        multiple = TRUE
      ),
      
      sliderInput(
        "bins",
        "Number of bins:",
        min = 10,
        max = 50,
        value = 30
      )
      
    ),
    
    mainPanel(
      plotOutput("hist_plot")
    )
    
  )
)


server <- function(input, output) {
  
  output$hist_plot <- renderPlot({
    
    filtered_data <- data_without_na[
      data_without_na$Location %in% input$location_select, 
    ]
    
    ggplot(filtered_data, aes(
      x = `Equivalent mg bone powder used for library preparation`
    )) +
      geom_histogram(bins = input$bins, fill = "steelblue") +
      facet_wrap(~ Location) +
      labs(
        title = "Distribution of Bone Powder Used for Library Preparation",
        subtitle = "Filtered by Selected Location(s)",
        x = "Equivalent mg of Bone Powder",
        y = "Count"
      ) +
      theme_minimal()
    
  })
  
}


shinyApp(ui = ui, server = server)


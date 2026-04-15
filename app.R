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
    
    ggplotly(p)  %>% 
    config(displayModeBar = FALSE)
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


# Last dynamic plot (comparing bone powder with culture)

data_without_na$Culture <- as.character(data_without_na$Culture)
data_without_na$`Equivalent mg bone powder used for library preparation` <- 
  as.numeric(as.character(data_without_na$`Equivalent mg bone powder used for library preparation`))

ui <- fluidPage(
  titlePanel("Bone powder usage by culture"),
  
  p("Authors: Carla Domingo, Mah Noor Fatima, Laia Zamora, Carla Zurita and Fatima Zahrae El Yaagoubi"),
  
  p("This plot shows the distribution of bone powder used for library preparation across different cultures. 
    The boxes represent the interquartile range, and the points show individual samples. Colors indicate the sample location."),
      
  sidebarLayout( 
    sidebarPanel( 
      selectInput("cultureFilter", "Select Culture:",
                  choices = unique(data_without_na$Culture),
                  selected = unique(data_without_na$Culture),
                  multiple = TRUE)
  ),
  
  mainPanel(
    plotlyOutput("interactivePlot")
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    req(input$cultureFilter)
    data_without_na %>%
      filter(Culture %in% input$cultureFilter)
  })
  
  output$interactivePlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = Culture, y = `Equivalent mg bone powder used for library preparation`,
                                     text = paste(
                                       "Culture:", Culture,
                                       "<br>Bone powder (mg):", `Equivalent mg bone powder used for library preparation`
                                     ), fill = Location)) +
      geom_boxplot(outlier.shape = NA, alpha = 0.6) +
      geom_jitter(width = 0.2, alpha = 0.5, color = "black") +
      labs(
        title = "Bone Powder Usage by Culture",
        x = "Culture",
        y = "Bone powder (mg)"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
        
    
    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui, server)

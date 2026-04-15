library(shiny)
library(ggplot2)
library(plotly)
library(readr)
library(dplyr)


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
data_without_na$`Equivalent mg bone powder used for library preparation` <- as.numeric(as.character(data_without_na$'Equivalent mg bone powder used for library preparation'))
data_without_na$Culture <- as.character(data_without_na$Culture)

# --- UI ---
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .hover-panel {
        position: absolute; 
        z-index: 1000; 
        background-color: rgba(255, 255, 255, 0.9); 
        border: 1px solid #ccc; 
        padding: 10px; 
        border-radius: 5px; 
        pointer-events: none;
      }
    "))
  ),
  
  titlePanel("Data analysis of archaeogenetics data"),
  p("Authors: Carla Domingo, Mah Noor Fatima, Laia Zamora, Carla Zurita and Fatima Zahrae El Yaagoubi"),
  
  tabsetPanel(
    #Heatmap
    tabPanel("Heatmap",
             br(),
             div(style = "position: relative;",
                 plotOutput("heatmap_plot", hover = hoverOpts(id = "hover_heatmap", delay = 50, delayType = "debounce")),
                 uiOutput("info_heatmap")
             )
    ),
    
    #Histograma
    tabPanel("Distribució Bone Powder",
             br(),
             sidebarLayout(
               sidebarPanel(
                 selectInput("location_select", "Select Location(s):",
                             choices = unique(data_without_na$Location),
                             selected = unique(data_without_na$Location)[1:3], # Seleccionem uns quants per defecte
                             multiple = TRUE),
                 sliderInput("bins", "Number of bins:", min = 10, max = 50, value = 30)
               ),
               mainPanel(
                 plotOutput("hist_plot") 
               )
             )
    ),
    
    #Boxplot Interactiu (Plotly)
    tabPanel("Cultures and Bone Powder",
             br(),
             p("Aquest plot mostra la distribució per cultures. Els colors indiquen la localització."),
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
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  
  # --- Plot 1: RENDER DEL GRÀFIC (Faltava això!) ---
  output$heatmap_plot <- renderPlot({
    ggplot(data_without_na, aes(Location, `mtDNA haplogroup`, fill = Culture)) +
      geom_tile(color = "white") +
      scale_fill_viridis_d(option = "turbo") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Heatmap of mtDNA haplogroups per location")
  })
  
  # Lògica del hover per al Heatmap
  output$info_heatmap <- renderUI({
    hover <- input$hover_heatmap
    if(is.null(hover)) return(NULL)
    
    res <- nearPoints(data_without_na, hover, xvar = "Location", yvar = "mtDNA haplogroup", threshold = 20, maxpoints = 1)
    if (nrow(res) == 0) return(NULL)
    
    wellPanel(
      class = "hover-panel",
      style = paste0("left:", hover$coords_css$x + 10, "px; top:", hover$coords_css$y + 10, "px;"),
      p(HTML(paste0("<b>Location:</b> ", res$Location, "<br>",
                    "<b>Haplogroup:</b> ", res$`mtDNA haplogroup`, "<br>",
                    "<b>Culture:</b> ", res$Culture)))
    )
  })
  
  # --- Plot 2: Histograma ---
  output$hist_plot <- renderPlot({
    filtered_data_hist <- data_without_na[data_without_na$Location %in% input$location_select, ]
    
    ggplot(filtered_data_hist, aes(x = `Equivalent mg bone powder used for library preparation`)) +
      geom_histogram(bins = input$bins, fill = "steelblue", color = "white") +
      facet_wrap(~ Location) +
      labs(title = "Distribution of Bone Powder Used", x = "mg of Bone Powder", y = "Count") +
      theme_minimal()
  })
  
  # --- Plot 3: Boxplot interactiu ---
  filtered_data_box <- reactive({
    req(input$cultureFilter)
    data_without_na %>% filter(Culture %in% input$cultureFilter)
  })
  
  output$interactivePlot <- renderPlotly({
    p3 <- ggplot(filtered_data_box(), aes(x = Culture, y = `Equivalent mg bone powder used for library preparation`,
                                          text = paste("Culture:", Culture, "<br>Bone powder (mg):", 
                                                       `Equivalent mg bone powder used for library preparation`), 
                                          fill = Location)) +
      geom_boxplot(outlier.shape = NA, alpha = 0.6) +
      geom_jitter(width = 0.2, alpha = 0.5, color = "black") +
      labs(title = "Bone Powder Usage by Culture", x = "Culture", y = "Bone powder (mg)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
    
    ggplotly(p3, tooltip = "text")
  })
}

shinyApp(ui = ui, server = server)

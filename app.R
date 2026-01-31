#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(viridis)
library(tidyverse)
library(ggiraph)
library(sf)
library(shinythemes)
library(plotly)


fam_per_country <- 
  read_rds("families_per_countries.rds")

static_df <- fam_per_country %>% select(-family)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Major African Language Families"),
    theme = shinytheme("darkly"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons(label = "Select language family",
                         inputId = "family",
                         choices = unique(fam_per_country$family) %>% str_trim())
        ),

        # Show a plot of the generated distribution
        mainPanel(
          div(textOutput("family_title"), style = "text-align: center;"),
           plotlyOutput("geo_sf_plot"),
           p("Data source: ", 
             a("Wikipedia - Languages of Africa", 
               href = "https://en.wikipedia.org/wiki/Languages_of_Africa"))
           )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Titling
  output$family_title <- renderText({
    paste(input$family, "language family")
  })
  
  # Create reactive df that responds to the selected family
  sf_interactive_df <- 
    reactive({
      fam_per_country %>% filter(family == input$family)
    })
  # Create the static color
  map_fill_static_color <- viridis_pal(option = "D", begin = 0, end = 0)(1)
  

    output$geo_sf_plot <- renderPlotly({
      
      max_n <- max(sf_interactive_df()$n, na.rm = TRUE)
      
      p <-
        ggplot(data = sf_interactive_df()) +
        geom_sf(data = static_df,
                mapping = aes(), fill = map_fill_static_color, show.legend = F) +
        geom_sf(aes(fill = n,
                    text = str_wrap(paste0("Language families in <b>", country,"</b>: ", n,"<br><br>Languages include:\n", languages), 40)), show.legend = T) +
        scale_fill_viridis_c(trans = "sqrt", 
                             name = "Number of\nfamilies",
                             begin = 0.1,
                             labels = scales::label_number(accuracy = 1)) +
        theme_void() +
        theme(plot.title = element_text(colour = "white"),
              legend.text = element_text(colour = "white"),
              legend.title = element_text(colour = "white"))
      
      ggplotly(p, tooltip = "text") %>%
        layout(plot_bgcolor = "rgba(0,0,0,0.1)")
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

# Assuming `aggregated_projections` already exists in your global environment
# If not, place your DI_df_existing → aggregated_projections processing here

ui <- fluidPage(
  titlePanel("Player Projections Over Time"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput("player_select", "Choose a player to highlight:",
                     choices = sort(unique(aggregated_projections$player)),
                     selected = NULL,
                     multiple = FALSE,
                     options = list(placeholder = 'Search player...'))
    ),
    
    mainPanel(
      plotlyOutput("projection_plot", height = "650px")
    )
  )
)

server <- function(input, output, session) {
  output$projection_plot <- renderPlotly({
    req(aggregated_projections)
    
    aggregated_projections <- aggregated_projections %>%
      mutate(highlight = if_else(player == input$player_select, "highlight", "dim"))
    
    p <- ggplot(aggregated_projections, 
                aes(x = nfl_age, y = avg_projection, group = player)) +
      geom_line(aes(color = highlight, alpha = highlight), size = 1) +
      geom_point(aes(color = highlight, alpha = highlight), size = 2) +
      scale_color_manual(values = c("highlight" = "red", "dim" = "gray70")) +
      scale_alpha_manual(values = c("highlight" = 1, "dim" = 0.3)) +
      labs(title = "Player Projections Over Time",
           x = "NFL Age",
           y = "Average Projection") +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5))
    
    ggplotly(p, tooltip = c("x", "y", "group"))
  })
}

shinyApp(ui, server)

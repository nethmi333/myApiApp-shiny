library(shiny)
library(ggplot2)
library(myApiApp)

ui <- fluidPage(
  titlePanel("Nobel Prize Laureates Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("cat_select", "Select Category:",
                  choices = c("All Categories" = "all",
                              "Chemistry" = "che",
                              "Physics" = "phy",
                              "Physiology or Medicine" = "med",
                              "Literature" = "lit",
                              "Peace" = "pea",
                              "Economic Sciences" = "eco"),
                  selected = "all"),
      textInput("year_input", "Filter by Year (Optional):", value = ""),
      actionButton("refresh", "Fetch Data", class = "btn-primary")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Laureates Table", tableOutput("laureate_table")),
        tabPanel("Gender Distribution Plot", plotOutput("gender_plot"))
      )
    )
  )
)

server <- function(input, output, session) {


  laureate_data <- eventReactive(input$refresh, {
    yr <- if (nchar(trimws(input$year_input)) > 0) input$year_input else NULL

    myApiApp::get_nobel_laureates(category = input$cat_select, year = yr)
  }, ignoreNULL = FALSE)


  output$laureate_table <- renderTable({
    df <- laureate_data()
    head(df, 20)
  })


  output$gender_plot <- renderPlot({
    df <- laureate_data()
    if (nrow(df) == 0) return(NULL)

    ggplot(df, aes(x = .data$Gender, fill = .data$Gender)) +
      geom_bar() +
      theme_minimal() +
      labs(title = "Laureate Count by Gender", y = "Count", x = "Gender") +
      scale_fill_brewer(palette = "Set2")
  })
}

shinyApp(ui = ui, server = server)

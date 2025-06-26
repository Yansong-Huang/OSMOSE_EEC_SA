library(shiny)
library(data.table)
library(ggplot2)
library(shinyWidgets)

# --- Load only biomass dataset ---
EE_biomass <- fread("EE_outputs/EE_biomass_total_biomass.csv")

# Add indicator name if missing
if (!"indicator" %in% names(EE_biomass)) EE_biomass[, indicator := "Total Biomass"]

# --- Add parameter type ---
EE_biomass[, param_type := fcase(
  grepl("^mortality\\.additional\\.(rate|larva\\.rate)", param_name), "Mortality",
  grepl("^(fisheries\\.rate\\.base|species\\.catchability)", param_name), "Fisheries",
  grepl("^(species\\.length2weight\\.condition\\.factor|species\\.k|species\\.l0|species\\.linf|species\\.maturity\\.size)", param_name), "Growth",
  grepl("^species\\.accessibility2fish", param_name), "Prey Field",
  grepl("^predation\\.predPrey\\.sizeRatio", param_name), "Predation",
  default = "Other"
)]

# --- UI ---
ui <- fluidPage(
  titlePanel("Elementary Effects on Total Biomass"),
  sidebarLayout(
    sidebarPanel(
      pickerInput("selected_types", "Show Parameter Types:",
                  choices = unique(EE_biomass$param_type),
                  selected = unique(EE_biomass$param_type),
                  multiple = TRUE, options = list(`actions-box` = TRUE))
    ),
    mainPanel(
      plotOutput("eePlot")
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  filtered_data <- reactive({
    EE_biomass[param_type %in% input$selected_types]
  })
  
  output$eePlot <- renderPlot({
    dt <- filtered_data()
    ggplot(dt, aes(x = mu_star, y = sigma, color = param_type)) +
      geom_abline(slope = c(0.1, 0.5, 1), intercept = 0,
                  linetype = "dashed", color = "grey60", show.legend = FALSE) +
      geom_point(size = 2, alpha = 0.8) +
      labs(x = expression(mu["*"]), y = expression(sigma), color = "Parameter Type") +
      theme_minimal(base_size = 13)
  })
}

# --- Run App ---
shinyApp(ui, server)

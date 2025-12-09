# —————— LIBRARIES —————— #
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(janitor)
library(rstanarm)
library(bayesplot)
library(forcats)

# —————— LOAD AND PREPARE DATA —————— #
marine_catch <- read_csv("ps_2024_csv/catch_20241.csv") %>%
  clean_names()

high_value_species <- c(
  "YELLOWFIN TUNA", "BLUEFIN TUNA", "SKIPJACK TUNA", "BLACKFIN TUNA",
  "LITTLE TUNNY", "WAHOO", "SAILFISH", "SWORDFISH", "ATLANTIC TARPON",
  "GROUPER GENUS (EPINEPHELUS)", "RED GROUPER", "BLACK GROUPER", "GOLIATH GROUPER",
  "SNAPPER GENUS", "GRAY SNAPPER", "MUTTON SNAPPER", "RED SNAPPER",
  "YELLOWTAIL SNAPPER", "GREATER AMBERJACK", "KING MACKEREL", "GREAT BARRACUDA",
  "COBIA", "BONNETHEAD", "HAMMERHEAD SHARK GENUS", "BLACKNOSE SHARK", "NURSE SHARK",
  "ATLANTIC SHARPNOSE SHARK", "REQUIEM SHARK GENUS", "SMOOTH DOGFISH", "SPINNER SHARK"
)

marine_catch <- marine_catch %>%
  mutate(
    common_up = toupper(common),
    high_value_interaction = if_else(common_up %in% high_value_species & !is.na(tot_cat) & tot_cat > 0, 1L, 0L),
    high_value_count = if_else(common_up %in% high_value_species, if_else(is.na(tot_cat), 0L, as.integer(tot_cat)), 0L),
    species_group = case_when(
      common_up %in% c("YELLOWFIN TUNA","BLUEFIN TUNA","SKIPJACK TUNA","BLACKFIN TUNA",
                       "LITTLE TUNNY","WAHOO","SAILFISH","SWORDFISH","ATLANTIC TARPON",
                       "KING MACKEREL","GREATER AMBERJACK","GREAT BARRACUDA") ~ "pelagic_game",
      common_up %in% c("GROUPER GENUS (EPINEPHELUS)","RED GROUPER","BLACK GROUPER","GOLIATH GROUPER",
                       "SNAPPER GENUS","GRAY SNAPPER","MUTTON SNAPPER","RED SNAPPER",
                       "YELLOWTAIL SNAPPER","COBIA") ~ "reef_game",
      common_up %in% c("BONNETHEAD","HAMMERHEAD SHARK GENUS","BLACKNOSE SHARK","NURSE SHARK",
                       "ATLANTIC SHARPNOSE SHARK","REQUIEM SHARK GENUS","SMOOTH DOGFISH","SPINNER SHARK") ~ "shark",
      common_up == "" ~ "unknown",
      TRUE ~ "other_fish"
    ),
    wave = as.factor(wave),
    area_x = factor(area_x, levels = c("1","2","3","4","5"),
                    labels = c("Ocean <= 3 mi", "Ocean > 3 mi", "Ocean <= 10 mi (WFL)", "Ocean > 10 mi (WFL)", "Inland")),
    mode_fx = factor(mode_fx, levels = c("3","5","7"), labels = c("Shore","Private","Charter")),
    year = as.integer(year)
  )

# —————— TRIP-LEVEL DATA —————— #
trip_level <- marine_catch %>%
  group_by(id_code) %>%
  summarise(
    target_hit = as.integer(any(high_value_interaction == 1)),
    total_catch = sum(tot_cat, na.rm = TRUE),
    mode_fx = first(mode_fx),
    area_x = first(area_x),
    wave = first(wave),
    year = first(year)
  )

# —————— FIT BAYESIAN MODEL —————— #
fit_logit <- stan_glm(
  target_hit ~ total_catch + mode_fx + area_x,
  data = trip_level,
  family = binomial(link = "logit"),
  prior = normal(0, 1),
  prior_intercept = normal(0, 2),
  chains = 4, iter = 2000, seed = 123
)

# —————— UI —————— #
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel(strong("High-Value Marine Catch Prediction Dashboard")),
  
  tags$style(HTML("
    .section-title {
      margin-top: 25px;
      margin-bottom: 15px;
      font-weight: bold;
      font-size: 20px;
    }
  ")),
  
  sidebarLayout(
    sidebarPanel(
      h4("Predict Target Hit Probability"),
      sliderInput("total_catch", "Total Catch:", 
                  min = 0, max = 50, value = 5, step = 1),
      selectInput("mode_fx", "Fishing Mode:", 
                  choices = levels(trip_level$mode_fx)),
      selectInput("area_x", "Area:", 
                  choices = levels(trip_level$area_x)),
      actionButton("predict", "Predict", class = "btn-primary"),
      br(), br(),
      wellPanel(
        style = "background-color:#e8f4f8;",
        h5(strong("Prediction Result")),
        textOutput("prob_result")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Prediction",
          div(class="section-title", "Posterior Distribution of Predicted Probability"),
          plotOutput("posterior_dist_plot"),
          
          div(class="section-title", "Model Coefficients"),
          plotOutput("coef_plot")
        ),
        
        tabPanel(
          "Feature Relationships",
          div(class="section-title", "Target Hit by Total Catch"),
          plotOutput("catch_relationship"),
          
          div(class="section-title", "Target Hit by Fishing Mode"),
          plotOutput("mode_relationship"),
          
          div(class="section-title", "Target Hit by Area"),
          plotOutput("area_relationship")
        ),
        
        tabPanel(
          "Descriptive Plots",
          h4("High-Value Individuals by Species Group"),
          plotOutput("species_group_plot"),
          
          h4("High-Value Interactions by Fishing Mode and Area"),
          plotOutput("mode_area_plot")
        )
      )
    )
  )
)

# —————— SERVER —————— #
server <- function(input, output, session) {
  
  # —————— PREDICTION TAB —————— #
  observeEvent(input$predict, {
    # Create new data for prediction
    new_trip <- tibble(
      total_catch = as.numeric(input$total_catch),
      mode_fx = factor(input$mode_fx, levels = levels(trip_level$mode_fx)),
      area_x = factor(input$area_x, levels = levels(trip_level$area_x))
    )
    
    # Get posterior distribution of probabilities
    prob_draws <- posterior_epred(fit_logit, newdata = new_trip)
    prob_mean <- mean(prob_draws)
    
    # Display probability
    output$prob_result <- renderText({
      paste0("Predicted probability of target hit: ", round(prob_mean * 100, 1), "%")
    })
    
    # Plot posterior distribution
    output$posterior_dist_plot <- renderPlot({
      density_obj <- density(prob_draws)
      plot(density_obj,
           main = "Posterior Distribution of Target Hit Probability",
           xlab = "Probability of Target Hit",
           ylab = "Density",
           lwd = 2, col = "steelblue", xlim = c(0, 1))
      polygon(density_obj, col = rgb(0.3, 0.5, 0.8, 0.3), border = NA)
      abline(v = prob_mean, col = "darkblue", lwd = 2, lty = 2)
      legend("topright", legend = paste0("Mean = ", round(prob_mean, 3)), 
             col = "darkblue", lty = 2, lwd = 2)
    })
  })
  
  # —————— COEFFICIENT PLOT —————— #
  output$coef_plot <- renderPlot({
    mcmc_intervals(fit_logit, 
                   pars = c("total_catch", "mode_fxPrivate", "mode_fxCharter",
                            "area_xOcean > 3 mi", "area_xOcean <= 10 mi (WFL)", 
                            "area_xOcean > 10 mi (WFL)", "area_xInland"),
                   prob = 0.8, prob_outer = 0.95) +
      labs(title = "Model Coefficients with 80% and 95% Credible Intervals",
           subtitle = "Log-odds scale") +
      theme_minimal()
  })
  
  # —————— FEATURE RELATIONSHIPS —————— #
  output$catch_relationship <- renderPlot({
    trip_level %>%
      filter(total_catch <= 50) %>%
      ggplot(aes(x = total_catch, y = target_hit)) +
      geom_jitter(alpha = 0.2, height = 0.05) +
      geom_smooth(method = "glm", method.args = list(family = "binomial"), 
                  se = TRUE, color = "steelblue", size = 1.5) +
      labs(x = "Total Catch", y = "Target Hit (0 = No, 1 = Yes)",
           title = "Relationship between Total Catch and Target Hit") +
      theme_minimal()
  })
  
  output$mode_relationship <- renderPlot({
    trip_level %>%
      group_by(mode_fx) %>%
      summarise(hit_rate = mean(target_hit), .groups = "drop") %>%
      ggplot(aes(x = mode_fx, y = hit_rate, fill = mode_fx)) +
      geom_col() +
      scale_fill_brewer(palette = "Set2") +
      labs(x = "Fishing Mode", y = "Target Hit Rate",
           title = "Target Hit Rate by Fishing Mode") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$area_relationship <- renderPlot({
    trip_level %>%
      group_by(area_x) %>%
      summarise(hit_rate = mean(target_hit), .groups = "drop") %>%
      ggplot(aes(x = fct_reorder(area_x, hit_rate), y = hit_rate, fill = area_x)) +
      geom_col() +
      scale_fill_brewer(palette = "Set1") +
      coord_flip() +
      labs(x = "", y = "Target Hit Rate",
           title = "Target Hit Rate by Area") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # —————— DESCRIPTIVE PLOTS —————— #
  output$species_group_plot <- renderPlot({
    marine_catch %>%
      group_by(species_group) %>%
      summarise(high_value_individuals = sum(high_value_count, na.rm = TRUE)) %>%
      ggplot(aes(x = fct_reorder(species_group, high_value_individuals), 
                 y = high_value_individuals)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(x = "", y = "Individuals Caught") +
      theme_minimal()
  })
  
  output$mode_area_plot <- renderPlot({
    marine_catch %>%
      group_by(mode_fx, area_x) %>%
      summarise(high_value = sum(high_value_interaction, na.rm = TRUE), .groups = "drop") %>%
      ggplot(aes(x = mode_fx, y = high_value, fill = area_x)) +
      geom_col(position = "dodge") +
      scale_fill_brewer(palette = "Spectral") +
      labs(x = "Fishing Mode", y = "High-Value Interactions", fill = "Area") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
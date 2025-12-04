library(shiny)
library(ggplot2)
library(ggforce)
library(dplyr)
library(modelr)
library(tidyr)
library(readr)
library(janitor)
library(tidybayes)
library(rstanarm)
library(plotly)
library(shinyWidgets)
library(shinycssloaders)
library(openai)

# --- OpenAI Model ---
openai_model <- "gpt-4o"

# --- Load and clean data ---
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

# --- Trip-level collapse ---
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


# --- UI ---
ui <- fluidPage(
  titlePanel("High-Value Species Dashboard"),
  sidebarLayout(
    sidebarPanel(
      textInput("user_question", "Ask a question about the dataset:", ""),
      actionButton("ask_btn", "Ask"),
      verbatimTextOutput("chat_answer")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Species Plots",
          h4("High-Value Individuals by Species Group"),
          withSpinner(plotOutput("species_group_plot")),
          h4("High-Value Interactions by Fishing Mode and Area"),
          withSpinner(plotOutput("mode_area_plot")),
          h4("Predicted Probability of High-Value Interaction"),
          withSpinner(plotOutput("posterior_plot"))
        )
      )
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  
  # --- Plots ---
  output$species_group_plot <- renderPlot({
    marine_catch %>%
      group_by(species_group) %>%
      summarise(high_value_individuals = sum(high_value_count, na.rm = TRUE)) %>%
      ggplot(aes(x = fct_reorder(species_group, high_value_individuals), y = high_value_individuals)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(x = "", y = "Individuals Caught")
  })
  
  output$mode_area_plot <- renderPlot({
    marine_catch %>%
      group_by(mode_fx, area_x) %>%
      summarise(high_value = sum(high_value_interaction, na.rm = TRUE)) %>%
      ggplot(aes(x = mode_fx, y = high_value, fill = area_x)) +
      geom_col(position = "dodge") +
      labs(x = "Fishing Mode", y = "High-Value Interactions")
  })
  
  output$posterior_plot <- renderPlot({
    ggplot(pred_data_cached, aes(total_catch, .epred, group = .draw)) +
      stat_lineribbon(.width = c(.50, .80, .95)) +
      facet_grid(mode_fx ~ area_x) +
      labs(x = "Total Catch", y = "Predicted Probability") +
      theme_minimal()
  })
  
  # --- Chatbot ---
  output$chat_answer <- renderText({
    req(input$ask_btn)
    user_prompt <- paste0(
      "You are an R assistant analyzing a marine catch dataset. Answer questions only using the dataset with columns: common, tot_cat, high_value_interaction, high_value_count, species_group, wave, area_x, mode_fx, year.\n",
      "Question: ", input$user_question
    )
    response <- openai::create_chat_completion(
      model = openai_model,
      messages = list(
        list(role = "system", content = "You are an R data assistant."),
        list(role = "user", content = user_prompt)
      ),
      temperature = 0
    )
    response$choices[[1]]$message$content
  })
}

shinyApp(ui, server)


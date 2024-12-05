library(shiny)
library(tidyverse)
library(plotly)
library(glmnet)
library(modelr)
set.seed(1)


## Data Importing
survey = read.csv("coffee_survey.csv") |>
  janitor::clean_names() |>
  select(-purchase_other, -favorite_specify, -additions_other, -prefer_abc,  
         -why_drink_other, -know_source, -value_cafe, -value_equipment, -gender_specify, 
         -ethnicity_race_specify, -number_children, -political_affiliation, 
         -coffee_a_notes, -coffee_b_notes, -coffee_c_notes, -coffee_d_notes) |>
  drop_na(gender) |>
  mutate_if(is.character, as.factor)


## Data Cleaning
survey_tidy = 
  survey %>% 
  rename_with(
    ~ sub("personal_preference", "preference", .),
    .cols = ends_with("personal_preference")
  ) %>% 
  pivot_longer(
    cols = coffee_a_bitterness:coffee_d_preference,
    values_to = "score",
    names_to = c("coffee_name", "coffee_feature"),
    names_pattern = "(.*)_(.*)"
  ) %>% 
  pivot_wider(
    names_from = coffee_feature,
    values_from = score
  ) %>% 
  mutate(
    coffee_name = case_match(coffee_name,
                             "coffee_a" ~ "Coffee A",
                             "coffee_b" ~ "Coffee B",
                             "coffee_c" ~ "Coffee C",
                             "coffee_d" ~ "Coffee D"),
    coffee_name = as.factor(coffee_name)
  ) %>% 
  mutate(
    age = factor(age, levels = rev(c("<18 years old", "18-24 years old", "25-34 years old", 
                                     "35-44 years old", "45-54 years old", "55-64 years old", 
                                     ">65 years old"))),
    cups = factor(cups, levels = rev(c("More than 4", "4", "3", "2", "1", "Less than 1"))),
    education_level = factor(education_level, 
                             levels = c("Doctorate or professional degree", "Master's degree",
                                        "Bachelor's degree", "Some college or associate's degree",
                                        "High school graduate", "Less than high school")),
    favorite = fct_recode(favorite, "Blended drink" = "Blended drink (e.g. Frappuccino)"),
    most_paid = factor(most_paid, levels = c("Less than $2", "$2-$4", "$4-$6", "$6-$8", 
                                             "$8-$10", "$10-$15", "$15-$20", "More than $20")),
    most_willing = factor(most_willing, levels = c("Less than $2", "$2-$4", "$4-$6", "$6-$8", 
                                                   "$8-$10", "$10-$15", "$15-$20", "More than $20")),
    total_spend = factor(total_spend, levels = c("<$20", "$20-$40", "$40-$60", "$60-$80", "$80-$100",
                                                 ">$100")),
    caffeine = factor(caffeine, levels = c("Decaf", "Half caff", "Full caffeine")),
    expertise = as.numeric(expertise),
    bitterness = as.numeric(bitterness),
    acidity = as.numeric(acidity),
    preference = as.numeric(preference)
  ) %>% 
  relocate(prefer_ad, prefer_overall, .after = everything())


## Logistic Regression & Bootstrapping

coffee_ad_df = 
  survey_tidy %>% 
  distinct(submission_id, prefer_ad, gender, age, expertise, style, strength, caffeine) %>%
  drop_na(gender, age, expertise, style, strength, caffeine) %>% 
  filter(
    gender %in% c("Male", "Female", "Non-binary"),
    age != "<18 years old"
    ) %>% 
  mutate(prefer_ad = if_else(prefer_ad == "Coffee D", 1, 0))

bootstraps_ad = 
  coffee_ad_df %>% 
  bootstrap(100) %>% 
  mutate(
    strap = map(strap, as_tibble),
    models = map(strap, \(df) glm(prefer_ad ~ gender + age + expertise + style + 
                                    strength + caffeine, data = df, family = "binomial")),
    results = map(models, broom::tidy)
  ) %>% 
  select(.id, results) %>% 
  unnest(results)

ad_results = 
  bootstraps_ad %>% 
  group_by(term) %>% 
  summarize(
    boot_mean = mean(estimate),
    boot_se = sd(estimate)
  ) 


## Prediction

intercept = 
  ad_results %>% 
  filter(term == "(Intercept)") %>% 
  pull(boot_mean)

coefs =
  ad_results %>% 
  filter(term != "(Intercept)") %>%
  select(term, boot_mean) %>%
  deframe()  


## UI

gender_choice = 
  coffee_ad_df %>% 
  distinct(gender) %>% 
  pull(gender)

age_choice = 
  coffee_ad_df %>% 
  distinct(age) %>% 
  mutate(age = factor(age, levels = c("18-24 years old", "25-34 years old", 
                                          "35-44 years old", "45-54 years old", "55-64 years old", 
                                          ">65 years old"))) %>% 
  arrange(age) %>% 
  pull(age)

style_choice = 
  coffee_ad_df %>% 
  distinct(style) %>% 
  arrange(style) %>% 
  pull(style)

strength_choice = 
  coffee_ad_df %>% 
  distinct(strength) %>% 
  mutate(strength = factor(strength, levels = c("Weak", "Somewhat light",
                                      "Medium", "Somewhat strong",
                                      "Very strong"))
         ) %>% 
  arrange(strength) %>% 
  pull(strength)

caffeine_choice = 
  coffee_ad_df %>% 
  distinct(caffeine) %>% 
  pull(caffeine)



ui = 
  
  fluidPage(
    
    
    selectInput(
      inputId = "gender_choice",
      label = h4("Select Your Gender"),
      choices = gender_choice,
      selected = "Female"
    ),
    
    selectInput(
      inputId = "age_choice",
      label = h4("Select Your Age"),
      choices = age_choice,
      selected = "18-24 years old"
    ),
    
    selectInput(
      inputId = "style_choice",
      label = h4("What kind of coffee do you like?"),
      choices = style_choice,
      selected = "Fruity"
    ),
    
    selectInput(
      inputId = "strength_choice",
      label = h4("What strength of coffee do you prefer?"),
      choices = strength_choice,
      selected = "Medium"
    ),
    
    radioButtons(
      inputId = "caffeine_choice",
      label = h4("How much caffeine do you like in your coffee?"),
      choices = caffeine_choice,
      selected = "Full caffeine"
    ),
    
    sliderInput(
      inputId = "expertise_slider",
      label = h4("Rate Your Coffee Expertise"),
      min = 0, 
      max = 10, 
      value = 5
    ),
    
    actionButton("submit", "Submit"),
    
    verbatimTextOutput("predicted_prob")
    
  )


## Server

server = function(input, output) {
  
  values = reactiveValues(predicted_prob = NULL)
  
  
  observeEvent(input[["submit"]], {
    user_input = 
      tibble(
        gender = factor(input[["gender_choice"]], levels = unique(coffee_ad_df$gender)),
        age = factor(input[["age_choice"]], levels = unique(coffee_ad_df$age)),
        expertise = input[["expertise_slider"]],
        style = factor(input[["style_choice"]], levels = unique(coffee_ad_df$style)),
        strength = factor(input[["strength_choice"]], levels = unique(coffee_ad_df$strength)),
        caffeine = factor(input[["caffeine_choice"]], levels = unique(coffee_ad_df$caffeine))
      ) %>% 
      model.matrix(~ . - 1, data = .) %>% 
      as_tibble()
    
    
    logit = intercept + sum(coefs * user_input)
    
    values[["predicted_prob"]] = 1 / (1 + exp(-logit))
    
  })
  
  
  output[["predicted_prob"]] = renderPrint({
    if (!is.null(values[["predicted_prob"]])) {
      values[["predicted_prob"]]
    } 
    else {
      "No predictions yet! Click Submit to calculate."
    }
  })
  
}


shinyApp(ui, server)



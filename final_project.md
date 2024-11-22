Final Project
================
Group 43
2024-11-21

# Data Importing and Cleaning

## Rating dataset

``` r
ratings_df = read_csv("data/coffee_ratings.csv") %>% 
  janitor::clean_names() %>%
  select(total_cup_points, species, country_of_origin, 
         region, number_of_bags, bag_weight, grading_date, variety,
         processing_method, aroma, flavor, aftertaste, acidity, body, balance,
         uniformity, clean_cup, sweetness, cupper_points, moisture, color,
         altitude_low_meters, altitude_high_meters, altitude_mean_meters) %>%
  mutate(grading_year = str_extract(grading_date, "\\d{4}"),
         species = as.factor(species),
         variety = as.factor(variety),
         processing_method = as.factor(processing_method),
         moisture = as.factor(moisture),
         color = as.factor(color))
```

## Survey dataset

``` r
survey = read_csv("data/coffee_survey.csv") |>
  janitor::clean_names() |>
  select(-purchase_other, -favorite_specify, -additions_other, -prefer_abc, -prefer_ad, 
         -why_drink_other, -know_source, -value_cafe, -value_equipment, -gender_specify, 
         -ethnicity_race_specify, -number_children, -political_affiliation, 
         -coffee_a_notes, -coffee_b_notes, -coffee_c_notes, -coffee_d_notes) |>
  drop_na(gender) |>
  mutate_if(is.character, as.factor)
```

``` r
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
  relocate(prefer_overall, .after = everything())
```
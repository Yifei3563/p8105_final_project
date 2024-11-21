Final Project
================
Group 43
2024-11-21

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(ggplot2)
library(modelr)
library(readxl)
library(haven)
```

## Import Data

``` r
ratings = read_csv("data/coffee_ratings.csv") |> 
  janitor::clean_names() |> 
  select(total_cup_points, species, country_of_origin, altitude, 
         region, number_of_bags, bag_weight, harvest_year, grading_date, variety,
         processing_method, aroma, flavor, aftertaste, acidity, body, balance,
         uniformity, clean_cup, sweetness, cupper_points, moisture, color,
         altitude_low_meters, altitude_high_meters, altitude_mean_meters,)
```

    ## Rows: 1339 Columns: 43
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (24): species, owner, country_of_origin, farm_name, lot_number, mill, ic...
    ## dbl (19): total_cup_points, number_of_bags, aroma, flavor, aftertaste, acidi...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
survey =read_csv("data/coffee_survey.csv") |> 
  janitor::clean_names()
```

    ## Rows: 4042 Columns: 57
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (44): submission_id, age, cups, where_drink, brew, brew_other, purchase,...
    ## dbl (13): expertise, coffee_a_bitterness, coffee_a_acidity, coffee_a_persona...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

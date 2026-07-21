library(shiny)
library(tidyverse)
library(readr)
library(DT)
library(plotly)
library(leaflet)
library(bslib)

championships <- read_csv("data/championships.csv", show_col_types = FALSE)
schools <- read_csv("data/schoolsgeo.csv", show_col_types = FALSE)
sports <- read_csv("data/sports.csv", show_col_types = FALSE)

dashboard_data <- championships %>%
  left_join(schools, by = c("Champion" = "School")) %>%
  left_join(sports, by = "Sport") %>%
  mutate(
    Year = as.integer(Year),
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude),
    Nickname = replace_na(Nickname, ""),
    Conference = replace_na(Conference, "Unknown"),
    City = replace_na(City, "Unknown"),
    State = replace_na(State, "Unknown"),
    Region = replace_na(Region, "Unknown"),
    PrimaryHex = replace_na(PrimaryHex, "#6B7280"),
    SecondaryHex = replace_na(SecondaryHex, "#FFFFFF")
  ) %>%
  filter(!is.na(Year), !is.na(Sport), !is.na(Champion))

school_choices <- sort(unique(dashboard_data$Champion))
sport_choices <- sort(unique(dashboard_data$Sport))
conference_choices <- sort(unique(dashboard_data$Conference))
state_choices <- sort(unique(dashboard_data$State))

default_school <- function(preferred) {
  if (preferred %in% school_choices) preferred else school_choices[[1]]
}

library(shiny)
library(tidyverse)
library(glue)
library(sf)
library(units)

list <- read_rds(file = '../../data/affordable_housing.rds')

sales_details <- list$sales_details
LIHTC <-  list$LIHTC

hudid = "TNA20130015"

get_project_data <- function(hudid){
  project <- LIHTC |> 
    filter(HUD_ID == hudid)
  project <- project |> 
    select(YR_ALLOC, YR_PIS, lat = LATITUDE, lng = LONGITUDE) |> 
    st_as_sf(coords = c("lng", "lat"), crs = 4326)
  
  # To take care of 8888 or 9999 YR_ALLOC or YR_PIS
  project <- project |> 
    mutate(YR_PIS = min(YR_ALLOC, YR_ALLOC + 10)) |> 
    mutate(YR_ALLOC = min(YR_ALLOC, YR_PIS))
  
  return(project)
}

get_model_data <- function(project) {
  sales_details <- sales_details %>% 
    mutate(dist = st_distance(home_loc, project$geometry[1])) |> 
    mutate(dist = drop_units(dist * 3.28084))
  
  model_data <- sales_details |> 
    filter(dist <= 3000) |> 
    filter(year(ownerdate) %in% c(
      (project$YR_ALLOC - 5):(project$YR_ALLOC - 1), 
      (project$YR_PIS + 1):(project$YR_PIS + 5)
    )
    )
  
  model_data <- model_data |> 
    mutate(
      treatment = if_else(dist <= 2000, 1, 0),
      after = if_else(year(ownerdate) %in% (project$YR_ALLOC - 5):(project$YR_ALLOC - 1), 0, 1)
    )
  
  model_data <- model_data |> 
    mutate(sale_year = year(ownerdate)) |> 
    mutate(age = sale_year - year_built)
  
  return(model_data)
}

get_estimates <- function(model, model_data, project){
  
  yr_range = min(model_data$sale_year):max(model_data$sale_year)
  
  control_pred <- tibble(
    sale_year = yr_range, 
    square_footage = rep(median(model_data$square_footage), length(yr_range)),
    building_condition = rep("Average", length(yr_range)),
    age = rep(median(model_data$age), length(yr_range)),
    treatment = rep(0, length(yr_range)),
  ) |> 
    mutate(after = if_else(sale_year < project$YR_PIS, 0, 1))
  
  control_pred <- control_pred |> 
    bind_cols(predict(model, newdata=control_pred, interval = "confidence") |> as_tibble())
  
  treatment_pred <- tibble(
    sale_year = yr_range, 
    square_footage = rep(median(model_data$square_footage), length(yr_range)),
    building_condition = rep("Average", length(yr_range)),
    age = rep(median(model_data$age), length(yr_range)),
    treatment = rep(1, length(yr_range)),
  ) |> 
    mutate(after = if_else(sale_year < project$YR_PIS, 0, 1))
  
  treatment_pred <- treatment_pred |> 
    bind_cols(predict(model, newdata=treatment_pred, interval = "confidence") |> as_tibble())
  
  estimate_df <- bind_rows(list("control" = control_pred, "treatment" = treatment_pred), .id = 'group')
  
  return(estimate_df)
}
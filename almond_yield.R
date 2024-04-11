#' Title
#'
#' @param dataset 
#'
#' @return
#' @export
#'
#' @examples
almond_yield<- function(dataset) {
  
  # calculate minimum temperatures in Feb from each year
  feb_tmin_c <- dataset %>%
    group_by(month, year) %>%
    filter(month == 2) %>% # select just feb
    summarise(feb_tmin_c = mean(tmin_c)) %>% 
    group_by() %>% 
    select(-month)# get the min
  
  # calculate total precipitation in Jan for each year
  jan_precip_mm <- dataset %>%
    group_by(month, year) %>%
    filter(month == 1) %>% # select just jan
    summarise(jan_precip_mm = sum(precip)) %>% 
    group_by() %>% 
    select(-month)# get the sum
  
  # calculate almond yield 
  yield <- full_join(feb_tmin_c, jan_precip_mm) %>%
    mutate(yield_tons = -0.015*feb_tmin_c - 0.0046*feb_tmin_c^2 - 0.07*jan_precip_mm + 0.0043*jan_precip_mm^2 + 0.28) # calc based on equation from lobell et al. 2006
  
  # calculate the min, max, and mean yield over the whole time period
  min_yield <- min(yield$yield_tons)
  max_yield <- max(yield$yield_tons)
  mean_yield <- mean(yield$yield_tons)
  
  # print the min, max, and mean
  return(print(paste0("Minimum Yield:", round(min_yield, 2), "ton(s) per acre , ",
                      "Maximum Yield:", round(max_yield, 2), "ton(s) per acre , ",
                      "Mean Yield:", round(mean_yield, 2), "ton(s) per acre")))
  
}
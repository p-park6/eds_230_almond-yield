#' Finding the almond yield from climate observations
#'
#' @param dataset The dataset that you are interested in that includes min and max for temperature, as well as precipitation data
#'
#' @return The minimum temperature (ton/acre), the maximum temperature (ton/acre), and the mean temperature of that month (ton/acre)
#' @export
#'
#' @examples
CopyOfalmond_yield<- function(dataset) {
  
  # calculate minimum temperatures in Feb from each year
  min_temp <- dataset %>%
    group_by(month, year) %>% # grouping by month and year
    filter(month == 2) %>% # select just feb
    summarise(feb_tmin_c = mean(tmin_c)) %>% #find mean temp per year
    group_by() %>% 
    select(-month)# get the min
  
  # calculate total precipitation in Jan for each year
  precip <- dataset %>%
    group_by(month, year) %>%
    filter(month == 1) %>% # select just jan
    summarise(jan_precip_mm = sum(precip)) %>% #find sum of precip per year
    group_by() %>% 
    select(-month)# get the sum
  
  # calculate almond yield 
  yield <- full_join(min_temp, precip) %>%
    mutate(yield_tons = -0.015*feb_tmin_c - 0.0046*feb_tmin_c^2 - 0.07*jan_precip_mm + 0.0043*jan_precip_mm^2 + 0.28) # calc based on equation from lobell et al. 2006
  
  return(yield)
  
  # # calculate the min, max, and mean yield over the whole time period
  # min_yield <- min(yield$yield_tons)
  # max_yield <- max(yield$yield_tons)
  # mean_yield <- mean(yield$yield_tons)
  # 
  # # print the min, max, and mean
  # return(c(min_yield, max_yield, mean_yield))
  
}

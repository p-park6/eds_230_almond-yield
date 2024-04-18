#' Almond Profit Model 
#'
#' @param gross_revenue 
#' @param almond_yield 
#' @param gallons gallons of water ; default = 9000 
#' @param acres default = 10
#' @param prop_of_loss proportion of yield loss ;default = 0.25
#' @param price_of_water price of water ; default = 0.007
#' @param price_per_ton price per ton of almonds ; defualt = 4000
#'
#' @return income as a gross revenue - expenses of water_price and price_of_yield_loss
#' @export
#'
#' @examples
almond_profit_model = function(gross_revenue, almond_yield, gallons = 9000, acres = 10, prop_of_loss = 0.25 ,  price_of_water = 0.007,  price_per_ton = 4000) {
  
  # water usage 
  water_per_acerage <- (gallons * acres)
  water_price = water_per_acerage * price_of_water
  
  # calculate yield_loss
  yield_loss = (almond_yield * 0.9) * prop_of_loss
  price_of_yield_loss = yield_loss * price_per_ton
  
  
  return(gross_revenue - (water_price + price_of_yield_loss))
}
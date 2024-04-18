# water_usage_per_ton
almond_profit_model = function(gross_revenue, almond_yield, gallons = 9000, acres = 10, prop_of_loss = 0.25 ,  price_of_water = 0.007,  price_per_ton = 4000) {
  
  # water usage 
  water_per_acerage <- (gallons * acres)
  water_price = water_per_acerage * price_of_water
  
  # calculate yield_loss
  yield_loss = (almond_yield * 0.9) * prop_of_loss
  price_of_yield_loss = yield_loss * price_per_ton
  
  
  return(gross_revenue - (water_price + price_of_yield_loss))
}
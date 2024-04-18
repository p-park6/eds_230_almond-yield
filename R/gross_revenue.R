#' Gross Revenue for Almond yield
#'
#' @param almond_yield The number of almonds obtained
#' @param price_per_ton Price of almonds per ton
#'
#' @return The total revenue of the almonds obtained
#' @export
#'
#' @examples
gross_revenue = function(almond_yield, price_per_ton = 4000) {
  #equation to find the gross revenue of almond yield
  gross_revenue = (almond_yield * 0.9) * price_per_ton
  
  #rename column to gross_revenue
  gross_revenue_rename <- rename(gross_revenue, gross_revenue = yield_tons)
  
  #return the gross revenue of the price of almond yield
  return(gross_revenue_rename)
}
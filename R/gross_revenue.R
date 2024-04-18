#' Title
#'
#' @param almond_yield 
#' @param price_per_ton 
#'
#' @return
#' @export
#'
#' @examples
gross_revenue = function(almond_yield, price_per_ton = 4000) {

  gross_revenue = (almond_yield * 0.9) * price_per_ton
  return(gross_revenue)
}
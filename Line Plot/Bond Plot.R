# Function to plot a relationship between yield and price
bond_plot <- function(bond_principle,
                      bond_coupon_rate = 0,
                      bond_year_to_mat,
                      n_an_b = 1,
                      low_bound_yield = 0,
                      high_bound_yield = 100){
  
  # Make an empty list for yields
  list_for_yields <- 0
  
  # Make an empty list for prices
  list_for_bond_prices <- 0
  
  # For each yield value
  for (n in low_bound_yield:high_bound_yield){
    
    # Calculate Interest Rate
    bond_interest_rate <- 0.01 * (1 * n)
    
    # Add interest rate value to list
    list_for_yields <- c(list_for_yields, bond_interest_rate)
    
    # Calculate coupon part
    coupon_part <- (bond_coupon_rate * bond_principle) / n_an_b
    
    # Calculate interest rate part
    bond_rate_part <- ((n_an_b/bond_interest_rate) -
                         n_an_b/(bond_interest_rate *
                                   (1 + bond_interest_rate/n_an_b) ^
                                   bond_year_to_mat * n_an_b))
    # Calculate principle part
    principle_part <- bond_principle / ((1 + bond_interest_rate/n_an_b) ^
                                          bond_year_to_mat * n_an_b)
    # Calculate price of bond
    price_of_the_bond = coupon_part * bond_rate_part + principle_part
    
    # Add bond price value to list
    list_for_bond_prices <- c(list_for_bond_prices, price_of_the_bond)
  }
  
  bond_min_value <- min(list_for_bond_prices)
  bond_max_value <- max(list_for_bond_prices)
  
  # Generate plot
  plot(x = list_for_yields,
       y = list_for_bond_prices,
       type = "l",
       xlab = "Bond Yield",
       ylab = "Bond Price",
       main = "Bond Pricing Dependency On Yield",
       sub = "Source: None",
       col = "red",
       lwd = 3)
       #ylim = c(bond_min_value, bond_max_value))
}
# Test
bond_plot(bond_principle = 1000,
          bond_coupon_rate = 0.08,
          bond_year_to_mat = 3,
          n_an_b = 1,
          low_bound_yield = 0,
          high_bound_yield = 20)

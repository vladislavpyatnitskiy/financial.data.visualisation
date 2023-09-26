# Function to show Duration dependency on Year to Maturity 
duration_plot <- function(bond_principle,
                          bond_coupon_rate,
                          bond_interest_rate){
  
  # Set a variable to contain duration values
  duration_periods <- NULL
  
  for (bond_year_to_mat in 2:100){
    # Coupon calculation
    coupon_part <- bond_coupon_rate * bond_principle # 100 Done
    
    # Calculate part for principle
    principle_part <- ((bond_principle + coupon_part) / # Done
                         ((1 + bond_interest_rate)^bond_year_to_mat))
    
    #
    pv_principle <- principle_part * bond_year_to_mat # Done
    
    # Set up duration sum
    pv_sum <- NULL
    
    # Calculate PV of coupons
    for (n in 1:(bond_year_to_mat-1)){
      # for each flow of cash
      duration_pv <- coupon_part / ((1 + bond_interest_rate) ^ n)
      
      # Put each flow of cash in list
      pv_sum <- cbind(pv_sum, duration_pv) # Done
    }
    
    values_for_denominator <- 0
    
    for (n in 1:(bond_year_to_mat - 1)){
      values_for_denominator <- values_for_denominator + pv_sum[n]
    }
    
    final_dur_denominator <- principle_part + 
      values_for_denominator
    
    # Set up new list to contain
    list_of_payments <- NULL
    
    #
    for (n in 1:(bond_year_to_mat - 1)) {
      payments_sum <- n * pv_sum[n]
      
      list_of_payments <- cbind(list_of_payments, payments_sum) # Done
    }
    
    # Define
    sum_of_duration_payments <- 0
    
    #
    for (n in 1:(bond_year_to_mat - 1)){
      sum_of_duration_payments <- sum_of_duration_payments + # Done
        list_of_payments[n]
    }
    
    # Sum cash flows to principle
    duration_value <- (sum_of_duration_payments + pv_principle) /
      final_dur_denominator
    
    duration_periods <- rbind(duration_periods, duration_value)
    }
  
  #
  maturity_periods <- seq(100, from = 2)
  
  #
  rownames(duration_periods) <- maturity_periods
  
  #
  colnames(duration_periods) <- "Duration"
  
  # Generate plot
  plot(x = maturity_periods,
       y = duration_periods,
       type = "l",
       xlab = "Bond Year to Maturity",
       ylab = "Bond Duration",
       main = "Bond Duration Dependency On Year to Maturity",
       sub = "Source: None",
       col = "red",
       lwd = 3) 
}
# Test
duration_plot(1000, 0.01, 0.1)

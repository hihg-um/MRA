# Meta-Reductive Analysis (MRA) function
# This function adjusts a full meta-analysis dataset by removing the effects of individual-level data.

MRA = function(beta_full, se_full, beta_sub, se_sub){
  
  # beta_full: Effect size (beta values) from the full meta-analysis dataset.
  # se_full: Standard error of the effect sizes from the full meta-analysis dataset.
  # beta_sub: Effect size (beta values) from the individual-level data.
  # se_sub: Standard error of the effect sizes from the individual-level data.
  
  # Compute the inverse of variance for the individual-level data.
  # 'wi' is the weight based on the variance of the individual-level data.
  wi = 1/(se_sub^2)
  
  # 'SE2' represents the variance of the full meta-analysis data.
  SE2 = se_full^2
  
  # Adjust the beta (effect size) from the meta-analysis by removing the effects of individual-level data.
  beta_red = beta_full + (SE2 * (beta_full * wi - beta_sub * wi) / (1 - SE2 * wi))
  
  # Adjust the standard error of the full dataset based on the weights of the individual-level data.
  SE_red = se_full / (sqrt(1 - SE2 * wi))
  
  # 'result_list' is a list containing the adjusted effect size and standard error.
  result_list = list(beta_red = beta_red, SE_red = SE_red)
  
  # Return the list of adjusted results.
  return(result_list)
}


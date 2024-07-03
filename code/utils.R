my_new_softmax <- function(option_values , option_chosen, temperature = 1){
  result <- exp(option_values[option_chosen]/temperature)/ sum(exp(option_values/temperature))
  return(result)
}

compute_utility_risk_amb <- function(alpha, beta, value, prob, ambiguity){
  if (!between(prob, 0, 1) | !between(ambiguity, 0, 1)){
    stop("The gamble risk/ambiguity must fall between [0,1]")
  }
  
  if(ambiguity != 0 & prob !=.50){
    warning("Non-zero ambiguity level specified. Risk level changed to 50%.")
    gamble_win_prob <- 0.5
  }
  utility <- (value^(alpha)) * (prob + ((beta * ambiguity)/2))
  return(utility)
  
  
}
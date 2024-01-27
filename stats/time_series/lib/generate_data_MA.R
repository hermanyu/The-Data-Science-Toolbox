
#' generates synthetic data for a Moving Average process of order q
generate_ma_series <- function(series_length = 100,
                               starting_shocks = NULL,
                               weights = c(1), 
                               error_sd = 1, 
                               seed = NULL){
  
  if (!is.null(seed)){
    set.seed(SEED)
  }
  
  q <- length(weights)
  
  if (is.null(starting_shocks)){
    
    most_recent_shocks <- rnorm(q, mean = 0, sd = error_sd)
    
  } else {
    
    start_index <- length(starting_shocks) - q + 1
    most_recent_shocks <- starting_shocks[ start_index : length(starting_shocks) ]
    
    rm(start_index)    
  }
  
  ma_series <- c()
  
  for (i in 1 : series_length){
    
    current_ma_term <- sum( most_recent_shocks * weights )
    current_error_term <- rnorm(1, mean = 0, sd = error_sd)
    
    # print("Recent Error Terms: ")
    # print(most_recent_shocks)
    # print(paste0("Current MA Term: ", current_ma_term, " | Current Error Term: ", current_error_term))
    # print(paste0("Current Term: ", current_ma_term + current_error_term))
    
    most_recent_shocks <- c(most_recent_shocks[2:q], current_error_term)
    
    ma_series <- c(ma_series, current_ma_term + current_error_term)
    
  }
  
  return(ma_series)
}
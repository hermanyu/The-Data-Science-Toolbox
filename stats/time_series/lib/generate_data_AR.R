
#' helper function for generate_ar_series()
#' computes the dot product of the back p many terms with the weights;
#' factored out from the main function for testing
compute_next_ar_term <- function(ar_series, weights){
  p <- length(weights)
  start_index <- length(ar_series) - p + 1
  values_to_use <- ar_series[ start_index : length(ar_series) ]
  
  return( sum( values_to_use * weights ) )
}

#' This function generates synthetic data for 
#' an autoregressive process of order p
generate_ar_series <- function(series_length = 100, 
                               start_values = NULL,
                               weights = c(1), 
                               error_sd = 1, 
                               seed = NULL){
  
  if (!is.null(seed)){
    set.seed(seed)
  }
  
  if (is.null(start_values)){
    ar_series <- rnorm(length(weights), mean = 0, sd = error_sd) # starting points
  } else {
    # Order of the AR process is inferred from the number of weights supplied,
    # so we throw an exception if the user does not supply enough starting values
    if (length(start_values) < length(weigths)){
      stop(
        paste0(
          "Not enough starting values for the desired order: there are ",
          length(weights),
          " many weights, but only ",
          length(start_values),
          " many starting values."
        )
      )
    }

    if (length(start_values) >= series_length){
      stop(
        paste0(
          "Desired series length is ", series_length,
          ", but there are already ", length(starting_value),
          " starting values."
        )
      )
    }
    
    ar_series <- start_values
  }
  
  for (i in seq(length(ar_series) + 1, series_length)){
    # grab the last p values; 
    # these make up the autoregressive term at time t
    current_ar_term <- compute_next_ar_term(ar_series, weights)
    
    # random shock at time t
    current_error_term <- rnorm(1, mean = 0, sd = 1)
    
    ar_series <- c(ar_series, current_ar_term + current_error_term)
  }
  
  return(ar_series)
}
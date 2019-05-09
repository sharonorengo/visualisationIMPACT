error_message_empty_env_boxplot <- function(x, subset.x=NULL, median, whisker_min, whisker_max, first_quantile, third_quantile, outlier_min, outlier_max){

  # Check and return message if empty evironnement
  stop_msg <- ""
  if( check_empty_env(x) == TRUE){ stop_msg <- paste0(stop_msg , rlang::get_expr(x)) }
  if( is.null(subset.x) == FALSE){ if(check_empty_env(subset.x) == TRUE ){ stop_msg <- paste(stop_msg , rlang::get_expr(subset.x)) }}
  if( check_empty_env(median) == TRUE ){ stop_msg <- paste(stop_msg , rlang::get_expr(median)) }
  if( check_empty_env(first_quantile) == TRUE ){ stop_msg <- paste(stop_msg , rlang::get_expr(first_quantile)) }
  if( check_empty_env(third_quantile) == TRUE ){ stop_msg <- paste(stop_msg , rlang::get_expr(third_quantile)) }
  if( check_empty_env(whisker_min) == TRUE ){ stop_msg <- paste(stop_msg , rlang::get_expr(whisker_min)) }
  if( check_empty_env(whisker_max) == TRUE ){ stop_msg <- paste(stop_msg , rlang::get_expr(whisker_max)) }
  if( check_empty_env(outlier_min) == TRUE & rlang::quo_is_null(outlier_min) == FALSE){ stop_msg <- paste(stop_msg , rlang::get_expr(outlier_min)) }
  if( check_empty_env(outlier_max) == TRUE & rlang::quo_is_null(outlier_max) == FALSE ){ stop_msg <- paste(stop_msg , rlang::get_expr(outlier_max)) }

  return(stop_msg)

}


error_message_empty_env_barchart <- function( x, subset.x = NULL, y, infimum_error, supremum_error){

  # Check and return message if empty evironnement
  stop_msg <- ""
  if( check_empty_env(x) == TRUE){ stop_msg <- paste0(stop_msg , rlang::get_expr(x)) }
  if(is.null(subset.x) == FALSE){if( check_empty_env(subset.x) == TRUE){ stop_msg <- paste(stop_msg , rlang::get_expr(subset.x)) }}
  if( check_empty_env(y) == TRUE ){ stop_msg <- paste(stop_msg , rlang::get_expr(y)) }
  if( check_empty_env(infimum_error) == TRUE ){ stop_msg <- paste(stop_msg , rlang::get_expr(infimum_error)) }
  if( check_empty_env(supremum_error) == TRUE ){ stop_msg <- paste(stop_msg , rlang::get_expr(supremum_error)) }

  return(stop_msg)

}


check_empty_env <- function(x){
  if( is_reference(quo_get_env(x), empty_env()) == TRUE ){
    return(TRUE)
  }
  return(FALSE)
}

#' A function that takes number and returns if its prime or not
#' @param number any number greater than 0
#' @return 'prime()' returns true or false depending on if the input was prime or not.
#' @export
#' @examples
#' prime(12)


prime <- function(n){
  if(n == 0){
    return(FALSE)
  }
  else if(n == 1){
    return(FALSE)
  }
  else if(n == 2){
    return(TRUE)
  }
  else if(0 %in% c(n %% c(seq(1, (n), by =1))[2:(n-1)])){
    return(FALSE)
  }else{
    return(TRUE)
  }
}







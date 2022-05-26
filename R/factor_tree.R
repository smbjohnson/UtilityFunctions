#' A function that takes number and returns its base factors
#' @param number any number greater than 0
#' @return 'fac_tree' returns a vector containing all the base facotrs of the input
#' @export
#' @examples
#' fac_tree(12)


fac_tree <- function(x){

  fac_ls <- c()

  if(prime(x) == TRUE){
    fac_ls <- c(1,x)
    return(fac_ls)
  }else{

    while(prime(x) == FALSE){

      remainder <- x %% c(seq(2, (x), by =1))

      remainder_index <- which.min(remainder)

      div <- remainder_index + 1

      fac_ls <- append(fac_ls,div)

      x <-  x/div

      if(prime(x) == TRUE){
        fac_ls <- append(fac_ls,x)
      }else{
        fac_ls
      }
    }
  }
  return(fac_ls)
}


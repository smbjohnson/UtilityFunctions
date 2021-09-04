#' A function that takes the first row of a dataframe and makes it the header
#' @param dataframe This is a dataframe
#' @import
#' @return 'CreateHeader()' returns a dataframe where the previous first row has now become the header.
#' @export
#' @examples
#' df <- data.frame(x = c("Height",180), y = c("Weight", 160))
#' df
#' CreateHeader(df)



CreateHeader <- function(dataframe){

  names(dataframe) <- dataframe[1,]


  finaldf <- dataframe[-1,]

  return(finaldf)

}

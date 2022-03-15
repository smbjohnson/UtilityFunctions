#' A function that extracts the information from the avPlots function
#' @param model This is the model you want to check linearity for
#' @import
#' @return 'ggAVplots()' return av plots for the variables in the model
#' @export

avPlots.invis <- function(MODEL, ...) {
  
  ff <- tempfile()
  png(filename = ff)
  OUT <- car::avPlots(MODEL, ...)
  dev.off()
  unlink(ff)
  OUT }
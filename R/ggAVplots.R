#' A function that takes a linear model and returns av plots that are ggplot objects
#' @param model This is the model you want to check linearity for
#' @param color This is the color of the dotted line
#' @return 'ggAVplots()' return av plots for the variables in the model
#' @export


ggAVplots  <- function(MODEL, color = 'red') {

  #Extract the information for AV plots
  AVPLOTS <- car::avPlots.invis(MODEL)
  K       <- length(AVPLOTS)

  #Create the added variable plots using ggplot
  GGPLOTS <- vector('list', K)
  for (i in 1:K) {
    DATA         <- data.frame(AVPLOTS[[i]])
    GGPLOTS[[i]] <- ggplot2::ggplot(aes_string(x = colnames(DATA)[1],
                                               y = colnames(DATA)[2]),
                                    data = DATA) +
      ggplot2::geom_point(colour = 'black') +
      ggplot2::geom_smooth(method = 'lm', se = FALSE,
                  color = color, formula = y ~ x, linetype = 'dashed') +
      labs(
        x = paste0('(', names(DATA)[1], ' | others)'),
        y = '' #ylab(paste0('(',ifelse(is.null(YLAB), paste0(names(DATA)[2], ' | others'), YLAB), ')'))
      )
  }

  #Return output object
  GGPLOTS }


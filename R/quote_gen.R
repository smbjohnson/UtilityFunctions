#' A function that returns a quote
#' @param
#' @import
#' @return 'quote_gen()' returns a quote
#' @export
#' @examples
#' quote_gen()


quotes <- c('Wake Me, When You Need Me. -- Halo 3 Master Chief',
            'Those who cannot do teach. Those who cannot teach, teach gym. -- School of Rock Dewey Fin',
            'The purpose of our lives is to be happy -- Dalai Lama'
            )

quote_gen <- function(){

  x <- round(runif(1, min = 1, max = length(quotes)))

  print(quotes[x])
}

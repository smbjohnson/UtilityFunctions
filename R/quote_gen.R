#' A function that returns a quote
#' @param
#' @import
#' @return 'quote_gen()' returns a quote
#' @export
#' @examples
#' quote_gen()


quotes <- c('Wake Me, When You Need Me. -- Halo 3 Master Chief',
            'Those who cannot do teach. Those who cannot teach, teach gym. -- School of Rock Dewey Fin',
            'The purpose of our lives is to be happy -- Dalai Lama',
            'Shut up grandma drink your prune juice -- Man from Transformers',
            'Wise guy eh -- Michael Scott',
            'You got a disease man -- School of Rock Freddie Jones',
            'What\'s the top prize -- School of Rock Fancy Pants',
            'You pushing me sir -- Toy Soilder 2 Drago',
            'Just like the simulation -- Starwars Battlefront 2',
            'Hello there! -- General Kenobi',
            'I was born in the dark -- Bane',
            'Hero get rembered but legends never die, follow your heart kid you\'ll never go wrong -- Babe Ruth'
            )

quote_gen <- function(){

  x <- round(runif(1, min = 1, max = length(quotes)))

  print(quotes[x])
}

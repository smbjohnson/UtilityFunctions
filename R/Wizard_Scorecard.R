#' A function that walks you through keeping score for a wizard game and returns the scorecard.
#' @param save Is a logical that indicates whether you want to save the scorecard as a csv
#'
#' @import dplyr
#' @import devtools
#' @importFrom magrittr %>%
#' @importFrom magrittr %$%
#'
#' @return 'wizard()' walks you through keeping score for a wizard game and returns the scorecard.
#' @export
#' @examples
#' df <- wizard()

wizard <- function(save = FALSE){
  # entering the number of players
  player_num <- menu(c(1,2,3,4,5,6),
                     title = "How many players are there?")
  # calculating how many round there are going to be
  round_num <- 60/player_num
  # creating a vector to collect the players name
  player_name <- vector(length = player_num)
  # collecting the players names
  for(i in 1:player_num) {
    player_name[i] <- readline(paste0("Who is player ", i, " ?"))
  }
  # creating an empty scorecard dataframe
  scorecard <- data.frame(matrix(nrow = 60, ncol = 5)) %>%
    dplyr::rename('Round' = 'X1',
           'Player' = 'X2',
           'Bid' = 'X3',
           'Actual' = 'X4',
           'Score' = 'X5')
  # populating the dataframe with the players names
  scorecard$Player <- rep(player_name,round_num)
  # populating the dataframe with the number of rounds
  scorecard$Round <- rep(1:round_num, times=1, each=player_num)
  # creating an order for which players need to bid
  player_order <- 1:player_num
  # this for loop conducts the game
  for(j in 1:round_num) {
    # printing out the round that we are on
    print(paste0("We are on round ", j))
    # collecting the bids for each player
    for(i in 1:player_num) {
      # getting the player's bid
      player_bid <- as.numeric(readline(paste0("What is ", player_name[player_order[i]], "'s bid?")))
      # checking to make sure it is a number
      while(is.na(player_bid)) {
        # asking the player to enter a number
        print("Please Enter a Number")
        # getting that new number
        player_bid <- as.numeric(readline(paste0("What is ", player_name[player_order[i]], "'s bid?")))
      }
      # saving the bid
      scorecard[scorecard$Round == j & scorecard$Player == player_name[player_order[i]],'Bid'] <- player_bid
    }
    # printing out the bid count
    print(paste0("We are ", scorecard %>% dplyr::filter(Round == j) %$% Bid %>% sum(), " of ", j))
    # collecting the actual number of tricks a player gets
    for(i in 1:player_num) {
      # getting the player's actual tricks
      player_trick <- as.numeric(readline(paste0("What did ", player_name[player_order[i]], " get?")))
      # checking to make sure it is a number
      while(is.na(player_trick)) {
        # asking the player to enter a number
        print("Please Enter a Number")
        # getting that new number
        player_trick <- as.numeric(readline(paste0("What did ", player_name[player_order[i]], " get?")))
      }
      # saving the actual
      scorecard[scorecard$Round == j & scorecard$Player == player_name[player_order[i]],'Actual'] <- player_trick
    }
    # checking for corrections
    cor <- menu(c('Yes','No'), title = 'Are there any corrections?')
    # inputing the correction
    if(cor == 'Yes' | cor == 1) {
      other_cor = TRUE
      while(other_cor == TRUE | other_cor == 1) {
        player <- menu(player_name, title = "Whose score needs correction?")
        bid_act_options <- c('Bid','Actual')
        bid_act <- menu(c('Bid','Actual'), title = "What it the bid or actual?")
        true_val <- as.numeric(readline(prompt = "What is the true value?"))

        if(is.numeric(player) & is.numeric(bid_act)) {
          scorecard[scorecard$Player == player_name[player] & scorecard$Round == j,bid_act_options[bid_act]] <- true_val
        } else if (is.numeric(player) & !is.numeric(bid_act)) {
          scorecard[scorecard$Player == player_name[player] & scorecard$Round == j,bid_act] <- true_val
        } else if (!is.numeric(player) & is.numeric(bid_act)) {
          scorecard[scorecard$Player == player & scorecard$Round == j,bid_act_options[bid_act]] <- true_val
        } else {
          scorecard[scorecard$Player == player & scorecard$Round == j,bid_act] <- true_val
        }

        other_cor <- menu(c(TRUE,FALSE), title = "Are there other corrections?")
      }
    }
    # calculating the score
    if(j==1) { # score for the first round is going off 0
      scorecard %<>%
        dplyr::group_by(Player) %>%
        dplyr::mutate(
          Score = ifelse(Bid == Actual, 20 + (10*Bid), -10*abs(Bid - Actual)),
          PotScore = 20 + (10 * Bid)
          )
    } else { # scores are updated based on previous scores
      scorecard %<>%
        dplyr::group_by(Player) %>%
        dplyr::mutate(
               Score = lag(Score, n = 1, default = 0, order_by = Round) + ifelse(Bid == Actual, 20 + (10 * Bid), -10*(abs(Bid - Actual))),
               Diff = Score - lag(Score, n = 1, default = 0),
               PotScore = lag(PotScore, n = 1, default = 0, order_by = Round) + (20 + (10 * Bid)),
               PotIncrease = PotScore - lag(PotScore, n = 1, default = 0)
               )
    }
    # printing off score update
    print(scorecard %>% dplyr::filter(Round == j) %>% dplyr::select(Player,Score) %>% dplyr::arrange(desc(Score), Player))
    # updating the order for players bids
    player_order <- c(player_order[-1], player_order[1])
  }
  # saving the scorecard to a csv
  if(save == TRUE) {
    game_date <- gsub(pattern = ":", replacement = ".", x = Sys.time())
    game_date <- gsub(pattern = " ", replacement = "_", game_date)
    write.csv(scorecard, paste0("wizard_scorecard_",game_date,".csv"))
    print(paste0("scorecard saved to ", getwd()))
  }
  # asking if there should be an asterisk
  asterisk <- menu(c('Yes','No'), title = "Should there be an asterisk")
  if(asterisk == 'Yes') {
    offender <- readline(prompt = "Who cheated?")
    scorecard %<>%
      mutate(asterisk = offender)
  }
  # returning the scorecard
  return(scorecard)
}

##' Start a New Game
##'
##' We want to start a new game of Hot Spots.  This function begins a new game
##' by setting the player's current spot to spot 1 (the starting spot).
##' Displays the board with the player at spot 1.  Selects a random number
##' as the seed for the current game to use in the move_to function.  This is
##' used to select and retain the hot spots for the current game.  Starts the
##' player with three chances.
##' @title Start a New Game
##' @return list with the current spot, number of chances the player has left, and seed
##' @author S. Ser
##' @export
##' @examples
##' new_game()
new_game <- function()
{
  current_spot <- 1

  plot(1, type="n", xlab="", ylab="", xlim=c(0, 5), ylim=c(0, 5), axes=FALSE)
  abline(v=0)
  abline(v=1)
  abline(v=2)
  abline(v=3)
  abline(v=4)
  abline(v=5)
  abline(h=0)
  abline(h=1)
  abline(h=2)
  abline(h=3)
  abline(h=4)
  abline(h=5)

  rect(0,5,1,4,col = "yellow")

  text(0.5, 4.5, label="1")
  text(1.5, 4.5, label="2")
  text(2.5, 4.5, label="3")
  text(3.5, 4.5, label="4")
  text(4.5, 4.5, label="5")

  text(0.5, 3.5, label="6")
  text(1.5, 3.5, label="7")
  text(2.5, 3.5, label="8")
  text(3.5, 3.5, label="9")
  text(4.5, 3.5, label="10")

  text(0.5, 2.5, label="11")
  text(1.5, 2.5, label="12")
  text(2.5, 2.5, label="13")
  text(3.5, 2.5, label="14")
  text(4.5, 2.5, label="15")

  text(0.5, 1.5, label="16")
  text(1.5, 1.5, label="17")
  text(2.5, 1.5, label="18")
  text(3.5, 1.5, label="19")
  text(4.5, 1.5, label="20")

  text(0.5, 0.5, label="21")
  text(1.5, 0.5, label="22")
  text(2.5, 0.5, label="23")
  text(3.5, 0.5, label="24")
  text(4.5, 0.5, label="25")

  text(0.25, 4.75, label="Start")
  text(4.75, 0.25, label="Finish")

  seed <- sample(1:10000, 1)
  chances <- 3

  out <- list(current_spot=current_spot, chances=chances, seed=seed)
  return(out)
}

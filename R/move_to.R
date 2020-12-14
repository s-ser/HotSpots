##' Move to a Spot on the Board
##'
##' We want to move to a spot on the board.
##' The player must input the spot the player is currently at, the spot the player
##' would like to move to, the number of chances the player has left, and the seed
##' designated at the beginning of the game.
##' Since the rules of the game only allow for a player to move one spot horizontally
##' or vertically, if the player requests to move to a spot that is not one spot
##' horizontally or vertically from the current spot, a message will print reminding
##' the player of that rule and what the current spot is.  They will also see where they
##' currently are on the board.
##' If the player requests to move to a spot that is allowed, the requested spot will
##' become the current spot.  Then, the function will check if the spot is a hot spot.
##' If the spot is a hot spot, the player will lose a chance and be sent back to the
##' starting spot (spot 1).
##' If the spot is not a hot spot, the player will stay at the desired spot and can
##' move again.
##' If the spot is the finishing spot (spot 25), the player has won the game and can
##' choose to start a new game using the new_game function.
##' @title Move to Spot
##' @param desired_spot the number of the spot they player would like to move to
##' @param current_spot the number of the spot the player is currently at
##' @param chances the number of chances the player has left in the game
##' @param seed the random number given to the player at the beginning of the game
##' @return list with the current spot, number of chances the player has left, and seed
##' @author S. Ser
##' @export
##' @examples
##' move_to(2, 1, 3, 1414)
move_to <- function(desired_spot, current_spot, chances, seed)
{


  set.seed(seed)
  hot_spot <- sample(3:24, 4)




  if (current_spot == 1 & (desired_spot == 2 | desired_spot == 6)){
    logical <- TRUE

  }

  else if (current_spot == 2 & (desired_spot == 1 | desired_spot == 3 | desired_spot == 7)){
    logical <- TRUE

  }

  else if (current_spot == 3 & (desired_spot == 2 | desired_spot == 4 | desired_spot == 8)){
    logical <- TRUE

  }

  else if (current_spot == 4 & (desired_spot == 3 | desired_spot == 5 | desired_spot == 9)){
    logical <- TRUE

  }

  else if (current_spot == 5 & (desired_spot == 4 | desired_spot == 10)){
    logical <- TRUE

  }

  else if (current_spot == 6 & (desired_spot == 1 | desired_spot == 7 | desired_spot == 11)){
    logical <- TRUE

  }

  else if (current_spot == 7 & (desired_spot == 2 | desired_spot == 6 | desired_spot == 8 | desired_spot == 12)){
    logical <- TRUE

  }

  else if (current_spot == 8 & (desired_spot == 3 | desired_spot == 7 | desired_spot == 9 | desired_spot == 13)){
    logical <- TRUE

  }

  else if (current_spot == 9 & (desired_spot == 4 | desired_spot == 8 | desired_spot == 10 | desired_spot == 14)){
    logical <- TRUE

  }

  else if (current_spot == 10 & (desired_spot == 5 | desired_spot == 9 | desired_spot == 15)){
    logical <- TRUE

  }

  else if (current_spot == 11 & (desired_spot == 6 | desired_spot == 12 | desired_spot == 16)){
    logical <- TRUE

  }

  else if (current_spot == 12 & (desired_spot == 7 | desired_spot == 11 | desired_spot == 13 | desired_spot == 17)){
    logical <- TRUE

  }

  else if (current_spot == 13 & (desired_spot == 8 | desired_spot == 12 | desired_spot == 14 | desired_spot == 18)){
    logical <- TRUE

  }

  else if (current_spot == 14 & (desired_spot == 9 | desired_spot == 13 | desired_spot == 15 | desired_spot == 19)){
    logical <- TRUE

  }

  else if (current_spot == 15 & (desired_spot == 10 | desired_spot == 14 | desired_spot == 20)){
    logical <- TRUE

  }

  else if (current_spot == 16 & (desired_spot == 11 | desired_spot == 17 | desired_spot == 21)){
    logical <- TRUE

  }

  else if (current_spot == 17 & (desired_spot == 12 | desired_spot == 16 | desired_spot == 18 | desired_spot == 22)){
    logical <- TRUE

  }

  else if (current_spot == 18 & (desired_spot == 13 | desired_spot == 17 | desired_spot == 19 | desired_spot == 23)){
    logical <- TRUE

  }

  else if (current_spot == 19 & (desired_spot == 14 | desired_spot == 18 | desired_spot == 20 | desired_spot == 24)){
    logical <- TRUE

  }

  else if (current_spot == 20 & (desired_spot == 15 | desired_spot == 19 | desired_spot == 25)){
    logical <- TRUE

  }

  else if (current_spot == 21 & (desired_spot == 16 | desired_spot == 22)){
    logical <- TRUE

  }

  else if (current_spot == 22 & (desired_spot == 21 | desired_spot == 17 | desired_spot == 23)){
    logical <- TRUE

  }

  else if (current_spot == 23 & (desired_spot == 22 | desired_spot == 18 | desired_spot == 24)){
    logical <- TRUE

  }

  else if (current_spot == 24 & (desired_spot == 23 | desired_spot == 19 | desired_spot == 25)){
    logical <- TRUE

  }


  else{
    logical <- FALSE
  }







  if(!logical & (chances > 0)){

    board <- current_spot

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

    if (board == 1){
      rect(0,5,1,4,col = "yellow")
    }

    if (board == 2){
      rect(1,5,2,4,col = "yellow")
    }

    if (board == 3){
      rect(2,5,3,4,col = "yellow")
    }

    if (board == 4){
      rect(3,5,4,4,col = "yellow")
    }

    if (board == 5){
      rect(4,5,5,4,col = "yellow")
    }


    if (board == 6){
      rect(0,4,1,3,col = "yellow")
    }

    if (board == 7){
      rect(1,4,2,3,col = "yellow")
    }

    if (board == 8){
      rect(2,4,3,3,col = "yellow")
    }

    if (board == 9){
      rect(3,4,4,3,col = "yellow")
    }

    if (board == 10){
      rect(4,4,5,3,col = "yellow")
    }


    if (board == 11){
      rect(0,3,1,2,col = "yellow")
    }

    if (board == 12){
      rect(1,3,2,2,col = "yellow")
    }

    if (board == 13){
      rect(2,3,3,2,col = "yellow")
    }

    if (board == 14){
      rect(3,3,4,2,col = "yellow")
    }

    if (board == 15){
      rect(4,3,5,2,col = "yellow")
    }


    if (board == 16){
      rect(0,2,1,1,col = "yellow")
    }

    if (board == 17){
      rect(1,2,2,1,col = "yellow")
    }

    if (board == 18){
      rect(2,2,3,1,col = "yellow")
    }

    if (board == 19){
      rect(3,2,4,1,col = "yellow")
    }

    if (board == 20){
      rect(4,2,5,1,col = "yellow")
    }


    if (board == 21){
      rect(0,1,1,0,col = "yellow")
    }

    if (board == 22){
      rect(1,1,2,0,col = "yellow")
    }

    if (board == 23){
      rect(2,1,3,0,col = "yellow")
    }

    if (board == 24){
      rect(3,1,4,0,col = "yellow")
    }

    if (board == 25){
      mtext("YOU WON!!!")
      rect(4,1,5,0,col = "yellow")
    }



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

    c <- paste0("You are currently at spot ", current_spot, ".")
    print(c)
    print("You can only move one spot horizontally or vertically.  Please try again.")
  }

  else if(!logical & !(chances > 0)){
    print("GAME OVER")
    print("To play again, use new_game()")
  }

  else{

    current_spot <- desired_spot



    if((desired_spot) %in% hot_spot)

    {
      chances <- chances - 1

      board_fail <- desired_spot
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
      mtext("UH-OH!!!")

      if (board_fail == 1){
        rect(0,5,1,4,col = "red")
        rect(0,5,1,4,col = "yellow")
      }

      if (board_fail == 2){
        rect(1,5,2,4,col = "red")
        rect(0,5,1,4,col = "yellow")
      }

      if (board_fail == 3){
        rect(2,5,3,4,col = "red")
        rect(0,5,1,4,col = "yellow")
      }

      if (board_fail == 4){
        rect(3,5,4,4,col = "red")
        rect(0,5,1,4,col = "yellow")
      }

      if (board_fail == 5){
        rect(4,5,5,4,col = "red")
        rect(0,5,1,4,col = "yellow")
      }


      if (board_fail == 6){
        rect(0,4,1,3,col = "red")
        rect(0,5,1,4,col = "yellow")
      }

      if (board_fail == 7){
        rect(1,4,2,3,col = "red")
        rect(0,5,1,4,col = "yellow")
      }

      if (board_fail == 8){
        rect(2,4,3,3,col = "red")
        rect(0,5,1,4,col = "yellow")
      }

      if (board_fail == 9){
        rect(3,4,4,3,col = "red")
        rect(0,5,1,4,col = "yellow")
      }

      if (board_fail == 10){
        rect(4,4,5,3,col = "red")
        rect(0,5,1,4,col = "yellow")
      }


      if (board_fail == 11){
        rect(0,3,1,2,col = "red")
        rect(0,5,1,4,col = "yellow")
      }

      if (board_fail == 12){
        rect(1,3,2,2,col = "red")
        rect(0,5,1,4,col = "yellow")
      }

      if (board_fail == 13){
        rect(2,3,3,2,col = "red")
        rect(0,5,1,4,col = "yellow")
      }

      if (board_fail == 14){
        rect(3,3,4,2,col = "red")
        rect(0,5,1,4,col = "yellow")
      }

      if (board_fail == 15){
        rect(4,3,5,2,col = "red")
        rect(0,5,1,4,col = "yellow")
      }


      if (board_fail == 16){
        rect(0,2,1,1,col = "red")
        rect(0,5,1,4,col = "yellow")
      }

      if (board_fail == 17){
        rect(1,2,2,1,col = "red")
        rect(0,5,1,4,col = "yellow")
      }

      if (board_fail == 18){
        rect(2,2,3,1,col = "red")
        rect(0,5,1,4,col = "yellow")
      }

      if (board_fail == 19){
        rect(3,2,4,1,col = "red")
        rect(0,5,1,4,col = "yellow")
      }

      if (board_fail == 20){
        rect(4,2,5,1,col = "red")
        rect(0,5,1,4,col = "yellow")
      }


      if (board_fail == 21){
        rect(0,1,1,0,col = "red")
        rect(0,5,1,4,col = "yellow")
      }

      if (board_fail == 22){
        rect(1,1,2,0,col = "red")
        rect(0,5,1,4,col = "yellow")
      }

      if (board_fail == 23){
        rect(2,1,3,0,col = "red")
        rect(0,5,1,4,col = "yellow")
      }

      if (board_fail == 24){
        rect(3,1,4,0,col = "red")
        rect(0,5,1,4,col = "yellow")
      }


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


      if(!(chances > 0)){
        print("You stepped on a hot spot!")
        b <- paste("You have", chances, "chance(s) left.")
        print(b)
        print("GAME OVER")
        print("To play again, use new_game()")
        current_spot <- 0

      }
      else{
        print("You stepped on a hot spot!")
        a <- paste("You have", chances, "chance(s) left.")
        print(a)
        print("You have to go back to the beginning.")
        current_spot <- 1
      }
    }



    else {


      board <- current_spot

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

      if (board == 1){
        rect(0,5,1,4,col = "yellow")
      }

      if (board == 2){
        rect(1,5,2,4,col = "yellow")
      }

      if (board == 3){
        rect(2,5,3,4,col = "yellow")
      }

      if (board == 4){
        rect(3,5,4,4,col = "yellow")
      }

      if (board == 5){
        rect(4,5,5,4,col = "yellow")
      }


      if (board == 6){
        rect(0,4,1,3,col = "yellow")
      }

      if (board == 7){
        rect(1,4,2,3,col = "yellow")
      }

      if (board == 8){
        rect(2,4,3,3,col = "yellow")
      }

      if (board == 9){
        rect(3,4,4,3,col = "yellow")
      }

      if (board == 10){
        rect(4,4,5,3,col = "yellow")
      }


      if (board == 11){
        rect(0,3,1,2,col = "yellow")
      }

      if (board == 12){
        rect(1,3,2,2,col = "yellow")
      }

      if (board == 13){
        rect(2,3,3,2,col = "yellow")
      }

      if (board == 14){
        rect(3,3,4,2,col = "yellow")
      }

      if (board == 15){
        rect(4,3,5,2,col = "yellow")
      }


      if (board == 16){
        rect(0,2,1,1,col = "yellow")
      }

      if (board == 17){
        rect(1,2,2,1,col = "yellow")
      }

      if (board == 18){
        rect(2,2,3,1,col = "yellow")
      }

      if (board == 19){
        rect(3,2,4,1,col = "yellow")
      }

      if (board == 20){
        rect(4,2,5,1,col = "yellow")
      }


      if (board == 21){
        rect(0,1,1,0,col = "yellow")
      }

      if (board == 22){
        rect(1,1,2,0,col = "yellow")
      }

      if (board == 23){
        rect(2,1,3,0,col = "yellow")
      }

      if (board == 24){
        rect(3,1,4,0,col = "yellow")
      }

      if (board == 25){
        mtext("YOU WON!!!")
        rect(4,1,5,0,col = "yellow")
      }



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


      if(current_spot == 25){
        print("Congratulations!!!  You made it!")
        print("To play again, use new_game()")
      }

    }

  }

  out <- list(current_spot=current_spot, chances=chances, seed=seed)
  return(out)
}

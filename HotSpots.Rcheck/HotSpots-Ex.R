pkgname <- "HotSpots"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('HotSpots')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("move_to")
### * move_to

flush(stderr()); flush(stdout())

### Name: move_to
### Title: Move to Spot
### Aliases: move_to

### ** Examples

move_to(2, 1, 3, 1414)



cleanEx()
nameEx("new_game")
### * new_game

flush(stderr()); flush(stdout())

### Name: new_game
### Title: Start a New Game
### Aliases: new_game

### ** Examples

new_game()



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')

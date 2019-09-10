#  For game rules : https://fivethirtyeight.com/features/get-perplexed-with-some-games-of-hex/
library(dplyr)
library(plot.matrix)
library(plotrix)
# library(purrr)

refresh_sim <- function(mat_size = 5, preset = 1) {
  
  
  ## Format for translation (Maybe make into a function)[row, col] (col - 1) * mat_size + row
  if(is.null(preset)) return(sim_mat)
  if(preset == 1) {
    mat_size <- 5
    precoords <- list("O" = c((1 - 1)  * mat_size + 1, (3 - 1) * mat_size + 3,
                              (3 - 1) * mat_size + 5),
                      "X" = c((2 - 1) * mat_size + 2, (3 - 1) * mat_size + 1,
                              (4 - 1) * mat_size + 3)) # bad math
  }
  
  sim_mat <- matrix(nrow = mat_size, ncol = mat_size) %>% 
    replace(precoords[[1]], names(precoords[1])) %>% 
    replace(precoords[[2]], names(precoords[2])) 
   
  
  return(sim_mat)
}

## Note need to finish fixing refresh sim preset work
# debug(refresh_sim)
## Determine if target link matches any of it's hexagonal neighbors and return their indices
link_match <- function(link, sim_mat) {
  coordrep <- length(sim_mat[,1])
  coordvec <- 1:length(sim_mat)
  
  
  hex_offset <- ifelse((link %% coordrep) %% 2 == 0,  1, -1)
  hex_offset <- ifelse(link %% coordrep == 0 & link %% 2 == 1,
                       -hex_offset, hex_offset)
  
  links <- c(link - 1, link + 1,
             link - coordrep, link + coordrep,
             link + coordrep * hex_offset - 1,
             link + coordrep * hex_offset + 1)
  links <- links[links > 0 & links < max(coordvec)]
  # Prevent wrapping (checking links from bottom against top)
  if (link %in% coordvec[coordvec %% coordrep %in% c(0, 1)]) {
    links <- links[!(links %% coordrep == 1 - link %% coordrep)]
  }
  player <- sim_mat[link]
  valid_links <- links[sim_mat[links] == player & !is.na(sim_mat[links])]
  return(valid_links)
}

## Iterate over links along edge to determine if there are any complete paths to opposing edge
winner_select <- function (sim_mat) {
  coordrep <- length(sim_mat[,1])
  coordvec <- 1:length(sim_mat)
  
  # Check sim_mat is square matrix (for a fair game)
  if(coordrep^2 != length(sim_mat)) stop("sim_mat must be a square matrix")
  
  winvec_list <- list("X" = 1:coordrep,
                      "O" = coordvec[coordvec %% coordrep == 1])
  
  start_edge_o <- coordvec[sim_mat == "O" & coordvec %% coordrep == 0]
  start_edge_x <- coordvec[sim_mat == "X" & coordvec %in% 
                             (coordrep^2 - coordrep + 1):(coordrep^2)]
  start_edge <- c(start_edge_o, start_edge_x)
  
  allchecked_links <- 0
  winner <- FALSE
  for(startloc in start_edge) {
    if(startloc %in% allchecked_links | is.na(sim_mat[startloc])) next()
    # Hexagonal grid
    player <- sim_mat[startloc]
    newlinks <- link_match(startloc, sim_mat)
    checked_links <- startloc
    
    while(any(!(newlinks %in% checked_links))) {
      tocheck_links <- newlinks[!(newlinks %in% checked_links |
                                    is.na(newlinks))]
      checked_links <- c(checked_links, newlinks)
      if(any(checked_links %in% winvec_list[[player]])) break()
      newlinks <- lapply(tocheck_links, FUN = link_match, sim_mat = sim_mat) %>% 
        unlist %>% as.numeric()
    } 
    
    if(any(checked_links %in% winvec_list[[player]])) {
      print(paste0("Player '", player, "' is victorious!"))
      plotmat <- sim_mat
      plotmat[checked_links] <- "Win-path"
      plot(plotmat, col = c("O" = "white", "Win-path" = "green", "X" = "black"), 
           na.col = "orange")
      winner <- TRUE
      return(winner)
    }
    allchecked_links <- c(allchecked_links, checked_links)
  }
  if(!winner) print("Draw Game, no Victors")
  return(winner)
}



self_play <- function(sim_mat, turn) {
  draw <- TRUE
  while(any(is.na(sim_mat))) {
    emptycoord <- which(is.na(sim_mat))
    nxtmove <- sample(emptycoord, size = 1)
    if(turn %% 2 == 0) move <- "X" else move <- "O"
    
    sim_mat[nxtmove] <- move
    turn <- turn + 1
    if(winner_select(sim_mat)) {
      draw <- FALSE
      break()
    }
  }
  if(draw) print("Draw") 
  return(sim_mat)
}

contra_turn <- function(sim_mat, turn) {
  emptycoord <- which(is.na(sim_mat))
  nxtmove <- sample(emptycoord, size = 1)
  if(turn %% 2 == 0) move <- "X" else move <- "O"
  
  sim_mat[nxtmove] <- move
  return(sim_mat)
}


hexplot <- function(plot_matrix, colorkey) {
  dims <- dim(plot_matrix)
  vals <- unique(c(plot_matrix))
  x <- matrix(rep(0, dims[1] * dims[2]), dims[2])
  cellcol <- matrix(rep("#DCDCDC", dims[1] * dims[2]), nrow = dims[2])
  
  for(i in 1:length(colorkey)) {
    cellcol[plot_matrix == names(colorkey[i])] <- colorkey[i][[1]]
  }
  
  color2D.matplot(x, cellcolors = cellcol, xlab = "Columns",
                  ylab = "Rows", border = "Black",
                  do.hex = TRUE, main = "2D matrix plot (hexagons)")
}






### Old Funs

# populate_mat <- function(sim_mat) {
#   turn <- 1
#   while(any(is.na(sim_mat))) {
#     emptycoord <- which(is.na(sim_mat))
#     nxtmove <- sample(emptycoord, size = 1)
#     if(turn %% 2 == 0) move <- "X" else move <- "O"
#     
#     sim_mat[nxtmove] <- move
#     turn <- turn + 1
#   }
#   return(sim_mat)
# }

####### Goals ==================================================================
## Short term
# Winner messages
# Be able to interact with the game through controls for self play
# Be able to input moves/play against the rng gods
#     - Specify move on specific turn
#     - run a game through console
#     - run a game through shiny


## Medium term
# Highlighting victory PATH in display
# Turn count

## Long Term
# make rng gods smarter 
#     - Simulating play? (take best result of X number of simulated games, but how many reps?)
#     - better formula (maximise future choise, efficiency, heuristics, ML?)

## Potential additions
# Saving state (bookmarking)
# Odds of success
# 1 and 2 player modes






























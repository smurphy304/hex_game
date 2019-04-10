#  For game rules : https://fivethirtyeight.com/features/get-perplexed-with-some-games-of-hex/
library(dplyr)
library(plot.matrix)
library(plotrix)

refresh_sim <- function() {
  sim_mat <- matrix(nrow = 5, ncol = 5)
  
  #  Initial state
  sim_mat[1,1] <- "O"
  sim_mat[3,3] <- "O"
  sim_mat[5,3] <- "O"
  
  sim_mat[2,2] <- "X"
  sim_mat[1,3] <- "X"
  sim_mat[3,4] <- "X"
  
  
  sim_mat
}


sim_mat <- refresh_sim()
populate_mat <- function(sim_mat) {
  turn <- 1
  while(any(is.na(sim_mat))) {
    emptycoord <- which(is.na(sim_mat))
    nxtmove <- sample(emptycoord, size = 1)
    if(turn %% 2 == 0) move <- "X" else move <- "O"
    
    sim_mat[nxtmove] <- move
    turn <- turn + 1
  }
  return(sim_mat)
}


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

# debug(winner_select)

sim_mat[3,3] <- NA
sim_mat <- refresh_sim()
sim_mat <- populate_mat(sim_mat)
winner_select(sim_mat)
hexplot(sim_mat, colorkey)

self_play <- function(sim_mat) {
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
sim_mat <- refresh_sim()
output_mat <- self_play(sim_mat)
hexplot(output_mat, colorkey)
winner_select(output_mat)


colorkey <- list("X" = "Black",
                 "O" = "White")

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


# Experiments with plots -----------------------------------------------------
## Hexbin
library(hexbin)


hexbin(output_mat)

hexbinplot(hexbin(output_mat))
x <- rnorm(10000)
y <- rnorm(10000)
(bin <- hexbin(x, y))

plot(hexbin(x, y + x*(x+1)/4),
     main = "(X, X(X+1)/4 + Y)  where X,Y ~ rnorm(10000)")

plot(bin, style = "nested.lattice")

x[runif(6, 0, length(x))] <- NA
y[runif(7, 0, length(y))] <- NA
hbN <- hexbin(x,y)
summary(hbN)


## plotrix (SUCCESS!!)
library(plotrix)

x<-matrix(rnorm(100),nrow=10)
cellcol<-matrix(rep("#DCDCDC",100),nrow=10)
cellcol[x<0]<-color.scale(x[x<0],c(1,0.8),c(0,0.8),0)
cellcol[x>0]<-color.scale(x[x>0],0,c(0.8,1),c(0.8,0))
# now do hexagons without borders
color2D.matplot(x,cellcolors=cellcol,xlab="Columns",ylab="Rows",
                do.hex=TRUE,main="2D matrix plot (hexagons)",border=NA)



#### Interesting color fade
rb<-colorRampPalette(c("red","blue"))(255)
trans<-sapply(seq(from=0,to=1,length.out=255),function(op) rgb(1,1,1,op))

image(matrix(1:255,ncol=255,nrow=255),col=rb,xaxt="n",yaxt="n")
par(new=T)
image(t(matrix(255:1,ncol=255,nrow=255)),col=trans,xaxt="n",yaxt="n")

####### Goals ==================================================================
# Be able to input moves/play against the rng gods
#     - Specify move on specific turn
#     - run a game through console
#     - run a game through shiny
# make rng gods smarter 
#     - Simulating play? (take best result of X number of simulated games, but how many reps?)
#     - better formula (maximise future choise, efficiency, heuristics, ML?)
































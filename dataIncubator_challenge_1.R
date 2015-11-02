options(digits=10)
#construct the initial prob grid, set universe based on how many moves there will be:
cPGrid <-  function(nMove){
  size <- nMove*2+1
  PG <- mat.or.vec(nr=size, nc=size)
  PG[ nMove+1, nMove+1] <- 1
  return(PG)
}

#given the last iteration, get the next probGrid:
getNextPGrid <- function(PG){
  newPG <- mat.or.vec(nr=nrow(PG), nc=nrow(PG))
  for(i in 1:nrow(PG)){
    if(i==1){
      newPG[i,] <- .25*PG[i+1,]
    } else if(i==nrow(PG)){
      newPG[i,] <- .25*PG[i-1,]
    } else {
      newPG[i,] <- .25*PG[i+1,] + .25*PG[i-1,]
    }
  }
  for(i in 1:ncol(PG)){
    if(i==1){
      newPG[,i] <- newPG[,i] + .25*PG[,i+1]
    } else if(i==ncol(PG)){
      newPG[,i] <- newPG[,i] + .25*PG[,i-1]
    } else {
      newPG[,i] <- newPG[,i] + .25*PG[,i+1] + .25*PG[,i-1]
    }
  }
  return(newPG)
}

#get euclidean distances to origin for all points on grid:
getDistToO <- function(PG){
  s <- 1+(nrow(PG)-1)/2
  subPG <- mat.or.vec(nr=s, nc=s)
  subPG[,1] <- subPG[1,] <- c(0:(nrow(subPG)-1))
  D <- subPG
  for(i in 2:nrow(subPG)){
    D[i,] <- sqrt(subPG[,1]^2 + subPG[1,i]^2)
  }
  DtoO <- PG
  DtoO[1:s, 1:s] <- D[ nrow(D):1 , nrow(D):1]
  DtoO[s:nrow(PG), 1:s] <- D[  ,nrow(D):1]
  DtoO[1:s, s:nrow(PG)] <- D[ nrow(D):1 , ]
  DtoO[s:nrow(PG), s:nrow(PG)] <- D
  return(DtoO)
}

#get taxicab boundary using a euclidean distance, defined by 0,1 grid:
getBdry <- function(D, thresh){
  f <- mat.or.vec(nr=nrow(D), nc=ncol(D))
  for(i in 1:((ncol(D)+1)/2) ){
    startInd <- (nrow(D)+1)/2
    if(D[startInd,i] == thresh) {
      f[startInd,i] <- 1
    } else if(D[startInd,i] < thresh) {
      for(j in startInd:nrow(D)){
        if(D[j,i] >= thresh) {
          f[j,i]  <- 1
          if( f[j,i-1]!=1 & f[j-1,i-1]!=1   ) {
            ind <- j-1
            while( f[ind,i-1] == 0){
              f[ind,i-1] <- 1
              ind <- ind - 1
            }
          }
          break
        }
      }
    }
  }
  s <- 1+(nrow(D)-1)/2
  Bdry <- f
  f <- f[ s:nrow(f) , 1:s]
  Bdry[1:s, 1:s] <- f[ nrow(f):1 , ]
  Bdry[s:nrow(D), 1:s] <- f
  Bdry[1:s, s:nrow(D)] <- f[ nrow(f):1 , nrow(f):1]
  Bdry[s:nrow(D), s:nrow(D)] <- f[  ,nrow(f):1]
  return(Bdry)
}

#get pascal weights of each square on grid, to apply to rings:
getPascalWeight <- function(PG){
  s <- 1+(nrow(PG)-1)/2
  subPG <- mat.or.vec(nr=s, nc=s)
  subPG[,1] <- subPG[nrow(subPG),] <- 1
  for(i in 2:ncol(subPG)){
    for(j in 2:nrow(subPG)){
      subPG[nrow(subPG)-j+1,i] <- subPG[nrow(subPG)-j+2,i]+subPG[nrow(subPG)-j+1,i-1]  #    sqrt(subPG[,1]^2 + subPG[1,i]^2)
    }
  }
  Pasc <- PG
  Pasc[1:s, 1:s] <- subPG[  ,nrow(subPG):1]
  Pasc[s:nrow(PG), 1:s] <- subPG[ nrow(subPG):1 , nrow(subPG):1]
  Pasc[1:s, s:nrow(PG)] <-  subPG
  Pasc[s:nrow(PG), s:nrow(PG)] <- subPG[ nrow(subPG):1 , ]
  return(Pasc)
}

# label each point on a grid with its ring number:
getRingLabels <- function(PG){
  s <- 1+(nrow(PG)-1)/2
  subPG <- mat.or.vec(nr=s, nc=s)
  subPG[,1] <- subPG[1,] <- c(1:(nrow(subPG)))
  for(i in 2:ncol(subPG)){
    for(j in 2:nrow(subPG)){
      subPG[i,j] <-  i+j-1
    }
  }
  DtoO <- PG
  DtoO[1:s, 1:s] <- subPG[ nrow(subPG):1 , nrow(subPG):1]
  DtoO[s:nrow(PG), 1:s] <- subPG[  ,nrow(subPG):1]
  DtoO[1:s, s:nrow(PG)] <- subPG[ nrow(subPG):1 , ]
  DtoO[s:nrow(PG), s:nrow(PG)] <- subPG
  return(DtoO)
}

#get probs of transitioning from ring n-1 to ring n:
getAvgMovesToBoundary <- function(D){
  avgMovesToNextRing <- rep(1,D)
  probOfMovingOutward <- rep(1,D)
  if(D==1) return( list(avgMovesToNextRing, probOfMovingOutward) )
  #avgMovesToNextRing[2] <- 1
  #probOfMovingOutward[2] <- 0.75
  #if(D==2) return( list(avgMovesToNextRing, probOfMovingOutward) )
  for(i in 2:D){
    pointNodeRange <- 12
    interiorNodeRange <- ( 4*(i-2) *2 )
    boundarySize <- 16*(i-1)
    expandFrac <-  (pointNodeRange+interiorNodeRange)/boundarySize
    probOfMovingOutward[i] <- expandFrac
    avgMovesToNextRing[i] <- (probOfMovingOutward[i] + ((1-probOfMovingOutward[i])*(avgMovesToNextRing[i-1]) ))/probOfMovingOutward[i]
  }
  #return a vector with avg moves for each integer up to D:
  return(list(avgMovesToNextRing, probOfMovingOutward))
}

#################################################
#################################################
#################################################

# problem 1:
PG <-  cPGrid(10)
for( i in 1:10){
  PG <- getNextPGrid(PG)
}
D <- getDistToO(PG)
filter <- D >= 3
sum( PG[filter] )

#problem 2:
PG <-  cPGrid(60)
for( i in 1:60){
  PG <- getNextPGrid(PG)
}
D <- getDistToO(PG)
filter <- D >= 10
sum( PG[filter] )

#problem 3- take 1-pr(never more than that distance within 10 moves):
PG <-  cPGrid(10)
D <- getDistToO(PG)
filter <- D >= 5
for( i in 1:10){
  PG <- getNextPGrid(PG)
  PG[filter] <- 0
}
1-sum( PG )


#problem 4- take 1-pr(never more than that distance within 60 moves):
PG <-  cPGrid(60)
D <- getDistToO(PG)
filter <- D >= 10
for( i in 1:10){
  PG <- getNextPGrid(PG)
  PG[filter] <- 0
}
1-sum( PG )


#problem 5- keep 2 separate grids, and aply a fltering step to allow bleed over of conditional success:
PG <-  cPGrid(10)
PG_2 <-  mat.or.vec(nr=nrow(PG),nc=nrow(PG))
D <- getDistToO(PG)
for( i in 1:10){
  PG <- getNextPGrid(PG)
  PG_2 <- getNextPGrid(PG_2)
  PG_2[, 13:ncol(PG)] <-  PG_2[, 13:ncol(PG)] + PG[, 13:ncol(PG)]
  PG[, 13:ncol(PG)] <- 0
}
sum( PG_2[,1:9] )


#problem 6- keep 2 separate grids, and aply a fltering step to allow bleed over of conditional success:
PG <-  cPGrid(30)
PG_2 <-  mat.or.vec(nr=nrow(PG),nc=nrow(PG))
D <- getDistToO(PG)
for( i in 1:30){
  PG <- getNextPGrid(PG)
  PG_2 <- getNextPGrid(PG_2)
  PG_2[, 33:ncol(PG)] <-  PG_2[, 33:ncol(PG)] + PG[, 33:ncol(PG)]
  PG[, 33:ncol(PG)] <- 0
}
sum( PG_2[,1:29] )


#problem 7:
PG <-  cPGrid(10)
D <- getDistToO(PG)
B <- getBdry(D,10)
filterInds <- which(B==1)
move <- 0
avgMoves <- 0
while(sum(PG) > 0.00000000001){
  avgMoves <- avgMoves + move*(sum(PG[filterInds]))
  PG[filterInds] <- 0
  PG <- getNextPGrid(PG)
  move <- move + 1
}
avgMoves
# 104.68785

#problem 8:
PG <-  cPGrid(60)
D <- getDistToO(PG)
B <- getBdry(D,60)
filterInds <- which(B==1)
move <- 0
avgMoves <- 0
while(sum(PG) > 0.00000000001){
  avgMoves <- avgMoves + move*(sum(PG[filterInds]))
  PG[filterInds] <- 0
  PG <- getNextPGrid(PG)
  move <- move + 1
}
avgMoves
# 3634.329502
























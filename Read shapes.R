setwd('/Users/hannah/Dropbox/Westneat Lab/Eartheater Project/Data/Videos/')

library(StereoMorph)
library(svgViewR)

source('path_connect.R')

get.dist <- function(p1, p2){
  distance <- sqrt((p1[1]-p2[1])^2+(p1[2]-p2[2])^2)
  return(distance)
}

dist.vect <- function(arr1, p1, p2) {
  new.vect <- rep(0, length(arr1[1,1,]))
  for (i in 1:length(arr1[1,1,])) {
    new.vect[i] <- get.dist(arr1[p1,,i], arr1[p2,,i])
  }
  return(new.vect)
}

TranslatePoints <- function(arr1) {
  
  # empty array to fill
  arr2 <- array(data = NA, dim(arr1))
  
  # fills array with translated coordinates
  for (i in 1:length(arr1[1,1,])){
    Dx <- arr1[8,1,i] # amount to translate x by (just X1)
    Dy <- arr1[8,2,i] # amount to translate y by
    TMatrix <- cbind(rep(-Dx, length(arr1[,1,1])), rep(-Dy, length(arr1[,1,1]))) # matrix to add to original matrix
    arr2[ , , i] <- arr1[ , , i] + TMatrix # translates each point by -Dx, -Dy
  }
  return(arr2)
}

RotatePoints <- function(arr1) {
  thetavector <- rep(0, length(arr1[1,1,])) # empty vector
  for (i in 1:length(arr1[1,1,])){ # angle to rotate is angle between point1/point2 and the horizontal
    X2 <- arr1[9,1,i]
    Y2 <- arr1[9,2,i]
    theta <- atan(Y2/X2) # tan(opposite/adjacent) to find angle
    theta <- theta - pi/4
    thetavector[i] <- -theta # fill vector
  }
  
  RotationMatrix <- array(data = NA, c(2,2,length(thetavector))) # empty array
  
  for (i in 1:length(thetavector)){ # create rotation matrix
    costheta <- cos(thetavector[i])
    sintheta <- sin(thetavector[i])
    nsintheta <- -sin(thetavector[i])
    RotationMatrix[,,i] <- matrix(c(costheta, nsintheta, sintheta, costheta),
                                  ncol=2, nrow=2)
  }
  
  for (i in 1:length(arr1[1,1,])) {
    arr1[ , , i] <- arr1[ , , i] %*% RotationMatrix[ , , i] 
    # rotate points by multiplying original coordinates by rotation matrix
  }
  return(arr1)
}

# rotates coordinates using matrix multiplication with rotation matrix

run <- function(file){

	shapes <- readShapes(file)
	shapes$landmarks.pixel[, 2, ] <- -shapes$landmarks.pixel[, 2, ]
	
	points <- shapes$landmarks.pixel # n x 2 x m array (n = # points, m = # frames)
	points2 <- TranslatePoints(points)
	# points3 <- RotatePoints(points2)
	
	frames <- length(points2[1,1,])
	points.n <- length(points2[,1,1])
	endpts <- points2[,,c(1,frames)] # points x X/Y x first/last frame
	point.names <- c("1", "2")
	
	plot(endpts[8,1,], endpts[8,2,], pch=19, col=8,  # plot eyes (will be stacked on top of each other)
	     xlim=c(min(points2[,1,]), max(points2[,1,])), 
	     ylim=c(min(points2[,2,]), max(points2[,2,])),
	     main='Points')

	for (i in 1:points.n){ # plot every other point start/end with labels
	  points(endpts[i,1,], endpts[i,2,], pch=19, col=i)
	  text(endpts[i,1,]+10, endpts[i,2,]+10, point.names, cex=0.5)
	  points(points2[i,1,], points2[i,2,], pch=19, col=i, type='l')
	}
	legend('topleft', c('Premax', 'Dent', 'Max.Ant', 'Max.Post', 'Mouth.corner', 'Operculum',
	                    'Branchiostegals', 'Eye', 'Epaxial'), pch=19, col=c(1:points.n), cex=0.6)

	
	# get branchiostegal distances
	plot(dist.vect(points2, 6, 7), pch=19, type='b', main='Branchiostegal distances',
	     ylab='Distance (pixels)', xlab = 'Frame')
	
	# mouth gape
	plot(dist.vect(points2, 1, 2), pch=19, type='b', main='Mouth gape',
	     ylab = 'Distance (pixels)', xlab = 'Frame')
	print(max(dist.vect(points2, 1, 2)))
	
	
	
	mouth.gape <- dist.vect(points2, 1, 2)/max(dist.vect(points2, 1, 2))
	branch.dist <- dist.vect(points2, 6, 7)/max(dist.vect(points2, 6, 7))
	
	# BOTH BOTH BOTH
	plot(branch.dist, pch=19, type='l', col = 'red', main='Branchiostegal distances + Mouth gape',
	     ylab='Distance (pixels)', lwd=2, xlab = 'Frame')
	points(c(1:frames), mouth.gape, pch=18, lwd=2, lty=2, col='blue', type='l')
	legend('bottomright', c('Branchs', 'Mouth'), lty=1, pch=c(19, 18), col=c('red', 'blue'), cex=0.6)
	
}


run('01-30-16/Shapes/W_SD3_01_15FrameShapes/')
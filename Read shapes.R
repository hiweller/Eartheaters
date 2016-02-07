library(StereoMorph)
library(svgViewR)

source('path_connect.R')

run <- function(){

	shapes <- readShapes('Shapes')

	shapes$landmarks.pixel[, 2, ] <- -shapes$landmarks.pixel[, 2, ]

	drawLandmarks(landmarks=shapes$landmarks.pixel, file='Animation.html', 
		path.connect=path_connect, animate.duration = 1)

}

run()
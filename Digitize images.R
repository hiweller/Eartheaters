setwd('/Users/hannah/Dropbox/Westneat Lab/Eartheater Project/Data/Videos/')
library(StereoMorph)

run <- function(){

	digitizeImages(
  		image.file='02-06-16/Intervals/S_SD3_01_5FrameIntervals/', 
		shapes.file='02-06-16/Shapes/S_SD3_01_5FrameShapes/', 
		landmarks.ref='landmarks.txt', 
		curves.ref='curves.txt'
	)

}

run()

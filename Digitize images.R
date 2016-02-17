setwd('/Users/hannah/Dropbox/Westneat Lab/Eartheater Project/Data/Videos/')
library(StereoMorph)
library(jpeg)

run <- function(){

	digitizeImages(
  		image.file='02-06-16/Intervals/W_SD3_20A_15FrameIntervals/', 
		shapes.file='02-06-16/Shapes/W_SD3_20A_15FrameShapes/', 
		landmarks.ref='landmarks.txt', 
		curves.ref='curves.txt'
	)

}

run()

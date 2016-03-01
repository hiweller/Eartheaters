setwd('/Users/hannah/Dropbox/Westneat Lab/Eartheater Project/Data/Videos/')
library(StereoMorph)
library(jpeg)

run <- function(){

	digitizeImages(
  	image.file='02-26-16/Intervals/W_SD2_21A_15FrameIntervals/', 
		shapes.file='02-26-16/Shapes/W_SD2_21A_15FrameShapes/', 
		landmarks.ref='landmarks.txt', 
		curves.ref='curves.txt'
	)

}

run()

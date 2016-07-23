setwd('/Users/hannah/Dropbox/Westneat_Lab/Eartheater Project/Data/Videos/')
# setwd('/Users/hannah/Dropbox/Westneat_Lab/Eartheater Project/Data/EBL/')
library(StereoMorph)
library(jpeg)

run <- function(){

	digitizeImages(
    image.file='02-26-16/Intervals/E_SD2_21_5FrameIntervals/', 
		shapes.file='02-26-16/Shapes/E_SD2_21_5FrameShapes/', 
		landmarks.ref='landmarks.txt', 
		curves.ref='shapecurves.txt'
	)

}

run()

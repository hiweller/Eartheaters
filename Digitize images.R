library(StereoMorph)

run <- function(){

	digitizeImages(
		image.file='Images', 
		shapes.file='Shapes', 
		landmarks.ref='landmarks.txt', 
		curves.ref='curves.txt'
	)

}

run()
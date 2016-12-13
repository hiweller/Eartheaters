# images <- dir('./Images/', pattern="*.png")
# for (i in 1:length(images)) {
#   oldName <- paste('./Images/', images[i], sep="")
#   newName <- gsub(" ", "-", oldName)
#   file.rename(oldName, newName)
# }

setwd('/Users/hannah/Dropbox/Westneat_Lab/Eartheater Project/Data/Dissection_photos/EBL/Lateral_dig/')

library(StereoMorph)
library(jpeg)

run <- function(){

  digitizeImages(
    image.file='./Images/',
    shapes.file='./Shapes/',
    landmarks.ref='landmarks.txt',
    curves.ref='curves.txt'
  )

}

run()

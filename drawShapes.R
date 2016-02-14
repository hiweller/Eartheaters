setwd('/Users/hannah/Dropbox/Westneat Lab/Eartheater Project/Data/Videos/')
library(StereoMorph)

run <- function(){
  
  shapes <- readShapes('01-30-16/Shapes/S_SD3_07_5FrameShapes/')
  
  landmarks <- shapes$landmarks.pixel
  landmarks[, 2, ] <- -landmarks[, 2, ]
  landmarks[c('premax_tip','dentary_tip'), , 1] <- NA

    # set NA to landmarks you don't want to use
  for(i in 2:dim(landmarks)[3])
  landmarks[, , i] <- findOptimalPointAlignment(landmarks[, , 1], landmarks[, , i])
  #print(landmarks[, , 2])

  shapes$landmarks <- landmarks
 # return(1)
  
  drawShapes(shapes=shapes, 
    file='01-30-16/S_SD3_07_5FrameShapes.html', window.title='S_SD3_07_5FrameShapes',
    path.connect='01-30-16/path_connect.txt')
}

run()


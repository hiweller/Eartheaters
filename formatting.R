setwd('/Users/hannah/Dropbox/Westneat Lab/Eartheater Project/Data/Coordinates/')
csvdir <- dir('/Users/hannah/Dropbox/Westneat Lab/Eartheater Project/Data/Coordinates/',
                pattern='*.csv')
txtdir <- dir('/Users/hannah/Dropbox/Westneat Lab/Eartheater Project/Data/Coordinates/',
              pattern='*.txt')
points <- 18

finalfile <- matrix(0, nrow=(length(coorddir)-1), ncol=points*2)
for (i in 1:(length(coorddir)-1)) {
  coordfile <- read.csv2(coorddir[i], sep=',', header=T)
  newcoords <- as.matrix(coordfile[,2:3])
  vectorcoords <- as.vector(t(newcoords))
  finalfile[i,] <- vectorcoords
}

finalfile <- as.data.frame(finalfile, row.names = coorddir, col.names=NULL)
write.csv(finalfile, 'MorphoJCoordinates.csv')

setwd('/Users/hannah/Dropbox/Westneat Lab/Eartheater Project/Data/Coordinates/')
coorddir <- dir('/Users/hannah/Dropbox/Westneat Lab/Eartheater Project/Data/Coordinates/',
                pattern='*.csv')

points <- 19

finalfile <- matrix(0, nrow=length(coorddir), ncol=points*2)
for (i in 1:length(coorddir)) {
  coordfile <- read.csv2(coorddir[i], sep=',', header=T)
  newcoords <- as.matrix(coordfile[,2:3])
  vectorcoords <- as.vector(t(newcoords))
  finalfile[i,] <- vectorcoords
}

finalfile <- as.data.frame(finalfile, row.names = substr(coorddir, 1, 10), col.names=NULL)
write.csv(finalfile, '../MorphoJ_files/MorphoJCoordinates.csv')

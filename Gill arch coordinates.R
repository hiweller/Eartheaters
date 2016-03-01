source('/Users/hannah/Dropbox/Westneat Lab/Eartheater Project/Code/genfunctions.R')
setwd('/Users/hannah/Dropbox/Westneat Lab/Eartheater Project/Data/Dissection_photos/Gill arches/')


gill.coord.dir <- dir(path='.', pattern='*.csv')
save.names <- c('Arch width (cm)', 'Arch height (cm)', 'EBL aspect ratio',
                'EBL raker aspect ratio', 'Gill raker aspect ratio',
                'Arch aspect ratio', 'EBL angle')

ID.names <- vector()
Arch.width <- vector()
Arch.height.ish <- vector()
EBL.AR <- vector()
EBL.raker.AR <- vector()
Gill.raker.AR <- vector()
Arch.AR <- vector()
Lobe.ang <- vector()

for (i in 1:length(gill.coord.dir)) {
  
  ID.names[i] <- as.character(strsplit(gill.coord.dir[i], split=".csv")[1])
  
  gill.coords <- read.csv(gill.coord.dir[i], header=TRUE)
  gill.coords <- gill.coords[,2:3]
  
  # distances
  d1.2 <- get.dist(gill.coords[1,], gill.coords[2,])
  d3.4 <- get.dist(gill.coords[3,], gill.coords[4,])
  d6.7 <- get.dist(gill.coords[6,], gill.coords[7,])
  d8.9 <- get.dist(gill.coords[8,], gill.coords[9,])
  d1.5 <- get.dist(gill.coords[1,], gill.coords[5,])
  d5.11 <- get.dist(gill.coords[5,], gill.coords[11,])
  d1.11 <- get.dist(gill.coords[1,], gill.coords[11,])
  d2.16 <- get.dist(gill.coords[2,], gill.coords[16,])
  d10.11 <- get.dist(gill.coords[10,], gill.coords[11,])
  d12.13 <- get.dist(gill.coords[12,], gill.coords[13,])
  d14.15 <- get.dist(gill.coords[14,], gill.coords[15,])
  
  # distances
  Arch.width[i] <- d10.11
  Arch.height.ish[i] <- d2.16
  
  # aspect ratios
  EBL.AR[i] <- d1.2/d3.4
  EBL.raker.AR[i] <- d6.7/d8.9
  Gill.raker.AR[i] <- d12.13/d14.15
  Arch.AR[i] <- Arch.width[i]/Arch.height.ish[i]
  
  # angle of lobe
  Lobe.ang[i] <- TriangleCalc(d1.5, d5.11, d1.11)[1]
  
}

gill.dataframe <- data.frame(Arch.width, Arch.height.ish, 
                             EBL.AR, EBL.raker.AR, Gill.raker.AR,
                             Arch.AR, Lobe.ang, row.names = ID.names)
write.csv(round(gill.dataframe, digits=3), file = '/Users/hannah/Dropbox/Westneat Lab/Eartheater Project/Data/Data_sheets/Gill_arch_individual.csv')

gill.dataframe <- as.matrix(gill.dataframe)

species.names <- vector()
species.dataframe <- matrix(NA, ncol=dim(gill.dataframe)[2], nrow=dim(gill.dataframe)[1])
counter <- 0
for (j in 1:(dim(gill.dataframe)[1]-1)) {
  ref1 <- strsplit(row.names(gill.dataframe)[j], split="_0")
  ref1 <- ref1[[1]][1]
  ref2 <- strsplit(row.names(gill.dataframe)[j+1], split="_0")
  ref2 <- ref2[[1]][1]
  
  if (ref1 == ref2) {
    counter <- counter+1
    species.names[counter] <- as.character(ref1)
    newrow <- as.vector((gill.dataframe[j,]+gill.dataframe[j+1,])/2)
    species.dataframe <- rbind(species.dataframe, newrow)
  }
}
new.dataframe <- as.data.frame(species.dataframe[rowSums(is.na(species.dataframe))<1,], 
                               row.names = species.names)
colnames(new.dataframe) <- save.names
write.csv(round(new.dataframe, digits=3), file = '/Users/hannah/Dropbox/Westneat Lab/Eartheater Project/Data/Data_sheets/Gill_arch_by_species.csv')
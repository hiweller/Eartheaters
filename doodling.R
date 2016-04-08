source('/Users/hannah/Dropbox/Westneat Lab/Eartheater Project/Code/genfunctions.R')
source('/Users/hannah/Dropbox/Westneat Lab/Eartheater Project/Code/Read shapes.R')
setwd('/Users/hannah/Dropbox/Westneat Lab/Eartheater Project/Data/Data_sheets/')
videos <- read.csv('Video_names.csv')

## correlate mouth gape with frequency?
# get max gape and frequency for each trial?
better2gether <- paste(videos$Date, videos$Trial)
# names of all strike videos
strikes <- as.character(better2gether[which(substr(better2gether, 10, 10) %in% 'S')])
# names of all winnowing videos
winnows <- as.character(better2gether[which(substr(better2gether, 10, 10) %in% 'W')])

matches <- intersect(paste(substr(winnows, 1, 8), substr(winnows, 11, 18)), paste(substr(strikes, 1, 8), substr(strikes, 11, 18)))
substr(strikes, 11, 18) %in% matches[i]

maxGape <- vector()
maxBranch <- vector()
maxPremax <- vector()
height <- vector()
frequency <- vector()
counter <- 0
peakNum <- vector()
for (i in 1:length(winnows)) {
  dateInd <- substr(winnows[i], 1, 8)
  dateInd <- gsub("/", "-", dateInd)
  winnowName <- paste(substr(winnows[i], 10, nchar(winnows[i])), 'FrameShapes', sep="")
  winnowDir <- paste('../Videos/', dateInd, '/Shapes/', winnowName, '/', sep="")
  
  if (length(dir(winnowDir, pattern='*.txt'))!=0) {
    counter <- counter + 1
    winnowDistances <- run.shapes(winnowDir, name.index='W')
    vec <- winnowDistances$branch.dist
    mouthvec <- winnowDistances$mouth.gape
    premaxvec <- winnowDistances$premax.dist
    peaks <- extract(turnpoints(vec), peak=1, pit=0)
    peaksMouth <- extract(turnpoints(mouthvec), peak=1, pit=0)
    peaksPremax <- extract(turnpoints(premaxvec), peak=1, pit=0)
    
    peakInd <- which(peaks %in% 1) # indices of peaks
    peakNum <- c(peakNum, length(peakInd))
    peakIndMouth <- which(peaksMouth %in% 1)
    peakIndPremax <- which(peaksPremax %in% 1)
    peakHeight <- vector()
    peakDist <- vector()
    peakHeightMouth <- vector()
    peakHeightPremax <- vector()
    for (k in peakInd) {
      peakHeight <- append(peakHeight, vec[k])
    }
    for (j in 1:(length(peakInd)-1)) {
      peakDist[j] <- peakInd[j+1]-peakInd[j]
    }
    for (l in peakIndMouth) {
      peakHeightMouth <- append(peakHeightMouth, mouthvec[l])
    }
    for (m in peakIndPremax) {
      peakHeightPremax <- append(peakHeightPremax, premaxvec[l])
    }
    peakDist <- peakDist*0.03 # milliseconds per 15 frames so peak distances are in seconds
    
    height <- c(height, mean(peakHeight))
    maxGape <- c(maxGape, mean(peakHeightMouth))
    maxPremax <- c(maxPremax, mean(peakHeightPremax))
    maxBranch <- c(maxBranch, mean(peakHeight))
    # NOTE: there's at least one trial with only 1 peak = no peakdist so frequency will have an NA entry
    frequency <- c(frequency, 1/mean(peakDist))
  }
}

maxGape <- vector()
maxBranch <- vector()
maxPremax <- vector()
height <- vector()
frequency <- vector()
counter <- 0

for (i in 1:length(matches)) {
  dateInd <- substr(matches[i], 1, 8)
  winnowName <- paste('W', substr(matches[i], 10, 17), '15FrameShapes', sep="")
  strikeName <- paste('S', substr(matches[i], 10, 17), '5FrameShapes', sep="")
  dateInd <- gsub("/", "-", dateInd)
  
  strikeDir <- paste('../Videos/', dateInd, '/Shapes/', strikeName, '/', sep="")
  winnowDir <- paste('../Videos/', dateInd, '/Shapes/', winnowName, '/', sep="")
  if (length(dir(strikeDir, pattern='*.txt'))!=0 && length(dir(winnowDir, pattern='*.txt'))!=0) {
    counter <- counter+1
    strikeDistances <- run.shapes(strikeDir, name.index='S')
    winnowDistances <- run.shapes(winnowDir, name.index='W')
    
    maxGape <- c(maxGape, max(strikeDistances$mouth.gape))
    maxBranch <- c(maxBranch, max(strikeDistances$branch.dist))
    maxPremax <- c(maxPremax, max(strikeDistances$premax.dist))
    
    vec <- winnowDistances$branch.dist
    peaks <- extract(turnpoints(vec), peak=1, pit=0)
    peakInd <- which(peaks %in% 1) # indices of peaks
    
    peakHeight <- vector()
    peakDist <- vector()
    
    for (k in peakInd) {
      peakHeight <- append(peakHeight, vec[k])
    }
    for (j in 1:(length(peakInd)-1)) {
      peakDist[j] <- peakInd[j+1]-peakInd[j]
    }
    peakDist <- peakDist*0.03 # milliseconds per 15 frames so peak distances are in seconds
    
    height <- c(height, mean(peakHeight))
    
    # NOTE: there's at least one trial with only 1 peak = no peakdist so frequency will have an NA entry
    frequency <- c(frequency, 1/mean(peakDist))
  }
}
# if there is a strike and winnowing trial for the same video name
# get the frequency and amplitude of the winnowing
# get the maximum mouth gape of the strike

summary(lm(frequency~maxGape))
# very weak...p = 0.1209, but positive linear relationship
# possible we would see stronger evidence with larger sample size. alas!

##

cbPalette <- c('dodgerblue', 'lightgreen', 'yellowgreen', 
               'green3', 'goldenrod1', 'brown1',
               'coral','lightslateblue', 'darkgreen',
               'mediumseagreen', 'turquoise', 'royalblue3')
procrustes <- read.delim('/Users/hannah/Dropbox/Westneat Lab/Eartheater Project/Data/MorphoJ_files/03-13-16.txt', sep='\t')

proc.coords <- procrustes[,3:40]

PCA <-prcomp(proc.coords)
PC1 <- -PCA$x[,1] # inverting it because they're inverted in morphoj for reasons i don't really understand
PC2 <- -PCA$x[,2]
eigenvalues <- PCA$sdev^2/sum(PCA$sdev^2)
Genus <- procrustes[,2]
PCdataframe <- data.frame(Genus, PC1, PC2)

g <- ggplot(PCdataframe, aes(x=PC1, y=PC2, label=Genus))
g <- g + geom_point(aes(color=factor(Genus)), size=3)
g <- g + scale_color_manual(values=cbPalette, name='Genus')
g <- g + xlab('PC1 (49.7% explained var.)') + ylab('PC2 (18.7% explained var.)')
g <- g + theme(text=element_text(size=15))
print(g)


## timing?
setwd('/Users/hannah/Dropbox/Westneat Lab/Eartheater Project/Data/Videos/')
strikes <- read.csv('Measurements/S_mouthgape.csv')[,-1]
winnows <- read.csv('Measurements/W_mouthgape.csv')[,-1]
ejections <- read.csv('Measurements/E_mouthgape.csv')[,-1]
# slengths <- (lengths(strikes)-1)*0.01
# wlengths <- (lengths(winnows)-1)*0.03
# elengths <- (lengths(ejections)-1)*0.01
# mean(slengths)
# mean(wlengths)
# mean(elengths)
maxes <- function(csv) {
  finalvec <- vector()
  for (i in 1:dim(csv)[1]) {
    finalvec[i] <- max(csv[i,], na.rm=TRUE)
  }
  return(finalvec)
}
paste(mean(maxes(strikes)), sd(maxes(strikes)))
paste(mean(maxes(winnows)), sd(maxes(winnows)))
paste(mean(maxes(ejections)), sd(maxes(ejections)))

lengths <- function(csv) {
  finalvec <- vector()
  for (i in 1:dim(csv)[1]) {
    row <- csv[i,]
    row <- row[is.na(row)==FALSE]
    finalvec[i] <- length(row)
  }
  return(finalvec)
}

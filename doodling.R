source('/Users/hannah/Dropbox/Westneat_Lab/Eartheater Project/Code/genfunctions.R')
setwd('/Users/hannah/Dropbox/Westneat_Lab/Eartheater Project/Data/Data_sheets/')
videos <- read.csv('Video_names.csv')

branchDist <- read.csv('../Videos/Measurements/W_branchiostegals.csv')[,-1]
for(i in 1:dim(branchDist)[1]){plot(branchDist[i,][is.na(branchDist[i,])==FALSE], type='l', ylab=i)}
# getting coefficient of variation for oral gape, premax excursion,
# branch expansion during stages

# for table 1 of text (3 phases)
tab1Means <- c(0.33, 7.14, 5.44, 2.74,
               1.31, 3.63, 3.46, 2.53,
               0.53, 8.53, 5.33, 2.11)
tab1SD <- c(0.09, 0.99, 0.70, 0.54,
            0.93, 1.01, 0.80, 0.49,
            0.21, 0.86, 0.78, 0.63)
CV1 <- tab1SD/tab1Means


# table 2 (just winnowing)
tab2Means <- c(6.10, 7.77, 1.99, 2.45, 2.72)
tab2SD <- c(0.99, 5.82, 0.48, 0.86, 0.62)
CV2 <- tab2SD/tab2Means
CV2

## correlate mouth gape with frequency?
# get max gape and frequency for each trial?
better2gether <- paste(videos$Date, videos$Trial)
# names of all strike videos
strikes <- as.character(better2gether[which(substr(better2gether, 10, 10) %in% 'S')])
# names of all winnowing videos
winnows <- as.character(better2gether[which(substr(better2gether, 10, 10) %in% 'W')])
# names of all the other ones. ejection videos?
ejections <- as.character(better2gether[which(substr(better2gether, 10, 10) %in% 'E')])


specTags <- vector("list", 3)
names(specTags) <- c("SD1", "SD2", "SD3")
# question: does max gape during strike depend on individual?
for (i in 1:length(strikes)) {
  # get date and specimen info
  dateInd <- substr(strikes[i], 1, 8)
  specTag <- as.numeric(substr(strikes[i], 14, 14))
  dateInd <- gsub("/", "-", dateInd)
  
  # make file names for accessing vids
  strikeName <- paste(substr(strikes[i], 10, nchar(strikes[i])), 'FrameShapes', sep="")
  strikeDir <- paste('../Videos/', dateInd, '/Shapes/', strikeName, '/', sep="")
  
  if (length(dir(strikeDir, pattern='*.txt'))!=0) {
    measurements <- run.shapes(strikeDir, name.index='S')
    mouthGape <- max(measurements$mouth.gape)
    specTags[[specTag]] <- append(specTags[[specTag]], mouthGape)
  }
}
summary(sigTest(specTags)[[1]])

specTags <- vector("list", 3)
names(specTags) <- c("SD1", "SD2", "SD3")
for (i in 1:length(winnows)) {
  # get date and specimen info
  dateInd <- substr(winnows[i], 1, 8)
  specTag <- as.numeric(substr(winnows[i], 14, 14))
  dateInd <- gsub("/", "-", dateInd)
  
  # make file names for accessing vids
  winnowName <- paste(substr(winnows[i], 10, nchar(winnows[i])), 'FrameShapes', sep="")
  winnowDir <- paste('../Videos/', dateInd, '/Shapes/', winnowName, '/', sep="")
  
  if (length(dir(winnowDir, pattern='*.txt'))!=0) {
    measurements <- run.shapes(winnowDir, name.index='S')
    mouthGape <- max(measurements$mouth.gape)
    specTags[[specTag]] <- append(specTags[[specTag]], mouthGape)
  }
}
summary(sigTest(specTags)[[1]])

specTags <- vector("list", 3)
names(specTags) <- c("SD1", "SD2", "SD3")
for (i in 1:length(ejections)) {
  # get date and specimen info
  dateInd <- substr(ejections[i], 1, 8)
  specTag <- as.numeric(substr(ejections[i], 14, 14))
  dateInd <- gsub("/", "-", dateInd)
  
  # make file names for accessing vids
  ejectionName <- paste(substr(ejections[i], 10, nchar(ejections[i])), 'FrameShapes', sep="")
  ejectionDir <- paste('../Videos/', dateInd, '/Shapes/', ejectionName, '/', sep="")
  
  if (length(dir(ejectionDir, pattern='*.txt'))!=0) {
    measurements <- run.shapes(ejectionDir, name.index='S')
    mouthGape <- max(measurements$mouth.gape)
    specTags[[specTag]] <- append(specTags[[specTag]], mouthGape)
  }
}
summary(sigTest(specTags)[[1]])

sigTest <- function(specTags) {
  allDist <- unlist(specTags)
  IDs <- c(rep("SD1", length(specTags[[1]])), 
           rep("SD2", length(specTags[[2]])),
           rep("SD3", length(specTags[[3]])))
  test <- data.frame(ID = IDs, dist = allDist)
  fit <- glm(allDist ~ factor(IDs))
  fit2 <- aov(allDist~factor(IDs))
  return(list(fit, fit2))}


matches <- intersect(paste(substr(winnows, 1, 8), substr(winnows, 11, 18)), paste(substr(strikes, 1, 8), substr(strikes, 11, 18)))
substr(strikes, 11, 18) %in% matches[i]

maxGape <- vector()
maxBranch <- vector()
maxPremax <- vector()
height <- vector()
frequency <- vector()
counter <- 0
peakNum <- vector()

specTags <- vector("list", 3)
names(specTags) <- c("SD1", "SD2", "SD3")
for (i in 1:length(winnows)) {
  # get date and specimen info
  dateInd <- substr(winnows[i], 1, 8)
  specTag <- as.numeric(substr(winnows[i], 14, 14))
  dateInd <- gsub("/", "-", dateInd)
  
  # make file names for accessing vids
  winnowName <- paste(substr(winnows[i], 10, nchar(winnows[i])), 'FrameShapes', sep="")
  winnowDir <- paste('../Videos/', dateInd, '/Shapes/', winnowName, '/', sep="")
  
  # only bother if this was actually digitized (i.e. there are actually shapes files in the folder)
  if (length(dir(winnowDir, pattern='*.txt'))!=0) {
    # use number of actually digitized trials for index
    counter <- counter + 1
    
    # pull out shape distances for each file using run.shapes from genfunctions.R
    winnowDistances <- run.shapes(winnowDir, name.index='W')
    vec <- winnowDistances$branch.dist
    mouthvec <- winnowDistances$mouth.gape
    premaxvec <- winnowDistances$premax.dist
    
    # get peaks of expansion for each of three measurements
    peaks <- extract(turnpoints(vec), peak=1, pit=0)
    peaksMouth <- extract(turnpoints(mouthvec), peak=1, pit=0)
    peaksPremax <- extract(turnpoints(premaxvec), peak=1, pit=0)
    
    peakInd <- which(peaks %in% 1) # indices of peaks
    
    # store number of peaks
    peakNum <- c(peakNum, length(peakInd))
    
    # peak indices
    peakIndMouth <- which(peaksMouth %in% 1)
    peakIndPremax <- which(peaksPremax %in% 1)
    peakHeight <- vector()
    peakDist <- vector()
    peakHeightMouth <- vector()
    peakHeightPremax <- vector()
    
    # for every peak index, store the measured value at that index in a new vector
    for (k in peakInd) {
      peakHeight <- append(peakHeight, vec[k])
    }
    
    # measure distances between peaks
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
    
    # store the distances between peaks
    byCycle[[counter]] <- peakDist
    
    specTags[[specTag]] <- append(specTags[[specTag]], mean(peakHeight))
    
    height <- c(height, mean(peakHeight))
    maxGape <- c(maxGape, mean(peakHeightMouth))
    maxPremax <- c(maxPremax, mean(peakHeightPremax))
    maxBranch <- c(maxBranch, mean(peakHeight))
    # NOTE: there's at least one trial with only 1 peak = no peakdist so frequency will have an NA entry
    frequency <- c(frequency, 1/mean(peakDist))
  }
}
# for (i in 1:length(byCycle)) {
#   if(sum(is.na(byCycle[[i]]))==0)
#   {plot(byCycle[[i]], pch=19, type = 'b')}
# }

allDist <- unlist(specTags)
IDs <- c(rep("SD1", length(specTags[[1]])), 
         rep("SD2", length(specTags[[2]])),
         rep("SD3", length(specTags[[3]])))
fit <- glm(allDist ~ factor(IDs))

# taking overall mean peak distance (rather than the mean of the mean for each trial)
# as i think about it now there is no point to this.
mean(1/unlist(byCycle), na.rm=T)
sd(unlist(byCycle), na.rm=T)
coVar(1/unlist(byCycle))

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
procrustes <- read.delim('/Users/hannah/Dropbox/Westneat_Lab/Eartheater Project/Data/MorphoJ_files/06-28-2016.txt', sep='\t')

proc.coords <- procrustes[,3:40]

PCA <-prcomp(proc.coords)
PC1 <- -PCA$x[,1] # inverting it because they're inverted in morphoj for reasons i don't really understand
PC2 <- -PCA$x[,2]
eigenvalues <- PCA$sdev^2/sum(PCA$sdev^2)
Genus <- procrustes[,2]
PCdataframe <- data.frame(Genus, PC1, PC2)
library(ggplot2)
g <- ggplot(PCdataframe, aes(x=PC1, y=PC2, label=Genus))
g <- g + geom_point(aes(color=factor(Genus)), size=4.5)
g <- g + scale_color_manual(values=cbPalette, name='Genus')
g <- g + xlab('PC1 (49.7% explained var.)') + ylab('PC2 (18.7% explained var.)')
g <- g + theme(text=element_text(size=20))
print(g)

ggsave(filename = '../Manuscript/Figs/PCA.eps', plot=last_plot(), dpi=600)


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

## FFTs
winnows <- as.matrix(read.csv('Measurements/W_branchiostegals.csv'))
winnow1 <- as.matrix(winnows[1,is.na(winnows[1,])==FALSE])
FFT.test <- fft(winnow1)

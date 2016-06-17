source('/Users/hannah/Dropbox/Westneat_Lab/Eartheater Project/Code/genfunctions.R')
setwd('/Users/hannah/Dropbox/Westneat_Lab/Eartheater Project/Data/Videos/')
videos <- read.csv('/Users/hannah/Dropbox/Westneat_Lab/Eartheater Project/Data/Data_sheets/Video_names.csv')
phases <- c("S", "W", "E")
filenames <- vector("list", 3)
names(filenames) <- phases

# k to summarize:
# CVtable is CVs of every feeding stage per individual per measurement
# shows that winnowing is most variable/has largest CVs for each, regardless of individual
# fit shows that even with effect of individual removed (which is significant) effect of stage is significant

mouthGape=numeric()
branchDist=numeric()
premaxDist=numeric()
specID=character()
stage=character()

col.names=c("mouthGape", "branchDist", "premaxDist", "specID", "stage")

for (i in 1:dim(videos)[1]) {
  dateInd <- substr(videos[i,2], 1, 8)
  dateInd <- gsub("/", "-", dateInd) # date index for finding shapes file
  specTag <- substr(videos[i,3], 3, 5) # specimen ID (SD1, SD2, or SD3)
  phaseTag <- substr(videos[i,3], 1, 1)
  
  # make file names for accessing vids
  trialName <- paste(videos[i,3], 'FrameShapes', sep="")
  trialDir <- paste('./', dateInd, '/Shapes/', trialName, '/', sep="")
  
  if (length(dir(trialDir, pattern='*.txt'))!=0) {
    measurements <- run.shapes(trialDir, name.index=phaseTag)
    mouthGape <- append(mouthGape, max(measurements$mouth.gape))
    branchDist <- append(branchDist, max(measurements$branch.dist))
    premaxDist <- append(premaxDist, max(measurements$premax.dist))
    specID <- append(specID, specTag)
    stage <- append(stage, phaseTag)
    # newRow <- c(mouthGape, branchDist, premaxDist, specTag, phaseTag)
    # anovaTable <- rbind(anovaTable, newRow)
  }
}
mouthGape <- mouthGape/max(mouthGape, na.rm = T)
branchDist <- branchDist/max(branchDist, na.rm = T)
premaxDist <- premaxDist/max(premaxDist, na.rm = T)

anovaTable <- data.frame(mouthGape, branchDist, premaxDist, specID, stage)
anovaTable <- na.omit(anovaTable)
fitBranch <- lm(branchDist~stage+specID, data=anovaTable)
fitMouth <- lm(mouthGape~stage+specID, data=anovaTable)
fitPmx <- lm(premaxDist~stage+specID, data=anovaTable)

# stageW has smallest p-value (machine epsilon) when controlling for measurement effects
# and individual effects which MEEEEANS that uh
# well that just means it's significantly different from the other two stages
anovaTable <- melt(anovaTable)
fit <- lm(value~specID+stage+variable, data=anovaTable)
summary(fit)


# COEFFICIENT OF VARIATION
# for every measurement, is relative variability (= CV = SD/mean) higher in winnowing?

Measurement <- rep(c(rep("Branch. exp.", 3), rep("Mouth gape", 3), rep("Pmx. protrusion", 3)), 3)
Individual <- c(rep("SD1", 9), rep("SD2", 9), rep("SD3", 9))
Stage <- c(rep(c("Strike", "Winnowing", "Ejection"), 9))
CVs <- vector()

CVlist <- vector("list", 3)
names(CVlist) <- c("SD1", "SD2", "SD3")
CVlist[[1]] <- anovaTable[anovaTable$specID=="SD1",]
CVlist[[2]] <- anovaTable[anovaTable$specID=="SD2",]
CVlist[[3]] <- anovaTable[anovaTable$specID=="SD3",]

for (i in 1:length(CVlist)) {
  individual <- CVlist[[i]]
  branchDist <- individual$value[individual$variable=='branchDist']
  mouthGape <- individual$value[individual$variable=='mouthGape']
  premaxDist <- individual$value[individual$variable=='premaxDist']
  
  # pull out CV for branchDist for S, W, E
  bS <- coVar(branchDist[individual$stage=='S'])
  bW <- coVar(branchDist[individual$stage=='W'])
  bE <- coVar(branchDist[individual$stage=='E'])
  
  # for mouthGape
  mS <- coVar(mouthGape[individual$stage=='S'])
  mW <- coVar(mouthGape[individual$stage=='W'])
  mE <- coVar(mouthGape[individual$stage=='E'])
  
  # for premaxDist
  pS <- coVar(premaxDist[individual$stage=='S'])
  pW <- coVar(premaxDist[individual$stage=='W'])
  pE <- coVar(premaxDist[individual$stage=='E'])
  CVs <- append(CVs, c(bS, bW, bE, mS, mW, mE, pS, pW, pE))
}

CVtable <- data.frame(Individual, Measurement, Stage, CVs)
fit2 <- glm(CVs~Stage+Individual, data=CVtable)
summary(fit2)
fit3 <- glm(CVs~Stage+Individual+Measurement, data=CVtable)
summary(fit3)
CVtable$Stage <- factor(CVtable$Stage, levels = c("Strike", "Winnowing", "Ejection"), ordered=TRUE)
CVtable$Measurement <- factor(CVtable$Measurement, levels=c("Mouth gape", "Pmx. protrusion", "Branch. exp."), ordered=TRUE)

p <- ggplot(data=CVtable[CVtable$Measurement=='mouthGape',], aes(x=Stage, y=CVs), color=factor(Stage))
p <- p + geom_point()

p <- ggplot(data=CVtable, aes(x=Stage, y = CVs, color=Measurement))
p <- p + geom_jitter(width = 0.4)
p

p <- ggplot(data=CVtable, aes(x=Stage, y=CVs, fill=Measurement))
p <- p+geom_boxplot() + ylab(label = 'CV (%)') + xlab(label = 'Feeding stage')
p <- p+scale_fill_manual(values=c('indianred2', 'mediumorchid', 'dodgerblue'))
p <- p + guides(fill=FALSE)

q <- ggplot(data=CVtable, aes(x=Individual, y=CVs, fill=Measurement))
q <- q+geom_boxplot() + ylab(label = 'CV (%)')
q <- q+scale_fill_manual(values=c('indianred2', 'mediumorchid', 'dodgerblue'))


multiplot(p,q, cols=2)

p <- ggplot(data=mean.df, aes(x=frame, y=vec), color=col) + xlab(xlab) + ylab(ylab) +
  geom_ribbon(aes(ymin=se.minus, ymax=se.plus), na.rm=TRUE, alpha=0.2) +
  theme(text = element_text(size=15)) 



# table with columns of: max branch, max protrusion, max gape, individual ID, stage (SWE)
# for every file in the list of video names


sigTest <- function(specTags) {
  allDist <- unlist(specTags)
  IDs <- c(rep("SD1", length(specTags[[1]])), 
           rep("SD2", length(specTags[[2]])),
           rep("SD3", length(specTags[[3]])))
  test <- data.frame(ID = IDs, dist = allDist)
  fit <- glm(allDist ~ factor(IDs))
  fit2 <- aov(allDist~factor(IDs))
  return(list(fit, fit2))}


for (i in 1:length(filenames)) {
  
  trials <- filenames[[i]]
  
  specTags <- vector("list", 3)
  names(specTags) <- c("SD1", "SD2", "SD3")
  
  for (j in 1:length(trials)) {
    # get date and specimen info
    dateInd <- substr(trials[j], 1, 8)
    specTag <- as.numeric(substr(trials[j], 14, 14))
    dateInd <- gsub("/", "-", dateInd)
    
    # make file names for accessing vids
    trialName <- paste(substr(videos[i], 10, nchar(trials[j])), 'FrameShapes', sep="")
    trialDir <- paste('../Videos/', dateInd, '/Shapes/', trialName, '/', sep="")
    
    if (length(dir(strikeDir, pattern='*.txt'))!=0) {
      measurements <- run.shapes(trialDir, name.index='S')
      mouthGape <- max(measurements$mouth.gape)
      branchDist <- max(measurements$branch.dist)
      premaxDist <- max(measurements$premax.dist)
      # newRow <- 
    }
  }
  
  
  
}

## timing the lag between local maximum mouth gape and local max branch expansion during winnowing

# getting the indices of the local maximum mouth gapes
mouthPeaks <- read.csv('S_mouthgape.csv')[,-1]
branchPeaks <- read.csv('W_branchiostegals.csv')[,-1]

mouthList <- vector("list", dim(mouthPeaks)[1])
branchList <- vector("list", dim(branchPeaks)[1])

for (i in 1:dim(mouthPeaks)[1]) {
  vec <- mouthPeaks[i,]
  vec2 <- branchPeaks[i,]
  vec <- vec[is.na(vec)==FALSE]
  vec2 <- vec2[is.na(vec2)==FALSE]
  peaks <- extract(turnpoints(vec), peak=1, pit=0)
  peaks2 <- extract(turnpoints(vec2), peak=1, pit=0)
  
  # get avg peak height and distance between peaks
  peakInd <- which(peaks %in% 1) # indices of peaks
  peakInd2 <- which(peaks2 %in% 1)

  mouthList[[i]] <- peakInd*0.03 # time (in seconds) when peak occurs
  branchList[[i]] <- peakInd2*0.03

}

for (i in 1:dim(mouthPeaks)[1]) {
  vec <- mouthPeaks[i,]
  vec2 <- branchPeaks[i,]
  vec <- vec[is.na(vec)==FALSE]
  vec <- vec - min(vec)
  vec2 <- vec2[is.na(vec2)==FALSE]
  vec2 <- vec2 - min(vec2)
  plot(vec2, col='cornflowerblue', type='l')
  points(x=c(1:length(vec)), y=vec, col='tomato1', type='l')
  plot(vec2-vec, type='l')
}


diffList <- vector("list", dim(mouthPeaks)[1])
for (i in 1:length(diffList)) {
  diffList[[i]] <- mouthList[[i]] - branchList[[i]]
}


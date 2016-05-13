setwd('/Users/hannah/Dropbox/Westneat Lab/Eartheater Project/Data/Videos/')
par(mar=c(3.1,3.1,3.1,2.1), mgp=c(2,1,0))

library(StereoMorph)
library(svgViewR)
library(KernSmooth)
library(pastecs)
library(ggplot2)
library(reshape2)

coVar <- function(vec) {
  return(sd(vec, na.rm=TRUE)/mean(vec, na.rm=TRUE)*100)
}

# gets avg frequencies (in Hz) and amplitudes (with peak normalized to 1)
# for every trial in csv, returns a dataframe with amplitudes and frequencies
# ONLY appropriate for winnowing files
peakFreq <- function(filepath) {
  alignMatrix <- alignPeaks(filepath)
  
  Height <- vector()
  Freq <- vector()
  
  for (i in 1:dim(alignMatrix)[1]) {
    vec <- alignMatrix[i,]
    vec <- vec[is.na(vec)==FALSE]
    peaks <- extract(turnpoints(vec), peak=1, pit=0)
    
    # get avg peak height and distance between peaks
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
    
    Height[i] <- mean(peakHeight)
    Freq[i] <- 1/mean(peakDist) # frequency in Hz (cycles/second)
  }
  peakFrame <- data.frame(Amplitude=Height/2, Frequency=Freq)
  return(peakFrame)
}


# makes and saves one graph per trial of distances over index
# also stores normalized vectors of mouth distance, 
# branchiostegal distance, premax/max distance in csv files in 'Measurements'
# one for each set of suction/winnowing/ejection
# always has to be run from /Videos/ directory
getShapeMeasurements <- function() {
  setwd('/Users/hannah/Dropbox/Westneat Lab/Eartheater Project/Data/Videos/')
  dates = dir(path='.', pattern='*16')
  
  # will be used as indices for naming
  phases <- c('S', 'W', 'E')
  measurements <- c('mouthgape', 'branchiostegals', 'premax', 'angle', 'premax.angle')
  Q <- length(phases)
  G <- length(measurements)
  
  # counter to keep track only of files that have data in them
  counter <- rep(0, Q)

  max.frames <- vector("list", Q) # empty vector to count max length
  trial.counts <- vector("list", Q) # empty vector to count number of trials
  
  # make empty matrices for storing mouth gape etc data
  measurementMatrix <- vector("list", G)
  
  # getting max lengths
  for (i in 1:length(dates)) {
    folder <- paste('./', dates[i], '/Shapes/', sep="") # which shapes folder to look in
    trials <- vector("list", Q)
    frames <- vector("list", Q)
    for (j in 1:Q) { # get indices for the different shapes folders
      trials[[j]] <- dir(path=folder, pattern=paste(phases[j], '_S', sep=""))
      trial.counts[[j]][i] <- length(dir(path=folder, pattern=paste(phases[j], '_S', sep="")))
    }
    
    # get max length of S, W, E trials in frames
    for (j in 1:Q) {
      for (k in 1:length(trials[[j]])) {
        folderName <- paste(folder, trials[[j]][k], sep="")
        frames[[j]][k] <- length(dir(folderName, pattern='*.txt'))
      }
    }
    
    for (j in 1:Q) {
      max.frames[[j]][i] <- max(frames[[j]])
    }
  
  }

  # making empty matrices of the right size
  for (i in 1:G) {
    # make a list of every measurement you want (mouth, branchs, premax, angle)
    measurementMatrix[[i]] <- vector("list", Q)
    for (j in 1:Q) {
      # for each measurement type, make one list for each phase (S, W, E)
      measurementMatrix[[i]][[j]] <- matrix(data=NA, nrow=sum(trial.counts[[j]]), ncol=max(max.frames[[j]]))
    }
  }
  for (i in 1:length(dates)) {
    # get trials per date
    folder <- paste('./', dates[i], '/Shapes/', sep="")
    trials <- vector("list", Q)
    for (j in 1:Q) { # get indices for the different shapes folders
      trials[[j]] <- dir(path=folder, pattern=paste(phases[j], '_S', sep=""))
      trial.counts[[j]][i] <- length(dir(path=folder, pattern=paste(phases[j], '_S', sep="")))
    }
    
    # get shapes, plot stuff, etc
    for (j in 1:Q) {
      for (k in 1:length(trials[[j]])) {
        folderName <- paste(folder, trials[[j]][k], sep="")
        name.ind <- strsplit(folderName, split='/')[[1]][[4]]
        name.ind <- strsplit(name.ind, split='_')[[1]][1]
        if (length(dir(folderName))!=0) {
          counter[j] <- counter[j]+1
          newMatrix <- run.shapes(folderName, name.index=name.ind)
          measurementMatrix[[1]][[j]][counter[j], 1:length(dir(folderName))] <- newMatrix$mouth.gape
          measurementMatrix[[2]][[j]][counter[j], 1:length(dir(folderName))] <- newMatrix$branch.dist
          measurementMatrix[[3]][[j]][counter[j], 1:length(dir(folderName))] <- newMatrix$premax.dist
          # measurementMatrix[[4]][[j]][counter[j], 1:length(dir(folderName))] <- newMatrix$mouth.angle
          # measurementMatrix[[5]][[j]][counter[j], 1:length(dir(folderName))] <- newMatrix$premax.angle
        }
      }
    }
    

  }
  
  # get rid of any columns of just NAs now that matrices are filled
  # save csv files in Measurements folder
  for (j in 1:G) {
    for (k in 1:Q) {
      measurementMatrix[[j]][[k]] <- measurementMatrix[[j]][[k]][rowSums(is.na(measurementMatrix[[j]][[k]]))>10, ]
      measurementMatrix[[j]][[k]] <- measurementMatrix[[j]][[k]][rowSums(is.na(measurementMatrix[[j]][[k]]))<length(measurementMatrix[[j]][[k]][1,]), ]
      savename <- paste('./Measurements/', phases[k], '_', measurements[j], '.csv', sep="")
      write.csv(measurementMatrix[[j]][[k]], savename)
    }
  }
}

run.shapes <- function(file, plotpoints = FALSE, savepath=NULL, scale=TRUE,
                       name.index=NULL) {
  
  shapes <- readShapes(file)
  points <- shapes$landmarks.pixel
  if (length(points[,1,1]) > 8) {
    if (sum(points[9,,] < 0, na.rm = TRUE) > 0) {
      points[9,,] <- NA
    }
  }
  
  # invert y coords
  points[, 2, ] <- -points[, 2, ]
  
  # translate points relative to eye position (= 0,0)
  points2 <- TranslatePoints(points)
  # points3 <- RotatePoints(points2)
  
  frames <- length(points2[1,1,])
  
  # name.index <- strsplit(strsplit(file, split="/")[[1]][3], split="")[[1]][1]
  if (name.index == 'W') {
    frame.index <- seq(1, (frames*15), 15)
  } else {frame.index <- seq(1, frames*5, 5)}
  
  mouth.gape <- dist.vect(points2, 1, 2)#/max(dist.vect(points2, 1, 2))
  branch.dist <- dist.vect(points2, 6, 7)#/max(dist.vect(points2, 6, 7))
  premax.dist <- dist.vect(points2, 1, 3)#/max(dist.vect(points2, 1, 3))
  premax.angle <- vector()
  # getting premax angle?
  for (i in 1:dim(points2)[3]) {
    a <- get.dist(points2[1,,i], points2[3,,i])
    b <- get.dist(points2[3,,i], points2[4,,i])
    c <- get.dist(points2[1,,i], points2[4,,i])
    if (NA %in% c(a, b, c)) {
      premax.angle[i] <- NA
    } else {
      premax.angle[i] <- TriangleCalc(a,b,c)[1]
    }
  }
  
  if (plotpoints == TRUE) {
    
    points.n <- length(points2[,1,1])
    endpts <- points2[,,c(1,frames)] # points x X/Y x first/last frame
    point.names <- c("1", "2")
    no.na <- na.omit(points2)
    
    plot(endpts[8,1,], endpts[8,2,], pch=19, col=8,  # plot eyes (will be stacked on top of each other)
         xlim=c(min(points2[,1,], na.rm=T), max(points2[,1,],  na.rm=T)), 
         ylim=c(min(points2[,2,], na.rm=T), max(points2[,2,], na.rm=T)),
         main=file)
    
    for (i in 1:points.n){ # plot every other point start/end with labels
      points(endpts[i,1,], endpts[i,2,], pch=19, col=i)
      text(endpts[i,1,]+10, endpts[i,2,]+10, point.names, cex=0.5)
      points(points2[i,1,], points2[i,2,], pch=19, col=i, type='l')
    }
    legend('topleft', c('Premax', 'Dent', 'Max.Ant', 'Max.Post', 'Mouth.corner', 'Operculum',
                        'Branchiostegals', 'Eye', 'Epaxial'), pch=19, col=c(1:points.n), cex=0.6)
  
    if (is.null(savepath)==FALSE) {
      dev.copy(png, filename = paste(savepath, 'Distances.png', sep=""))
      plot(frame.index, branch.dist, pch=19, type='l', col = 'red', main=file,
           ylab='Distance (pixels)', lwd=2, xlab = 'Frame')
      points(frame.index, mouth.gape, pch=18, lwd=2, lty=2, col='blue', type='l')
      points(frame.index, premax.dist, lwd=2, lty=3, col='darkgrey', type='l')
      legend('bottomright', c('Branchs', 'Mouth', 'Premax'), lty=1, lwd=2, col=c('red', 'blue', 'grey'), cex=0.6)
      dev.off()
    } else {
      plot(frame.index, branch.dist, pch=19, type='l', col = 'red', main=file,
           ylab='Distance (pixels)', lwd=2, xlab = 'Frame')
      points(frame.index, mouth.gape, pch=18, lwd=2, lty=2, col='blue', type='l')
      points(frame.index, premax.dist, lwd=2, lty=3, col='darkgrey', type='l')
      legend('bottomright', c('Branchs', 'Mouth', 'Premax'), lty=1, lwd=2, col=c('red', 'blue', 'grey'), cex=0.6)
    }
  }
  
  # name.index <- strsplit(strsplit(file, split="/")[[1]][3], split="")[[1]][1]
  
  if (name.index == 'W') {
    frame.index <- seq(1, (frames*15), 15)*1/500
  } else {frame.index <- seq(1, frames*5, 5)*1/500}
  
  getback <- data.frame(mouth.gape, branch.dist, premax.dist, frame.index)
  
  if (scale==TRUE) {
    fishScales <- read.csv('../Data_sheets/Video_names.csv')
    fishScales <- droplevels(fishScales)
    folderID <- strsplit(file, split='FrameShapes')[[1]][1]
    folderID <- strsplit(folderID, split='/')[[1]]
    folderID <- folderID[length(folderID)]
    scaleIndex <- match(folderID, fishScales$Trial)
    scaleFactor <- (fishScales$Scale_factors[scaleIndex])/10 # pixels/cm*1 cm/10 mm = pixels/mm
    getback$mouth.gape <- getback$mouth.gape*1/scaleFactor # pixels * mm/pixel = mm
    getback$branch.dist <- getback$branch.dist*1/scaleFactor
    getback$premax.dist <- getback$premax.dist*1/scaleFactor
  }
  
  return(getback)
}

repTrial <- function (q=5, vid='*_SD2_06_*', linetypes=c(1,5,6), linewidths=c(1,1,1),
                      ym=0.35) {
  # one representative trial
  date <- dir('./', pattern='*16')
  rep.trial <- dir(paste('./', date[q], '/Shapes', sep=""), pattern=vid) # in order: E, S, W
  order <- c(2,3,1)
  lengths <- vector('list', 3)
  trial.mats <- vector('list', 3)
  frame.spacing <- c(0.01, 0.03, 0.01)
  name.index <- c('E', 'W', 'S')
  M <- 3 # order is mouth gape, branch dist, premax dist
  distances <- vector("list", M)
  frame.indices <- vector("list", M)
  for (i in 1:3) {
    trial.mats[[i]] <- run.shapes(paste('./', date[q], '/Shapes/', rep.trial[order[i]], sep=""), name.index=name.index[i])
    lengths[[i]] <- length(dir(paste('./', date[q], '/Shapes/', rep.trial[order[i]], '/', sep=""), pattern="*.txt"))*frame.spacing[i]
  }
  for (i in 1:M) {
    for (j in 1:3) {
      if (j == 1) {
        frames <- trial.mats[[j]]$frame.index
      } else {frames <- trial.mats[[j]]$frame.index+frame.indices[[i]][length(frame.indices[[i]])]}
      distances[[i]] <- c(distances[[i]], trial.mats[[j]][,i])
      frame.indices[[i]] <- c(frame.indices[[i]], frames)
    }
  }
  
  spline.dist <- vector("list", M)
  for (i in 1:M) {
    splinefit <- smooth.spline(frame.indices[[i]], distances[[i]], spar=0.25)
    predicts <- predict(splinefit, data=frame.indices[[i]])$y
    predicts <- predicts - min(predicts)
    spline.dist[[i]] <- predicts
  }
  
  # messy messy
  representative.df <- data.frame(mouth.gape=spline.dist[[1]], branch.dist=spline.dist[[2]],
                                  premax=spline.dist[[3]], frames=frame.indices[[1]])
  meltRep <- melt(representative.df, id=4)
  
  r <- ggplot(data=meltRep, aes(x=frames, y=value, color=variable)) +
    geom_line(data=meltRep, aes(linetype=variable, size=variable)) + 
    scale_linetype_manual(values=linetypes, name='Measurements',
                          breaks=c('mouth.gape', 'branch.dist', 'premax'),
                          labels=c('Mouth gape', 'Branchiostegal exp.', 'Premax protrusion')) + 
    scale_size_manual(values=linewidths, name='Measurements',
                      breaks=c('mouth.gape', 'branch.dist', 'premax'),
                      labels=c('Mouth gape', 'Branchiostegal exp.', 'Premax protrusion')) + 
    scale_color_manual(values=c('indianred2', 'dodgerblue', 'mediumorchid2'), name='Measurements',
                       breaks=c('mouth.gape', 'branch.dist', 'premax'),
                       labels=c('Mouth gape', 'Branchiostegal exp.', 'Premax protrusion')) + 
    xlab('Seconds') + ylab('Distance (mm)') + 
    geom_vline(xintercept = (lengths[[1]]-0.01), lwd=0.7, alpha=0.5) +
    geom_vline(xintercept = (lengths[[2]] + lengths[[1]] - 0.03), lwd=0.7, alpha=0.5)
  r <- r + annotate("rect", xmin=(lengths[[1]]/3-0.1), xmax=(lengths[[1]]/3+0.1), ymin=6-ym, ymax=6+ym, alpha=.2)
  r <- r + annotate("rect", xmin=(0.75-0.185), xmax=(0.75+0.185), ymin=6-ym, ymax=6+ym, alpha=.2)
  r <- r + annotate("rect", xmin=(1.51-0.115), xmax=(1.51+0.115), ymin=6-ym, ymax=6+ym, alpha=.2)
  r <- r + annotate("text", x = c(lengths[[1]]/3, 0.75, 1.51), y = 6, label = c("Strike", "Winnowing", "Ejection"), size=8)
  r <- r + theme(text=element_text(size=15))
  return(r)
}

meanPlots <- function() {
  # plotting means w/ standard error bars to look at general behavior
  phases <- c('S', 'W', 'E')
  titles <- c('Strike', 'none', 'none', 'Winnowing', 'none', 'none', 'Ejection', 'none', 'none')
  xlabel <- c("", "", 'Seconds')
  Wstate <- phases=='W'
  measurements <- c('mouthgape', 'premax', 'branchiostegals')
  Q <- length(phases)
  G <- length(measurements)
  ylabels <- c('Mouth gape', '', '', 'Premax. dist.', '', '', 'Branch. exp.', '', '')
  graphs <- rep(list(vector("list", Q)), G)
  colors <- c('sienna1', 'tomato1', 'firebrick1', 
              'orchid1', 'mediumorchid2', 'darkorchid2',
              'cyan2', 'dodgerblue1', 'royalblue1')
  # 'seagreen1', 'springgreen3', 'forestgreen')
  
  for (i in 1:G) { # for every measurement
    for (j in 1:Q) { # for every phase
      graphs[[i]][[j]] <- graphAlignPeaksGG(paste('./Measurements/', phases[j], '_', measurements[i], '.csv', sep=""),
                                            col=paste(colors[Q*(i-1)+j]), xlab=xlabel[i],
                                            main=titles[Q*(j-1)+i], ylab=ylabels[Q*(i-1)+j], W=Wstate[j])
    }
  }
  
  # plot mouth gape, premax distance, branch expansion for each phase
  for (i in 1:G) {
    multiplot(graphs[[1]][[i]], graphs[[2]][[i]], graphs[[3]][[i]])
  }
}

# takes matrix of distance vectors generated by getShapeMeasurements()
# aligns them by peaks and returns matrix of aligned vectors
# graphs aligned vectors all on same plot
# good for E and S, but NOT W (doesn't make sense to align by highest peak)
graphAlignPeaks <- function(alignMatrix) {
  plot(alignMatrix[1,], type='l', col=1) # plots first set
  for (i in 2:dim(alignMatrix)[1]) { # plots remaining vectors
    points(alignMatrix[i,], type='l', col=i)
  }
}

graphAlignPeaksGG <- function(filepath, col=4, xlab='Seconds', ylab='Normalized distance', 
                              main='none',
                              W=FALSE, lty=1) {
  
  # get matrix aligned by peaks
  if (W==FALSE) {
    alignMatrix <- alignPeaks(filepath, smooth=TRUE)
  } else {
    rawMatrix <- read.csv(filepath)[,-1]
    alignMatrix <- matrix(data=NA, nrow = dim(rawMatrix)[1], ncol = 2*dim(rawMatrix)[2]-1)
    t0 <- (dim(alignMatrix)[2]+1)/2 # reference for where to place maximum (center)
    
    for (i in 1:dim(rawMatrix)[1]) {
      vec <- rawMatrix[i,]
      vec <- vec[is.na(vec)==FALSE] # get rid of NAs
      peaks <- extract(turnpoints(vec), peak=1, pit=0)
      maxInd <- match(1, peaks) # index of first peak
      startPt <- t0 - maxInd+1 # where to start placing the vector
      endPt <- startPt + length(vec)-1 # where to end it
      alignMatrix[i, startPt:endPt] <- vec # put vector there
    }
  }
  
  
  # get standard error at each timestep
  stderrors <- vector()
  for (i in 1:dim(alignMatrix)[2]) {
    vec <- alignMatrix[,i]
    vec <- vec[is.na(vec)==FALSE]
    stderrors[i] <- sd(vec) # will have value of NA when vec has only one value
  }
  
  # index for mean
  frame.index <- seq(1, dim(alignMatrix)[2], 1)
  frame.index <- (frame.index-1)*1/100
  if (W == TRUE) {
    frame.index <- frame.index*3
  }
  
  vec <- colMeans(alignMatrix, na.rm=TRUE)
  vec[is.na(stderrors)==TRUE] <- NA
  frame.index[is.na(stderrors)==TRUE] <- NA
  alignMatrix <- alignMatrix[,is.na(stderrors)==FALSE]
  
  mean.df <- data.frame(vec=vec, frame=frame.index)
  
  mean.df$se.plus <- mean.df$vec+stderrors 
  mean.df$se.minus <- mean.df$vec-stderrors 
  
  # melt into a dataframe for ggplot2
  align.df <- melt(alignMatrix)
  
  # change frame index to time in seconds
  align.df$Var2 <- (align.df$Var2-1)*1/100
  
  if (W == TRUE) {
    align.df$Var2 <- align.df$Var2*3
  }
  # get standard error at each timestep
  stderrors <- vector()
  for (i in 1:dim(alignMatrix)[2]) {
    vec <- alignMatrix[,i]
    vec <- vec[is.na(vec)==FALSE]
    stderrors[i] <- sd(vec) # will have value of NA when vec has only one value
  }
 
  # index for mean
  frame.index <- seq(1, dim(alignMatrix)[2], 1)
  frame.index <- (frame.index-1)*1/100
  if (W == TRUE) {
    frame.index <- frame.index*3
  }
  
#   mean.df <- data.frame(vec=colSums(alignMatrix, na.rm=TRUE)/(dim(alignMatrix)[1]),
#                         frame=frame.index)
  
  vec <- colMeans(alignMatrix, na.rm=TRUE)
  vec[is.na(stderrors)==TRUE] <- NA
  frame.index[is.na(stderrors)==TRUE] <- NA
  mean.df <- data.frame(vec=vec, frame=frame.index)
  
  mean.df$se.plus <- mean.df$vec+stderrors 
  mean.df$se.minus <- mean.df$vec-stderrors 
  
  p <- ggplot(data=mean.df, aes(x=frame, y=vec), color=col) + xlab(xlab) + ylab(ylab) +
    geom_ribbon(aes(ymin=se.minus, ymax=se.plus), na.rm=TRUE, alpha=0.2) +
    theme(text = element_text(size=15)) 
  p <- p + geom_line(data=align.df, aes(x=Var2, y=value, group=Var1, color=factor(Var1)), alpha=0.5, na.rm = TRUE) + theme(legend.position = "none")
  p <- p + geom_line(data=mean.df, aes(x=frame, y=vec), na.rm=TRUE, color=col, lwd=1.5, alpha=1, lty=lty)
  
  if (main != 'none') {
    p <- p + ggtitle(main)
  }
  
  return(p)
}



graphAlignMeanGG <- function(filepath, col=4, main='You forgot to give me a title again you sad bastard',
                             xlab='Seconds', ylab='Normalized distance') {
  alignMatrix <- alignPeaks(filepath, smooth=TRUE, phase='E')
  for (i in 1:dim(alignMatrix)[2]) {
    vec <- alignMatrix[,i]
    vec <- vec[is.na(vec)==FALSE]
    stderrors[i] <- sd(vec) # will have value of NA when vec has only one value
  }
  alignMatrix <- alignMatrix[,is.na(stderrors)==FALSE]
  stderrors <- stderrors[is.na(stderrors)==FALSE]
  frame.index <- seq(1, dim(alignMatrix)[2]*5, 5)
  frame.index <- frame.index*1/500
  mean.df <- data.frame(vec=colSums(alignMatrix, na.rm=TRUE)/(dim(alignMatrix)[1]),
                        frame=frame.index)
  mean.df$se.plus <- mean.df$vec+stderrors 
  mean.df$se.minus <- mean.df$vec-stderrors 
  
  p <- ggplot(data=mean.df, aes(x=frame, y=vec), color=col) + xlab(xlab) + ylab(ylab) + ggtitle(main) + 
    geom_line(na.rm=TRUE, color=col) + geom_ribbon(aes(ymin=se.minus, ymax=se.plus), na.rm=TRUE, alpha=0.2)
  p
  return(p)
}

# takes matrix of distance vectors generated by getShapeMeasurements()
# aligns them by peaks and returns matrix of aligned vectors
# if smooth=TRUE, spline smoothing function is applied
alignPeaks <- function(filepath, smooth=FALSE, phase=NA) {
  rawMatrix <- read.csv(filepath)[,-1]
  # empty matrix with 2x-1 columns (extreme case: two longest vectors overlap at opposite ends)
  # will almost certainly have extra flanking columns of NAs once filled
  alignMatrix <- matrix(data=NA, nrow = dim(rawMatrix)[1], ncol = 2*dim(rawMatrix)[2]-1)
  t0 <- (dim(alignMatrix)[2]+1)/2 # reference for where to place maximum (center)
  
  for (i in 1:dim(rawMatrix)[1]) {
    vec <- rawMatrix[i,]
    vec <- vec[is.na(vec)==FALSE] # get rid of NAs
    if (smooth==TRUE) {
    frame.index <- seq(1, length(vec)*5, 5)*1/500
    splinefit <- smooth.spline(frame.index, vec) # smooth vector
    vec2 <- predict(splinefit, data=frame.index) # use splinefit to get smoothed values
    vec2 <- vec2$y
    } else {vec2 <- vec}
    
    maxInd <- match(max(vec2), vec2) # index of maximum
    startPt <- t0 - maxInd+1 # where to start placing the vector
    endPt <- startPt + length(vec2)-1 # where to end it
    alignMatrix[i, startPt:endPt] <- vec2 # put vector there
  }
  # trim superfluous NA columns (only columns with just NA; most columns will
  # have at least some because vectors are not of equal length when aligned)
  alignMatrix <- alignMatrix[,colSums(is.na(alignMatrix))<dim(alignMatrix)[1]]
  return(alignMatrix)

}

get.dist <- function(p1, p2){
  distance <- sqrt((p1[1]-p2[1])^2+(p1[2]-p2[2])^2)
  return(distance)
}

# gets distances for all pairs of points across 3rd dimension of array
dist.vect <- function(arr1, p1, p2) {
  new.vect <- rep(0, length(arr1[1,1,]))
  for (i in 1:length(arr1[1,1,])) {
    new.vect[i] <- get.dist(arr1[p1,,i], arr1[p2,,i])
  }
  return(new.vect)
}

TranslatePoints <- function(arr1, p1=8) {
  
  # empty array to fill
  arr2 <- array(data = NA, dim(arr1))
  
  # fills array with translated coordinates
  for (i in 1:length(arr1[1,1,])){
    Dx <- arr1[p1,1,i] # amount to translate x by (just X1)
    Dy <- arr1[p1,2,i] # amount to translate y by
    TMatrix <- cbind(rep(-Dx, length(arr1[,1,1])), rep(-Dy, length(arr1[,1,1]))) # matrix to add to original matrix
    arr2[ , , i] <- arr1[ , , i] + TMatrix # translates each point by -Dx, -Dy
  }
  return(arr2)
}

RotatePoints <- function(arr1) {
  thetavector <- rep(0, length(arr1[1,1,])) # empty vector
  for (i in 1:length(arr1[1,1,])){ # angle to rotate is angle between point1/point2 and the horizontal
    X2 <- arr1[9,1,i]
    Y2 <- arr1[9,2,i]
    theta <- atan(Y2/X2) # tan(opposite/adjacent) to find angle
    theta <- theta - pi/4
    thetavector[i] <- -theta # fill vector
  }
  
  RotationMatrix <- array(data = NA, c(2,2,length(thetavector))) # empty array
  
  for (i in 1:length(thetavector)){ # create rotation matrix
    costheta <- cos(thetavector[i])
    sintheta <- sin(thetavector[i])
    nsintheta <- -sin(thetavector[i])
    RotationMatrix[,,i] <- matrix(c(costheta, nsintheta, sintheta, costheta),
                                  ncol=2, nrow=2)
  }
  
  for (i in 1:length(arr1[1,1,])) {
    arr1[ , , i] <- arr1[ , , i] %*% RotationMatrix[ , , i] 
    # rotate points by multiplying original coordinates by rotation matrix
  }
  return(arr1)
}

bigangle <- function(arr, p1, p2, p3) {
  angles <- rep(0, length(arr[1,1,]))
  for (i in 1:length(arr[1,1,])) {
    vec1 <- c(abs(arr[p1,1,i]-arr[p2,1,i]), abs(arr[p1,2,i]-arr[p2,2,i]))
    vec2 <- c(abs(arr[p2,1,i]-arr[p3,1,i]), abs(arr[p2,2,i]-arr[p3,2,i]))
    angle.m <- angle(t(vec2), vec1)
    if (angle.m > pi) {
      angle.m <- pi - angle.m
    }
    angles[i] <- angle.m
  }
  return(angles)
}


horiz.angle <- function(arr, p1,p2){
  angles <- vector()
  for (i in 1:dim(arr)[3]) {
    num <- abs(arr[p1,2,i]-arr[p2,2,i])
    denom <- abs(arr[p1,1,i]-arr[p2,1,i])
    angles[i] <- atan(num/denom)
  }
  return(angles)
}

mouth.angles <- function(arr,p1,p2,p3) {
  angles1 <- horiz.angle(arr, p1, p2)
  angles2 <- horiz.angle(arr, p2, p3)
  return(angles1+angles2)
}

TriangleCalc <- function(a, b, c) {
  
  check1 = 0
  check2 = 0
  check3 = 0
  
  con <- 2*pi/360
  
  if ((a*a) < (b*b + c*c)){
    a1 = b^2 + c^2 - a^2
    b1 = 2*b*c
    bcAngle <- acos(a1/b1)/con
    check1 = 1
  }
  
  if ((b*b) < (a*a + c*c)) {
    a2 = a^2 + c^2 - b^2
    b2 = 2*a*c
    acAngle <- acos(a2/b2)/con
    check2 = 1
  }
  
  if ((c*c) < (a*a + b*b)) {
    a3 = a^2 + b^2 - c^2
    b3 = 2*a*b
    abAngle <- acos(a3/b3)/con
    check3 = 1
  }
  
  if (check1 == 1) {
    if (check2 == 1) {abAngle <- 180 - (bcAngle + acAngle)}
  } 
  
  if (check1 == 1) {
    if (check3 ==1) {acAngle <- 180 - (abAngle + bcAngle)}
  }
  
  if (check2 == 1) {
    if (check3 == 1) {bcAngle <- 180 - (abAngle + acAngle)}
  }
  
  return(c(abAngle, bcAngle, acAngle))
  
} 

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
# mouth
mouth.S <- graphAlignPeaksGG('./Measurements/S_mouthgape.csv', col=2, main='Mouth gape (strike)')
mouth.S
mouth.E <- graphAlignPeaksGG('./Measurements/E_mouthgape.csv', col=4, main='Mouth gape (ejection)')
mouth.E
mouth.W <- graphAlignPeaksGG('./Measurements/W_mouthgape.csv', col=5, main='Regret', W=TRUE)
mouth.W
#align by first peak?

graphAlignPeaksGG('./Measurements/S_branch.csv', col=2, main='Branch distance (strike)')
graphAlignPeaksGG('./Measurements/W_branch.csv', col=4, main='Branch distance (winnow)', W=TRUE)
graphAlignPeaksGG('./Measurements/E_branch.csv', col=5, main='Branch distance (ejection)')
q


ssp <- spectrum(winnow.vec)
sort(ssp$spec)[length(ssp$spec)-1]
per <- 1/ssp$freq[ssp$spec==max(ssp$spec)]
reslm <- lm(winnow.vec ~ sin(2*pi/per*frames)+cos(2*pi/per*frames))
summary(reslm)

rg <- diff(range(winnow.vec))
plot(winnow.vec~frames,ylim=c(min(winnow.vec)-0.1*rg,max(winnow.vec)+0.1*rg))
lines(fitted(reslm)~frames,col=4,lty=2)   # dashed blue line is sin fit

# including 2nd harmonic really improves the fit
reslm2 <- lm(winnow.vec ~ sin(2*pi*per*frames)+cos(2*pi*per*frames)+sin(4*pi/per*frames)+cos(4*pi/per*frames))
summary(reslm2)
lines(fitted(reslm2)~frames,col=3) 

# raw.fft = fft(values)
# truncated.fft = raw.fft[seq(1, length(values)/2 - 1)]
# truncated.fft[1] = 0
# W = which.max(abs(truncated.fft)) * 2 * pi / length(values)
# 
# r2<-nls(values~C+alpha*sin(W*T+phi), start=list(C=8958.34, alpha=115.886, W=W, phi=0))
# 
# lines(predict(r2)~T, col="red")  
# 
# summary(r2)

# // find largest peak in power spectrum
# max_magnitude = -INF
# max_index = -1
# for i = 0 to N / 2 - 1
# if magnitude[i] > max_magnitude
# max_magnitude = magnitude[i]
# max_index = i
# 
# // convert index of largest peak to frequency
# freq = max_index * Fs / N


winnow.test <- read.csv('./Measurements/W_branch.csv')[,-1]
winnow.test <- alignPeaks('./Measurements/W_branch.csv')
vec <- winnow.test[1,]
vec <- vec[is.na(vec)==FALSE]
peaks <- extract(turnpoints(vec), peak=1, pit=0)
plot(vec, type='l')
for (i in 1:length(peaks)) {
  if (peaks[i]==1) {
    abline(v=i, col='red', lwd=2, lty=2)
  }
}


rawMatrix <- winnow.test
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
  
  peaks <- extract(turnpoints(vec2), peak=1, pit=0)
  
  maxInd <- match(1, peaks) # index of first peak
  startPt <- t0 - maxInd+1 # where to start placing the vector
  endPt <- startPt + length(vec2)-1 # where to end it
  alignMatrix[i, startPt:endPt] <- vec2 # put vector there
}
# trim superfluous NA columns (only columns with just NA; most columns will
# have at least some because vectors are not of equal length when aligned)
alignMatrix <- alignMatrix[,colSums(is.na(alignMatrix))<dim(alignMatrix)[1]]
write.csv(alignMatrix, 'test.csv')

fishScales <- read.csv('../Data_sheets/Video_names.csv')
eyeSizes <- c(0.0815, 0.0928, 0.0993) # eye area in cm^2 for SD1, SD2, SD3
scaleFactors <- vector()
for (i in 1:dim(fishScales)[1]) {
  fishID <- strsplit(as.character(fishScales$Trial[i]), split = '_')[[1]][2]
  if (fishID == 'SD1') {
    scaleFactors[i] <- sqrt(fishScales$Eye.size..pixels.[i]/eyeSizes[1])
  } else if (fishID == 'SD2') {
    scaleFactors[i] <- sqrt(fishScales$Eye.size..pixels.[i]/eyeSizes[2])
  } else if (fishID == 'SD3') {
    scaleFactors[i] <- sqrt(fishScales$Eye.size..pixels.[i]/eyeSizes[3])
  }
}
fishScales$Scale_factors <- scaleFactors
write.csv(fishScales, '../Data_sheets/Video_names.csv')


# Aligning vectors --------------------------------------------------------

mouth.S <- graphAlignPeaksGG('./Measurements/S_mouthgape.csv', col=2, main='Strike', xlab="")
mouth.E <- graphAlignPeaksGG('./Measurements/E_mouthgape.csv', col=5, main='Ejection')
mouth.W <- graphAlignPeaksGG('./Measurements/W_mouthgape.csv', col=4, main='Winnowing', xlab="")
branch.S <- graphAlignPeaksGG('./Measurements/S_branch.csv', col=2, main='Strike')
branch.W <- graphAlignPeaksGG('./Measurements/W_branch.csv', col=5, main='Winnowing')
branch.E <- graphAlignPeaksGG('./Measurements/E_branch.csv', col=4, main='Ejection')

multiplot(branch.S, branch.W, branch.E)
multiplot(mouth.S, mouth.W, mouth.E)

strike.mouth <- read.csv('./Measurements/S_mouthgape.csv')[,-1]
strike.branch <- read.csv('./Measurements/S_branch.csv')[,-1]
strike.premax <- read.csv('./Measurements/S_premax.csv')[,-1]
mouth.test <- strike.mouth[2,]
mouth.test <- mouth.test[is.na(mouth.test)==FALSE]
branch.test <- strike.branch[2,]
branch.test <- branch.test[is.na(branch.test)==FALSE]
premax.test <- strike.premax[2,]
premax.test <- premax.test[is.na(premax.test)==FALSE]

frame.index <- seq(1, length(mouth.test), 1)
frame.index <- (frame.index-1)*1/100


splinefit <- smooth.spline(frame.index, mouth.test) # smooth vector
mouth.test <- predict(splinefit, data=frame.index)$y


splinefit2 <- smooth.spline(frame.index, branch.test) # smooth vector
branch.test <- predict(splinefit2, data=frame.index)$y

df.test <- data.frame(mouth.test=mouth.test, branch.test=branch.test, premax=premax.test, frame.index=frame.index)

p <- ggplot(data=df.test, aes(x=frame.index, y=mouth.test)) + 
  geom_line(data=df.test, aes(x=frame.index, y=mouth.test), lwd=1, col='blue')
p <- p + geom_line(data=df.test, aes(c=frame.index, y=branch.test), lwd=1, lty=2, col='red')
p <- p + geom_line(data=df.test, aes(c=frame.index, y=premax), lwd=1, lty=3, col='purple', alpha=1)
p



## get csv of all video names
dates <- dir('./', pattern='*16')
vidnames <- vector("list", length(dates))
datenames <- vector("list", length(dates))

vidnames <- vector()
datenames <- character()
for (i in 1:length(dates)) {
  shapenames <- dir(paste(dates[i], '/Shapes/', sep=""), pattern="*_*")
  videonames <- as.character(strsplit(shapenames, split='FrameShapes'))
  vidnames <- c(vidnames, videonames)
  datenames <- c(datenames,rep(dates[i], length(videonames)))
}
ref.df <- data.frame(Date=datenames, Trial=vidnames)

write.csv(ref.df, '../Data_sheets/Video_names.csv')

## FOR SCALING
# get eye size in pixels
# have eye size in cm^2
# scale factor (pixels/cm) will be:
# sqrt(pixel area/known area in cm)


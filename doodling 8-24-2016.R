setwd('/Users/hannah/Dropbox/Westneat_Lab/Eartheater Project/Data/Videos/Measurements/')
StrikeMouth <- read.csv('S_mouthgape.csv')[,2:56]
StrikePmx <- read.csv('S_premax.csv')[,2:56]
StrikeBranch <- read.csv('S_branchiostegals.csv')[,2:56]

graphAlignPeaksGG('./S_mouthgape.csv')

test <- alignPeaks('./S_mouthgape.csv', phase='S')

temp <- apply(test, 1, function(x) {61-sum(is.na(x))})
mean(temp)
test2 <- test[,15:46]
crossCor <- acf(test2, na.action = na.pass, plot=FALSE, lag.max=18)

maxMouth <- apply(test, 1, function(x) {max(x, na.rm=TRUE)})
(min(temp)-1)/2


getCrossCorr <- function(filepath) {
  df <- alignPeaks(filepath,smooth = FALSE)
  getMin <- min(apply(df, 1, function(x) {dim(df)[2]-sum(is.na(x))}))
  const <- floor((getMin-1)/2)

  df2 <- df[, colSums(is.na(df)) == 0]
  # df2 <- t(apply(df, 1, function(x) x/max(x, na.rm=T)))
  ref <- match(max(df2[1,], na.rm=T), df2[1,])
  dfCorr <- acf(df2, plot=F, na.action=na.pass, lag=0)

  returnList <- vector("list", 2)
  returnList[[1]] <- dfCorr
  returnList[[2]] <- mean(dfCorr$acf, na.rm=T)
  
  return(returnList)
}

getTopCrossCorr <- function(filepath) {
  # read in and align peaks
  df <- alignPeaks(filepath,smooth = FALSE)
  # get average length and select for vectors of that length or higher
  lengths <- apply(df, 1, function(x) {dim(df)[2]-sum(is.na(x))})
  getMean <- unique(sort(lengths, decreasing=T))[3]
  # getMean <- floor(mean(lengths))
  # ref <- match(max(df[1,], na.rm=T), df[1,])
  # const <- floor((getMean-1)/2)
  df2 <- df[(lengths >= getMean),]
  df3 <- df2[, colSums(is.na(df2)) == 0]
  
  # if (dim(df3)[1] > dim(df3)[2]) {
  #   dfCorr <- acf(df3, plot=F, type = "correlation", lag=0)
  # } else { dfCorr <- acf(t(df3), plot=F, type = "correlation", lag=0)}
  dfCorr <- acf(df3, plot=F, type="correlation", lag=0)
  # dfCorr <- acf(df3, plot=F)

  returnList <- vector("list", 2)
  returnList[[1]] <- dfCorr
  returnList[[2]] <- mean(dfCorr$acf, na.rm=T)
  
  return(returnList)
}

strikes <- vector("list", 3)
names(strikes) <- c("Mouth", "Pmx", "Branch")
strikeCSV <- c('S_mouthgape.csv', 'S_premax.csv', 'S_branchiostegals.csv')
for (i in 1:3) {
  strikes[[i]] <- getCrossCorr(strikeCSV[i])[[1]] 
}

ejections <- vector("list", 3)
names(ejections) <- c("Mouth", "Pmx", "Branch")
ejectionCSV <- c('E_mouthgape.csv', 'E_premax.csv', 'E_branchiostegals.csv')
for (i in 1:3) {
  ejections[[i]] <- getCrossCorr(ejectionCSV[i])[[1]] 
}

# ok what if we only use the longer than average ones?
strikesLong <- vector("list", 3)
names(strikesLong) <- c("Mouth", "Pmx", "Branch")
strikeCSV <- c('S_mouthgape.csv', 'S_premax.csv', 'S_branchiostegals.csv')
for (i in 1:3) {
  strikesLong[[i]] <- getTopCrossCorr(strikeCSV[i])[[1]] 
}

ejectionsLong <- vector("list", 3)
names(ejectionsLong) <- c("Mouth", "Pmx", "Branch")
ejectionCSV <- c('E_mouthgape.csv', 'E_premax.csv', 'E_branchiostegals.csv')
for (i in 1:3) {
  ejectionsLong[[i]] <- getTopCrossCorr(ejectionCSV[i])[[1]] 
}

a<-mean(ejections$Mouth$acf)
b<-mean(ejections$Pmx$acf)
c<-mean(ejections$Branch$acf)
mean(c(a,b,c))

a <- mean(strikes$Mouth$acf)
b<-mean(strikes$Pmx$acf)
c<-mean(strikes$Branch$acf)
mean(c(a,b,c))

shapiro.test(strikes$Mouth$acf)
shapiro.test(strikes$Pmx$acf)
shapiro.test(strikes$Branch$acf)

shapiro.test(ejections$Mouth$acf)
shapiro.test(ejections$Pmx$acf)
shapiro.test(ejections$Branch$acf)

ACFs <- c(unlist(strikes$Mouth$acf), unlist(strikes$Pmx$acf), unlist(strikes$Branch$acf),
  unlist(ejections$Mouth$acf), unlist(ejections$Pmx$acf), unlist(ejections$Branch$acf))
hist(ACFs) # maybe this is an ok fig?

longACFs <- c(unlist(strikesLong$Mouth$acf), unlist(strikesLong$Pmx$acf), unlist(strikesLong$Branch$acf),
          unlist(ejectionsLong$Mouth$acf), unlist(ejectionsLong$Pmx$acf), unlist(ejectionsLong$Branch$acf))
hist(longACFs)

longACFMouth <- c(unlist(strikesLong$Mouth$acf), unlist(strikesLong$Pmx$acf), unlist(strikesLong$Branch$acf))
mean(longACFMouth)

longACFEject <- longACFs <- c(unlist(ejectionsLong$Mouth$acf), unlist(ejectionsLong$Pmx$acf), unlist(ejectionsLong$Branch$acf))
# for strike + ejection -- restricting just to overlaps, get very consistent shapes
# using ACF/lag of 0 (aligning by max gape):
# get AVERAGE CORRELATION COEFFICIENT (~+0.5 for both strike and ejection)
# get p-value for corrcoef distribution compared to normal (p << 10E-10 or smth)
# conclusion -- strongly autocorrelated = stereotyped behavior, but usage/timing of those events modular for ejections
# even when including just longer ones (since change of correlation would go down) get nearly identical results
# if anything, ejection event even more stereotyped -- but usage/timing is not
strikeP <- c(getPvalue(strikes$Mouth), getPvalue(strikes$Pmx), getPvalue(strikes$Branch))
getPvalue <- function(matrix) {
  corrMat <- matrix$acf
  dim(corrMat) <- NULL
  N <- matrix$n.used
  vec <- vector()
  for (i in 1:length(corrMat)) {
    vec <- c(vec, 2 * (1 - pnorm(abs(corrMat[i]), mean = 0, sd = 1/sqrt(N)))
)
  }
  return(vec)
}



mean(getPvalue(strikes$Mouth))
mean(getPvalue(strikes$Pmx))
mean(getPvalue(strikes$Branch))

mean(getPvalue(ejections$Mouth))
mean(getPvalue(ejections$Pmx))
mean(getPvalue(ejections$Branch))

test <- ejections$Branch$acf[1,1,2]
2 * (1 - pnorm(abs(test), mean = 0, sd = 1/sqrt(ejections$Branch$n.used)))


graphAlignPeaksGG('./W_mouthgape.csv', W=TRUE)

WM <- read.csv('./W_mouthgape.csv')[,2:112]
test <- acf(WM, lag.max = 11, na.action = na.pass, plot=F)
WM <- alignPeaks('./W_mouthgape.csv', phase='W')
lengths <- apply(WM, 1, function(x) {dim(WM)[2]-sum(is.na(x))})
CV1 <- lengths * 0.03 # 1 s/500 frames * 15 frames/index = seconds/index
sd(CV1)/mean(CV1)

CVadj <- function(vec) {
  vec <- vec[is.na(vec)==FALSE]
  CVa <- (1 + 1/(4*length(vec)))*sd(vec)/mean(vec)
  return(CVa)
}
CVstats <- function(filepath) {
  alignMatrix <- alignPeaks(filepath)
  lengths <- apply(alignMatrix, 1, function(x) {dim(alignMatrix)[2]-sum(is.na(x))})
  
  # get duration?
  duration <- lengths * 0.03 # in seconds
  
  Height <- vector()
  SDHeight <- vector()
  Freq <- vector()
  SDFreq <- vector()
  peakNum <- vector()
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
    peakNum[i] <- length(peakDist)
    Height[i] <- mean(peakHeight)
    SDHeight[i] <- sd(peakHeight)
    Freq[i] <- 1/mean(peakDist) # frequency in Hz (cycles/second)
    SDFreq[i] <- sd(peakDist)
  }
  
  # get frequency?
  frequency <- Freq
  
  # get number of peaks?
  peakNum <- peakNum
  result <- data.frame(frequency=frequency, peak.number = peakNum, duration=duration)
  # peakFrame <- data.frame(Height=Height, SDHeight=SDHeight, Frequency=Freq, SDFreq=SDFreq)
  
  return(result)
}
getCVadj <- function(filepath) {
  df <- alignPeaks(filepath)
  lengths <- apply(df, 1, function(x) {dim(df)[2]-sum(is.na(x))})
  getCVstats <- CVstats(filepath)
  
  durationCV <- CVadj(getCVstats$duration)
  cycleCV <- CVadj(getCVstats$peak.number)
  freqCV <- CVadj(getCVstats$frequency)
  
  
  return(data.frame(durationCV, cycleCV, freqCV))
}

getCVadj('./W_mouthgape.csv')
getCVadj('./W_premax.csv')
getCVadj('./W_branchiostegals.csv')

graphAlignPeaksGG('./W_branchiostegals.csv')


# are mouth gape, pmx gape, branch exp synchronized?
# read in/align each
SM <- alignPeaks('./S_mouthgape.csv')
SP <- alignPeaks('./S_premax.csv')
SB <- alignPeaks('./S_branchiostegals.csv')

getSummary <- function(filepath) {
  df <- alignPeaks(filepath,smooth = FALSE)
  # getMin <- min(apply(df, 1, function(x) {dim(df)[2]-sum(is.na(x))}))
  # const <- floor((getMin-1)/2)
  
  df2 <- df[, colSums(is.na(df)) == 0]
  # df2 <- t(apply(df, 1, function(x) x/max(x, na.rm=T)))
  ref <- match(max(df2[1,], na.rm=T), df2[1,])
  dfCorr <- acf(df2, plot=F, na.action=na.pass, lag=0)
  
  returnList <- vector("list", 2)
  returnList[[1]] <- dfCorr
  returnList[[2]] <- mean(dfCorr$acf, na.rm=T)
  
  return(returnList)
}

## CV plots
Fishes <- c(0.2017, 0.2133, 0.1262,
            0.3020, 0.2292, 0.2645,
            0.3060, 0.3147, 0.2776)

Lepidosaurs <- c(0.4828, 0.2786, 0.6864,
                 0.4653, 1.0876, 0.2324,
                 0.3399, 0.4158, 0.4191,
                 0.5240, 0.3721,
                 0.2112, 0.7026)

Satanoperca <- c(0.6384374, 0.5902670, 0.6973746)

Names <- c(rep("Fishes", length(Fishes)), 
           rep("Satanoperca", length(Satanoperca)),
           rep("Lepidosaurs", length(Lepidosaurs)))
CV <- c(Fishes, Satanoperca, Lepidosaurs)
df <- data.frame(Names=Names, CV=CV)
df$Names <- factor(df$Names, levels=c("Fishes", "Satanoperca", "Lepidosaurs"))
df$Names

p <- ggplot(data=df, aes(x=factor(Names), y=CV, fill=factor(Names)))
p <- p + geom_violin()
p

p <- ggplot(data=df, aes(x=factor(Names), y=CV, fill=factor(Names)))
p <- p + geom_violin() + scale_fill_manual(values=c('#1a8cff', '#ff6666', '#40bf40'), guide=guide_legend(title = NULL))
p <- p + xlab("") + theme(text = element_text(size=17))
p

ggsave(filename = '../Manuscript/Figs/PCA.eps', plot=last_plot(), dpi=600)



source('/Users/hannah/Dropbox/Westneat Lab/Eartheater Project/Code/genfunctions.R')
source('/Users/hannah/Dropbox/Westneat Lab/Eartheater Project/Code/Read shapes.R')

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

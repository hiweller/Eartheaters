source('/Users/hannah/Dropbox/Westneat_Lab/Eartheater Project/Code/genfunctions.R')
setwd('/Users/hannah/Dropbox/Westneat_Lab/Eartheater Project/Data/Videos/Measurements/')
videos <- read.csv('../../Data_sheets/Video_names.csv')

## getting the strike and ejection durations

## STRIKE
mouthStrike <- read.csv('S_mouthgape.csv')[,-1]
branchStrike <- read.csv('S_branchiostegals.csv')[,-1]
premaxStrike <- read.csv('S_premax.csv')[,-1]

for (i in 1:dim(mouthPeaks)[1]) {plot(mouthPeaks[i,][is.na(mouthPeaks[i,])==FALSE], type='b', xlab=i)}

mouthStrikeStart <- c(1, 1, 6, 4, 9,
                      6, 10, 6, 12, 1,
                      5, 3, 4, 4, 3, 
                      17, 1, 3, 2, 1,
                      1, 1, 1, 1, 5)
mouthStrikeEnd <- c(22, 14, 12, 15, 30,
                    29, 29, 28, 33, 20,
                    27, 19, 26, 28, 24,
                    31, 24, 21, 23, 14,
                    20, 21, 31, 30, 25)
duration <- mouthStrikeEnd - mouthStrikeStart
mouthStrikeMax <- apply(mouthStrike, 1, max, na.rm=TRUE)





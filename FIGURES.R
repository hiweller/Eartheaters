source('/Users/hannah/Dropbox/Westneat Lab/Eartheater Project/Code/genfunctions.R')

# uncomment only if new data have been added to shapes folders! takes ~30 seconds to run
# gets and normalizes mouth gape, branchiostegal expansion, premax distance
# can be modded to get other stuff. probably will be
getShapeMeasurements()

phases <- c('S', 'W', 'E')
titles <- c('Strike', 'Winnowing', 'Ejection')
xlabel <- c("", "", 'Seconds')
Wstate <- phases=='W'
measurements <- c('mouthgape', 'branchiostegals', 'premax', 'angle', 'premax.angle')
Q <- length(phases)
G <- length(measurements)

graphs <- rep(list(vector("list", Q)), G)
colors <- c('orange', 'tomato1', 'red2', 
            'lightskyblue1', 'dodgerblue1', 'blue',
            'orchid1', 'mediumorchid2', 'darkorchid3',
            'darkseagreen2', 'green', 'forestgreen',
            2, 4, 5)

for (i in 1:G) {
  for (j in 1:Q) {
    graphs[[i]][[j]] <- graphAlignPeaksGG(paste('./Measurements/', phases[j], '_',
                                                measurements[i], '.csv', sep=""),
                                            col=paste(colors[Q*(i-1)+j]), xlab=xlabel[j],
                                          main=titles[j], W=Wstate[j])
  }
  multiplot(graphs[[i]][[1]], graphs[[i]][[2]], graphs[[i]][[3]])
}

par(mar=c(3.1,3.1,3.1,2.1), mgp=c(2,1,0))

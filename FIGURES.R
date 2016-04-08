source('/Users/hannah/Documents/Westneat Lab/Eartheater Project/Code/genfunctions.R')

# uncomment only if new data have been added to shapes folders! takes ~30 seconds to run
# gets and normalizes mouth gape, branchiostegal expansion, premax distance
# can be modded to get other stuff. probably will be
# getShapeMeasurements()
# par(mar=c(3.1,3.1,3.1,2.1), mgp=c(2,1,0))

# plots all the different mouth, branch, premax distances 
# mouthgape = red, branch = blue, premax = purple
# organized as strike, winnow, ejection
meanPlots()

# all three phases for one representative trial
repTrial()



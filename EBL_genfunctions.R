library('StereoMorph')
library('dplyr')

# function for visualizing curves
seeCurves <- function(shapesDir, pausing=TRUE) {
  for (i in 1:length(shapesDir)) {
    shapes <- readShapes(paste('./Shapes/', shapesDir[i], sep=""))
    coords <- shapes$curves.scaled
    
    if (is.null(coords)==FALSE) {
      xrange <- range(sapply(coords, function(x) {range(x[,1])}))
      yrange <- range(sapply(coords, function(x) {range(x[,2])}))
      
      plot(coords[[1]], xlim=xrange, ylim=yrange, 
           asp=1, type='l', col=i, xlab="", ylab="", main=substr(shapesDir[i], 1, nchar(shapesDir[i])-4))
      for (j in 2:length(coords)) {
        points(coords[[j]], type='l', col=i)
      }
      
      if (pausing==TRUE){
        cat ("Press [enter] to continue")
        line <- readline() 
      }
    }
    
  }
  
}

# takes 3 arguments: 
# number of points to convert each curve into
# path to directory with shapes files
# path for saving coordinates
curves_to_points <- function(n=10, filepath, savepath=FALSE) {
  shapesDir <- dir(filepath, pattern = '*.txt') # directory of all shapes files
  
  for (i in 1:length(shapesDir)) {
    print(i)
    file <- readShapes(paste(filepath, shapesDir[i], sep="")) # read shapes file
    
    if (is.null(file$curves.scaled)==FALSE) {
      curveCount <- length(file$curves.scaled) # count number of curves
      pointCurves <- vector("list", curveCount) # create empty list for storing curve coordinates
      names(pointCurves) <- names(file$curves.scaled) # give list elements same names as curves
      
      for (j in 1:curveCount) { # for every curve in the shapes file
        curve <- file$curves.scaled[[j]] # isolate that particular curve
        pointVec <- matrix(data = NA, nrow=(n-2), ncol = 2) # empty matrix for storing coordinates
        ref <- round(seq(1, dim(curve)[1], round(dim(curve)[1])/n)) # references for evenly spaced coordinates
        # ex, with 10 coordinates and 1000 curve points you want to take every 100th (1000/10) coordinate
        ref <- ref[2:(n-1)] # since StereoMorph stores the base and the tip (coords 1 and n), only take central
        
        pointVec <- curve[ref,] # store referenced coordinates
        
        # save as dataframe with curve name and X and Y coords
        pointCurves[[j]] <- data.frame(Curve = rep(names(file$curves.scaled)[j], (n-2)), 
                                       X=pointVec[,1], 
                                       Y=pointVec[,2])
        # convert 'Curve' to character instead of factor to avoid error message
        pointCurves[[j]]$Curve <- as.character(pointCurves[[j]]$Curve)
      }
      
      # combine all curve measurements in one dataframe
      stack <- bind_rows(pointCurves)
      stack <- rbind(stack, data.frame(Curve = row.names(file$landmarks.scaled),
                                       X = as.numeric(file$landmarks.scaled[,1]),
                                       Y = as.numeric(file$landmarks.scaled[,2])))
      
      if (savepath != FALSE) {
        savename <- paste(savepath, unlist(strsplit(shapesDir[i], split = "[.]"))[1], '_points.csv', sep="")
        write.csv(x = stack, file = savename)
      }
    }
    
  }
}

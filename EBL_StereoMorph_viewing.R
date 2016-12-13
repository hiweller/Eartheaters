setwd('/Users/hannah/Dropbox/Westneat_Lab/Eartheater Project/Data/Dissection_photos/EBL/Lateral_dig/')
library(StereoMorph)

# shapesDir <- dir('./Shapes/', pattern='*.txt')

# curves_to_points(n=15, filepath='./Shapes/', savepath='./Coordinates/')

coordinates <- read.csv('/Users/hannah/Dropbox/Westneat_Lab/Eartheater Project/Data/MorphoJ_files/12-13-2016_EBL.csv')

EBL.pca <- prcomp(coordinates[,2:93], center=TRUE, scale. = TRUE)

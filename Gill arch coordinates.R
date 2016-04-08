source('/Users/hannah/Dropbox/Westneat Lab/Eartheater Project/Code/genfunctions.R')
setwd('/Users/hannah/Dropbox/Westneat Lab/Eartheater Project/Data/Dissection_photos/Gill arches/')
library(VGAM)

gill.coord.dir <- dir(path='.', pattern='*.csv')
save.names <- c('Arch width (cm)', 'Arch height (cm)', 'EBL aspect ratio',
                'EBL raker aspect ratio', 'Gill raker aspect ratio',
                'Arch aspect ratio', 'EBL angle', 'EBL:Arch aspect ratio', 'Lobe length')

ID.names <- vector()
Arch.width <- vector()
Arch.height.ish <- vector()
EBL.AR <- vector()
EBL.raker.AR <- vector()
Gill.raker.AR <- vector()
Arch.AR <- vector()
Lobe.ang <- vector()
EBL.Arch.AR <- vector()
Lobe.length <- vector()

for (i in 1:length(gill.coord.dir)) {
  
  ID.names[i] <- as.character(strsplit(gill.coord.dir[i], split=".csv")[1])
  
  gill.coords <- read.csv(gill.coord.dir[i], header=TRUE)
  gill.coords <- gill.coords[,2:3]
  
  # distances
  d1.2 <- get.dist(gill.coords[1,], gill.coords[2,])
  d3.4 <- get.dist(gill.coords[3,], gill.coords[4,])
  d6.7 <- get.dist(gill.coords[6,], gill.coords[7,])
  d8.9 <- get.dist(gill.coords[8,], gill.coords[9,])
  d1.5 <- get.dist(gill.coords[1,], gill.coords[5,])
  d5.11 <- get.dist(gill.coords[5,], gill.coords[11,])
  d1.11 <- get.dist(gill.coords[1,], gill.coords[11,])
  d2.16 <- get.dist(gill.coords[2,], gill.coords[16,])
  d10.11 <- get.dist(gill.coords[10,], gill.coords[11,])
  d12.13 <- get.dist(gill.coords[12,], gill.coords[13,])
  d14.15 <- get.dist(gill.coords[14,], gill.coords[15,])
  
  # distances
  Arch.width[i] <- d10.11
  Arch.height.ish[i] <- d2.16
  Lobe.length[i] <- d1.2
  # aspect ratios
  EBL.AR[i] <- d1.2/d3.4
  EBL.raker.AR[i] <- d6.7/d8.9
  Gill.raker.AR[i] <- d12.13/d14.15
  Arch.AR[i] <- Arch.width[i]/Arch.height.ish[i]
  EBL.Arch.AR[i] <- d1.2/d2.16
  
  # angle of lobe
  Lobe.ang[i] <- TriangleCalc(d1.5, d5.11, d1.11)[1]
  
}

gill.dataframe <- data.frame(Arch.width, Arch.height.ish, 
                             EBL.AR, EBL.raker.AR, Gill.raker.AR,
                             Arch.AR, Lobe.ang, EBL.Arch.AR, Lobe.length, row.names = ID.names)
write.csv(round(gill.dataframe, digits=3), file = '/Users/hannah/Dropbox/Westneat Lab/Eartheater Project/Data/Data_sheets/Gill_arch_individual.csv')

gill.dataframe <- as.matrix(gill.dataframe)

species.names <- vector()
species.dataframe <- matrix(NA, ncol=dim(gill.dataframe)[2], nrow=dim(gill.dataframe)[1])
bannedForLife <- vector()
counter <- 0
for (j in 1:(dim(gill.dataframe)[1])) {
  ref1 <- strsplit(row.names(gill.dataframe)[j], split="_0")
  ref1 <- ref1[[1]][1]
  
  IDmatch <- which(substr(row.names(gill.dataframe), 1, 7) %in% ref1)
  if (length(IDmatch)>0 && ((ref1 %in% bannedForLife)==FALSE)) {
    bannedForLife <- c(bannedForLife, ref1)
    if (length(IDmatch)>1) {
      newrow <- as.vector(colSums(gill.dataframe[IDmatch,])/length(IDmatch))
    } else { newrow <- gill.dataframe[IDmatch,]}
    species.dataframe[j,] <- newrow
  }
}
new.dataframe <- as.data.frame(species.dataframe[rowSums(is.na(species.dataframe))<1,], 
                               row.names = bannedForLife)
colnames(new.dataframe) <- save.names
write.csv(round(new.dataframe, digits=3), file = '/Users/hannah/Dropbox/Westneat Lab/Eartheater Project/Data/Data_sheets/Gill_arch_by_species.csv')


## correlating with winnowing strategy?
winnow <- read.csv('/Users/hannah/Dropbox/Westneat Lab/Eartheater Project/Data/Data_sheets/Gill_arch_individual_winnowing.csv')
winnow <- winnow[-12,]
GARfit <- glm(data=winnow, Winnowing~Gill.raker.AR)
winnow$fitted <- GARfit$fitted.values
cbPalette <- c('dodgerblue', 'lightgreen', 'lightgoldenrod1', 
               'green3', 'goldenrod1', 'brown1',
               'coral','lightslateblue', 'darkgreen',
               'mediumseagreen', 'turquoise', 'royalblue3')
p <- ggplot(data=winnow, aes(x=Gill.raker.AR, y=Winnowing, label=X))
p <- p + geom_point(data=winnow, aes(color=factor(Genus)), size=3)
p <- p + scale_color_manual(values=cbPalette, name='Genus')
p <- p + xlab('Gill raker length:width') + ylab('Winnowing category')
p <- p + geom_line(data=winnow, aes(x=Gill.raker.AR, y=fitted), lty=2, alpha=0.8)
GillRaker <- p + ggtitle('Gill raker aspect ratio')
GillRaker
p <- ggplot(data=winnow, aes(x=Arch.width, y=Winnowing, label=X))
p <- p + geom_point(data=winnow, aes(color=factor(Genus)), size=3)
p <- p + scale_color_manual(values=cbPalette, name='Genus')
p <- p + xlab('Width (cm)') + ylab('Winnowing level')
ArchWidth <- p + ggtitle('Arch Width')


p <- ggplot(data=winnow, aes(x=EBL.raker.AR, y=Winnowing, label=X))
p <- p + geom_point(data=winnow, aes(color=factor(Genus)), size=3)
p <- p + scale_color_manual(values=cbPalette, name='Genus')
p <- p + xlab('Epibranchial lobe length:width') + ylab('Winnowing level')
EBLAspect <- p + ggtitle('Epibranchial lobe aspect ratio')

p <- ggplot(data=winnow, aes(x=Lobe.ang, y=Winnowing, label=X, color=factor(Genus)))
p <- p + geom_point(size=3)
p <- p + scale_color_manual(values=cbPalette, name='Genus')
p <- p + xlab('Epibranchial lobe angle (degrees)') + ylab('Winnowing level')
EBLAngle <- p + ggtitle('Epibranchial Lobe Angle')


p <- ggplot(data=winnow, aes(x=EBL.Arch.AR, y=Winnowing, label=X, color=factor(Genus)))
p <- p + geom_point(size=3)
p <- p + scale_color_manual(values=cbPalette, name='Genus')
p <- p + xlab('Epibranchial lobe:arch length') + ylab('Winnowing level')
EBL.AR <- p + ggtitle('Epibranchial lobe size')

p <- ggplot(data=winnow, aes(x=Lobe.length, y=Winnowing, label=X))
p <- p + geom_point(data=winnow, aes(color=factor(Genus)), size=3)
p <- p + scale_color_manual(values=cbPalette, name='Genus')
p <- p + xlab('Length (cm)') + ylab('Winnowing level')
LL <- p + ggtitle('Lobe length')

ArchWidth
GillRaker
EBLAspect
EBLAngle
EBL.AR
LL


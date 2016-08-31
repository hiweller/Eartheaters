source('/Users/hannah/Dropbox/Westneat_Lab/Eartheater Project/Code/genfunctions.R')

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
ggsave('/Users/hannah/Dropbox/Westneat_Lab/Eartheater Project/Manuscript/Figs/Paper/Fig05D.pdf', dpi=600)
## CV plots (FIG. 5E)
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
leg <- expression("Fishes", italic("Satanoperca"), "Lepidosaurs")

p <- ggplot(data=df, aes(x=factor(Names), y=CV, fill=factor(Names)))
p <- p + geom_violin() + scale_fill_manual(values=c('#00b2cc', '#ffb31a', '#40bf40')) + guides(fill=FALSE)
p <- p + scale_x_discrete("Names", labels=expression(Fishes, italic(Satanoperca), Lepidosaurs))
p <- p + theme(text = element_text(size=20), legend.text.align=0, axis.title.x=element_blank())
p <- p + ylab("Cycle duration CV")
p
ggsave(filename = '/Users/hannah/Dropbox/Westneat_Lab/Eartheater Project/Manuscript/Figs/Paper/Fig05E.eps', plot=last_plot(), dpi=600)



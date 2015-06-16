# plots
require(ggplot2)
require(xtable)
load("macro1.RData")
data <- data[, -21]
data0 <- data[-1,]
for (i in 3:(ncol(data)-1)) {
  data0[, i] <- diff(log(data[, i]))
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

###################################################################################

plot1 <- ggplot(data = data0, mapping = aes(x = dates, y = GNPK))
plot1 <- plot1 + geom_line() + theme_bw() + ylab("log difference in GNP") + xlab("")
plot2 <- ggplot(data = data0, mapping = aes(x = dates, y = CPIIT))
plot2 <- plot2 + geom_line() + theme_bw() + ylab("inflation") + xlab("")
plot3 <- ggplot(data = data0, mapping = aes(x = dates, y = X3MBOT))
plot3 <- plot3 + geom_line() + theme_bw() + ylab("3 month interest rate") + xlab("")

grid.arrange(plot1, plot2)
multiplot(plot1, plot2)

print(xtable(data0[, c(3, 23)]))

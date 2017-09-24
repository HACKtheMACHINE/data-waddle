library(magrittr)
library(tidyverse)

dataset <- read.csv("classC_ship1_MRG.csv")

names(dataset)

MRG1 <-
  dataset$MRG2.TN.GEAR.FWD.BRG.T %>%
  is.na %>%
  which %>%
  dataset[.,]
x1 <-
  dataset %>%
  names %>%
  grep("MRG1", .)
x1 <- c(56, x1)
MRG1 <- MRG1[, x1]

MRG2 <-
  dataset$MRG1.TN.GEAR.FWD.BRG.T %>%
  is.na %>%
  which %>%
  dataset[.,]
x2 <-
  dataset %>%
  names %>%
  grep("MRG2", .)
x2 <- c(56, x2)
MRG2 <- MRG2[, x2]



colnames(MRG1) <- 
  c("time", "TN.GEAR.FWD.BRG.T",  "PRI.TN.GEAR.ENGGED", "PRI.TN.GEAR.DISENG", 
    "LO.PMP.A.DISC.PRES", "THR.BRG.AHEAD.TEMP", "SEC.TN.GEAR.DISENG", 
    "THR.BRG.ASTN.TEMP", "LO.SUMP.LVL",  "PN.AFT.SBD.BG.TEMP",
    "HMRB.PT2.PRES", "TN.GEAR.AFT.BRG.T", "LO.SUMP.TEMP", "SEC.TN.GEAR.ENGGED",
    "CLUTCH.AIR.PRES", "PN.FWD.SBD.BG.TEMP", "HMRB.PT1.PRES", 
    "LO.CLR.FW.OUT.TEMP", "LO.CLR.OUT.TEMP", "THR.BRG.OIL.TEMP", 
    "LS.GR.FWD.BRG.TEMP", "LO.HDR.PRES", "LOSP.DISCH.PRES", "LO.HDR.TEMP",
    "LO.PMP.B.DISC.PRES", "PN.AFT.PRT.BG.TEMP", "PN.FWD.PRT.BG.TEMP", 
    "LS.GR.AFT.BRG.TEMP", "LO.STR.DP") %>%
  tolower

colnames(MRG2) <- 
  c("time", "TN.GEAR.FWD.BRG.T",  "PRI.TN.GEAR.ENGGED", "PRI.TN.GEAR.DISENG", 
    "LO.PMP.A.DISC.PRES", "THR.BRG.AHEAD.TEMP", "SEC.TN.GEAR.DISENG", 
    "THR.BRG.ASTN.TEMP", "LO.SUMP.LVL",  "PN.AFT.SBD.BG.TEMP",
    "HMRB.PT2.PRES", "TN.GEAR.AFT.BRG.T", "LO.SUMP.TEMP", "SEC.TN.GEAR.ENGGED",
    "CLUTCH.AIR.PRES", "PN.FWD.SBD.BG.TEMP", "HMRB.PT1.PRES", 
    "LO.CLR.FW.OUT.TEMP", "LO.CLR.OUT.TEMP", "THR.BRG.OIL.TEMP", 
    "LS.GR.FWD.BRG.TEMP", "LO.HDR.PRES", "LOSP.DISCH.PRES", "LO.HDR.TEMP",
    "LO.PMP.B.DISC.PRES", "PN.AFT.PRT.BG.TEMP", "PN.FWD.PRT.BG.TEMP", 
    "LS.GR.AFT.BRG.TEMP", "LO.STR.DP") %>%
  tolower

names(MRG1)
names(MRG2)

MRG1$time %<>% 
  as.POSIXct(format = "%m/%d/%Y %H:%M:%S %p") %>% 
  as.numeric
MRG2$time %<>% 
  as.POSIXct(format = "%m/%d/%Y %H:%M:%S %p") %>% 
  as.numeric

time1 <- (MRG1$time - 1473320464)
time2 <- (MRG2$time - 1474870948)
MRG1 <- cbind(zero.time = time1, MRG1)
MRG2 <- cbind(zero.time = time2, MRG2)

x1 <- which(duplicated(MRG1))
x2 <- which(duplicated(MRG2))
MRG1.tiny <- MRG1[x1, ]
MRG2.tiny <- MRG2[x2, ]

percentage <-  (count(MRG1.tiny) + count(MRG2.tiny)) / count(dataset) * 100

plot1 <- 
  ggplot(data = MRG1.tiny) +
  aes(x = as.numeric(rownames(MRG1.tiny)), y = MRG1.tiny$zero.time) +
  geom_point(color = I("Red")) + ggtitle("Class C Ship 1 MRG 1") + xlab("Row Order") +
  ylab("POSIX time from t=0") +
  theme(axis.title.x = element_text(color = "DarkBlue", size = 16),
        axis.title.y = element_text(color = "DarkBlue", size = 16),
        plot.title = element_text(color = "DarkBlue", size = 20))

plot2 <-
  ggplot(data = MRG2.tiny) + 
  aes(x = as.numeric(rownames(MRG2.tiny)), y = MRG2.tiny$zero.time) +
  geom_point(color = I("Blue")) + ggtitle("Class C Ship 1 MRG 2") + xlab("Row Order") +
  ylab("POSIX time from t=0") +
  theme(axis.title.x = element_text(color = "DarkBlue", size = 16),
        axis.title.y = element_text(color = "DarkBlue", size = 16),
        plot.title = element_text(color = "DarkBlue", size = 20))

multiplot(plot1, plot2)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=2, layout=NULL) {
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

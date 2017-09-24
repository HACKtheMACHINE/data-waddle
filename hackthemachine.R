#### HACKtheMACHINE R Script #############################################
#
# Data for HACKtheMACHINE at MIT CSAIL Weekend Sep 22, 2017
#
# Author: Daniel Brown

#### Data Preprocessing ##################################################
library(magrittr)
library(tidyverse)

### Importing the dataset ################################################
dataset <- read.csv("classC_ship1_MRG.csv")
M1A_10 <- read.csv("classC_ship2_MPDE70.csv")
M1A_68 <- read.csv("classC_ship2_MPDE12.csv")

### Seperating dataset by indicator into MRG1, MRG2 ######################
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

### Despecifying colnames for MRG1 and MRG2 ##############################
colnames(MRG1) <- 
  c("time", "TN.GEAR.FWD.BRG.T",  "PRI.TN.GEAR.ENGGED", 
    "PRI.TN.GEAR.DISENG", "LO.PMP.A.DISC.PRES", "THR.BRG.AHEAD.TEMP", 
    "SEC.TN.GEAR.DISENG", "THR.BRG.ASTN.TEMP", "LO.SUMP.LVL",  
    "PN.AFT.SBD.BG.TEMP", "HMRB.PT2.PRES", "TN.GEAR.AFT.BRG.T", 
    "LO.SUMP.TEMP", "SEC.TN.GEAR.ENGGED", "CLUTCH.AIR.PRES", 
    "PN.FWD.SBD.BG.TEMP", "HMRB.PT1.PRES", "LO.CLR.FW.OUT.TEMP", 
    "LO.CLR.OUT.TEMP", "THR.BRG.OIL.TEMP", "LS.GR.FWD.BRG.TEMP", 
    "LO.HDR.PRES", "LOSP.DISCH.PRES", "LO.HDR.TEMP", "LO.PMP.B.DISC.PRES",
    "PN.AFT.PRT.BG.TEMP", "PN.FWD.PRT.BG.TEMP", "LS.GR.AFT.BRG.TEMP", 
    "LO.STR.DP") %>%
  tolower

colnames(MRG2) <- 
  c("time", "TN.GEAR.FWD.BRG.T",  "PRI.TN.GEAR.ENGGED", 
    "PRI.TN.GEAR.DISENG", "LO.PMP.A.DISC.PRES", "THR.BRG.AHEAD.TEMP", 
    "SEC.TN.GEAR.DISENG", "THR.BRG.ASTN.TEMP", "LO.SUMP.LVL",  
    "PN.AFT.SBD.BG.TEMP", "HMRB.PT2.PRES", "TN.GEAR.AFT.BRG.T", 
    "LO.SUMP.TEMP", "SEC.TN.GEAR.ENGGED", "CLUTCH.AIR.PRES", 
    "PN.FWD.SBD.BG.TEMP", "HMRB.PT1.PRES", "LO.CLR.FW.OUT.TEMP", 
    "LO.CLR.OUT.TEMP", "THR.BRG.OIL.TEMP", "LS.GR.FWD.BRG.TEMP", 
    "LO.HDR.PRES", "LOSP.DISCH.PRES", "LO.HDR.TEMP", "LO.PMP.B.DISC.PRES",
    "PN.AFT.PRT.BG.TEMP", "PN.FWD.PRT.BG.TEMP", "LS.GR.AFT.BRG.TEMP", 
    "LO.STR.DP") %>%
  tolower

### Converting timestamp to R recognized POSIX numeric ###################
MRG1$time %<>% 
  as.POSIXct(format = "%m/%d/%Y %H:%M:%S %p") %>% 
  as.numeric
time1 <- (MRG1$time - 1473320464)
MRG1 <- cbind(zero.time = time1, MRG1)

MRG2$time %<>% 
  as.POSIXct(format = "%m/%d/%Y %H:%M:%S %p") %>% 
  as.numeric
time2 <- (MRG2$time - 1474870948)
MRG2 <- cbind(zero.time = time2, MRG2)

### Pulling duplicated rows from MRG1 and MRG2 ###########################
x1 <- which(duplicated(MRG1))
MRG1.tiny <- MRG1[x1, ]

x2 <- which(duplicated(MRG2))
MRG2.tiny <- MRG2[x2, ]

#### Plottting Data ##########################################################

plot1 <- 
  ggplot(data = MRG1.tiny) +
  aes(x = as.numeric(rownames(MRG1.tiny)), y = MRG1.tiny$zero.time) +
  geom_point(color = I("Red")) + ggtitle("Class C Ship 1 MRG 1") + 
  xlab("Row Order") + ylab("POSIX time from t=0") +
  theme(axis.title.x = element_text(color = "DarkBlue", size = 16),
        axis.title.y = element_text(color = "DarkBlue", size = 16),
        plot.title = element_text(color = "DarkBlue", size = 20))

plot2 <-
  ggplot(data = MRG2.tiny) + 
  aes(x = as.numeric(rownames(MRG2.tiny)), y = MRG2.tiny$zero.time) +
  geom_point(color = I("Blue")) + ggtitle("Class C Ship 1 MRG 2") + 
  xlab("Row Order") + ylab("POSIX time from t=0") +
  theme(axis.title.x = element_text(color = "DarkBlue", size = 16),
        axis.title.y = element_text(color = "DarkBlue", size = 16),
        plot.title = element_text(color = "DarkBlue", size = 20))

plot3 <-
  ggplot(data = M1A_10) + 
  aes(x = as.numeric(rownames(M1A_10)), y = M1A_10$time.zero) +
  geom_point(color = I("Purple")) + 
  ggtitle("Class C Ship 2 MPDE (sixty eight attributes)") + 
  xlab("Row Order") + ylab("POSIX time from t=0") +
  theme(axis.title.x = element_text(color = "DarkBlue", size = 16),
        axis.title.y = element_text(color = "DarkBlue", size = 16),
        plot.title = element_text(color = "DarkBlue", size = 20))

plot4 <-
  ggplot(data = M1A_68) + 
  aes(x = as.numeric(rownames(M1A_68)), y = M1A_68$time.zero) +
  geom_point(color = I("Orange")) + 
  ggtitle("Class C Ship 2 MPDE (ten attributes)") + 
  xlab("Row Order") + ylab("POSIX time from t=0") +
  theme(axis.title.x = element_text(color = "DarkBlue", size = 16),
        axis.title.y = element_text(color = "DarkBlue", size = 16),
        plot.title = element_text(color = "DarkBlue", size = 20))

### Call multiplot function ##############################################
multiplot(plot1, plot2, plot3, plot4)

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

write.csv(MRG1, "classC_ship1_MRG1.csv")
write.csv(MRG2, "classC_ship1_MRG2.csv")

# time <- (MRG1$time - 1473320464)/360
# MRG1$time <- time




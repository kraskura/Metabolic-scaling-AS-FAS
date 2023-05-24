# Simulation Aerobic scope ------

library(ggplot2)
library(tidyverse)
library(cowplot)
library(here)
library(ggformat2)
library(here)
here()
# create directory "./Figures" to save all figures generated in functions. 

dir.create("Figures", recursive = T)
dir.create("Data", recursive = T)
list.files() # check what local directory is like. 


# *******************************************
# *******************************************
source("./R/FAS_AS_conceptual_only.R")
source("./R/simulations.R")

# *******************************************
# *******************************************

# Function runs ------
data.sim0<-as.data.frame(expand.grid(BW_kg = c(0.05, 0.5, 1, 1.5, 2, 10)))
data.sim<-as.data.frame(expand.grid(BW_kg = seq(0.1, 20.1 , 2))) # body size from 0.1 kg to 20 kg

# to keep this static 
intMMR<-log(5)
intSMR<-log(0.5)


## CONDITIONS 1: b 0.89 = VARY, aMMR = constant, bSMR = constant, a SMR = constant   ---------
# mmrVar<-simulation.loop(color.group.vary = "byMMR",
#                         slopeseq.mmr = seq(0.66,1,0.01),
#                         slopeseq.smr = rep(0.89, length(seq(0.66,1,0.01))),
#                         intseq.mmr = log(5), intseq.smr = log(0.5), data.sim0)
# 
# data.simFULL.MMRvar<-mmrVar[[1]]
# P1.MMRvar<-mmrVar[[2]]
# P2.MMRvar<-mmrVar[[3]]
# ggsave(plot = P1.MMRvar, filename = "./Figures/MMRvar_plot1.png", width = 8.5, height = 9)
# ggsave(plot = P2.MMRvar, filename = "./Figures/MMRvar_plot2.png", width = 12, height = 8)
# 

## CONDITIONS 2: bMMR 0.89 = constant, aMMR = constant, bSMR = VARY, aSMR = constant   ---------
# smrVar<-simulation.loop(color.group.vary = "bySMR",
#                         slopeseq.smr = seq(0.66,1,0.01),
#                         slopeseq.mmr = rep(0.89, length(seq(0.66,1,0.01))),
#                         intseq.mmr = intMMR, intseq.smr = intSMR, data.sim0)
# 
# data.simFULL.SMRvar<-smrVar[[1]]
# P1.SMRvar<-smrVar[[2]]
# P2.SMRvar<-smrVar[[3]]
# ggsave(plot = P1.SMRvar, filename = "./Figures/SMRvar_plot1.png", width = 8.5, height = 9)
# ggsave(plot = P2.SMRvar, filename = "./Figures/SMRvar_plot2.png", width = 12, height = 8)


## CONDITIONS 3: bMMR = VARY down, aMMR = constant, bSMR = VARY up, aSMR = constant    ---------
# smrmmrVar<-simulation.loop(color.group.vary = "byMMR",
#                         slopeseq.smr = seq(0.66,1,0.01),
#                         slopeseq.mmr = rev(seq(0.66,1,0.01)),
#                         intseq.mmr = intMMR, intseq.smr = intSMR, data.sim0)
# 
# data.simFULL.SMRMMRvar<-smrmmrVar[[1]]
# P1.SMRMMRvar<-smrmmrVar[[2]]
# P2.SMRMMRvar<-smrmmrVar[[3]]
# ggsave(plot = P1.SMRMMRvar, filename = "./Figures/SMRMMRvar1_plot1.png", width = 8.5, height = 9)
# ggsave(plot = P2.SMRMMRvar, filename = "./Figures/SMRMMRvar1_plot2.png", width = 12, height = 8)
# 

## CONDITIONS 4: bMMR = VARY up, aMMR = constant, bSMR = VARY down, aSMR = constant    ---------
# smrmmrVar2<-simulation.loop(color.group.vary = "byMMR",
#                            slopeseq.smr = rev(seq(0.66,1,0.01)),
#                            slopeseq.mmr = seq(0.66,1,0.01),
#                            intseq.mmr = intMMR, intseq.smr = intSMR, data.sim0)
# 
# data.simFULL.SMRMMRvar2<-smrmmrVar2[[1]]
# P1.SMRMMRvar2<-smrmmrVar2[[2]]
# P2.SMRMMRvar2<-smrmmrVar2[[3]]
# ggsave(plot = P1.SMRMMRvar2, filename = "./Figures/SMRMMRvar2_plot1.png", width = 8.5, height = 9)
# ggsave(plot = P2.SMRMMRvar2, filename = "./Figures/SMRMMRvar2_plot2.png", width = 12, height = 8)
# 

## CONDITIONS 5-for conceptual: ---------

data.simCONCEPT<-as.data.frame(expand.grid(BW_kg = c(0.1, 10, 1))) # body size from 0.1 kg to 20 kg

smrmmrVar2<-simulation.loop(color.group.vary = "byMMR",
                           slopeseq.smr = rev(c( 0.89, 0.75, 0.75)),
                           slopeseq.mmr = c(1, 0.75, 0.75),
                           intseq.mmr = c(log(7)),
                           intseq.smr = c(log(2)),
                           data.simCONCEPT)

data.simFULL.SMRMMRvar2<-smrmmrVar2[[1]]
P1.SMRMMRvar2<-smrmmrVar2[[2]]
P2.SMRMMRvar2<-smrmmrVar2[[3]]
ggsave(plot = P1.SMRMMRvar2, filename = "./Figures/Concept_plot1.png", width = 8.5, height = 9)
ggsave(plot = P2.SMRMMRvar2, filename = "./Figures/Concept_plot2.png", width = 12, height = 8)

# concept_FAS_AS -------

figures<-concept_FAS_AS(data = data.simFULL.SMRMMRvar2)

p1<-(figures[[1]])
p2<-(figures[[2]])
p3<-(figures[[3]])

# p1.raw<-(figures[[4]])
# p2.raw<-(figures[[5]])
# p3.raw<-(figures[[6]])

cowplot::plot_grid(  p3, p1,p2,
                   nrow=1)
# cowplot::plot_grid(p1.raw, p2.raw, p3.raw, 
                   # nrow=1)


setwd("/Users/michael/CogEmoFaceReward")
library(plyr)
library(ggplot2)
library(reshape2)
library(gdata)
learningParams <- read.table("fit_behavior/SubjsSummary_emoexplore.txt", header=TRUE)
behav <- read.xls("data/clock_demographics_questionnaires.xls", sheet="Sheet1", skip=1)
learningParams <- rename(learningParams, c(Subject="LunaID"))
behav <- merge(learningParams, behav[,c("LunaID", "AgeAtVisit", "Urg", "PosUrg", "SS")], by="LunaID")

#explore by emotion
explore.melt <- melt(behav[,c("LunaID", "AgeAtVisit", "SS", "explore_scram", "explore_fear", "explore_happy")], id.vars=c("LunaID", "AgeAtVisit", "SS"))

png("Explore_by_emotion.png", width=12, height=10, units="in", res=300)
ggplot(explore.melt, aes(x=value)) + geom_histogram(binwidth=1000) + facet_wrap(~variable) + ggtitle("Exploration parameter by emotion")
dev.off()

#plot scatter plot of exploration by condition and age

ggplot(explore.melt, aes(x=AgeAtVisit, y=value)) + geom_point() + stat_smooth(se=FALSE, method="lm") + facet_wrap(~variable) + ggtitle("Exploration parameter by emotion and age")
ggplot(explore.melt, aes(x=SS, y=value)) + geom_point() + stat_smooth(se=FALSE, method="lm") + facet_wrap(~variable) + ggtitle("Exploration parameter by emotion and sensation seeking")

ggplot(behav, aes(x=AgeAtVisit, y=alphaG)) + geom_point() + stat_smooth(se=FALSE, method="lm") + ggtitle("Go param by age")
ggplot(behav, aes(x=AgeAtVisit, y=alphaN)) + geom_point() + stat_smooth(se=FALSE, method="lm") + ggtitle("NoGo param by age")
ggplot(behav, aes(x=AgeAtVisit, y=rho)) + geom_point() + stat_smooth(se=FALSE, method="lm") + ggtitle("rho param by age")

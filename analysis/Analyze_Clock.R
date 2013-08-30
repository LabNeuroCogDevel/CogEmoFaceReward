setwd("/Users/michael/CogEmoFaceReward/analysis")
library(plyr)
library(ggplot2)
library(reshape2)
library(gdata)

##compare fits across subjects and models
fitFiles <- list.files(path="../fit_behavior", pattern="SubjsSummary.*", full.names = TRUE)
allM <- list()
for (f in fitFiles) {
    model <- sub(pattern="^.*SubjsSummary_(.*)\\.txt", replacement="\\1", x=f, perl=TRUE)
    p <- read.table(f, header=TRUE, sep="\t")
    p$nparams <- length(which(names(p) %notin% c("Subject", "Session", "ignore", "SSE", "model")))
    ##p <- subset(p, select=c("Subject", "SSE", "nparams"))
    p$ntrials <- 504 #fixed: 12 runs of 42 trials
    p$Subject <- factor(as.integer(p$Subject))
    p$model <- factor(model)
    allM[[f]] <- p
}

allM <- do.call(rbind.fill, allM)

allM$AIC <- with(allM, ntrials*(log(2*pi*(SSE/ntrials))+1) + 2*nparams)

##for compatibility with spm_BMS.m (Bayesian Model Selection),
##need a subjects x models AIC matrix
AICmat <- do.call(cbind, lapply(split(allM, allM$model), "[[", "AIC"))

##per discussion with Michael Frank (and review of Stephan 2009),
##the use of AIC as an approximation of log-evidence is the LL - nparams
##Thus, for use with spm_BMS, we need to negate the values of AIC computed
AICmat <- -1*AICmat

library(R.matlab)
writeMat(con="AICmatrix_n36.mat", AICmat=AICmat, mnames=levels(allM$model))

##run SPM BMS in MATLAB
system("matlab -nodisplay < computeBMSprobs.m")

BMSresults <- readMat(con="AICresults_n36.mat")

##form into a data.frame
BMSresults <- with(BMSresults, data.frame(model=unlist(mnames), alpha=as.vector(alpha), expr=as.vector(expr), xp=as.vector(xp), mAIC=apply(AICmat, 2, mean)))

print(BMSresults)

allM <- ddply(allM, .(Subject), function(subdf) {
    minAIC <- min(subdf$AIC)
    minSSE <- min(subdf$SSE)
    subdf$AICdiff <- subdf$AIC - minAIC
    subdf$SSEdiff <- subdf$SSE - minSSE
    subdf
})


png("Subject_SSE_byModel.png", width=8, height=6, units="in", res=300)
ggplot(allM, aes(x=Subject, y=SSE, color=model)) + geom_jitter(size=2, position = position_jitter(width = .3, height=0)) + xlab("Subject") + ylab("SSE") +
    theme(axis.text.x=element_text(angle=90)) + scale_color_brewer(palette="Dark2") + coord_flip() + theme_bw(base_size=14)
dev.off()

png("Avg_SSE_byModel.png", width=8, height=6, units="in", res=300)
ggplot(allM, aes(x=model, y=SSE)) + geom_boxplot() + xlab("Model") + ylab("SSE") +
    theme(axis.text.x=element_text(angle=90))
dev.off()

png("Avg_AIC_byModel.png", width=8, height=6, units="in", res=300)
ggplot(allM, aes(x=model, y=AIC)) + geom_boxplot() + xlab("Model") + ylab("AIC") +
    theme(axis.text.x=element_text(angle=90))
dev.off()

sort(tapply(allM$AIC, allM$model, mean))
tapply(allM$AIC, allM$model, sd)

library(nlme)
modelDiffs <- lme(AIC ~ model, random = ~1 | Subject, data=allM)
#library(lme4)
#modelDiffs <- lmer(AIC ~ model + (1 | Subject), data=allM)
anova(modelDiffs)
summary(modelDiffs)
library(multcomp)
summary(glht(modelDiffs, linfct=mcp(model="Tukey")))

png("Subject_AICdiff_byModel.png", width=8, height=6, units="in", res=300)
ggplot(allM, aes(x=Subject, y=AICdiff, color=model)) + geom_jitter(size=2, position = position_jitter(width = .3, height=0)) + xlab("Subject") + ylab("AIC difference (from best)") +
    theme(axis.text.x=element_text(angle=90)) + scale_color_brewer(palette="Dark2") + coord_flip() + theme_bw(base_size=14)
dev.off()

png("Subject_SSEdiff_byModel.png", width=8, height=6, units="in", res=300)
ggplot(allM, aes(x=Subject, y=AICdiff, color=model)) + geom_jitter(size=2, position = position_jitter(width = .3, height=0)) + xlab("Subject") + ylab("SSE difference (from best)") +
    theme(axis.text.x=element_text(angle=90)) + scale_color_brewer(palette="Dark2") + coord_flip() + theme_bw(base_size=14)
dev.off()



##look at how allowing epsilon to be negative improves fits
noemo <- gdata::drop.levels(subset(allM, model %in% c("noemo", "noemosticky")))
noemo <- ddply(noemo, .(Subject), function(subdf) {
    negeps <- subset(subdf, model=="noemosticky")
    poseps <- subset(subdf, model=="noemo")

    data.frame(Subject=negeps$Subject[1L],
               epsChange=(poseps$explore - negeps$explore),
               AICchange=(poseps$AIC - negeps$AIC),
               stickyEps=negeps$explore,
               constrainedEps=poseps$explore)
})

cor.test(~ AICchange + epsChange, noemo)
plot(~ AICchange + epsChange, noemo)

#learningParams <- read.table("../fit_behavior/SubjsSummary_emoexplore.txt", header=TRUE)
learningParams <- read.table("../fit_behavior/SubjsSummary_emoexploresticky.txt", header=TRUE)
behav <- read.xls("clock_questionnaires_n36.xls", sheet="Sheet1", skip=1)
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



##More simple checks and analyses of RTs
##Build big data frame of subjects, trials, RTs, and conditions

tcMats <- list.files(path="../subjects", pattern=".*_tc\\.mat", full.names=TRUE)

##sadly, readMat is very slow!
for (f in tcMats) {
    sdata <- readMat(f)
    sdf <- data.frame(subject=as.vector(sdata$subject[[1L]]),
                      sex=as.vector(sdata$subject[[2L]]),
                      condition=unlist(lapply(sdata$order, "[[", 1), use.names=FALSE),
                      

                      )
    browser()
}

# Some thoughts on the top-level algorithm object
# probably makes sense to have an object for each run of data?
# then have a multi-dimensional list of clock_model objects for subjects x runs?
# or could shove it all into one clock_model object and have $fit specify how to minimize SSE (over all subjects and runs, for each subject, for each run within subject?)
# maybe a subject class would be good... Containing runs, and fit objects per run?



a <- clock_model(RTobs=c(1,2,3), Reward=c(1,2,3))
a$add_params(
    gold=goForGold(min_value=0, max_value=500, init_value=0, cur_value=10),
    K=meanRT(max_value=4000, cur_value=1000)
    )

a$add_params(gold=goForGold(min_value=-1, max_value=1, init_value=0, cur_value=10))

#a$add_params(gold=goForGold(min_value=-1, max_value=1, init_value=0, cur_value=10))
#a$add_params(gold1=goForGold(min_value=-1, max_value=1, init_value=0, cur_value=10), gold2=goForGold(min_value=10, max_value=1, init_value=0, cur_value=10))
a$list_params()
a$get_param_minimum_vector()
a$params$gold$min_value <- 22 
a$params$gold$getRTUpdate()
a$params$gold$workspace$sharedX <- -10
ls(a$workspace) #workspace environment is shared between params and clock_model 
a$workspace$sharedX



a$reorder_params(c("gold2", "gold1", "gold"))
a$reorder_params(c("gold2", "gold"))
a$reorder_params(c("gold2", "gold22"))

x <- a$params$gold
x$getRTUpdate()

a$get_param_current_vector()


RTobs <- rnorm(100, 1000, 100)

#evaluating within environment
n <- new.env()

evalq(
    {
      a <- 2
      b <- 25
      
      if(a==2) { 
        print(paste0("b is ", b))
        a <- 3
      }
    },
    n)


evalq(
    x <- y <- 2,
    n
    )
    
attach(n, pos=1)
x1 <- 1
x2 <- 2

detach(n)



a <- clock_model(RTobs=c(1719, 2896, 3260, 3414, 3425, 3633, 3320, 3414, 3438, 3507), 
    Reward=c(0, 78, 87, 92, 88, 0, 89, 95, 89, 0))

#simple fit with just mean RT
a$add_params(K=meanRT(max_value=4000, cur_value=1000))
a$predict()
a$fit()
mean(RTobs[2:length(RTobs)]) #expected value of K: first trial does not contribute to cost

a <- clock_model(RTobs=c(1719, 2896, 3260, 3414, 3425, 3633, 3320, 3414, 3438, 3507), 
    Reward=c(0, 78, 87, 92, 88, 0, 89, 95, 89, 0))

a$add_params(
    K=meanRT(max_value=4000, cur_value=1000),
    gold=goForGold(min_value=0, max_value=500, init_value=0, cur_value=10),
    art1=autocorrPrevRT(),
    g=go(),
    n=noGo(),
    m=meanSlowFast(),
    e=exploreBeta()
)

#check parameterization
a$list_params()
a$fit()

#look at decomposition of prediction by parameter
predMat <- do.call(data.frame, lapply(a$params, "[[", "pred_contrib"))
predMat$RTpred <- apply(predMat, 1, sum)
print(predMat)

#pilot data that MF had initially fit. Use to test whether my estimates are close
pilot1001 <- read.table("/Users/michael/CogEmoFaceReward/subjects/pilot/1001_tc.txt", header=TRUE)

p1001 <- clock_model(RTobs=pilot1001$RT, Reward=pilot1001$ScoreInc)
p1001$add_params(
    K=meanRT(max_value=4000, cur_value=1000),
    gold=goForGold(min_value=0, max_value=500, init_value=0, cur_value=10),
    art1=autocorrPrevRT(),
    g=go(),
    n=noGo(),
    m=meanSlowFast(),
    e=exploreBeta()
)

p1001$list_params()
p1001$fit()


RTobs=c(1719, 2896, 3260, 3414, 3425, 3633, 3320, 3414, 3438, 3507)
RTpred=c(RTobs[1L], rep(1000, length(RTobs) - 1))
sseKOnly <- sum((RTobs - RTpred)^2)
(match <- a$predict() - sseKOnly)

#gold=goForGold(min_value=0, max_value=500, init_value=0, cur_value=10, bestRT=a$w$avg_RT)
#gold$getRTUpdate()

gold=goForGold(min_value=0, max_value=500, init_value=0, cur_value=10, bestRT=10)
art1=autocorrPrevRT(min_value=0, cur_value=0.2)
g=go()

f <- function (x, a) (x - a)^2
xmin <- optimize(f, c(0, 1), tol = 0.0001, a = 1/3)
xmin


#try out creation of clockSubject with runs
s <- clockdata_subject(subject_ID="006_mb", csv_file="/Users/michael/Dropbox/Hallquist_K01/Data/fMRI/006mb_05Nov2013/fMRIEmoClock/fMRIEmoClock_6_tc_tcExport.csv")
s$import_runs()

#is the basic mean RT fit working across all blocks?
x <- read.csv("/Users/michael/Dropbox/Hallquist_K01/Data/fMRI/006mb_05Nov2013/fMRIEmoClock/fMRIEmoClock_6_tc_tcExport.csv", header=TRUE)
library(plyr)
ms <- ddply(x, .(run), function(subdf) {
      return(data.frame(m=mean(subdf$rt[2:nrow(subdf)])))
    })
print(mean(ms$m))

a <- clock_model()

a$add_params(K=meanRT(max_value=4000, cur_value=1000))

Rprof("profile1.out")

a$fit(toFit=s)

Rprof(NULL)
print(a$get_param_current_vector())
#okay, this does match. so, the fit across runs is apparently basically working

library(profr)
ggplot.profr(parse_rprof("profile1.out"))
summaryRprof("profile1.out")#, lines = "show")

a <- clock_model()

a$add_params(
    K=meanRT(max_value=4000),
    gold=goForGold(),
    art1=autocorrPrevRT(),
    g=go(),
    n=noGo()
    m=meanSlowFast()
    e=exploreBeta()
)
#run fit
#a$fit(toFit=s$runs[[1]])
Rprof("profile1.out")
system.time(a$fit(toFit=s$runs[[2]]))
Rprof(NULL)

print(a$get_param_current_vector())

summaryRprof("profile1.out")#, lines = "show")

#try to figure out a match with MF fits
#pilot: 1000
library(fitclock)
s1000 <- clockdata_subject(subject_ID="1000_pilot", data="/Users/michael/CogEmoFaceReward/subjects/pilot/1000_tc_tcExport.csv")
#s1000$import_runs()

library(compiler)
enableJIT(3) #byte compile everything for faster overall run (~10% speedup)w
atest <- clock_model(clock_data=s1000)
atest$add_params(
    K=meanRT(max_value=4000),
    art1=autocorrPrevRT(),
    gold=goForGold(),
    g=go(),
    n=noGo(),
    m=meanSlowFast(),
    e=exploreBeta(),
    s=stickyChoice(init_value=c(weight=0.4, decay=0.2), by="run_condition")
)

atest$set_data(s1000)

f <- atest$fit()

f$AIC


atest <- clock_model()
atest$add_params(
    K=meanRT(max_value=4000),
    art1=autocorrPrevRT(),
    gold=goForGold(),
    g=go(),
    n=noGo(),
    m=meanSlowFast(),
    e=exploreBeta(by="run_condition")
)

f_emoexplore <- atest$fit(toFit=s1000)

allF <- lapply(s1000$runs, function(r) {
      atest$fit(toFit=r)
    })

sum(sapply(allF, "[[", "AIC"))

#test sticky choice
atest <- clock_model(clock_data=s1000)
atest$add_params(
    K=meanRT(max_value=4000),
    stickyChoice=stickyChoice(),
    alphaG=go(),
    alphaN=noGo(),
    rho=meanSlowFast(),
    epsilonBeta=exploreBeta(min_value=-100000) #allow negative epsilon
)
f <- atest$fit(toFit=s1000, random_starts=NULL)

atest$params$K$cur_value <- c(K=317.5653)
atest$params$alphaG$cur_value <- c(alphaG=.1151)
atest$params$alphaN$cur_value <-  c(alphaN=.0882)
atest$params$epsilonBeta$cur_value <- c(epsilonBeta=2016.7350)
atest$params$rho$cur_value <- c(rho=476.8464)
atest$params$stickyChoice$cur_value <- c(stickyWeight=.7709, stickyDecay=.0856)

atest$predict(updateFields=TRUE)



#test whether we obtain identical SSE using fitted values from MATLAB


times <- list()

#testing parameter variation by condition
atest <- clock_model()
atest$add_params(
    meanRT(max_value=4000),
    autocorrPrevRT(),
    goForGold(),
    go(),
    noGo(),
    meanSlowFast(),
    exploreBeta()
)

atest$set_data(s1000)
atest$predict()
f <- atest$fit()

s1000Dat <- read.csv("/Users/michael/CogEmoFaceReward/subjects/pilot/1000_tc_tcExport.csv", header=TRUE)
library(plyr)
ms <- ddply(s1000Dat, .(rewFunc, emotion), function(subdf) {
      return(data.frame(m=mean(subdf$rt[2:nrow(subdf)])))
    })
print(mean(ms$m))


library(microbenchmark)
#microbenchmark( lapply(params, function(p) { getRTUpdate(p, theta, updateFields=updateFields) }), unname(lapply(params, function(p) { getRTUpdate(p, theta, updateFields=updateFields) } )), times=10000)

for (n in 1:10) {
  times[[n]] <- atest$fit(s1000)#[[2]]["elapsed"]
}

#benchmark: 103.3338 seconds average on imac
mean(sapply(lapply(times, "[[", 2), "[", "elapsed")[c(1:8, 10)])



sqrt(f[[1]]$value) #sse
atest$list_params()
lapply(atest$params, function(p) { p$value_history })

#values from MATLAB with K, lambda (autoRT), Go, NoGo
#6Jan2014: Verified exact match with MATLAB for RTpred and SSE
#atest$params[["K"]]$cur_value <- 335.777
#atest$params[["art1"]]$cur_value <- 0.84477
#atest$params[["g"]]$cur_value <- 0.13896
#atest$params[["n"]]$cur_value <- 0.16138

#values from MATLAB with K, lambda (autoRT), Go, NoGo, and scale/nu (gold)
#have now verified that scale parameter works as expected when manually setting parameters, as here.
atest$params[["K"]]$cur_value <- 792.565
atest$params[["art1"]]$cur_value <- 0.62524
atest$params[["g"]]$cur_value <- 0.05058
atest$params[["n"]]$cur_value <- 0.08567
atest$params[["gold"]]$cur_value <- 0.26445


#values from MATLAB for full model

atest$params$K$cur_value <- c(K=809.8756)
atest$params$alphaG$cur_value <- c(alphaG=0.06214)
atest$params$alphaN$cur_value <-  c(alphaN=0.11034)
atest$params$epsilonBeta$cur_value <- c(epsilonBeta=2162.829)
atest$params$rho$cur_value <- c(rho=192.7773)
atest$params$scale$cur_value <- c(scale=0.27036)
atest$params$lambda$cur_value <- c(lambda=0.60583)


#atest$params[["K"]]$cur_value <- 809.8756
#atest$params[["gold"]]$cur_value <- 0.27036
#atest$params[["art1"]]$cur_value <- 0.60583
#atest$params[["g"]]$cur_value <- 0.06214
#atest$params[["n"]]$cur_value <- 0.11034
#atest$params[["m"]]$cur_value <- 192.7773
#atest$params[["e"]]$cur_value <- 2162.829

atest$list_params()
atest$clockData <- s1000
sse <- atest$predict(updateFields=TRUE)
sqrt(sse)

#look at predicted and observed RTs
RTobs <- lapply(s1000$runs, function(r) { r$RTobs })
RTpred <- lapply(s1000$runs, function(r) { r$w$RTpred })

atest$params[]

#s1000$plotRTs()

#try fitting using parameters from MATLAB. Do we recover identical SSE?
#Nope



#trying out a real fMRI subject
library(fitclock)

#setup subject
jh <- clockdata_subject(subject_ID="008_jh", dataset=clocksubject_fMRI_008jh)

#setup model to fit RT differences
expDiff_model <- clock_model(fit_RT_diffs=TRUE, smooth=5)
expDiff_model$add_params(
    meanRT(max_value=4000),
    autocorrPrevRT(),
    goForGold(),
    go(),
    noGo(),
    meanSlowFast(),
    exploreBeta()
)

#tell model which dataset to use
expDiff_model$set_data(jh)

#test the incremental contribution of each parameter to AIC (fit)
incr_fit <- expDiff_model$incremental_fit(njobs=6)

png("Incremental Fit Example.png", width=9, height=6, units="in", res=300)
incr_fit$AICplot
dev.off()

#vector of AIC values
sapply(incr_fit$incremental_fits, "[[", "AIC")

#fit full model, using 5 random starts and choosing the best fit
f <- expDiff_model$fit(random_starts=5)

#d <- f$build_design_matrix(regressors=c("button_press", "rel_uncertainty"), event_onsets=c("rt", "clock_onset"), durations=c(0, "rt"), baselineCoefOrder=2)
#d <- f$build_design_matrix(regressors=c("rpe_pos", "rel_uncertainty"), event_onsets=c("feedback_onset", "clock_onset"), durations=c("feedback_duration", "rt"))

#design matrix matching Badre et al. 2012 Neuron
d <- f$build_design_matrix(regressors=c("mean_uncertainty", "rel_uncertainty", "rpe_pos", "rpe_neg", "rt"), 
    event_onsets=c("clock_onset", "clock_onset", "feedback_onset", "feedback_onset", "feedback_onset"), 
    durations=c("rt", "rt", "feedback_duration", "feedback_duration", 0), baselineCoefOrder=2, writeTimingFiles="AFNI")

library(ggplot2)
gmat <- lapply(d$design.convolve, function(r) {
      r$vol <- 1:nrow(r)
      r.melt <- reshape2::melt(r, id.vars="vol")
      print(ggplot(r.melt, aes(x=vol, y=value)) + geom_line() + facet_grid(variable~., scales="free_y"))
    })

corstarsl(d$design.convolve[[1]])

#contingencies
sapply(jh$runs, "[[", "rew_function")

#collinearity of convolved data
d$collin.convolve




#####
#fit the heck out of 008JH (alternative models and visualizations)
library(fitclock)
library(parallel)
#setup subject
jh <- clockdata_subject(subject_ID="008_jh", dataset=clocksubject_fMRI_008jh)

#basic TC explore
##pos epsilon constraint

exp_model <- clock_model()
exp_model$add_params(
    meanRT(max_value=4000),
    autocorrPrevRT(),
    goForGold(),
    go(),
    noGo(),
    meanSlowFast(),
    exploreBeta(min_value=0.0) #positive epsilon
)

#tell model which dataset to use
exp_model$set_data(jh)

#test the incremental contribution of each parameter to AIC (fit)
incr_fit <- exp_model$incremental_fit(toFit=jh, njobs=6)

#vector of AIC values
sapply(incr_fit$incremental_fits, "[[", "AIC")

#fit full model, using 5 random starts and choosing the best fit
f <- exp_model$fit(toFit=jh, random_starts=NULL)

#design matrix matching Badre et al. 2012 Neuron
d <- f$build_design_matrix(regressors=c("mean_uncertainty", "rel_uncertainty", "rpe_pos", "rpe_neg", "rt"), 
    event_onsets=c("clock_onset", "clock_onset", "feedback_onset", "feedback_onset", "feedback_onset"), 
    durations=c("rt", "rt", "feedback_duration", "feedback_duration", 0), baselineCoefOrder=2, writeTimingFiles="AFNI",
    runVolumes=c(223,273,280,244,324,228,282,310))

#simpler model
setwd("~/")
#EV, clock onset, feedback_onset, PE+, PE-
d <- f$build_design_matrix(regressors=c("clock", "feedback", "ev", "rpe_neg", "rpe_pos"), 
    event_onsets=c("clock_onset", "feedback_onset", "clock_onset", "feedback_onset", "feedback_onset"), 
    durations=c(0, 0, "clock_duration", "feedback_duration", "feedback_duration"), baselineCoefOrder=2, writeTimingFiles="AFNI",
    runVolumes=c(223,273,280,244,324,228,282,310))


d <- f$build_design_matrix(regressors=c("clock", "feedback", "ev", "rpe_neg", "rpe_pos"), 
    event_onsets=c("clock_onset", "feedback_onset", "clock_onset", "feedback_onset", "feedback_onset"), 
    durations=c(0, 0, "clock_duration", "feedback_duration", "feedback_duration"), baselineCoefOrder=2, writeTimingFiles="FSL",
    runVolumes=c(223,273,280,244,324,228,282,310))



##unique fits per run
runFits <- mclapply(jh$runs, mc.cores=6, FUN=function(r) {
      exp_model$fit(toFit=r)
    })

#look at total SSE
totalSSE <- sum(sapply(runFits, "[[", "SSE"))
totalParams <- sum(sapply(runFits, "[[", "nparams"))
#AIC, allowing for unique params
AIC <- ntrials*(log(2*pi*(SSE/ntrials))+1) + 2*nparams
sum(sapply(allF, "[[", "AIC"))




#usual explore model


#plot of predicted versus actual RTs
df <- data.frame(rtobs)

library(reshape2)
library(plyr)
run <- 3
run1 <- f$pred_contrib[[run]]
trials <- 50
run1.melt <- melt(run1[,1:trials], value.name="RTcontrib", varnames=c("var", "trial"))
run1.melt <- ddply(run1.melt, .(trial), function(trialdf) {
      trialdf$xmin <- NA
      trialdf$xmax <- NA
      trialdf$ymin <- NA
      trialdf$ymax <- NA
      for (i in 1:nrow(trialdf)) {
        if (i == 1) {
          trialdf$xmin[i] <- 0.0
          trialdf$ymin[i] <- 0.0
          trialdf$ymax[i] <- trialdf$RTcontrib[i]
        } else {
          trialdf$xmin[i] <- trialdf$xmax[i-1] 
          trialdf$ymin[i] <- trialdf$ymax[i-1]
          trialdf$ymax[i] <- trialdf$ymax[i-1] + trialdf$RTcontrib[i]
        }
        trialdf$xmax[i] <- i*(1/nrow(trialdf))
      }
      trialdf

    })
#make sure that variable order matches original data (since parameter order matters)

run1.melt$var <- factor(run1.melt$var, levels=dimnames(run1)[[1L]])
run1.rtobs <- data.frame(trial=1:trials, rtobs=f$RTobs[run,1:trials])
run1.rtpred <- data.frame(trial=1:trials, rtpred=f$RTpred[run,1:trials])

pdf("plottest.pdf", width=24, height=9)
ggplot(run1.melt, aes(x=xmin, ymin=ymin, ymax=ymax, color=var)) + 
    geom_linerange(size=1.2) + geom_segment(aes(x=xmin, y=ymax, xend=xmax, yend=ymax)) + facet_wrap(~trial, scales="free_y") + 
    xlab("") + ylab("RT") + theme(axis.text.x=element_blank()) + geom_hline(data=run1.rtobs, aes(yintercept=rtobs), color="blue") +
    geom_hline(data=run1.rtpred, aes(yintercept=rtpred), color="red")
dev.off()


#try out V model
jh <- clockdata_subject(subject_ID="008_jh", dataset=clocksubject_fMRI_008jh)
vm <- deltavalue_model(clock_data=jh, alphaV=0.3) #N.B. This matches V matrix from full time-clock algorithm fit.
f <- vm$predict(returnFit=TRUE) #SSE for prediction errors at learning rate of 0.1
#V_0p1 <- vm$V #v matrix for learning rate of 0.1
f_1p <- vm$fit() #estimate learning rate as a free parameter
#V_free <- vm$V #v matrix for free parameter

#separate learning rates for rewards versus omissions.
vm <- deltavalue_model(clock_data=jh, alphaV=0.3, betaV=0.3) #N.B. This matches V matrix from full time-clock algorithm fit.
f_2p <- vm$fit() #estimate learning rate as a free parameter

V_0p3 <- vm$V #v matrix for learning rate of 0.1
library(ggplot2); library(reshape2)

m <- melt(V_0p3, varnames=c("Run", "Trial"))
m$Run <- factor(m$Run, levels=c(1:8), labels=paste0(f$run_condition, f$rew_function))
ggplot(m, aes(x=Trial, y=value)) + geom_point() + geom_line() + facet_wrap(~Run)

#simple design matrix for value regressors
setwd("~/") #write timing files to home directory
#EV, clock onset, feedback_onset, PE+, PE-
d <- f$build_design_matrix(regressors=c("clock", "feedback", "ev", "rpe_neg", "rpe_pos"), 
    event_onsets=c("clock_onset", "feedback_onset", "feedback_onset", "feedback_onset", "feedback_onset"), 
    durations=c(0, 0, "feedback_duration", "feedback_duration", "feedback_duration"), baselineCoefOrder=2, writeTimingFiles="AFNI",
    runVolumes=c(223,273,280,244,324,228,282,310))



#006mb
mb <- clockdata_subject(subject_ID="006_mb", dataset="/Users/michael/Dropbox/Hallquist_K01/Data/fMRI/006mb_05Nov2013/fMRIEmoClock/fMRIEmoClock_6_tc_tcExport.csv")
vm <- deltavalue_model(clock_data=mb, alphaV=0.3) #N.B. This matches V matrix from full time-clock algorithm fit.
f <- vm$predict(returnFit=TRUE) #SSE for prediction errors at learning rate of 0.1

#005ai
ai <- clockdata_subject(subject_ID="005_ai", dataset="/Users/michael/Dropbox/Hallquist_K01/Data/fMRI/005ai_06Nov2013/fMRIEmoClock_5_tc_tcExport.csv")
vm <- deltavalue_model(clock_data=ai, alphaV=0.3) #N.B. This matches V matrix from full time-clock algorithm fit.
f <- vm$predict(returnFit=TRUE) #SSE for prediction errors at learning rate of 0.1



#RT plot
m <- melt(f$RTobs, varnames=c("Run", "Trial"))
m$Run <- factor(m$Run, levels=c(1:8), labels=paste0(f$run_condition, f$rew_function))
ggplot(m, aes(x=Trial, y=value)) + geom_point() + geom_line() + facet_wrap(~Run)

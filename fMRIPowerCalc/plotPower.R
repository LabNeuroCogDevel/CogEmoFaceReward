##median RT in behavioral data is 1753ms
trialLength <- 2 #5
nullMin <- 1.0 #feedback period is a forced 1-second NULL
nullMax <- 14.0 #a reasonable upper bound
times <- seq(150,700,by=50)
ntrialsProps <- seq(.25, .65, by=.05)
comb <- expand.grid(t=times, p=ntrialsProps)
comb$n <- ceiling((comb$t * comb$p)/trialLength)

library(plyr)
library(doMC)
registerDoMC(6)
system("rm -f *.log *.mat *.par")
a_ply(.data=comb, .parallel=TRUE, .margins=1, .fun=function(r) {
    system2(command="optseq2", args=paste0("--ntp ", r$t, " --tr 1.0 --psdwin 0 20 1.0 --ev evt1 ", trialLength, " ", r$n,
                " --nkeep 5 --tnullmin ", nullMin, " --tnullmax ", nullMax, " --o clock_t", r$t, "_n", r$n, "_p", r$p,
                " --nsearch 1000 --polyfit 2 --sum summary_t", r$t, "_n", r$n, "_p", r$p, ".log"), stdout=NULL, stderr=NULL) #show.output.on.console = FALSE)

    ## cat(paste0("optseq2 --ntp ", r$t, " --tr 1.0 --psdwin 0 22 1.0 --ev evt1 ", trialLength, " ", r$n,
    ##             " --nkeep 5 --o clock_t", r$t, "_n", r$n, "_p", r$p,
    ##             " --nsearch 1000 --polyfit 2 --sum summary_t", r$t, "_n", r$n, "_p", r$p, ".log\n"))

})

files = list.files(pattern="summary_.*\\.log")

lens <- c()
effs <- c()
resmat <- c()
for (f in files) {
  s <- scan(f, what="character", sep="\n")
  #runLength <- as.numeric(sub("summary_(\\d+)\\.log", "\\1", f, perl=TRUE))
  runLength <- as.numeric(sub("summary_t(\\d+)_.*\\.log", "\\1", f, perl=TRUE))
  ntrials <- as.numeric(sub("summary_t\\d+_n(\\d+).*\\.log", "\\1", f, perl=TRUE))
  trialprop <- as.numeric(sub("summary_t\\d+_n\\d+_p([\\d\\.]+).*\\.log", "\\1", f, perl=TRUE))
  
  maxEff <- grep("Max Eff Encountered:", s, fixed=TRUE)
  maxEffNum <- as.numeric(sub("^\\s*Max Eff Encountered:\\s+([\\d.]+).*$", "\\1", s[maxEff[1L]], perl=TRUE))
  #lens <- c(lens, runLength)
  #effs <- c(effs, maxEffNum)
  resmat <- rbind(resmat,
                  c(t=runLength, n=ntrials, p=trialprop, eff=maxEffNum))
}

resmat <- as.data.frame(resmat)
resmat$t <- factor(resmat$t)
#resmat$n <- factor(resmat$n)
resmat$p <- factor(as.character(resmat$p*100))

library(ggplot2)
pdf(paste0("powerCurves_oneEvent_trial", trialLength, "s_nullmin", nullMin, "s_nullmax", nullMax, "s.pdf"), width=11, height=8)
ggplot(resmat, aes(x=n, y=eff, group=1)) + geom_point() + geom_line() + facet_wrap(~t, scales="free_x") + #, color=n
  geom_text(aes(x=n, y=eff+.2, group=1, label=p)) + ggtitle(paste0("Estimation efficiency for ", trialLength, "-second single event.\nLabels denote %run devoted to trials\nPaneled by total run length (seconds)")) +
  xlab("Number of trials") + ylab("Efficiency")
dev.off()

#plot(lens, effs, main="Efficiency at differing run lengths with 40 5-second trials", type="b")

##back of envelope calculations for expected run length
##we will convolve HRF with boxcar of length RT
##thus, responses are expected to be between 1.5 and 3 seconds (occasionally 0 - 4 seconds)
##the feedback phase is a kind of "rest" trial since we do not expect neural computation for the clock per se
##to be isomorphic with computations for feedback.
##rather, PEs would be aligned with feedback, whereas exploration, Go, and NoGo would likely align with RT-convolved clock
##Thus, optimize to have a minimum "NULL" length of 1 second, since this is feedback, not strategy.

##if we were to consider the whole thing (clock + feedback) a compound trial with no minimum ITI,
##based on power curves, we would want to keep trial proportions in the 50-60% range, if possible, with a peak around 55%

##but if we take the stance of a 2-second response, on average, then with a 1-second minimum NULL to account for
##feedback phase, then the optimal percentage drops to 37% (between 35 and 40%)


##mean response times per subject will vary around a mean of 1.5 - 2.7 seconds
##thus, with a 1s ISI + feedback phase, trials will be 2.5-3.7s in length
##we do *not* want to optimize for max-length trials (4s response + 1s feedback), as this will be rare

##optimize the distribution of ITIs based on an estimated 2s response time (since this is closest to the mean)
##Thus, total length of rest will be based on this calculation
##As a result, proportion and distribution of rests will become suboptimal as RT drifts away from 2s

ntrials <- c(40, 45, 47, 48, 50, 55, 60, 65)
optimalTrialP <- .38
trialLength <- c(1,2,3,4)
feedbackLength <- 1 #how long feedback + ITI lasts
estAvgLength <- 2 #do not include feedback
deadTime <- 20 #8s pre task and 12s after last trial
nruns <- 8 #(fear + happy + scrambled)*(IEV + DEV) + scrambled*(CEV + CEVR)
TR <- 1.0

combTest <- expand.grid(len=trialLength, n=ntrials)
combTest$estRestTime <- ceiling((combTest$n*estAvgLength) * ((1-optimalTrialP)/optimalTrialP))
combTest$estTaskTime <- ceiling((combTest$n*combTest$len)) #* ((1-optimalTrialP)/optimalTrialP))

##combTest$actualTaskPct <- with(combTest, (n*(len+feedbackLength)) / (estRestTime + n*(len+feedbackLength)))
##combTest$runMins <- with(combTest, round((n*(len+feedbackLength) + estRestTime + deadTime)/60, 3))

combTest$actualTaskPct <- with(combTest, (n*(len)) / (estRestTime + n*(len)))
combTest$runMins <- with(combTest, round((n*(len) + estRestTime + deadTime)/60, 3))

combTest$runTRs <- with(combTest, ceiling((n*len + estRestTime + deadTime)/TR))
combTest$totalExpLength <- combTest$runMins*nruns

print(combTest)

## for a 1-second minimum ITI, n=50 results in total run length of 37.9 minutes on task
## 284 TRs (seconds) is optimal, including 8 seconds pre-task and 12 seconds post-task
## So, for optseq optimization, use 264 TRs (don't include dead time in task/rest balance)

##11-sec max ITI yields good efficiency while not making it too aversive to subject

##optseq2 --ntp 264 --tr 1.0 --psdwin 0 21 1 --ev evt1 2 50 --nkeep 8 --o clock_t264_p38 --nsearch 10000 --polyfit 2 --sum summary_t264_p38.log --tnullmax 11 --tnullmin 1

###THIS IS OUTDATED BELOW BECAUSE DID NOT ASSUME 1s FEEDBACK EVENTS
##based on the information above, looks like n=50 runs are optimal:
##55% task for a total run length of 4.9 minutes assuming a mean RT of 2s.
##Total experiment length: 39 minutes
##If participant has a mean RT of 3s, would be extended to 45.74 minutes

##upper bound on number of volumes per run is: 350 (this would correspond to an avg. RT of ~3.15s,
##which is longer than any of our behavioral data)

##we expect to terminate runs around 293 volumes for an avg RT of 2s

##probably a lag of 45 seconds between runs * 8 runs is 6 more minutes
##so expected total length of clock task is 45 minutes

##optseq2 --ntp 293 --tr 1.0 --psdwin 0 21 1 --ev evt1 2 50 --nkeep 8 --o clock_t293_p55 --nsearch 10000 --polyfit 2 --sum summary_t293_p55.log --tnullmax 12 --tnullmin 1

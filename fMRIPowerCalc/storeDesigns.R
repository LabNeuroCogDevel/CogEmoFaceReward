##file to convert outputs of optseq2 into reasonable timing files for MATLAB
rawOpt <- list.files(pattern="clock_t264_p38-.*\\.par")

stopifnot(length(rawOpt) > 0L)

fFile <- read.table(rawOpt[1L], header=FALSE)
nTrials <- nrow(subset(fFile, V5=="evt1"))

finalRest <- 12

#create a order x ITI matrix
ITImat <- matrix(data=NA_real_, nrow=length(rawOpt), ncol=nTrials)

for (f in 1:length(rawOpt)) {
    p <- read.table(rawOpt[f], header=FALSE)
    
    names(p) <- c("onset", "evtCode", "duration", "dummy", "evtName")

    p <- subset(p, evtName == "NULL")

    ##subtract 1 second because feedback event (which is NULL wrt clock) takes 1s
    ##essentially optseq2 was run with a min NULL of 1s, which corresponds to feedback
    ITImat[f,] <- p$duration - 1.0

}

##Always use 12-second NULL at the end of the task
ITImat[,ncol(ITImat)] <- finalRest


library(R.matlab)
writeMat(con="../fMRIOptITIs_284s_38pct.mat", itimat=ITImat)

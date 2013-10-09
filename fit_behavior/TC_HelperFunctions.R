loadTCBehavior <- function(fname) {
	stopifnot(file.exists(fname))
	components <- strsplit(fname, split="[\\/]")[[1]]
	
	subject <- sub("^[^\\d]*(\\d+)_tc\\.txt$", "\\1", components[length(components)], perl=TRUE)
	
	sdata <- read.table(fname, header=TRUE, comment.char="#")
	sdata$Null <- NULL #delete dummy column
	sdata$Subject <- factor(subject)
	
	##rearrange column headers for readability
	row.names(sdata) <- NULL
	sdata <- sdata[,c("Subject", "Run", "Block", "Trial", "Func", "Emotion", "Mag", "Freq", "ScoreInc", "EV", "RT", "Image")]
	
	##verify that each subject completed 42 trials for each Func x Emotion condition
	##with(allData, table(LunaID, Func, Emotion))
	
	##compute trial within a block
	sdata$TrialRel <- unlist(lapply(split(sdata, f=list(sdata$Func, sdata$Emotion)), function(l) { return(1:nrow(l)) } ))
	
	return(sdata)
}

setwd("~/CogEmoFaceReward/subjects")
x <- loadTCBehavior("10152_tc.txt")


avgRT <- mean(x$RT)
TC_Alg(x$RT, x$ScoreInc, params, priors, avg_RT, rewFunc, emo, model, 
        distType="beta", generative=FALSE, stickyChoice=FALSE) {
    

#The actual scoring function. Taken from Frank et al
getClockRew <- function(RT, contingency) {
    #Values for Reward computation - constant for all phases
    k               <- 37
    Shift           <- 700
    rt_extended     <- 7000
    DEV_factor      <- 10
    DEV_factor2     <- 1
    sin_factor      <- 0.25
    
    # score response time based on one of 3 functions
    # given in master file (either inc,dec,or const)
    if (contingency == "CEV") {
        Mag = (k*rt_extended)/(rt_extended-(RT+Shift)); 
        Freq = 1-((RT+Shift)/rt_extended);
    } else if (contingency == "DEV") {
        Mag = DEV_factor*log(DEV_factor2*(RT+Shift)); 
        CEV_x = 1-((RT+Shift)/rt_extended);
        IEV_x = CEV_x + (CEV_x*(sin_factor*sin((RT*pi)/5000)));
        Freq = (2*CEV_x)-IEV_x;
    } else if (contingency == "IEV") {
        CEV_x = (k*rt_extended)/(rt_extended-(RT+Shift)); 
        DEV_x = DEV_factor*log(DEV_factor2*(RT+Shift));
        Mag = (2*CEV_x)-(DEV_x);
        CEV_x2 = 1-((RT+Shift)/rt_extended);
        Freq = CEV_x2 + (CEV_x2*(sin_factor*sin((RT*pi)/5000)));
    } else if (contingency == "CEVR") {
        Mag = 1-((RT+Shift)/rt_extended);
        Mag = Mag*200;
        Freq = (k*rt_extended)/(rt_extended-(RT+Shift)) ;
        Freq = Freq/200;        
    } else {
        Mag = 0
        Score = 0
        warning("Unknown contingency: ", contingency)
    }
    
    return(c(Mag=Mag, Freq=Freq))
}

#par(mfrow=c(3,1))
#
#time <- seq(0,5000)
#rew <- t(sapply(time, getClockRew, contingency="IEV"))
#plot(time, rew[,"Mag"], type="l", main="IEV Magnitude")
#plot(time, rew[,"Freq"], type="l", main="IEV Frequency")
#plot(time, apply(rew, 1, prod), type="l", main="IEV EV")


####
## FOR BEHAVIORAL PILOT
# randomize events to be inserted into eprime lists
#
#   |------------------------------------------|
#   |           42  presentations              |   x {inc,dec,cev,cevr} 
#   |                    of                    |   x {happy,fear,scram)}
#   |                                          |   x 1
#   | facenum_{emotion} | ITI | {reward} | ISI |  ______________________
#   |------------------------------------------|   504 presentations across 12 blocks
#
#
# saved to FaceITI.csv | facenum,ITI,ISI,emotion,reward
#
####
#
# * dataframe 'root' can be exported to xls and copied directly into the list 'root' in eprime, or ignored and read from blocklist
# * blocklist is saved as a csv to be imported by eprime (or better presentation software )
#   - lists each presentations face number, ITIs, emotion, and reward (with a header)
#
#   12 blocks: 4 reward functions (CEV, CEVR, IEV, DEV) and 3 emotions (happy, fear, scram). Blocks are 42 trials
#    each presentation can have a unique ITI pair
#    within each (42 trials long) block, faces should appear in a different order and each repeats at least once
#
####

randUnifConstrain <- function(len, vals, targetMean) {
   sampVec <- sample(vals, len, replace=TRUE)
   while (mean(sampVec) != targetMean) sampVec <- sample(vals, len, replace=TRUE)
   return(sampVec)
}

numFaces   <- 21  # length(glob('faces/happy_*png')) # number of distinct faces
numPresent <- 42  # number of presentations of a reward_emotion combo
numRep     <- 1   # how many times will we show the same reward_emotion combo
emotions   <- c("happy","fear","scram") #emotions   <- c("happy","neutral","fear")
rewardFuns <- c("DEV", "IEV", "CEVR", "CEV") #rewardFuns <- c("DEV","IEV","CEV", "CEVR")
numTrials  <- length(emotions)*length(rewardFuns)*numRep # 18 # how many reward_emotion combos total
halfTrial  <- floor(numTrials/2)

#force no repeats of emotions or reward conditions
conditionGrid <- expand.grid(emotion=emotions, reward=rewardFuns, occurrence=1:numRep)

#a brute force permutation approach (below) doesn't work well -- unlikely to identify matching sets
#manual order for 4 x 3 grid to force no repeats and relative balance of first 6 versus second 6 blocks
conditionGrid <- conditionGrid[c(11,   #fear CEV                                 
                                 1,    #happy DEV
                                 5,    #fear IEV
                                 9,    #scram CEVR
                                 10,   #happy CEV
                                 6,    #scram IEV                                
                                 7,    #happy CEVR
                                 3,    #scram DEV
                                 8,    #fear CEVR
                                 4,    #happy IEV
                                 2,    #fear DEV
                                 12    #scram CEV
                                 ),]  


#conditionGrid <- conditionGrid[
#                               c(1, 5, 9, 2, 4, 3, 8, 6, 7,
#                                 11, 13, 12, 16, 14, 18, 10, 15, 17),]


#conditionGrid <- conditionGrid[  c( sample(1:halfTrial,halfTrial),
#                                    sample((halfTrial+1):numTrials,(numTrials-halfTrial)) )
#                              , ]

 ## emotion reward occurrence
 ##   happy    DEV          1
 ##    fear    IEV          1
 ##   scram   CEVR          1
 ##    fear    DEV          1
 ##   happy    IEV          1
 ##   scram    DEV          1
 ##    fear   CEVR          1
 ##   scram    IEV          1
 ##   happy   CEVR          1
 ##    fear    DEV          2
 ##   happy    IEV          2
 ##   scram    DEV          2
 ##   happy   CEVR          2
 ##    fear    IEV          2
 ##   scram   CEVR          2
 ##   happy    DEV          2
 ##   scram    IEV          2
 ##    fear   CEVR          2

## adjacentDupes <- TRUE
## while(adjacentDupes) {
##   conditionGrid <- conditionGrid[sample(1:nrow(conditionGrid), nrow(conditionGrid)),]

##   prevEmotion <- rep(NA, nrow(conditionGrid))
##   prevReward <- rep(NA, nrow(conditionGrid))
##   for (i in 1:nrow(conditionGrid)) {
##     prevEmotion[i] <- ifelse(i==1, NA, conditionGrid$emotion[i-1]==conditionGrid$emotion[i])
##     prevReward[i] <- ifelse(i==1, NA, conditionGrid$reward[i-1]==conditionGrid$reward[i])
##   }

##   #just enforce no adjacent face condition
##   #hasDupes <- any(prevEmotion[-1])

##   #enforce no dupes on emotion and reward
##   hasDupes <- any(prevEmotion[-1] | prevReward[-1])
##   if (!hasDupes) adjacentDupes <- FALSE
## }
    
#root <- data.frame(
#          emotion=sample(   rep( emotions,   numTrials/length(emotions)), numTrials ),
#           reward=sample(   rep( rewardFuns, numTrials/length(rewardFuns)), numTrials )
#        )

ITI.min <- 1000; ITI.max <- 2000; ITI.mean <- 1500;
ISI.min <- 400;  ISI.max <- 1500; ISI.mean <- 900;

faceRepeats=numTrials*numPresent/numFaces
blocklist <- data.frame(
              facenum = as.vector(sapply(1:faceRepeats, function(x){ sample(1:numFaces,numFaces)  })), 
              ITI    = as.vector(sapply(1:numTrials, function(x){ randUnifConstrain(numPresent,seq(ITI.min,ITI.max,by=100), ITI.mean)   })),
              ISI    = as.vector(sapply(1:numTrials, function(x){ randUnifConstrain(numPresent,seq(ISI.min,ISI.max,by=100), ISI.mean)   })),
              block   = rep(1:numTrials, each=numPresent),
              emotion = rep(conditionGrid$emotion, each=numPresent),
              reward  = rep(conditionGrid$reward , each=numPresent)
         )

write.table(blocklist,file="FaceITI.csv",row.names=FALSE,sep=",")

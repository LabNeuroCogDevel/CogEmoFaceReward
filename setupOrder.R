####
# randomize events to be inserted into eprime lists
#
#   |------------------------------------------|
#   |           40  presentations              |   x {inc,dec,const} 
#   |                    of                    |   x {happy,neutral,fear}
#   |                                          |   x 2
#   | facenum_{emotion} | ITI | {reward} | ISI |  ______________________
#   |------------------------------------------|   720 presentations across 18 trials
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
#   18 trials (ea. combo of reward(3) x emotion(3) done twice) of 40 presentations with 20 faces
#   each combo has 80 presentations to the screen
#    each presentation can have a unique ITI pair
#    within each (40 presentations long) trial, faces should appear in a different order and each repeats once 
#
####

randUnifConstrain <- function(len, vals, targetMean) {
   sampVec <- sample(vals, len, replace=TRUE)
   while (mean(sampVec) != targetMean) sampVec <- sample(vals, len, replace=TRUE)
   return(sampVec)
}

numFaces   <- 20  # length(glob('faces/happy_*png')) # number of distinct faces
numPresent <- 40  # number of presentations of a reward_emotion combo
numRep     <- 2   # how many times will we show the same reward_emotion combo
emotions   <- c("happy","neutral","scram") #emotions   <- c("happy","neutral","fear")
rewardFuns <- c("CEV","DEV","IEV","CEVR") #rewardFuns <- c("constant","increase","decrease")
numTrials  <- length(emotions)*length(rewardFuns)*numRep # 18 # how many reward_emotion combos total

root <- data.frame(
          emotion=sample(   rep( emotions,   numTrials/length(emotions)), numTrials ),
           reward=sample(   rep( rewardFuns, numTrials/length(rewardFuns)), numTrials )
        )

ITI.min <- 1000; ITI.max <- 2000; ITI.mean <- 1500;
ISI.min <- 400;  ISI.max <- 1500; ISI.mean <- 900;
faceRepeats=numTrials*numPresent/numFaces
blocklist <- data.frame(
              facenum = as.vector(sapply(1:faceRepeats, function(x){ sample(1:numFaces,numFaces)  })), 
              ITI    = as.vector(sapply(1:numTrials, function(x){ randUnifConstrain(numPresent,seq(ITI.min,ITI.max,by=100),ITI.mean)   })),
              ISI    = as.vector(sapply(1:numTrials, function(x){ randUnifConstrain(numPresent,seq(ISI.min,ISI.max,by=100),ISI.mean)   })),
              block   = rep(1:numTrials, each=numPresent),
              emotion = rep(root$emotion, each=numPresent),
              reward  = rep(root$reward , each=numPresent)
         )
write.table(blocklist,file="FaceITI.csv",row.names=FALSE,sep=",")

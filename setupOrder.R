####
# randomize events to be inserted into eprime lists
#
#   |--------------------------------------------|
#   |            40  presentations               |   x {inc,dec,const} 
#   |                    of                      |   x {happy,neutral,fear}
#   |                                            |   x 2
#   | facenum_{emotion} | ITI1 | {reward} | ITI2 |  ______________________
#   |--------------------------------------------|   720 presentations across 18 trials
#
#
# saved to FaceITI.csv | facenum,ITI1,ITI2,emotion,reward
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
emotions   <- c("happy","neutral","fear")
rewardFuns <- c("constant","increase","decrease")
numTrials  <- length(emotions)*length(rewardFuns)*numRep # 18 # how many reward_emotion combos total

root <- data.frame(
          emotion=sample(   rep( emotions,   numTrials/3), numTrials ),
           reward=sample(   rep( rewardFuns, numTrials/3), numTrials )
        )

ITI1.min <- 400;  ITI1.max <- 1500; ITI1.mean <- 900;
ITI2.min <- 1000; ITI2.max <- 2000; ITI2.mean <- 1500;
faceRepeats=numTrials*numPresent/numFaces
blocklist <- data.frame(
              facenum = as.vector(sapply(1:faceRepeats, function(x){ sample(1:numFaces,numFaces)  })), 
              ITI1 =    as.vector(sapply(1:numTrials, function(x){ randUnifConstrain(numPresent,seq(ITI1.min,ITI1.max,by=100),ITI1.mean)   })),
              ITI2 =    as.vector(sapply(1:numTrials, function(x){ randUnifConstrain(numPresent,seq(ITI2.min,ITI2.max,by=100),ITI2.mean)   })),
              emotion = rep(root$emotion, each=numPresent),
              reward  = rep(root$reward , each=numPresent)
         )
write.table(blocklist,file="FaceITI.csv",row.names=FALSE,sep=",")

####
## FOR Behavioral MULTIMODAL
## randomize events to be inserted into PsychToolbox lists
##
##   |------------------------------------------|
##   |           50  presentations              |   x {iev,dev} 
##   |                    of                    |   x {happy,fear,scram)}
##   |                                          |   + scram_CEV + scram_CEVR
##   | facenum_{emotion} | {reward}             |  ______________________
##   |------------------------------------------|   400 presentations across 8 blocks
##
##
## saved to FaceBehavOrder.csv | facenum,emotion,reward
##
## within each block, faces should appear in a different order and each repeats once
##
####

numFaces    <- 25 # number of distinct faces
numTrials   <- 50  # number of presentations of a reward_emotion combo
numFaceRepeats  <- ceiling(numTrials/numFaces)  # how many times we use the same face within block
emotions   <- c("happy","fear","scram")
rewardFuns <- c("CEV", "CEVR")

#force no repeats of emotions or reward conditions
conditionGrid <- expand.grid(emotion=emotions, reward=rewardFuns)

#to avoid total boredom, manually add IEV happy and DEV fear blocks (most discrepancy between contingency and face)
conditionGrid <- rbind(conditionGrid, data.frame(emotion=c("happy", "fear"), reward=c("IEV", "DEV")))

numRuns <- nrow(conditionGrid)

#use a brute force permutation to order 8 runs with no repeats of emotion or contingency
adjacentDupes <- TRUE
while(adjacentDupes) {
  conditionGrid <- conditionGrid[sample(1:nrow(conditionGrid), nrow(conditionGrid)),]

  prevEmotion <- rep(NA, nrow(conditionGrid))
  prevReward <- rep(NA, nrow(conditionGrid))
  for (i in 1:nrow(conditionGrid)) {
    prevEmotion[i] <- ifelse(i==1, NA, conditionGrid$emotion[i-1]==conditionGrid$emotion[i])
    prevReward[i] <- ifelse(i==1, NA, conditionGrid$reward[i-1]==conditionGrid$reward[i])
  }

  #just enforce no adjacent face condition
  #hasDupes <- any(prevEmotion[-1])

  #enforce no dupes on emotion and reward
  hasDupes <- any(prevEmotion[-1] | prevReward[-1])
  if (!hasDupes) adjacentDupes <- FALSE
}

blocklist <- data.frame(
              facenum = as.vector(sapply(1:(numRuns*numFaceRepeats), function(x){ sample(1:numFaces, numFaces)  })), 
              block   = rep(1:numRuns, each=numTrials),
              emotion = rep(conditionGrid$emotion, each=numTrials),
              reward  = rep(conditionGrid$reward , each=numTrials)
         )

write.table(blocklist,file="FaceBehavOrder.csv",row.names=FALSE,sep=",", quote=FALSE)

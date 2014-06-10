####
## FOR Behavioral follow-up (SPECC)
## randomize events to be inserted into PsychToolbox lists
##
##   |------------------------------------------|
##   |           50  presentations              |   x {iev,dev,cev,cevr} 
##   |                    of                    |   x {scram)}
##   |                                          |   
##   | facenum_{emotion} | {reward}             |  ______________________
##   |------------------------------------------|   200 presentations across 4 blocks
##
##
## saved to FaceBehavOrder_Followup.csv | facenum,emotion,reward
##
## within each block, faces should appear in a different order and each repeats once
##
####

numFaces    <- 25 # number of distinct faces
numTrials   <- 50  # number of presentations of a reward_emotion combo
numFaceRepeats  <- ceiling(numTrials/numFaces)  # how many times we use the same face within block
emotions   <- c("scram")
rewardFuns <- c("CEVR", "IEV", "DEV", "CEV")

#force no repeats of emotions or reward conditions
conditionGrid <- expand.grid(emotion=emotions, reward=rewardFuns)

numRuns <- nrow(conditionGrid)

#expand into a trialwise list with a random ordering of image number
blocklist <- data.frame(
              facenum = as.vector(sapply(1:(numRuns*numFaceRepeats), function(x){ sample(1:numFaces, numFaces)  })), 
              block   = rep(1:numRuns, each=numTrials),
              emotion = rep(conditionGrid$emotion, each=numTrials),
              reward  = rep(conditionGrid$reward , each=numTrials)
         )

write.table(blocklist,file="FaceBehavOrder_Followup.csv",row.names=FALSE,sep=",", quote=FALSE)

####
## FOR MEG
# randomize events to be inserted into eprime lists
# see setupOrder.R for more detail
####

randUnifConstrain <- function(len, vals, targetMean) {
   vals<-c(vals,vals) # so if vals is only one value, we do the right thing
                      # doesn't change randomness if vals is more than one value
   sampVec <- sample(vals, len, replace=TRUE)
   while (mean(sampVec) != targetMean){
     #print(mean(sampVec)-targetMean)
     sampVec <- sample(vals, len, replace=TRUE)
   }
   return(sampVec)
}

# ITI and ISI spacing. NOTE: these are backwards :( ITI is actuall ISI
ITI.min <- 300;  ITI.max <- 300; ITI.mean <- 300;   # constant ITI for MEG
ISI.min <- 1000; ISI.max <- 1500; ISI.mean <- 1250;

numFaces   <- 21  # length(glob('faces/happy_*png')) # number of distinct faces
# number of presentations of a reward_emotion combo
numPresent <- 63  # 42  for behav, multiple of numFaces for easy calculations
numRep     <- 1   # how many times will we show the same reward_emotion combo
emotions   <- c("happy","fear","scram") #emotions   <- c("happy","neutral","fear")
#rewardFuns <- c("DEV", "IEV", "CEVR", "CEV") #rewardFuns <- c("DEV","IEV","CEV", "CEVR")
rewardFuns <- c("DEV", "IEV") # MEG paradigm only has time for two  -- so drop the constant ones
numTrials  <- length(emotions)*length(rewardFuns)*numRep # 18 # how many reward_emotion combos total
numTrials  <- numTrials + 2 # we are going to add CEV and CEVR scram conditions
#halfTrial  <- floor(numTrials/2)

# block for each emotion+face combo
conditionGrid.orig <- expand.grid(emotion=emotions, reward=rewardFuns, occurrence=1:numRep)
# also add in scram+{CEV,CEVR}
levels(conditionGrid.orig$reward)<-c(levels(conditionGrid.orig$rewar),'CEV','CEVR')
conditionGrid.orig <- rbind(conditionGrid.orig,c('scram','CEVR',1),c('scram','CEV',1))

for(i in 1:999) {
  # randomly order blocks
  conditionGrid <- conditionGrid.orig[sample(1:nrow(conditionGrid)),]  
  print(conditionGrid)
  
  faceRepeats=numTrials*numPresent/numFaces
  blocklist <- data.frame(
                facenum = as.vector(sapply(1:faceRepeats, function(x){ sample(1:numFaces,numFaces)  })), 
                ITI    = as.vector(sapply(1:numTrials, function(x){ randUnifConstrain(numPresent,seq(ITI.min,ITI.max,by=100), ITI.mean)   })),
                ISI    = as.vector(sapply(1:numTrials, function(x){ randUnifConstrain(numPresent,seq(ISI.min,ISI.max,by=50), ISI.mean)   })),
                block   = rep(1:numTrials, each=numPresent),
                emotion = rep(conditionGrid$emotion, each=numPresent),
                reward  = rep(conditionGrid$reward , each=numPresent)
           )
  # mkdir -p MEGorder/{un,}used/
  write.table(blocklist,file=sprintf("MEGorder/unused/%03d.csv",i),row.names=FALSE,sep=",")
}
# check:
# for i in MEGorder/unused/*csv; do for j in MEGorder/unused/*csv; do [ $j == $i ] && continue;  diff -q $i $j >/dev/null && echo "$i $j"; done; done 

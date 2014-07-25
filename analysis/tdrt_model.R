#temporal difference RT model
#try on one run
library(fitclock)

exSubj <- clockdata_subject(subject_ID="008", dataset=clocksubject_fMRI_008jh)
run1 <- exSubj$runs[[1L]]

#rts <- run1$RTraw
rts <- round(run1$RTraw/10) #round to 10ms bins
rewards <- run1$Reward

rewards[1L] <- 60 #just for testing

#adapted ludvig code
#Simple Code for CSC/PR/MS TD Model with Actions.

#Model Parameters
numms_basis = 12   #Number of microstimuli per stimulus
numstimuli = 2     #Number of stimuli, including the reward/US (CS is trial onset)
alpha = 0.05       #Step-size
#decay = 0.985     #Memory Trace decay rate
#decay = 0.999     #Memory Trace decay rate
decay = 0.995
gamma = 0.99       #Discount factor
lambda = 0.95      #Eligibility trace decay rate
sigma = 0.04       #Microstimulus width
theta = 0.25       #Action Threshold
upsilon = 0.9      #Action Decay
presence = 0.2     #Presence microstimulus level

# Experiment Code:
# CS2 is used for Overshadowing and Blocking

numtrials = length(rts)    
#numtimesteps = 4000 #1ms bins in a 4000ms trial
numtimesteps = 400 #10ms bins in a 4000ms trial (round RT)

## Initialize Data Vectors:
x = matrix(data=0, nrow=numstimuli, ncol=numms_basis)        #Stimulus representation matrix (nstimuli x nbasis)
w = rep(0, numstimuli*numms_basis)                           #Weight vector (length nstimuli x nbasis)
e = rep(0, numstimuli*numms_basis)                            #Eligibility Traces (length nstimuli x nbasis)

#x = matrix(data=0, nrow=1, ncol=(numms_basis+1)*numstimuli)
#w = matrix(data=0, nrow=1, ncol=(numms_basis+1)*numstimuli)
#e = matrix(data=0, nrow=1, ncol=(numms_basis+1)*numstimuli)

delta = matrix(data=0, nrow=numtrials, ncol=numtimesteps)    #TD Errors
value = matrix(data=0, nrow=numtrials, ncol=numtimesteps)    #Value functions
action = matrix(data=0, nrow=numtrials, ncol=numtimesteps)   #Response Levels
ms = matrix(data=0, nrow=numtimesteps, ncol=numms_basis)     #Microstimulus levels
#maxaction = zeros (1, numtrials);

stimulustime <- matrix(cbind(1, rts), ncol=2)#, dimnames=list(c(), c("onset", "RT"))) #stimulus onset time, here just the US

## MS is the master matrix of basis functions for any given stimulus
## Microstimulus Generation (NIPS2009 Version)
trace = 1
for (timestep in 1:numtimesteps) {
  for (micro in 1:numms_basis) {
    ms[timestep, micro] = trace * ((1/sqrt(2*pi))*exp(-((trace-(micro/numms_basis))^2)/(2*(sigma^2))));    
  }
  trace = trace * decay;
}

#plot microstimuli
library(reshape2)
library(ggplot2)

ms_melt <- melt(ms, varnames=c("timestep", "msnumber"))
ms_melt$msnumber <- factor(ms_melt$msnumber)
ggplot(ms_melt, aes(x=timestep, y=value, color=msnumber)) + geom_line()

##fit data
##Run Simuluation:

rewdeliv <- 0

#matrices for tracking weights and eligibility traces for each timestep of a given trial
wtrial <- matrix(0, nrow=numtimesteps, ncol=numstimuli*numms_basis)
etrial <- matrix(0, nrow=numtimesteps, ncol=numstimuli*numms_basis)

library(animation)
vplot <- list()

#i trials
#t timesteps
#s stimuli
for (i in 1:numtrials) {
  oldvalue = 0;               # Reset on every trial
  #oldreward = 0;
  oldaction = 0;
  e = rep(0, numstimuli*numms_basis) #reset eligibility trace
  rewardtime = rts[i];
  
  for (t in 1:numtimesteps) {
    if (t == rewardtime) {
      rewdeliv <- rewdeliv + 1
      reward = rewards[i]
      cat("rew count: ", rewdeliv, "\n")
    } else {    
      reward = 0
    }
    
    #microstimulus representation of stimulus
    #x is updated for each stimulus at each timestep
    #so, it always represents the MS heights of a given stimulus relative to its onset time: stimulustime[i, s]
    for (s in 1:numstimuli) {
      if (t >= stimulustime[i, s]) {
        #noise = rand*0.02 -.01; (unused)
        noise = 0;
        x [s, ] <- ms[t - stimulustime[i, s] + 1, ] + noise
        #x [((s-1)*numms_basis +1):((s-1)*numms_basis +numms_basis)] = ms[t-stimulustime[i, s]+1,] + noise;     
      } else {
        x [s, ] <- 0
        #x [((s-1)*numms_basis +1):((s-1)*numms_basis +numms_basis)] = 0;
        #x [numstimuli * numms_basis + s] = 0; #this is updating the numms + 1 initial specification of x (19:21 in the 3 stim x 6 basis setup...). Don't get it. -- this is used by presence representation! 
      }
    }

    #if (reward > 0) browser()
    xvec <- as.vector(x) #flatten x for computation of value, w, and e. (x as mat for ease of indexing/representation above)
    value[i, t] = crossprod(xvec,w) #inner product across all stimuli
    
    #Action Selection:
    action[i, t] = upsilon * oldaction + max (0, oldvalue - theta)
    oldaction=action[i, t]
    
    #Learning Algorithm:
    delta[i, t] = reward + (gamma * value[i, t]) - oldvalue #TD Learning
    
    w = w + (alpha * delta[i, t] * e)
    e = xvec + (gamma * lambda * e)
    wtrial[t, ] <- w
    etrial[t, ] <- e
    
    oldvalue = crossprod(xvec,w) #Or oldvalue = value. Difference in which weights are used.
    
    #cat("range of value: ", range(value), "\n")
    #oldreward = reward
  
  } #end timestep loop    
  
  plotdf <- data.frame(t=1:numtimesteps, v=value[i,])
  vplot[[i]] <- ggplot(plotdf, aes(x=t, y=v)) + ylim(-5, 50) + geom_line() + annotate(geom="text", label=rewards[i], x=stimulustime[i,2], y=max((plotdf$v)-min(plotdf$v))/2) + annotate(geom="text", x=350, y=5, label=as.character(i))
  
  #browser() #pause for each trial after walking through timesteps
} #end trial loop


ani.record(reset = TRUE)

for (v in vplot) { 
  print(v)
  ani.record()
}

ani.replay()


  

#%% Real-time plotting:
#    %     maxaction(k) = max(max(action));
#    plot(value (trial, :), 'r');
#hold on;
#plot(delta (trial, :), 'b');
#plot(action (trial, :), 'g');
#hold off;
#drawnow;
#end

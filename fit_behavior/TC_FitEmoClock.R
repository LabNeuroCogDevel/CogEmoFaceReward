#R implementation of time clock algorithm

#wrapper function to determine whether behavior fit improves with additoinal parameters

#core TC algorithm
TC_Alg <- function(RTobs, Reward, params, priors, avg_RT, rewFunc, emo, model) {
  # Time clock R-L algorithm developed by Michael Frank
  #
  #
  # inputs:
  #    RTobs:    vector of observed reaction times
  #    Reward:   vector of obtained rewards (points)
  #    params:   vector of model parameters used to fit data
  #    avg_RT:   scalar of average reaction time (across trials from all blocks)
  #    rewFunc:  reward contingency, 1=CEV; 2=CEVR; 3=DEV; 4=IEV
  #    emo:      emotion, 1=happy; 2=fear; 3=scrambled
  #
  #
  # RTobs is a t x 1 column vector of reaction times for t trials.
  # Reward is a t x 1 column vector of rewards obtained for t trials.
  # params is an 8 x 1 column vector of model parameters to be used to fit behavior.

  numTrials <- length(RTobjs)
  
  RTpred	<- rep(NA_real_, numTrials) #vector of predicted RTs
  V				<- rep(NA_real_, numTrials) #state-value function (expected value)
  Go			<- rep(NA_real_, numTrials)
  NoGo		<- rep(NA_real_, numTrials)
  
  Q <- 0
  Noise <- 0
  dist_type <- "beta"

  mean_s <- 0
  mean_f <- 0
  
  
  #can't predict first choice due to learning so just set it to actual subject RT 
  #and then predict starting trial 2

  RTpred[1L] <- RTobs[1L]
  
  V[1L]			<- priors$V
  Go[1L]		<- priors$Go
  NoGo[1L]	<- priors$NoGo
  
  #V_fast = V(1); V_slow = V(1); #no differentiation of slow vs. fast to start
  #joint_ent=1.0; #unused at the moment.
  
  alphaV <- 0.1 # just set this to avoid degeneracy
  
  # GENERATIVE model just pick some params to generate data
  if (generative == 1) {
    lambda = 0.2
    explore = 3000
    alpha1 = .3
    alpha2 = .3
    K = 1500
    exp_alt =500
    scale = .25
    meandiff = 1000
    if (dist_type=="Gauss") {
      meandiff=20
      explore = 10
    }
    Noise=2000
  } 
  
  #initialize algorithm parameters
  exp					<- 0
  exp1				<- 0
  exp1a				<- 0 #not used
  lose_switch <- 0
  regress 		<- 0
  mean_short	<- 0.5
  mean_long		<- 0.5
  RT_avg = avg_RT; # set avg on first trial..
  
  
  alph_long=1.01; b_long=1.01; % init counters and beta distribution hyperparams..
      alph_short=1.01; b_short=1.01;
  cnt_short=0; cnt_long=0;
  cnt_speed=0; cnt_slow=0;
  
  RT_new      = RTobs(1); % just for init
      RT_last     = RTobs(1); % just for init
      sticky      = 0;        % initialize sticky choice
      
  
  #iterate over trials 2..n
  for (trial in 2:numTrials) {
    lastTrial <- trial - 1
    
    exp1_last = exp1; exp_last = exp; exp1a_last = exp1a;
    means_last = mean_short;
    meanl_last = mean_long;
    vars_last = var_short;
    varl_last = var_long;
  }
  
  
}
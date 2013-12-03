#R implementation of time clock algorithm

TC_Alg <- setRefClass(
    Class="TC_Alg",
    fields=list(
        RTobs="numeric",
        Reward="numeric",
        )
)

#methods
#initialize: (
#fit: (gradient descent to select optimal theta values -- minimize SE)
#predict: (get model-predicted values for a given theta)


#wrapper function to determine whether behavior fit improves with additional parameters

#core TC algorithm
TC_Alg <- function(RTobs, Reward, params, priors, avg_RT, rewFunc, emo, model, 
        distType="beta", generative=FALSE, stickyChoice=FALSE) {
    
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
    
    numTrials <- length(RTobs)
    
    RTpred  <- rep(NA_real_, numTrials) #vector of predicted RTs
    V       <- rep(NA_real_, numTrials) #state-value function (expected value)
    Go      <- rep(NA_real_, numTrials)
    NoGo    <- rep(NA_real_, numTrials)
    
    Q <- 0
    Noise <- 0
    
    mean_s <- 0
    mean_f <- 0
    
    #initial variances of fast/slow resps for kalman filter
    #Use variance of observed rewards so initial lr = 0.5
    var_s <- var_f <- rewvar <- var(Reward)
    rtvar <- var(RTobs)
    
    #can't predict first choice due to learning so just set it to actual subject RT 
    #and then predict starting trial 2
    
    RTpred[1L] <- RTobs[1L]
    
    V[1L]			<- priors$V
    Go[1L]          <- priors$Go
    NoGo[1L]        <- priors$NoGo
    
    #V_fast = V(1); V_slow = V(1); #no differentiation of slow vs. fast to start
    #joint_ent=1.0; #unused at the moment.
    
    alphaV <- 0.1 # just set this to avoid degeneracy
    
    # GENERATIVE model just pick some params to generate data
    if (generative) {
        lambda = 0.2
        explore = 3000
        alphaG = .3
        alphaN = .3
        K = 1500
        exp_alt =500
        scale = .25
        meandiff = 1000
        if (distType=="Gauss") {
            meandiff=20
            explore = 10
        }
        Noise=2000
    } 
    
    #initialize algorithm parameters
    exp             <- 0
    exp1            <- 0
    exp1a           <- 0 #not used
    lose_switch     <- 0
    regress         <- 0
    mean_short      <- 0.5
    mean_long       <- 0.5
    RT_avg          <- avg_RT # TODO: set avg on first trial.. WEIRD NAMING INEFFICIENCY?
    
    alpha_long       <- 1.01 #init counters and beta distribution hyperparams..
    beta_long          <- 1.01
    alpha_short      <- 1.01
    beta_short         <- 1.01
    
    ##TODO: need to figure the naming of these counts..
    cnt_short <- 0; cnt_long <- 0
    cnt_speed <- 0; cnt_slow <- 0
    
    RT_new      <- RTobs[1L] #just for init
    RT_last     <- RTobs[1L] #just for init
    sticky      <- 0         #initialize sticky choice
    bestRT      <- avg_RT
    
    #initialize data.frame to be returned
    retDf <- data.frame(
            rtObs=rep(NA_real_, numTrials),
            rtPred=rep(NA_real_, numTrials),
            rpe=rep(NA_real_, numTrials),
            explore=rep(NA_real_, numTrials),
            sdShort=rep(NA_real_, numTrials),
            sdLong=rep(NA_real_, numTrials),
            meanShort=rep(NA_real_, numTrials),
            meanLong=rep(NA_real_, numTrials),
            go=rep(NA_real_, numTrials),
            noGo=rep(NA_real_, numTrials)
    )
    
    
    #iterate over trials 2..n
    for (trial in 2:numTrials) {
        lastTrial <- trial - 1
        
        exp1_last = exp1; exp_last = exp; exp1a_last = exp1a;
        means_last = mean_short;
        meanl_last = mean_long;
        vars_last = var_short;
        varl_last = var_long;
        
        #add process noise to kalman variances (only for kalman filter model)
        if (distType == "Gaussian") {
            vars <- vars + Q
            varf <- varf + Q
        }
        
        if (generative) { # if generating responses make last rt the prev predicted rt
            if (trial > 2) { RT_last2 <- RT_last }
            RT_last <- RT_new
        } else {
            RT_last <- RTobs[lastTrial]
            if (scale == -1) { sticky <- RT_last + sticky_decay*sticky } #update sticky choice if used
            if (trial > 2) { RT_last2 <- RTobs[trial - 2L] }
            if (trial > 3) { RT_last3 <- RTobs[trial - 2L] }
        }
        
        #reverse momentum model
        mom <- RT_last - RT_last2 #momentum
        if (mom == 0) { mom <- 1 }
        
        Rew_last = Reward[lasttrial]
        if (generative) {
            Rew_last = RewFunction(RT_last, rewFunc) # calculate reward if model generating own rt's
        }    
        
        if (RT_last > RT_last2) { # Reverse-momentum model
            cnt_slow <- cnt_slow + 1 # count number of responses slower than previous
            cnt_speed <- 0 
        } else {
            cnt_speed <- cnt_speed + 1
            cnt_slow <- 0
        }
        
        V_last <- V[lastTrial];
        V_new = V_last + alphaV*(Rew_last - V_last) # update critic expected value
        
        rew_max <- max(Reward[1:lasttrial]) # max reward received in block thus far -- used for v scaling v[RT_best - RT_avg]
        rew_sd <- sd(Reward[1:lasttrial]) # sd of rewards in block thus far
        
        # If PPE on prior trial and obtained reward within one SD of max, save as bestRT
        if (Rew_last > V_last && Rew_last >= (rew_max - rew_sd)) {
            bestRT <- RT_last
        }
        
        ##Process speed up of RT for PPE (Go) or slow down of RT for NPE (NoGo)
        Go_last   <- Go[lastTrial]
        NoGo_last <- NoGo[lastTrial]
        
        #carry forward Go and NoGo terms unless updated below by PE
        Go_new    <- Go_last
        NoGo_new  <- NoGo_last
        
        if (Rew_last > V_last) {
            #if obtained reward was better than expected (PPE), speed up (scaled by alphaG)
            Go_new = Go_last + alphaG*(Rew_last - V_last)
        }  else if (Rew_last <= V_last) {
            #if obtained reward was worse than expected, slow down (scaled by alphaN)
            NoGo_new = NoGo_last + alphaN*(V_last - Rew_last)            
        }
        
        #model tracks two distributions, one for fast/short responses (less than mean RT)
        #and one for long/slow responses (above mean RT)
        #here, we update the estimates of the corresponding beta distribution for slow or fast responses
        
        #last response was slow/long
        if(RT_last > RT_avg) {
            cnt_long  <- cnt_long + 1 # for sutton exp bonus control model, count how many trials in a row have been long
            cnt_short <- 0
            
            regress   <- -exp_alt # for simple oscillation regression to mean explore control model
            
            if (dist_type == "beta") {
                if(Rew_last> V_last) {
                    alpha_long <- alpha_long + 1 # increment count for beta distribution
                } else {
                    beta_long <- beta_long + 1
                    lose_switch <- -exp_alt # if was slow, go fast after neg PE
                }
                
                alpha_long      <- decay*alpha_long # if decay < 1 then this decays counts, making beta dists less confident
                beta_long       <- decay*beta_long
                alpha_short     <- decay*alpha_short
                beta_short      <- decay*beta_short
                
                # these are mode and variances of beta dists
                var_short   <- alpha_short * beta_short / ( (alpha_short+beta_short)^2 * (alpha_short + beta_short + 1))
                var_long    <- alpha_long * beta_long / ( (alpha_long+beta_long)^2 * (alpha_long + beta_long + 1))
                mode_long   <- (alpha_long - 1) / (alpha_long + beta_long - 2) #modes are not used at the moment
                mode_short  <- (alpha_short - 1) / (alpha_short + beta_short - 2)
                mean_long   <- alpha_long / (alpha_long + beta_long)
                mean_short  <- alpha_short / (alpha_short + beta_short)
                
                exp1 = -explore * (sqrt(var_short) - sqrt(var_long))  # speed up if more uncertain about fast responses
                
            } else if (dist_type == "Gaussian") {
                
                alphaKs <- var_s / (var_s + rewvar) # Kalman gain for slow responses
                var_s <- (1 - alphaKs) * var_s; # Kalman variance for slow responses
                
                ##TODO: Why is V_last taken out?
                mean_s = mean_s + alphaKs*((Rew_last - 0*V_last) - mean_s) # kalman mean
                
                ##TODO: Figure out mean_long versus mean_s and mean_short versus mean_f
                mean_long   <- mean_s
                mean_short  <- mean_f
                
                ##TODO: This line is not in some versions of MF's code
                var_short <- var_f
                var_long <- var_s
                
                exp1 = - explore*(sqrt(var_f) - sqrt(var_s));  # using kalman filter gaussian distributions.
                
                
            }
            
            #already explored in this direction last trial (see supplement of  Frank et al 09)
            if (RT_last < RT_last2 && exp1 < 0) { 
                exp1 <- 0 # %-exp1; % reset if 
            } else if (RT_last > RT_last2 && exp1 > 0) {
                exp1 <- 0 
            }
            
        } else if (RT_last <= RT_avg) { #last resp was fast/short
            # only update rew statistics if subject actually responded
            # non-response is counted as 0 in e-prime version
            if(RT_last > 0) {
                cnt_short <- cnt_short + 1; 
                cnt_long <- 0; # for sutton exp bonus control model
                
                regress <- +exp_alt #  regress to mean control model
                
                if (dist_type == "beta") {
                    if(Rew_last> V_last) {
                        alpha_short <- alpha_short + 1
                    } else {
                        beta_short = beta_short + 1
                        lose_switch = exp_alt # if was fast, slow down after neg PE (lose switch control model)
                    }
                    
                    alpha_long      <- decay * alpha_long
                    beta_long       <- decay * beta_long
                    alpha_short     <- decay * alpha_short
                    beta_short      <- decay * beta_short
                    
                    # mode and variances of beta distribution
                    #TODO: Are these redundant with the beta updates above? If so, figure out how to consolidate
                    var_short   <- alpha_short * beta_short / ( (alpha_short + beta_short)^2 * (alpha_short + beta_short + 1))
                    var_long    <- alpha_long*beta_long/( (alpha_long + beta_long)^2 * (alpha_long + beta_long + 1) )
                    mode_long   <- (alpha_long - 1) / (alpha_long + beta_long - 2)
                    mode_short  <- (alpha_short - 1) / (alpha_short + beta_short - 2)
                    mean_long   <- alpha_long / (alpha_long + beta_long)
                    mean_short  <- alpha_short / (alpha_short + beta_short)
                    
                    exp1 = + explore*(sqrt(var_long) - sqrt(var_short))
                } else if (dist_type=="Gaussian") {
                    alphaKf   <- varf / (varf + rewvar)
                    varf      <- (1 - alphaKf) * varf
                    
                    #TODO: Why is V_last taken out here?
                    mean_f      <- mean_f + alphaKf*((Rew_last - 0*V_last) - mean_f)
                    mean_short  <- mean_f
                    mean_long   <- mean_s
                    var_short   <- varf
                    var_long    <- vars
                    
                    exp1 = + explore*(sqrt(vars) - sqrt(varf))  # using kalman filter normal distributions.
                }
                
                # reset if already explored in this direction last trial (see supplement of Frank et al 09)
                #TODO: Should be able to use one compount logical separated by || to handle the two sets of conditions here 
                if (RT_last < RT_last2 && exp1 < 0) {
                    exp1 <- 0      
                } else if (RT_last > RT_last2 && exp1 > 0) {
                    exp1 <- 0     
                } 
                
                
            }
        }
        
        revmom <- 0 # for reverse momentum control model
        if (cnt_speed > scale) { revmom <- exp_alt * cnt_speed }
        else if (cnt_slow > scale) { revmom <- -exp_alt*cnt_slow }
        
        exp <- exp_alt*(sqrt(cnt_short)-sqrt(cnt_long)) # sutton exploration bonus model, for control model in supplement
        
        if (RT_last==0) { RT_last = RT_last2 } # if last trial there was no response, use trial before that for updating RT avg and autocorrelation effects (otherwise counted as 0)
        
        #Update average RT locally
        #Note that alphaV is fixed above at 0.1, essentially updating the average by 10% of the difference
        #of the current RT and the average RT
        #TODO: See whether this tracking of average RT is in the Frank papers.
        RT_avg <- RT_avg + alphaV * (RT_last - RT_avg)
        
        #TODO: Figure out whether a logical sticky parameter to the function is useful
        #or whether the model should auto-detect the stickyness based on the vector of parameters
        
        ##Main update of predicted reaction time modulation (brings together the various updates above)
        if (stickyChoice) {
            #sticky model: scale effect of prior RTs (decayed) on current RT by lambda
            #model does not include going for the gold (scale) update.
            
            RT_new <- K + lambda*sticky - Go_new + NoGo_new + exp1 + 0*regress +
                    meandiff*(mean_long-mean_short) + Noise * (rand-0.5);
        } else {
            RT_new <- K + lambda*RT_last - Go_new + NoGo_new + exp1 + 0*regress +
                    meandiff*(mean_long-mean_short) + scale*(bestRT-avg_RT) + Noise*(rand-0.5);
            
        }
        
        if (RTobs[trial] == 0) { RT_new = 0 } # don't try to predict response failures, which are counted as 0 in e-prime
        
        if (dist_type == "Gaussian") {
            alphaK <- varK / (varK + rtvar) # alphaK = kalman gain;
            varK <- (1 - alphaK) * varK
        }
        
        #commit trial-by-trial model predictions of various parameters to "memory"
        RTpred[trial]  <- RT_new
        V[trial]       <- V_new
        Go[trial]      <- Go_new
        NoGo[trial]    <- NoGo_new
        
        retDf <- data.frame(
                rtObs=rep(NA_real_, numTrials),
                rtPred=rep(NA_real_, numTrials),
                rpe=rep(NA_real_, numTrials),
                explore=rep(NA_real_, numTrials),
                sdShort=rep(NA_real_, numTrials),
                sdLong=rep(NA_real_, numTrials),
                meanShort=rep(NA_real_, numTrials),
                meanLong=rep(NA_real_, numTrials),
                go=rep(NA_real_, numTrials),
                noGo=rep(NA_real_, numTrials)
        )
        
        
        #commit algorithm values to return structure
        #here we are committing results from trial t-1
        #TODO: Is there a better way to implement this so we don't have a unique update on the last trial?
        retDf[trial-1, c("rpe", "explore", "sdShort", "sdLong", "meanShort", "meanLong", "go", "noGo", "ev")] <- list(
                rpe=Rew_last - V_last,
                explore=exp1_last,
                sdShort=sqrt(vars_last),
                sdLong=sqrt(varl_last),
                meanShort=means_last,
                meanLong=meanl_last,
                go=Go_last,
                noGo=NoGo_last,
                ev=V_last
        )
        
        if (trial == numTrials) {
            retDf[trial, c("rpe", "explore", "sdShort", "sdLong", "meanShort", "meanLong", "go", "noGo", "ev")] <- list(
                    rpe=Reward[trial] - V_new,
                    explore=exp1,
                    sdShort=sqrt(var_short),
                    sdLong=sqrt(var_long),
                    meanShort=mean_short,
                    meanLong=mean_long,
                    go=Go_new,
                    noGo=NoGo_new,
                    ev=V_new
            )
        }
    }
}

#R implementation of time clock algorithm

#go for gold specification:
#initialize best to average RT: bestRT      <- avg_RT

#track whether PPE occurs and reward is within one SD of max
#rew_max <- max(Reward[1:lasttrial]) # max reward received in block thus far -- used for v scaling v[RT_best - RT_avg]
#rew_sd <- sd(Reward[1:lasttrial]) # sd of rewards in block thus far
#        # If PPE on prior trial and obtained reward within one SD of max, save as bestRT
#        if (Rew_last > V_last && Rew_last >= (rew_max - rew_sd)) {
#            bestRT <- RT_last
#        }

# contribute to RT prediction according to scale parameter
#
#RT_new <- K + lambda*RT_last - Go_new + NoGo_new + exp1 + 0*regress +
#    meandiff*(mean_long-mean_short) + scale*(bestRT-avg_RT) + Noise*(rand-0.5);

#scratch variables:
# - bestRT: RT associated with best reward to trial t
# - rew_max: maximum obtained reward to trial t
# - rew_sd: standard deviations of rewards to trial t

#depends:
# - V_last: expected value/payoff on trial t
# - Rew_last: obtained reward on prior trial
# - avg_RT: average reaction time across block
# - lasttrial: counter of trial up to t (iterating over trials)

#model parameters:
# - scale: weight for modulating RT toward best: scale*(bestRT - avg_RT)

#influence on prediction update
# - scale*(bestRT - avg_RT)

###
#abstracting to other aspects of the model, we have
# parameter proper:
#   - initial value
#   - min and max values (constraints)
#   - current value

# scratch variables (updated trial-to-trial)

# dependencies on other scratch variables

# influence on RT prediction (update)

#dataset object is supposed to represent a group of data over which a single set of parameters (theta) are to be found
#so in the group case, get best-fitting parameters for all subjects and runs
#for a single subject, best fitting-parameters across subject's runs
#for a single run, best-fitting parameters for this run

#have predict in alg detect the class of the dataset to determine how to proceed

clockDataset <- setRefClass(
    class="clockDataset",
    fields=list(
        subjects="list",
        dataset_name="character"
    ),
    methods=list(
        initialize=function(dataset_name=NULL, subjects=NULL) {
          if (!is.null(dataset_name)) { dataset_name <<- dataset_name }
          if (!is.null(subjects)) {
            if (!is.list(subjects) && class(subjects) == "clockSubject") {
              subjects <<- list(subjects) #single subject
            } else if (is.list(subjects) && all(sapply(subjects, class) == "clockSubject")) {
              subjects <<- subjects
            } else { stop ("subjects should be a list of clockSubject objects or a single clockSubject object")}
          }
        },
        add_subjects=function(...) {
          if (!is.null(subjects)) {
            existingSubjectIDs <- sapply(subjects, function(s) { s$subject_ID } )
          } else { existingSubjectNums <- c() }
          
          s_objs <- as.list(match.call())[-1L] #first element of match.call is a class for refMethodDef for the alg object
          
          #verify that all arguments are clockSubject objects
          if (!all(sapply(subjects, class) == "clockSubject")) { stop("All arguments to add_subjects must be clockSubject objects.") }
          
          newSubjectIDs <- sapply(s_objs, function(s) { s$subject_ID } )
          
          if (any(dupes <- newSubjectIDs %in% existingSubjectIDs)) {
            cat("Cannot add a subject with the same ID as an existing subject\n")
            stop("The following subjects conflict: ", paste(newSubjectIDs[dupes], collapse=", "))
          }
          
          subjects <<- c(subjects, s_objs) #concatenate
          #subjects <<- subjects[sort(sapply(subjects, function(s) { s$subject_ID }))] #sort in ascending order
          
        },
        delete_subjects=function(subjects_to_delete=NULL) {
          if (is.null(subjects_to_delete)) { return(invisible()) }
          if (!is.character(subjects_to_delete)) { stop("delete_subjects expects a character vector of subject IDs to delete") }
          
          subjects_to_keep <- which(! sapply(subjects, function(s) { s$subject_ID } ) %in% subjects_to_delete)
          unmatched <- which(! subjects_to_delete %in% subjects)
          
          subjects <<- subjects[subjects_to_keep]
          
          if (length(unmatched) > 0L) {
            warning("Unable to find the following subjects for deletion: ", paste0(unmatched, collapse=", "))
          }
        }

        
        )
)

clockSubject <- setRefClass(
    class="clockSubject",
    fields=list(
        subject_ID="character",
        runs="list"
    ),
    initialize=function(subject_ID=NULL, runs=NULL, ...) {
      if (is.null(subject_ID)) { stop("clockSubject requires ID at initialization.") }
      if (!is.null(runs)) {
        if (!is.list(runs) && class(runs) == "clockRun") {
          runs <<- list(runs) #single run
        } else if (is.list(runs) && all(sapply(runs, class) == "clockRun")) {
          runs <<- runs
        } else { stop ("runs should be a list of clockRun objects or a single clockRun object")}
      }
    },
    add_runs=function(...) {
      if (!is.null(runs)) {
        existingRunNums <- sapply(runs, function(r) { r$run_number } )
      } else { existingRunNums <- c() }
        
      r_objs <- as.list(match.call())[-1L] #first element of match.call is a class for refMethodDef for the alg object
      
      #verify that all arguments are clockRuns objects
      if (!all(sapply(runs, class) == "clockRun")) { stop("All arguments to add_runs must be clockRun objects.") }
      
      newRunNums <- sapply(r_objs, function(r) { r$run_number } )
      
      if (any(dupes <- newRunNums %in% existingRunNums)) {
        cat("Cannot add a run with the same number as an existing run\n")
        stop("The following runs conflict: ", paste(newRunNums[dupes], collapse=", "))
      }
      
      runs <<- c(runs, r_objs) #concatenate
      runs <<- runs[sort(sapply(runs, function(r) { r$run_number }))] #sort in ascending order
      
    },
    delete_runs=function(runs_to_delete=NULL) {
      if (is.null(runs_to_delete)) { return(invisible()) }
      if (!is.numeric(runs_to_delete)) { stop("delete_runs expects a numeric vector of run numbers to delete") }
      
      runs_to_keep <- which(! sapply(runs, function(r) { r$run_number} ) %in% runs_to_delete)
      unmatched <- which(! runs_to_delete %in% runs)
      
      runs <<- runs[runs_to_keep]
      
      if (length(unmatched) > 0L) {
        warning("Unable to find the following runs for deletion: ", paste0(unmatched, collapse=", "))
      }
    }
    
)

clockRun <- setRefClass(
    class="clockRun",
    fields=list(
        w="environment",
        run_number="numeric",
        SSE="numeric",
        RTobs="numeric",
        Reward="numeric"
        ),
        methods=list(
            initialize=function(RTobs=NA_integer_, Reward=NA_integer_, ...) {
              if (is.na(RTobs[1L])) { warning("construction of clockRun requires observed reaction times (RTobs)") }
              if (is.na(Reward[1L])) { warning("construction of clockRun requires observed rewards (Reward)") }
              if (length(RTobs) != length(Reward)) { stop("RTobs and Reward are of different length") }
                            
              reset_workspace() #initial setup of workspace
              
            },
            reset_workspace=function() {
              #initialize shared workspace
              #clear out old values if refitting
              w <<- new.env() #new.env(parent = emptyenv()) #make sure we do not accidentally inherit objects from .Globalenv as parent 
              
              w$ntrials <<- length(Reward)
              w$RTobs   <<- RTobs #initialize fields
              w$Reward  <<- Reward
              w$RTpred  <<- rep(NA_real_, w$ntrials) #initialize empty predicted RT
              w$RTpred[1L] <<- w$RTobs[1L] #cannot predict first trial behavior per se, so use observed RT so that t=1 doesn't contribute to SSE
              w$rpe     <<- rep(NA_real_, w$ntrials) #vector of reward prediction errors
              w$V       <<- rep(NA_real_, w$ntrials) #expected value vector
              w$V[1L]   <<- 0 #just for now -- come back to allow V to carry over blocks.
              
              w$avg_RT  <<- mean(RTobs, na.rm=TRUE)
 
            }
            )
    )

    #add a reset method here to set values back to initial (clear for re-using the same alg object with a new dataset)
#actually, couldn't we just note when the dataset is updated and zero out parameters?
alg <- setRefClass(
    Class="alg",
    fields=list(
        #updateEquation="expression",
        params="list",
        noiseWt="numeric",
        SSE="numeric",
        clockData="ANY" #allow this to be dataset, subject, or run
    ),
    methods=list(
        initialize=function(clockData=NULL, ...) {
          cat("Initializing alg\n")
          if (!is.null(clockData) && !class(clockData) %in% c("clockDataset", "clockSubject", "clockRun")) {
            stop("data for alg object must be one of the following types: clockDataset, clockSubject, or clockRun")
          }
          
          #should probably make this a member of this class, not in workspace
          #w$alphaV  <<- 0.1 # learning rate for critic (V).  Just set this to avoid degeneracy
          
          params <<- list() #initialize empty list of model parameters
          noiseWt <<- 0 #do not add noise to RT prediction
          
          callSuper(...) #for classes inheriting from this, pass through unmatched iniitalization values
        },
        lappend=function(target, append) {
          if (!is.list(target)) stop("target is not a list.")
          if (!is.list(append)) stop("append is not a list.")
          
          for (elementName in names(append)) {
            if (!is.null(target[[elementName]])) warning("Element is already present in target list: ", elementName)
            target[[elementName]] <- append[[elementName]]
          }
          
          return(target)
        },
        add_params=function(...) {
          #accept a variable number of parameter objects
          p_objs <- as.list(match.call())[-1L] #first element of match.call is a class for refMethodDef for the alg object
          p_objs <- lapply(p_objs, eval) #force initialization of param objects (otherwise stays as a language expression)
          
          #pass along address of shared workspace to sub-objects
          p_objs <- lapply(p_objs, function(p) { p$w <- .self$w; p$postInit(); return(p) })
          
          params <<- lappend(params, p_objs)
        },
        list_params=function() {
          cat("Current parameters in algorithm\n\n")
          df <- data.frame(
              p_name=names(params), 
              min_value=get_param_minimum_vector(),
              max_value=get_param_maximum_vector(),
              init_value=get_param_initial_vector(),
              cur_value=get_param_current_vector()
              )
          #cat(names(params), sep="\n")
          print(format(df,digits=2,scientific=FALSE))
        },
        reorder_params=function(neworder) {
          #reorders the parameter collection
          #potentially useful if there are dependencies among parameters or workspace values: params are evaluated in sequential order 
          left_out <- names(params)[! names(params) %in% neworder]
          if (length(left_out) > 0L) {
            warning("reorder_params call did not include position for: ", paste(left_out, collapse=", "), ". Defaulting to end")
            neworder <- c(neworder, left_out)
          }
          if (length(unmatched <- neworder[! neworder %in% names(params)]) > 0L) {
            stop("reorder_params unable to find parameters in alg: ", paste(unmatched, collapse=", "))
          }
          params <<- params[neworder]
        },
        
        get_param_current_vector=function() {
          #corral a named vector of parameters for optim
          cur_p_vec <- sapply(params, function(p) {
                return(p$cur_value)
              })
          names(cur_p_vec) <- sapply(params, function(p) { return(p$name) }) #use internal param labels, not names provided by user
          return(cur_p_vec)
        },
        
        get_param_minimum_vector=function() {
          #corral a named vector of parameters for optim
          min_p_vec <- sapply(params, function(p) {
                return(p$min_value)
              })
          names(min_p_vec) <- sapply(params, function(p) { return(p$name) }) #use internal param labels, not names provided by user
          return(min_p_vec)					
        },
        
        get_param_maximum_vector=function() {
          #corral a named vector of parameters for optim
          max_p_vec <- sapply(params, function(p) {
                return(p$max_value)
              })
          names(max_p_vec) <- sapply(params, function(p) { return(p$name) }) #use internal param labels, not names provided by user
          return(max_p_vec)					
        },
        
        get_param_initial_vector=function() {
          #corral a named vector of parameters for optim
          init_p_vec <- sapply(params, function(p) {
                return(p$init_value)
              })
          names(init_p_vec) <- sapply(params, function(p) { return(p$name) }) #use internal param labels, not names provided by user
          return(init_p_vec)
        },
        
        fit=function(toFit=NULL, initialValues=get_param_initial_vector(),
            lower=get_param_minimum_vector(),
            upper=get_param_maximum_vector()) {
          
          if (!is.null(toFit) && !class(toFit) %in% c("clockDataset", "clockSubject", "clockRun")) {
            stop("data for alg object must be one of the following types: clockDataset, clockSubject, or clockRun")
          } else {
            clockData <<- toFit
          }
            
          #call predict using optimizer
          if (length(initialValues) == 1L) { method="Brent"
          } else { method="L-BFGS-B" }
          
          p <- optim(par=initialValues, fn=.self$predict, method=method,
              lower=lower, upper=upper)
          
          return(p)
          #optimize(.self$predict, interval=c(100, 4000))
  
          #actually, to force optim to choose a single vector parameters over more than one run of
          #data, we need a superordinate method that loops over subjects and runs, calling predict
          #with a given set of values.
  
          #to fit multiple subjects and/or runs, probably we should pass a run of data to predict
          #that way, predict always has a vector of RTs and Rewards to work with, as well as priors
          #on any working variables like V, Go, NoGo, etc.
  
        },
        
        predict=function(theta=get_param_current_vector()) {
          totalSSE <- 0
          
          if (class(clockData)=="clockDataset") {
            #loop over subjects and runs (group fit)
            for (s in clockData$subjects) {
              for (r in s$runs) {
                
              }
            }
          } else if (class(clockData)=="clockSubject") {
            for (r in clockData$runs) {
              totalSSE <- totalSSE + predictRun(r)
            }
            
          } else if (class(clockData)=="clockRun") {
            totalSSE <- predictRun()
          }
          
          for (s in 1:length(data$subjects)) {
            for (r in 1:length(data$runs)) {
              predict(theta=get_param_current_vector(), data=data[s,r])
              #should now add workspace information and trial-by-trial stuff to the data object
              #so that we can do
              totalSSE <- totalSSE + data[s,r]$predStuff$SSE
            }
          }
          return(totalSSE)
          #I guess this would mean having a shared workspace per run, right?
          #so w becomes a member of the run object
          #and each run object contains a w.
          #this would let you look back at it later and understand the trial-by-trial fit progression
        },
        
        ##TODO: Consider whether we want to have a "saveValues" T/F parameter here
        #during the function minimization, there's no need to save trial-by-trial estimates of the parameters
        #since the parameter values are not final yet. Could be faster not to save pred_contrib and trialwise_value, among others
        predictRun=function(clockRun=NULL, priorRun=NULL, theta=get_param_current_vector()) {
          if (is.null(clockRun) || !class(clockRun) == "clockRun") { stop("predictRun requires a clockRun object") }
          clockRun$reset_workspace()
          #should now copy w to alg? and set to emptyenv() at the end?
          
          #optim using Brent, or optimize fail to pass names of parameters into predict.
          #names are critical for the params to lookup properly.
          if (is.null(names(theta))) {
            #warning("Unnamed theta vector passed to predict. Looking up from parameter list.")
            names(theta) <- sapply(params, function(p) { return(p$name) })
          }
          
          #reset workspace before proceeding!
          
          w$RT_new  <<- w$RTobs[1L] #use observed RT as predicted RT for t=1
          w$RT_last <<- w$RTobs[1L]
          w$RT_last2 <<- w$RTobs[1L]
          
          #TODO: Risk that we initialize some vals above (like initial V), but predict is called iteratively for optimization, so values will not be reset here if they were set in initialize
          
          for (t in 2:w$ntrials) {
            w$cur_trial <<- t
            w$lastTrial <<- t - 1
            
            evalq(
                {
                  Rew_last <- Reward[lastTrial]
                  RT_last  <- RTobs[lastTrial]
                  if (cur_trial > 2) { RT_last2 = RTobs[cur_trial - 2] }
                  
                  V_last <- V[lastTrial];
                  V_new = V_last + alphaV*(Rew_last - V_last) # update critic expected value
                  V[cur_trial] <- V_new
                }, w)
            
            w$RT_new <<- sum(sapply(params, function(p) { p$getRTUpdate(theta) } )) + noiseWt*(runif(1,-0.5,0.5)) #add or subtract noise according to noiseWt (0 for now)
            w$RTpred[w$cur_trial] <<- w$RT_new        
            
            #TODO: incorporate this code from matlab. Affects beta dist local_RT and autocorrelation parameters 
            #if RT_last==0, RT_last = RT_last2; end; %% if last trial there
            #was no response, use trial before that for updating RT avg and autocorrelation effects (otherwise counted as 0)
            
            w$rpe[t-1] <<- w$Rew_last - w$V_last
            
          }
          
          SSE <<- sum((w$RTobs - w$RTpred)^2) #sum of squared error: cost
          cat("SSE: ", SSE, "\n")
          return(SSE)
        })

)



#generic parameter class
#should expose:
#  - min_value:  minimum parameter value in optimization
#  - max_value:  maximum parameter value
#  - init_value: initial parameter value
#  - cur_value:  current parameter value
#
#  - getRTUpdate: compute contribution to RT prediction for this parameter
param <- setRefClass(
    Class="param",
    fields=list(
        name="character",
        min_value="numeric",
        max_value="numeric",
        init_value="numeric",
        cur_value="numeric",
        w="environment",
        pred_contrib="numeric", #trial-wise contribution to RT pred
        trialwise_value="numeric" #trial-wise estimate of the parameter
    ),
    methods=list(
        initialize=function(min_value=-Inf, max_value=Inf, init_value=0, cur_value=0, ...) {
          w <<- emptyenv() #workspace should never be unique to parameter, but is set upstream by alg
          
          #general sanity checks and field assignmend handled here so subordinate classes need not duplicate
          stopifnot(cur_value <= max_value)
          stopifnot(cur_value >= min_value)
          stopifnot(init_value <= max_value)
          stopifnot(init_value >= min_value)
          
          min_value <<- min_value
          max_value <<- max_value
          init_value <<- init_value
          cur_value <<- cur_value
          
          callSuper(...) #pass along any unmatched parameters for assignment          
        },
        postInit=function(...) {
          #function that is called after object initialization
          #and after the shared workspace (w) address is passed to parameter.
          pred_contrib <<- rep(NA_real_, w$ntrials) #vector of this parameter's contribution to predicted RT
          trialwise_value <<- rep(NA_real_, w$ntrials) #vector of parameter value at each trial
        },
        #default to NULL RT (should be overloaded by sub-class)
        getRTUpdate=function() {
          return(NULL)
        }
    )

)

##K: mean reaction time
meanRT <- setRefClass(
    Class="p_K",
    contains="param",
    fields=list(),
    methods=list(
        initialize=function(min_value=100, max_value=5000, init_value=1000, cur_value=init_value, ...) {
          name <<- "K"
          callSuper(min_value, max_value, init_value, cur_value, ...) #call upstream constructor to initialize fields
        },
        getRTUpdate=function(theta) {
          cur_value <<- theta[name] #update current value in param object
          trialwise_value[w$cur_trial] <<- cur_value #save estimate of parameter at this trial
          pred_contrib[w$cur_trial] <<- cur_value #for baseline RT, the parameter itself is the speed in ms 
          return(pred_contrib[w$cur_trial])
        }
    )
)

##lambda: RT autocorrelated with t-1
autocorrPrevRT <- setRefClass(
    Class="p_autocorrPrevRT",
    contains="param",
    fields=list(),
    methods=list(
        initialize=function(min_value=0.0, max_value=1.0, init_value=0.3, cur_value=init_value, ...) {
          name <<- "lambda"
          
          if (min_value < 0.0) { stop("Autocorr prev RT (lambda) parameter cannot be negative") }
          if (max_value > 1.0) { stop("Autocorr prev RT (lambda) parameter cannot exceed 1.0") }
          
          callSuper(min_value, max_value, init_value, cur_value, ...) #call upstream constructor to initialize fields
        },
        getRTUpdate=function(theta) {          
          cur_value <<- theta[name] #update current value based on optimization
          trialwise_value[w$cur_trial] <<- cur_value #save estimate of parameter at this trial
          pred_contrib[w$cur_trial] <<- cur_value*w$RT_last 
          return(pred_contrib[w$cur_trial])
        }
    )
)


##Go: speed up of RT for PPE
go <- setRefClass(
    Class="p_go",
    contains="param",
    fields=list(), #Go="numeric"), #put Go vector into w
    methods=list(
        initialize=function(min_value=0.01, max_value=5.0, init_value=0.2, cur_value=init_value, ...) {
          if (min_value < 0.01) { stop("alphaG min_value must be at least 0.01") }
          if (max_value > 5.0) { stop("alphaG max_value must be less than 5.0") }
          name <<- "alphaG"
          callSuper(min_value, max_value, init_value, cur_value, ...) #initialize upstream
        },
        postInit=function(...) {
          w$Go <<- rep(NA_real_, w$ntrials) #vector of Go term
          w$Go[1L] <<- 0.0 #may want to override this later...
          callSuper(...) #setup pred_contrib upstream
        },
        getRTUpdate=function(theta) {
          cur_value <<- theta[name] #update current value based on optimization
          #a bit of a hack here to copy the alphaG learning rate into w for easier code below
          w$cur_value <<- cur_value
          evalq(
              {
                Go_last   <- Go[lastTrial]
                
                #carry forward Go term unless updated below by PPE
                Go_new <- Go_last
                
                #if obtained reward was better than expected (PPE), speed up (scaled by alphaG)
                if (Rew_last > V_last) {                  
                  Go_new <- Go_last + cur_value*(Rew_last - V_last)
                }
                
                Go[cur_trial] = Go_new                
              },
              w
          )
          rm(cur_value, envir=w)
          
          trialwise_value[w$cur_trial] <<- cur_value #save estimate of parameter at this trial
          pred_contrib[w$cur_trial] <<- -1.0*w$Go_new 
          return(pred_contrib[w$cur_trial])
          
        }
    )
)

##NoGo: slow down of RT for NPE
noGo <- setRefClass(
    Class="p_nogo",
    contains="param",
    fields=list(),
    methods=list(
        initialize=function(min_value=0.01, max_value=5.0, init_value=0.2, cur_value=init_value, ...) {
          if (min_value < 0.01) { stop("alphaN min_value must be at least 0.01") }
          if (max_value > 5.0) { stop("alphaN max_value must be less than 5.0") }
          name <<- "alphaN"
          callSuper(min_value, max_value, init_value, cur_value, ...) #initialize upstream
        },
        postInit=function(...) {
          w$NoGo <<- rep(NA_real_, w$ntrials) #vector of NoGo term
          w$NoGo[1L] <<- 0.0 #may want to override this later...
          callSuper(...) #setup pred_contrib upstream
        },
        getRTUpdate=function(theta) {
          cur_value <<- theta[name] #update current value based on optimization
          #a bit of a hack here to copy the alphaN learning rate into w for easier code below
          w$cur_value <<- cur_value
          evalq(
              {
                NoGo_last   <- NoGo[lastTrial]
                
                #carry forward NoGo term unless updated below by NPE
                NoGo_new <- NoGo_last
                
                #if obtained reward was worse than expected (NPE), slow down (scaled by alphaN)
                if (Rew_last <= V_last) {                  
                  NoGo_new <- NoGo_last + cur_value*(V_last - Rew_last)
                }
                
                NoGo[cur_trial] = NoGo_new                
              },
              w
          )
          rm(cur_value, envir=w)
          
          pred_contrib[w$cur_trial] <<- +1.0*w$NoGo_new
          trialwise_value[w$cur_trial] <<- cur_value #save estimate of parameter at this trial
          return(pred_contrib[w$cur_trial])          
        }
    )
)


##scale: go for the gold
goForGold <- setRefClass(
    Class="p_gold",
    contains="param",
    fields=list(bestRT_t1="numeric"),
    methods=list(
        initialize=function(min_value=0, max_value=5000, init_value=0.1, cur_value=init_value, ...) {
          #if user wants to override default behavior for expected bestRT on first trial, must pass in a bestRT_t1 value at initialize
          name <<- "scale"
          if (min_value < 0) { stop("Go for gold (nu) parameter cannot be negative") }
          callSuper(min_value, max_value, init_value, cur_value, ...) #call upstream constructor to initialize fields
        },
        postInit=function(...) {
          #After shared workspace (w) address is passed in, setup expectation on bestRT for first trial
          w$bestRT  <<- rep(NA_real_, w$ntrials) #initialize empty bestRT vector
          if (identical(bestRT_t1, numeric(0))) {
            #default to average RT
            warning("Using average reaction time across block as best RT prior.")
            w$bestRT[1L] <<- w$avg_RT
          } else {
            #use the user-specified value for the bestRT on t=1
            w$bestRT[1L] <<- bestRT_t1
          }
          callSuper(...) #setup pred_contrib using upstream method
        },
        getRTUpdate=function(theta) {
          cur_value <<- theta[name] #update current value based on optimization
          
          #compute maximum reward and reward variability up to current trial
          #evaluate update within shared workspace
          evalq(
              { 
                rew_max <- max(Reward[1:lastTrial]) # max reward received in block thus far -- used for updating best RT
                rew_sd  <- ifelse(lastTrial > 1, sd(Reward[1:lastTrial]), 0) # sd of rewards in block thus far (use a value of 0 if just one trial)
                # If PPE on prior trial and obtained reward falls within one SD of max, save as bestRT
                #N.B. This works magically well in the test case
                #was trying to see whether the PPE aspect here is necessary
                #if (Rew_last >= (rew_max - rew_sd)) {
                if (Rew_last > V_last && Rew_last >= (rew_max - rew_sd)) {
                  bestRT[cur_trial] <- RT_last #make sure to use <-, not <<- to avoid scope confusion with bestRT field
                } else {
                  bestRT[cur_trial] <- bestRT[lastTrial] #carry forward best so far
                }
                
              },
              w
          )
          
          trialwise_value[w$cur_trial] <<- cur_value #save estimate of parameter at this trial
          pred_contrib[w$cur_trial] <<- cur_value*with(w, bestRT[cur_trial] - avg_RT)
          return(pred_contrib[w$cur_trial])          
        }
    
    )
)

#rho and epsilon parameters both depend on tracking two beta
#distributions for fast and slow responses.
#rho scales with the difference in mean expected payoff for fast versus slow responses
#epsilon scales with difference in uncertainty (SD) for fast versus slow responses
#abstract the beta tracking to a shared class
#and the means vs SDs lookup is extrapolated to the explore and meandiff classes
betaFastSlow <- setRefClass(
    Class="betaFastSlow",
    fields=list(w="environment",
        alpha_slow="numeric", #alpha shape parameter for slow (above average) RTs
        beta_slow="numeric", #beta shape parameter for slow RTs
        alpha_fast="numeric",
        beta_fast="numeric",
        decay="numeric",
        mean_fast="numeric", #mean of fast beta dist
        mean_slow="numeric",
        mode_fast="numeric", #mode of fast beta dist
        mode_slow="numeric",
        var_fast="numeric",
        var_slow="numeric",
        mean_fast_last="numeric", #mean of fast beta dist on previous trial
        mean_slow_last="numeric",
        var_fast_last="numeric",
        var_slow_last="numeric",
        explore_last="numeric",
        explore="numeric",
        lastUpdateTrial="numeric",
        local_RT="numeric",
        local_RT_last="numeric",
        local_RT_learning_rate="numeric" #TODO: Does the learning rate for RT need to be yoked to alphaV for the critic update, as in MF's code?
    ),
    methods=list(
        initialize=function(w=NULL, alpha_slow=1.01, beta_slow=1.01, alpha_fast=1.01, beta_fast=1.01, decay=1.0, lastUpdateTrial=1L, local_RT_learning_rate=0.1, ...) {
          stopifnot(decay <= 1.0)
          if (is.null(w) || !is.environment(w)) { stop ("Shared workspace w must be passed at initialization into betaFastSlow") }
          w <<- w #setup shared workspace
          
          #initialize shape parameters for fast and slow RT beta distributions
          alpha_slow <<- alpha_slow
          beta_slow <<- beta_slow
          alpha_fast <<- alpha_fast 
          beta_fast  <<- beta_fast
          decay <<- decay
          lastUpdateTrial <<- lastUpdateTrial
          #local_RT <<- rep(NA_real_, w$ntrials) #model for fast/slow responses tracks recent RT average using alphaV learning rate
          #not using a vector for now
          local_RT[1L] <<- local_RT_last[1L] <<- w$avg_RT #set initial local average to the block mean RT
          local_RT_learning_rate <<- local_RT_learning_rate #rate at which estimate of recent RTs is updated
          
          callSuper(...)
        },
        postInit=function(...) {
        },
        updateBetaDists=function() {
          #because explore and meandiff parameters may both be present in the model
          #need to check whether the beta distribution has already been updated on this trial
          #if so, do not update again
          if (lastUpdateTrial == w$cur_trial) { return(invisible(NULL)) }
          
          lastUpdateTrial <<- w$cur_trial #update the trial count for beta dist tracking
          
          #model tracks two distributions, one for fast responses (less than mean RT)
          #and one for slow responses (above mean RT)
          #here, we update the estimates of the corresponding beta distribution for slow or fast responses
          
          #cache means and variances of prior trial
          mean_fast_last <<- mean_fast
          mean_slow_last <<- mean_slow
          var_fast_last  <<- var_fast
          var_slow_last  <<- var_slow
          local_RT_last  <<- local_RT
          
          if (w$RT_last > local_RT_last) { #last response was slower than local average
            if (w$Rew_last > w$V_last) { #ppe: increment alpha shape parameter for slow dist
              alpha_slow <<- alpha_slow + 1
            } else {
              beta_slow <<- beta_slow + 1
            }
          } else if (w$RT_last <= local_RT_last) { #last response was faster than average
            if(w$Rew_last > w$V_last) {
              alpha_fast <<- alpha_fast + 1
            } else {
              beta_fast <<- beta_fast + 1
            }
          }
          
          if (decay < 1.0) {
            alpha_slow     <<- decay * alpha_slow # if decay < 1 then this decays counts, making beta dists less confident
            beta_slow      <<- decay * beta_slow
            alpha_fast     <<- decay * alpha_fast
            beta_fast      <<- decay * beta_fast
          }
          
          # compute mode and variances of beta distribution
          var_fast    <<- alpha_fast * beta_fast / ( (alpha_fast + beta_fast)^2 * (alpha_fast + beta_fast + 1) )
          var_slow    <<- alpha_slow*beta_slow/( (alpha_slow + beta_slow)^2 * (alpha_slow + beta_slow + 1) )
          mode_slow   <<- (alpha_slow - 1) / (alpha_slow + beta_slow - 2)
          mode_fast   <<- (alpha_fast - 1) / (alpha_fast + beta_fast - 2)
          mean_slow   <<- alpha_slow / (alpha_slow + beta_slow)
          mean_fast   <<- alpha_fast / (alpha_fast + beta_fast)
          
          local_RT <<- local_RT_last + local_RT_learning_rate * (w$RT_last - local_RT_last) # update estimate of recent RTs by 10% (0.1) of deviation of this trial's RT from the local average
          
        }
    )

)

##meandiff (rho)
meanSlowFast <- setRefClass(
    Class="p_meanSlowFast",
    contains="param",
    fields=list(),
    methods=list(
        initialize=function(min_value=0, max_value=10000, init_value=300, cur_value=init_value, ...) {
          name <<- "rho"
          if (min_value < 0) { stop("Slow versus fast mean parameter (rho) cannot be negative") }
          callSuper(min_value, max_value, init_value, cur_value, ...) #call upstream constructor to initialize fields
        },
        postInit=function(...) {
          if (!exists("betaFastSlow", envir=w, inherits=FALSE)) { w$betaFastSlow <<- betaFastSlow(w) }
          
          callSuper(...)
        },
        getRTUpdate=function(theta) {          
          cur_value <<- theta[name] #update current value based on optimization
          w$betaFastSlow$updateBetaDists() #update fast/slow beta dists
          
          trialwise_value[w$cur_trial] <<- cur_value #save estimate of parameter at this trial
          pred_contrib[w$cur_trial] <<- cur_value * with(w$betaFastSlow, mean_slow - mean_fast) 
          return(pred_contrib[w$cur_trial])
        }
    )
)


#strategic explore parameter using beta distribution for counts of prediction errors
exploreBeta <- setRefClass(
    Class="p_epsilonBeta",
    contains="param",
    fields=list(),
    methods=list(
        initialize=function(min_value=0, max_value=100000, init_value=2000, cur_value=init_value, ...) {          
          name <<- "epsilonBeta"
          callSuper(min_value, max_value, init_value, cur_value, ...) #call upstream constructor to initialize fields
        },
        postInit=function(...) {
          if (!exists("betaFastSlow", envir=w, inherits=FALSE)) { w$betaFastSlow <<- betaFastSlow(w) }
          
          callSuper(...)
        },
        getRTUpdate=function(theta) {
          #model tracks two distributions, one for fast responses (less than mean RT)
          #and one for slow responses (above mean RT)
          #here, we update the estimates of the corresponding beta distribution for slow or fast responses
          
          cur_value <<- theta[name] #update current value based on optimization
          
          w$explore_last <<- w$explore #cache prior explore product prior to beta update
          w$betaFastSlow$updateBetaDists() #update fast/slow beta dists
          
          w$cur_value <<- cur_value
          evalq(
              {                
                if (RT_last > betaFastSlow$local_RT_last) {
                  #explore parameter scales the difference in SDs between the fast and slow beta dists
                  explore <- -1.0*cur_value * (sqrt(betaFastSlow$var_fast) - sqrt(betaFastSlow$var_slow))  # speed up if more uncertain about fast responses
                } else {
                  explore <- +1.0*cur_value * (sqrt(betaFastSlow$var_slow) - sqrt(betaFastSlow$var_fast)) #slow down if more uncertain about slow responses
                }
                
                # reset if already explored in this direction last trial (see supplement of Frank et al 09)
                if ( (RT_last < RT_last2 && explore < 0) ||
                    (RT_last > RT_last2 && explore > 0) ) {
                  explore <- 0
                }
              },
              w
          )
          rm(cur_value, envir=w)
         
          trialwise_value[w$cur_trial] <<- cur_value #save estimate of parameter at this trial
          pred_contrib[w$cur_trial] <<- w$explore 
          return(pred_contrib[w$cur_trial])
        }
    
    )
)




#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#TC_Alg <- setRefClass(
#		Class="TC_Alg",
#		fields=list(
#				RTobs="numeric",
#				RTpred="numeric",
#				Reward="numeric",
#				prior="numeric",
#				updateEquation="expression",
#				PEdistribution="character",
#				V="numeric",
#				Go="numeric",
#				NoGo="numeric",
#				params="numeric", #named vector of model parameters
#				scratchScalars="numeric" #named vector of scratch variables used by sub-models 
#		
#		),
#		methods=list(
#				predict=function() {
#					#compute predicted values and SE for a given set of parameters
#					#should be called repeatedly by fit method
#					
#					#use a scalar workspace that is updated trial-by-trial by various sub-methods to have
#					#the necessary variables to compute a predicted RT.
#					scalarWorkspace <- list()
#					
#					
#					
#					numTrials <- length(RTobs) 
#					
#					# Time clock R-L algorithm developed by Michael Frank
#					#
#					#
#					# inputs:
#					#    RTobs:    vector of observed reaction times
#					#    Reward:   vector of obtained rewards (points)
#					#    params:   vector of model parameters used to fit data
#					#    avg_RT:   scalar of average reaction time (across trials from all blocks)
#					#    rewFunc:  reward contingency, 1=CEV; 2=CEVR; 3=DEV; 4=IEV
#					#    emo:      emotion, 1=happy; 2=fear; 3=scrambled
#					#
#					#
#					# RTobs is a t x 1 column vector of reaction times for t trials.
#					# Reward is a t x 1 column vector of rewards obtained for t trials.
#					# params is an 8 x 1 column vector of model parameters to be used to fit behavior.
#					
#					RTpred  <<- rep(NA_real_, numTrials) #vector of predicted RTs
#					V       <<- rep(NA_real_, numTrials) #state-value function (expected value)
#					Go      <<- rep(NA_real_, numTrials)
#					NoGo    <<- rep(NA_real_, numTrials)
#					
#					Q <- 0
#					Noise <- 0
#					
#					mean_s <- 0
#					mean_f <- 0
#					
#					#initial variances of fast/slow resps for kalman filter
#					#Use variance of observed rewards so initial lr = 0.5
#					var_s <- var_f <- rewvar <- var(Reward)
#					rtvar <- var(RTobs)
#					
#					#can't predict first choice due to learning so just set it to actual subject RT 
#					#and then predict starting trial 2
#					
#					RTpred[1L] <- RTobs[1L]
#					
#					V[1L]     			<- priors$V
#					Go[1L]          <- priors$Go
#					NoGo[1L]        <- priors$NoGo
#					
#					#V_fast = V(1); V_slow = V(1); #no differentiation of slow vs. fast to start
#					#joint_ent=1.0; #unused at the moment.
#					
#					# learning rate for expected value V based on PE
#					# also used for updating local RT average
#					alphaV <- 0.1 # just set this to avoid degeneracy
#					
#					#initialize algorithm parameters
#					exp             <- 0
#					exp1            <- 0
#					exp1a           <- 0 #not used
#					lose_switch     <- 0
#					regress         <- 0
#					mean_short      <- 0.5
#					mean_long       <- 0.5
#					RT_avg          <- avg_RT # TODO: set avg on first trial.. WEIRD NAMING INEFFICIENCY?
#					
#					alpha_long      <- 1.01 #init counters and beta distribution hyperparams..
#					beta_long       <- 1.01
#					alpha_short     <- 1.01
#					beta_short      <- 1.01
#					
#					##TODO: need to figure the naming of these counts..
#					cnt_short <- 0; cnt_long <- 0
#					cnt_speed <- 0; cnt_slow <- 0
#					
#					
#					
#				},
#				modelBuildAIC=function() {
#					#call fit function with a restricted set of parameters
#					#use the order of the user-specified parameters as the basis for computing the successive fits
#					#should return a vector of AIC values for each parameterization
#				},
#				fit=function() {
#					#constrOptim descent over theta, calling predict with varying parameters
#					
#					
#				}
#		)
#)
#
##methods
##initialize: (
##fit: (gradient descent to select optimal theta values -- minimize SE)
##predict: (get model-predicted values for a given theta)
#
#
##wrapper function to determine whether behavior fit improves with additional parameters
#
#
#
##core TC algorithm
#TC_Alg <- function(RTobs, Reward, params, priors, avg_RT, rewFunc, emo, model, 
#		distType="beta", generative=FALSE, stickyChoice=FALSE) {
#	
#	# GENERATIVE model just pick some params to generate data
#	if (generative) {
#		lambda = 0.2
#		explore = 3000
#		alphaG = .3
#		alphaN = .3
#		K = 1500
#		exp_alt =500
#		scale = .25
#		meandiff = 1000
#		if (distType=="Gauss") {
#			meandiff=20
#			explore = 10
#		}
#		Noise=2000
#	} 
#	
#	
#	
#	
#	
#	
#	RT_new      <- RTobs[1L] #just for init
#	RT_last     <- RTobs[1L] #just for init
#	sticky      <- 0         #initialize sticky choice
#	bestRT      <- avg_RT
#	
#	#initialize data.frame to be returned
#	retDf <- data.frame(
#			rtObs=rep(NA_real_, numTrials),
#			rtPred=rep(NA_real_, numTrials),
#			rpe=rep(NA_real_, numTrials),
#			explore=rep(NA_real_, numTrials),
#			sdShort=rep(NA_real_, numTrials),
#			sdLong=rep(NA_real_, numTrials),
#			meanShort=rep(NA_real_, numTrials),
#			meanLong=rep(NA_real_, numTrials),
#			go=rep(NA_real_, numTrials),
#			noGo=rep(NA_real_, numTrials)
#	)
#	
#	
#	
#	#iterate over trials 2..n
#	for (trial in 2:numTrials) {
#		lastTrial <- trial - 1
#		
#		exp1_last <- exp1 
#		exp_last <- exp 
#		exp1a_last = exp1a
#		means_last = mean_short
#		meanl_last = mean_long
#		vars_last = var_short
#		varl_last = var_long
#		
#		#add process noise to kalman variances (only for kalman filter model)
#		if (distType == "Gaussian") {
#			vars <- vars + Q
#			varf <- varf + Q
#		}
#		
#		if (generative) { # if generating responses make last rt the prev predicted rt
#			if (trial > 2) { RT_last2 <- RT_last }
#			RT_last <- RT_new
#		} else {
#			RT_last <- RTobs[lastTrial]
#			if (scale == -1) { sticky <- RT_last + sticky_decay*sticky } #update sticky choice if used
#			if (trial > 2) { RT_last2 <- RTobs[trial - 2L] }
#			if (trial > 3) { RT_last3 <- RTobs[trial - 2L] }
#		}
#		
#		#reverse momentum model
#		mom <- RT_last - RT_last2 #momentum
#		if (mom == 0) { mom <- 1 }
#		
#		Rew_last = Reward[lasttrial]
#		if (generative) {
#			Rew_last = RewFunction(RT_last, rewFunc) # calculate reward if model generating own rt's
#		}    
#		
#		if (RT_last > RT_last2) { # Reverse-momentum model
#			cnt_slow <- cnt_slow + 1 # count number of responses slower than previous
#			cnt_speed <- 0 
#		} else {
#			cnt_speed <- cnt_speed + 1
#			cnt_slow <- 0
#		}
#		
#		V_last <- V[lastTrial];
#		V_new = V_last + alphaV*(Rew_last - V_last) # update critic expected value
#		
#		rew_max <- max(Reward[1:lasttrial]) # max reward received in block thus far -- used for v scaling v[RT_best - RT_avg]
#		rew_sd <- sd(Reward[1:lasttrial]) # sd of rewards in block thus far
#		
#		# If PPE on prior trial and obtained reward within one SD of max, save as bestRT
#		if (Rew_last > V_last && Rew_last >= (rew_max - rew_sd)) {
#			bestRT <- RT_last
#		}
#		
#		##Process speed up of RT for PPE (Go) or slow down of RT for NPE (NoGo)
#		Go_last   <- Go[lastTrial]
#		NoGo_last <- NoGo[lastTrial]
#		
#		#carry forward Go and NoGo terms unless updated below by PE
#		Go_new    <- Go_last
#		NoGo_new  <- NoGo_last
#		
#		if (Rew_last > V_last) {
#			#if obtained reward was better than expected (PPE), speed up (scaled by alphaG)
#			Go_new = Go_last + alphaG*(Rew_last - V_last)
#		}  else if (Rew_last <= V_last) {
#			#if obtained reward was worse than expected, slow down (scaled by alphaN)
#			NoGo_new = NoGo_last + alphaN*(V_last - Rew_last)            
#		}
#		
#		#model tracks two distributions, one for fast/short responses (less than mean RT)
#		#and one for long/slow responses (above mean RT)
#		#here, we update the estimates of the corresponding beta distribution for slow or fast responses
#		
#		#last response was slow/long
#		if(RT_last > RT_avg) {
#			cnt_long  <- cnt_long + 1 # for sutton exp bonus control model, count how many trials in a row have been long
#			cnt_short <- 0
#			
#			regress   <- -exp_alt # for simple oscillation regression to mean explore control model
#			
#			if (dist_type == "beta") {
#				if(Rew_last> V_last) {
#					alpha_long <- alpha_long + 1 # increment count for beta distribution
#				} else {
#					beta_long <- beta_long + 1
#					lose_switch <- -exp_alt # if was slow, go fast after neg PE
#				}
#				
#				alpha_long      <- decay*alpha_long # if decay < 1 then this decays counts, making beta dists less confident
#				beta_long       <- decay*beta_long
#				alpha_short     <- decay*alpha_short
#				beta_short      <- decay*beta_short
#				
#				# these are mode and variances of beta dists
#				var_short   <- alpha_short * beta_short / ( (alpha_short+beta_short)^2 * (alpha_short + beta_short + 1))
#				var_long    <- alpha_long * beta_long / ( (alpha_long+beta_long)^2 * (alpha_long + beta_long + 1))
#				mode_long   <- (alpha_long - 1) / (alpha_long + beta_long - 2) #modes are not used at the moment
#				mode_short  <- (alpha_short - 1) / (alpha_short + beta_short - 2)
#				mean_long   <- alpha_long / (alpha_long + beta_long)
#				mean_short  <- alpha_short / (alpha_short + beta_short)
#				
#				exp1 = -explore * (sqrt(var_short) - sqrt(var_long))  # speed up if more uncertain about fast responses
#				
#			} else if (dist_type == "Gaussian") {
#				
#				alphaKs <- var_s / (var_s + rewvar) # Kalman gain for slow responses
#				var_s <- (1 - alphaKs) * var_s; # Kalman variance for slow responses
#				
#				##TODO: Why is V_last taken out?
#				mean_s = mean_s + alphaKs*((Rew_last - 0*V_last) - mean_s) # kalman mean
#				
#				##TODO: Figure out mean_long versus mean_s and mean_short versus mean_f
#				mean_long   <- mean_s
#				mean_short  <- mean_f
#				
#				##TODO: This line is not in some versions of MF's code
#				var_short <- var_f
#				var_long <- var_s
#				
#				exp1 = - explore*(sqrt(var_f) - sqrt(var_s));  # using kalman filter gaussian distributions.
#				
#				
#			}
#			
#			#already explored in this direction last trial (see supplement of  Frank et al 09)
#			if (RT_last < RT_last2 && exp1 < 0) { 
#				exp1 <- 0 # %-exp1; % reset if 
#			} else if (RT_last > RT_last2 && exp1 > 0) {
#				exp1 <- 0 
#			}
#			
#		} else if (RT_last <= RT_avg) { #last resp was fast/short
#			# only update rew statistics if subject actually responded
#			# non-response is counted as 0 in e-prime version
#			if(RT_last > 0) {
#				cnt_short <- cnt_short + 1; 
#				cnt_long <- 0; # for sutton exp bonus control model
#				
#				regress <- +exp_alt #  regress to mean control model
#				
#				if (dist_type == "beta") {
#					if(Rew_last> V_last) {
#						alpha_short <- alpha_short + 1
#					} else {
#						beta_short = beta_short + 1
#						lose_switch = exp_alt # if was fast, slow down after neg PE (lose switch control model)
#					}
#					
#					alpha_long      <- decay * alpha_long
#					beta_long       <- decay * beta_long
#					alpha_short     <- decay * alpha_short
#					beta_short      <- decay * beta_short
#					
#					# mode and variances of beta distribution
#					#TODO: Are these redundant with the beta updates above? If so, figure out how to consolidate
#					var_short   <- alpha_short * beta_short / ( (alpha_short + beta_short)^2 * (alpha_short + beta_short + 1))
#					var_long    <- alpha_long*beta_long/( (alpha_long + beta_long)^2 * (alpha_long + beta_long + 1) )
#					mode_long   <- (alpha_long - 1) / (alpha_long + beta_long - 2)
#					mode_short  <- (alpha_short - 1) / (alpha_short + beta_short - 2)
#					mean_long   <- alpha_long / (alpha_long + beta_long)
#					mean_short  <- alpha_short / (alpha_short + beta_short)
#					
#					exp1 = + explore*(sqrt(var_long) - sqrt(var_short))
#				} else if (dist_type=="Gaussian") {
#					alphaKf   <- varf / (varf + rewvar)
#					varf      <- (1 - alphaKf) * varf
#					
#					#TODO: Why is V_last taken out here?
#					mean_f      <- mean_f + alphaKf*((Rew_last - 0*V_last) - mean_f)
#					mean_short  <- mean_f
#					mean_long   <- mean_s
#					var_short   <- varf
#					var_long    <- vars
#					
#					exp1 = + explore*(sqrt(vars) - sqrt(varf))  # using kalman filter normal distributions.
#				}
#				
#				# reset if already explored in this direction last trial (see supplement of Frank et al 09)
#				#TODO: Should be able to use one compount logical separated by || to handle the two sets of conditions here 
#				if (RT_last < RT_last2 && exp1 < 0) {
#					exp1 <- 0      
#				} else if (RT_last > RT_last2 && exp1 > 0) {
#					exp1 <- 0     
#				} 
#				
#				
#			}
#		}
#		
#		revmom <- 0 # for reverse momentum control model
#		if (cnt_speed > scale) { revmom <- exp_alt * cnt_speed }
#		else if (cnt_slow > scale) { revmom <- -exp_alt*cnt_slow }
#		
#		exp <- exp_alt*(sqrt(cnt_short)-sqrt(cnt_long)) # sutton exploration bonus model, for control model in supplement
#		
#		if (RT_last==0) { RT_last = RT_last2 } # if last trial there was no response, use trial before that for updating RT avg and autocorrelation effects (otherwise counted as 0)
#		
#		#Update average RT locally
#		#Note that alphaV is fixed above at 0.1, essentially updating the average by 10% of the difference
#		#of the current RT and the average RT
#		#TODO: See whether this tracking of average RT is in the Frank papers.
#		RT_avg <- RT_avg + alphaV * (RT_last - RT_avg)
#		
#		#TODO: Figure out whether a logical sticky parameter to the function is useful
#		#or whether the model should auto-detect the stickyness based on the vector of parameters
#		
#		##Main update of predicted reaction time modulation (brings together the various updates above)
#		if (stickyChoice) {
#			#sticky model: scale effect of prior RTs (decayed) on current RT by lambda
#			#model does not include going for the gold (scale) update.
#			
#			RT_new <- K + lambda*sticky - Go_new + NoGo_new + exp1 + 0*regress +
#					meandiff*(mean_long-mean_short) + Noise * (rand-0.5);
#		} else {
#			RT_new <- K + lambda*RT_last - Go_new + NoGo_new + exp1 + 0*regress +
#					meandiff*(mean_long-mean_short) + scale*(bestRT-avg_RT) + Noise*(rand-0.5);
#			
#		}
#		
#		if (RTobs[trial] == 0) { RT_new = 0 } # don't try to predict response failures, which are counted as 0 in e-prime
#		
#		if (dist_type == "Gaussian") {
#			alphaK <- varK / (varK + rtvar) # alphaK = kalman gain;
#			varK <- (1 - alphaK) * varK
#		}
#		
#		#commit trial-by-trial model predictions of various parameters to "memory"
#		RTpred[trial]  <- RT_new
#		V[trial]       <- V_new
#		Go[trial]      <- Go_new
#		NoGo[trial]    <- NoGo_new
#		
#		retDf <- data.frame(
#				rtObs=rep(NA_real_, numTrials),
#				rtPred=rep(NA_real_, numTrials),
#				rpe=rep(NA_real_, numTrials),
#				explore=rep(NA_real_, numTrials),
#				sdShort=rep(NA_real_, numTrials),
#				sdLong=rep(NA_real_, numTrials),
#				meanShort=rep(NA_real_, numTrials),
#				meanLong=rep(NA_real_, numTrials),
#				go=rep(NA_real_, numTrials),
#				noGo=rep(NA_real_, numTrials)
#		)
#		
#		
#		#commit algorithm values to return structure
#		#here we are committing results from trial t-1
#		#TODO: Is there a better way to implement this so we don't have a unique update on the last trial?
#		retDf[trial-1, c("rpe", "explore", "sdShort", "sdLong", "meanShort", "meanLong", "go", "noGo", "ev")] <- list(
#				rpe=Rew_last - V_last,
#				explore=exp1_last,
#				sdShort=sqrt(vars_last),
#				sdLong=sqrt(varl_last),
#				meanShort=means_last,
#				meanLong=meanl_last,
#				go=Go_last,
#				noGo=NoGo_last,
#				ev=V_last
#		)
#		
#		if (trial == numTrials) {
#			retDf[trial, c("rpe", "explore", "sdShort", "sdLong", "meanShort", "meanLong", "go", "noGo", "ev")] <- list(
#					rpe=Reward[trial] - V_new,
#					explore=exp1,
#					sdShort=sqrt(var_short),
#					sdLong=sqrt(var_long),
#					meanShort=mean_short,
#					meanLong=mean_long,
#					go=Go_new,
#					noGo=NoGo_new,
#					ev=V_new
#			)
#		}
#	}
#}
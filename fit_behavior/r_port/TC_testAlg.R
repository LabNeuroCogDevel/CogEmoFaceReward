# Some thoughts on the top-level algorithm object
# probably makes sense to have an object for each run of data?
# then have a multi-dimensional list of alg objects for subjects x runs?
# or could shove it all into one alg object and have $fit specify how to minimize SSE (over all subjects and runs, for each subject, for each run within subject?)
# maybe a subject class would be good... Containing runs, and fit objects per run?



a <- alg(RTobs=c(1,2,3), Reward=c(1,2,3))
a$add_params(
    gold=goForGold(min_value=0, max_value=500, init_value=0, cur_value=10),
    K=meanRT(max_value=4000, cur_value=1000)
    )

a$add_params(gold=goForGold(min_value=-1, max_value=1, init_value=0, cur_value=10))

#a$add_params(gold=goForGold(min_value=-1, max_value=1, init_value=0, cur_value=10))
#a$add_params(gold1=goForGold(min_value=-1, max_value=1, init_value=0, cur_value=10), gold2=goForGold(min_value=10, max_value=1, init_value=0, cur_value=10))
a$list_params()
a$get_param_minimum_vector()
a$params$gold$min_value <- 22 
a$params$gold$getRTUpdate()
a$params$gold$workspace$sharedX <- -10
ls(a$workspace) #workspace environment is shared between params and alg 
a$workspace$sharedX



a$reorder_params(c("gold2", "gold1", "gold"))
a$reorder_params(c("gold2", "gold"))
a$reorder_params(c("gold2", "gold22"))

x <- a$params$gold
x$getRTUpdate()

a$get_param_current_vector()


RTobs <- rnorm(100, 1000, 100)

#evaluating within environment
n <- new.env()

evalq(
    {
      a <- 2
      b <- 25
      
      if(a==2) { 
        print(paste0("b is ", b))
        a <- 3
      }
    },
    n)


evalq(
    x <- y <- 2,
    n
    )
    
attach(n, pos=1)
x1 <- 1
x2 <- 2

detach(n)



a <- alg(RTobs=c(1719, 2896, 3260, 3414, 3425, 3633, 3320, 3414, 3438, 3507), 
    Reward=c(0, 78, 87, 92, 88, 0, 89, 95, 89, 0))

#simple fit with just mean RT
a$add_params(K=meanRT(max_value=4000, cur_value=1000))
a$predict()
a$fit()
mean(RTobs[2:length(RTobs)]) #expected value of K: first trial does not contribute to cost

a <- alg(RTobs=c(1719, 2896, 3260, 3414, 3425, 3633, 3320, 3414, 3438, 3507), 
    Reward=c(0, 78, 87, 92, 88, 0, 89, 95, 89, 0))

a$add_params(
    K=meanRT(max_value=4000, cur_value=1000),
    gold=goForGold(min_value=0, max_value=500, init_value=0, cur_value=10),
    art1=autocorrPrevRT(),
    g=go(),
    n=noGo(),
    m=meanSlowFast(),
    e=exploreBeta()
)

#check parameterization
a$list_params()
a$fit()

#look at decomposition of prediction by parameter
predMat <- do.call(data.frame, lapply(a$params, "[[", "pred_contrib"))
predMat$RTpred <- apply(predMat, 1, sum)
print(predMat)

#pilot data that MF had initially fit. Use to test whether my estimates are close
pilot1001 <- read.table("/Users/michael/CogEmoFaceReward/subjects/pilot/1001_tc.txt", header=TRUE)

p1001 <- alg(RTobs=pilot1001$RT, Reward=pilot1001$ScoreInc)
p1001$add_params(
    K=meanRT(max_value=4000, cur_value=1000),
    gold=goForGold(min_value=0, max_value=500, init_value=0, cur_value=10),
    art1=autocorrPrevRT(),
    g=go(),
    n=noGo(),
    m=meanSlowFast(),
    e=exploreBeta()
)

p1001$list_params()
p1001$fit()


RTobs=c(1719, 2896, 3260, 3414, 3425, 3633, 3320, 3414, 3438, 3507)
RTpred=c(RTobs[1L], rep(1000, length(RTobs) - 1))
sseKOnly <- sum((RTobs - RTpred)^2)
(match <- a$predict() - sseKOnly)

#gold=goForGold(min_value=0, max_value=500, init_value=0, cur_value=10, bestRT=a$w$avg_RT)
#gold$getRTUpdate()

gold=goForGold(min_value=0, max_value=500, init_value=0, cur_value=10, bestRT=10)
art1=autocorrPrevRT(min_value=0, cur_value=0.2)
g=go()

f <- function (x, a) (x - a)^2
xmin <- optimize(f, c(0, 1), tol = 0.0001, a = 1/3)
xmin


#try out creation of clockSubject with runs
s <- clockSubject(subject_ID="006_mb", csv_file="/Users/michael/Dropbox/Hallquist_K01/Data/fMRI/006mb_05Nov2013/fMRIEmoClock/fMRIEmoClock_6_tc_tcExport.csv")
s$import_runs_from_csv()

#is the basic mean RT fit working across all blocks?
x <- read.csv("/Users/michael/Dropbox/Hallquist_K01/Data/fMRI/006mb_05Nov2013/fMRIEmoClock/fMRIEmoClock_6_tc_tcExport.csv", header=TRUE)
library(plyr)
ms <- ddply(x, .(run), function(subdf) {
      return(data.frame(m=mean(subdf$rt[2:nrow(subdf)])))
    })
print(mean(ms$m))

a <- alg()

a$add_params(K=meanRT(max_value=4000, cur_value=1000))

Rprof("profile1.out")

a$fit(toFit=s)

Rprof(NULL)
print(a$get_param_current_vector())
#okay, this does match. so, the fit across runs is apparently basically working

library(profr)
ggplot.profr(parse_rprof("profile1.out"))
summaryRprof("profile1.out")#, lines = "show")

a <- alg()

a$add_params(
    K=meanRT(max_value=4000),
    gold=goForGold(),
    art1=autocorrPrevRT(),
    g=go(),
    n=noGo()
    #m=meanSlowFast()
    #e=exploreBeta()
)
#run fit
#a$fit(toFit=s$runs[[1]])
Rprof("profile1.out")
system.time(a$fit(toFit=s$runs[[2]]))
Rprof(NULL)

print(a$get_param_current_vector())

summaryRprof("profile1.out")#, lines = "show")

#try to figure out a match with MF fits
#pilot: 1000
s1000 <- clockSubject(subject_ID="1000_pilot", csv_file="/Users/michael/CogEmoFaceReward/subjects/pilot/1000_tc_tcExport.csv")
#s1000$import_runs_from_csv()

atest <- alg()
atest$add_params(
    K=meanRT(max_value=4000),
    art1=autocorrPrevRT()
#    gold=goForGold(),
#    g=go(),
#    n=noGo(),
#    m=meanSlowFast(),
#    e=exploreBeta()
)

f <- atest$fit(s1000)
times <- list()

atest <- alg()
atest$add_params(
    K=meanRT(max_value=4000, by="rew_function"),
    art1=autocorrPrevRT(by="emotion")
#    gold=goForGold(),
#    g=go(),
#    n=noGo(),
#    m=meanSlowFast(),
#    e=exploreBeta()
)

atest$set_data(s1000)


library(microbenchmark)
#microbenchmark( lapply(params, function(p) { getRTUpdate(p, theta, updateFields=updateFields) }), unname(lapply(params, function(p) { getRTUpdate(p, theta, updateFields=updateFields) } )), times=10000)

for (n in 1:10) {
  times[[n]] <- atest$fit(s1000)#[[2]]["elapsed"]
}

#benchmark: 103.3338 seconds average on imac
mean(sapply(lapply(times, "[[", 2), "[", "elapsed")[c(1:8, 10)])



sqrt(f[[1]]$value) #sse
atest$list_params()
lapply(atest$params, function(p) { p$value_history })

#values from MATLAB with K, lambda (autoRT), Go, NoGo
#6Jan2014: Verified exact match with MATLAB for RTpred and SSE
#atest$params[["K"]]$cur_value <- 335.777
#atest$params[["art1"]]$cur_value <- 0.84477
#atest$params[["g"]]$cur_value <- 0.13896
#atest$params[["n"]]$cur_value <- 0.16138

#values from MATLAB with K, lambda (autoRT), Go, NoGo, and scale/nu (gold)
#have now verified that scale parameter works as expected when manually setting parameters, as here.
atest$params[["K"]]$cur_value <- 792.565
atest$params[["art1"]]$cur_value <- 0.62524
atest$params[["g"]]$cur_value <- 0.05058
atest$params[["n"]]$cur_value <- 0.08567
atest$params[["gold"]]$cur_value <- 0.26445


#values from MATLAB for full model
#atest$params[["K"]]$cur_value <- 809.8756
#atest$params[["gold"]]$cur_value <- 0.27036
#atest$params[["art1"]]$cur_value <- 0.60583
#atest$params[["g"]]$cur_value <- 0.06214
#atest$params[["n"]]$cur_value <- 0.11034
#atest$params[["m"]]$cur_value <- 192.7773
#atest$params[["e"]]$cur_value <- 2162.829

atest$list_params()
atest$clockData <- s1000
sse <- atest$predict(updateFields=TRUE)
sqrt(sse)

#look at predicted and observed RTs
RTobs <- lapply(s1000$runs, function(r) { r$RTobs })
RTpred <- lapply(s1000$runs, function(r) { r$w$RTpred })

atest$params[]

#s1000$plotRTs()

#try fitting using parameters from MATLAB. Do we recover identical SSE?
#Nope

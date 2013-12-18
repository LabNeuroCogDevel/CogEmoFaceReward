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
    n=noGo()
)

a$fit()

#look at decomposition of prediction by parameter
predMat <- do.call(data.frame, lapply(a$params, "[[", "predContrib"))
predMat$RTpred <- apply(predMat, 1, sum)




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
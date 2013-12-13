
a <- alg(RTobs=c(1,2,3), Reward=c(1,2,3))
a$add_params(gold=goForGold(min_value=-1, max_value=1, init_value=0, cur_value=0))
#a$add_params(gold1=goForGold(min_value=-1, max_value=1, init_value=0, cur_value=0), gold2=goForGold(min_value=10, max_value=1, init_value=0, cur_value=0))
a$list_params()
a$get_param_minimum_vector()
a$params$gold$min_value <- 22 
a$params$gold$getRTUpdate()
a$params$gold$scratch$sharedX <- -10
ls(a$scratch)
a$scratch$sharedX



a$reorder_params(c("gold2", "gold1", "gold"))
a$reorder_params(c("gold2", "gold"))
a$reorder_params(c("gold2", "gold22"))

x <- a$params$gold
x$getRTUpdate()
#a$add_gold()
#a$add_dummy()


RTobs <- rnorm(100, 1000, 100)

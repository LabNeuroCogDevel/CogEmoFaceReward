\name{updateBetaDists}
\alias{updateBetaDists}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
updateBetaDists(bfs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bfs}{
%%     ~~Describe \code{bfs} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (bfs) 
{
    if (bfs$lastUpdateTrial == bfs$w$cur_trial) {
        return(invisible(NULL))
    }
    bfs$lastUpdateTrial <- bfs$w$cur_trial
    eval(quote({
        mean_fast_last <- mean_fast
        mean_slow_last <- mean_slow
        var_fast_last <- var_fast
        var_slow_last <- var_slow
        local_RT_last <- local_RT
        if (w$RT_last > local_RT_last) {
            if (w$Rew_last > w$V_last) {
                alpha_slow <- alpha_slow + 1
            } else {
                beta_slow <- beta_slow + 1
            }
        } else if (w$RT_last <= local_RT_last) {
            if (w$Rew_last > w$V_last) {
                alpha_fast <- alpha_fast + 1
            } else {
                beta_fast <- beta_fast + 1
            }
        }
        if (decay < 1) {
            alpha_slow <- decay * alpha_slow
            beta_slow <- decay * beta_slow
            alpha_fast <- decay * alpha_fast
            beta_fast <- decay * beta_fast
        }
        var_fast <- alpha_fast * beta_fast/((alpha_fast + beta_fast)^2 * 
            (alpha_fast + beta_fast + 1))
        var_slow <- alpha_slow * beta_slow/((alpha_slow + beta_slow)^2 * 
            (alpha_slow + beta_slow + 1))
        mean_slow <- alpha_slow/(alpha_slow + beta_slow)
        mean_fast <- alpha_fast/(alpha_fast + beta_fast)
        local_RT <- local_RT_last + local_RT_learning_rate * 
            (w$RT_last - local_RT_last)
    }), bfs)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

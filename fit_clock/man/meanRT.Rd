\name{meanRT}
\alias{meanRT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
meanRT(min_value = 100, max_value = 5000, init_value = 1000, cur_value = init_value, par_scale = 1000, by = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{min_value}{
%%     ~~Describe \code{min_value} here~~
}
  \item{max_value}{
%%     ~~Describe \code{max_value} here~~
}
  \item{init_value}{
%%     ~~Describe \code{init_value} here~~
}
  \item{cur_value}{
%%     ~~Describe \code{cur_value} here~~
}
  \item{par_scale}{
%%     ~~Describe \code{par_scale} here~~
}
  \item{by}{
%%     ~~Describe \code{by} here~~
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
function (min_value = 100, max_value = 5000, init_value = 1000, 
    cur_value = init_value, par_scale = 1000, by = NULL) 
{
    obj <- structure(list(name = "K"), class = c("p_meanRT", 
        "param"))
    obj <- initialize_par(obj, min_value, max_value, init_value, 
        cur_value, par_scale, by)
    return(invisible(obj))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

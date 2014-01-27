\name{initialize_par}
\alias{initialize_par}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
initialize_par(obj, min_value = NULL, max_value = NULL, init_value = NULL, cur_value = NULL, par_scale = NULL, by = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
%%     ~~Describe \code{obj} here~~
}
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
function (obj, min_value = NULL, max_value = NULL, init_value = NULL, 
    cur_value = NULL, par_scale = NULL, by = NULL) 
{
    stopifnot(cur_value <= max_value)
    stopifnot(cur_value >= min_value)
    stopifnot(init_value <= max_value)
    stopifnot(init_value >= min_value)
    obj$min_value <- min_value
    obj$max_value <- max_value
    obj$init_value <- init_value
    obj$cur_value <- cur_value
    obj$par_scale <- par_scale
    names(obj$min_value) <- names(obj$max_value) <- names(obj$init_value) <- names(obj$cur_value) <- names(obj$par_scale) <- obj$name
    obj$by <- by
    obj$w <- emptyenv()
    return(obj)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

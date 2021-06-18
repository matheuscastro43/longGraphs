\name{profile}
\alias{profile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Profile plots
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
profile(dataset, response, index, id, group = NA, legend = "group", nGroups = NULL, dolTheme = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataset}{
%%     ~~Describe \code{dataset} here~~
}
  \item{response}{
%%     ~~Describe \code{response} here~~
}
  \item{index}{
%%     ~~Describe \code{index} here~~
}
  \item{id}{
%%     ~~Describe \code{id} here~~
}
  \item{group}{
%%     ~~Describe \code{group} here~~
}
  \item{legend}{
%%     ~~Describe \code{legend} here~~
}
  \item{nGroups}{
%%     ~~Describe \code{nGroups} here~~
}
  \item{dolTheme}{
%%     ~~Describe \code{dolTheme} here~~
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
function (dataset, response, index, id, group = NA, legend = "group",
    nGroups = NULL, dolTheme = TRUE)
{
    response = as.character(substitute(response))
    index = as.character(substitute(index))
    id = as.character(substitute(id))
    group = as.character(substitute(group))
    thereGr = !is.na(group)
    aux = as.data.frame(dataset)
    names(aux)[names(aux) == response] = "response"
    names(aux)[names(aux) == index] = "index"
    names(aux)[names(aux) == id] = "id"
    if (thereGr) {
        names(aux)[names(aux) == group] = "group"
        aux$group = as.factor(aux$group)
        if (!is.null(nGroups))
            levels(aux$group) = nGroups
    }
    else group = NULL
    p = ggplot(aux, aes(x = index, y = response, group = id,
        color = group)) + geom_line() + geom_point() + labs(color = legend) +
        ylab(response) + xlab(index)
    if (dolTheme) {
        p = p + theme_bw()
    }
    return(p)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

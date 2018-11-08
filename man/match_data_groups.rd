\name{match_data_groups}
\alias{match_data_groups}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Match Data with Group Information
}
\description{
An extension of the \code{match.data} function from the \code{MatchIt} library.
Equivalent to performing \code{match.data(m.out)} with the addition of providing
the matched group data, which is provided in the \code{STRATA} variable.
}
\usage{
match_data_groups(m.out, df, join_index)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m.out}{
  The m.out object from calling 'matchit' (e.g. matchit(formula, data=prematch_df)).
  }
  \item{df}{
  The data.frame object that was called by 'matchit' to create 'm.out'.
  }
  \item{join_index}{
  The unique identifier used to left join items from the prematch data.frame to
  the postmatch data.frame.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  Returns a data.frame that has the matched data (created by 'match.data(m.out)'
  with a new column called STRATA which has the matching group information.
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
library(MatchIt)

# m.out was already created by calling 'matchit'

matched_data <- match_data_groups(m.out, prematch_data, join_index="ID")
}

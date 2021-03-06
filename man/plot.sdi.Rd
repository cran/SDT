% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.sdi.R
\name{plot.sdi}
\alias{plot.sdi}
\title{Plot Method for Objects of Class sdi}
\usage{
\method{plot}{sdi}(x, minscore = 1, maxscore = 5, ...)
}
\arguments{
\item{x}{A required object of the class \code{sdi}, obtained from a call to
the function \code{\link{sdi}}.}

\item{minscore, maxscore}{Optional numerics, integer-valued, giving the
minimum score (typically 1) or maximum score (typically 4, 5, or 7) used in
the scale procedure, respectively. See also \sQuote{Details}.}

\item{\dots}{Further arguments to be passed are ignored in this function.}
}
\value{
If the arguments \code{x}, \code{minscore}, and \code{maxscore} are
  of required types, \code{plot.sdi} produces the plot and invisibly returns
  \code{NULL}.
}
\description{
S3 method to plot objects of the class \code{sdi}.
}
\details{
This \code{plot} method graphs the results obtained from calculating the
original or adjusted SDI or RAI index. It produces a scatterplot of the
confounded or adjusted external locus component values (\eqn{y}-axis) versus
the confounded or adjusted internal locus component values (\eqn{x}-axis),
respectively. In addition, for comparison, the reference line \eqn{y = x}
(red full line) for visual inspection of the deviation of the two types of
values and the admissible range (gray dashed lines) for the original or
adjusted SDI or RAI index component values are plotted. Points with higher
original or adjusted SDI or RAI overall index values are shown in darker gray
tone.

To define and plot the admissible range, we do need the \code{minscore} and
\code{maxscore} arguments, where \code{minscore} is the minimum score used in
the scale procedure (typically 1), and \code{maxscore} is the maximum score
used in the scale procedure (typically 4, 5, or 7). Note that translation
with \sQuote{\eqn{-} \code{minscore}} and averaging are applied in the
definitions of the components of the adjusted SDI or RAI index. The
admissible range in the adjusted measure case is given by \eqn{[0, maxscore -
minscore]}, which yields \eqn{[0, 4]} for the default values. The admissible
range in the original index case is the interval \eqn{[(2 * minscore) +
minscore, (2 * maxscore) + maxscore]}, that is, \eqn{[3, 15]} for the default
values.

The function \code{\link{gray}} of the package \pkg{grDevices} is used to
plot the points in the scatterplot at different gray levels determined by
their respective original or adjusted SDI or RAI overall index values.
}
\examples{
## attach dataset to search path (so a variable can be accessed by name)
attach(learning_motivation)

## adjusted index plot
plot(sdi(intrinsic, identified, introjected, external))

## original index plot
plot(sdi(intrinsic, identified, introjected, external, compute.adjusted = FALSE))
}
\references{
Uenlue, A. (2016) Adjusting potentially confounded scoring
  protocols for motivation aggregation in organismic integration theory: An
  exemplification with the relative autonomy or self-determination index.
  \emph{Frontiers in Educational Psychology}, \bold{7}(272), 1--4. URL
  \url{https://doi.org/10.3389/fpsyg.2016.00272}.
}
\seealso{
The main function of the package, which creates objects of the class
  \code{sdi}: \code{\link{sdi}} for the original and adjusted SDI or RAI
  index. \code{\link{print.sdi}}, the S3 method for printing objects of the
  class \code{sdi}; \code{\link{summary.sdi}}, the S3 method for summarizing
  objects of the class \code{sdi}. See also \code{\link{SDT-package}} for
  general information about this package.
}
\author{
Ali Uenlue <ali.uenlue@icloud.com>
}
\keyword{hplot}
\keyword{methods}

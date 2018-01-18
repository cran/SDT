#' Self-Determination Theory Measures: The R Package SDT
#'
#' \emph{Self-determination theory} (SDT) is a theory of human motivation. The
#' package \pkg{SDT} provides functions and an example dataset for computing
#' measures of motivation internalization and of motivation simplex structure,
#' and the original and adjusted self-determination or relative autonomy index
#' in \R.
#'
#' \tabular{ll}{
#'   Package: \tab \pkg{SDT}\cr
#'   Type: \tab Package\cr
#'   Version: \tab 1.0.0\cr
#'   Date: \tab 2018-01-12\cr
#'   License: \tab \acronym{GPL} (>= 2)
#' }
#' SDT was proposed by \cite{Deci and Ryan (1985, 2000, 2002)} and is a popular
#' theory of motivation. This theory is useful for understanding the
#' motivational basis of human behaviors. The general aim is to investigate the
#' interplay between the extrinsic forces or factors acting on people (e.g.,
#' grades or payment) and the intrinsic motives or needs inherent in humans
#' (e.g., interests or enjoyment).
#'
#' Applications are numerous and are extensively referenced, with
#' comprehensive additional materials on the theory and the available
#' questionnaires, on the website \url{http://www.selfdeterminationtheory.org}.
#'
#' In particular, SDT postulated different types of motivation. As to the
#' introjected and identified regulation of extrinsic motivation, their
#' internalizations were described as \emph{\dQuote{somewhat external}} and
#' \emph{\dQuote{somewhat internal}} and remained undetermined in the theory.
#' The function \code{\link{internalization}} implements the constrained
#' regression analysis approach by \cite{Uenlue and Dettweiler (2015)} that
#' allows these vaguely expressed intermediate motivations to be estimated from
#' questionnaire data. The approach can also be generalized and applied for
#' simplex structure analysis in SDT, where the simplex structure of SDT means
#' that motivation regulation types theoretically closer to one another are more
#' strongly interrelated/correlated. Simplex structure analysis in \R is
#' provided with the function \code{\link{simplex}}. Finally, the third main
#' function \code{\link{sdi}} of the package \pkg{SDT} implements the popular
#' \emph{self-determination} or \emph{relative autonomy index} (SDI or RAI),
#' which is a scoring protocol or summary statistic aggregating individual test
#' or subscale scores to yield an overall informative measure. As discussed in
#' \cite{Uenlue (2016)}, the original SDI or RAI index is confounded (i.e.,
#' generally not accommodating biasing effects on the overall index value that
#' may result from a mixture of internal and external motivation), therefore the
#' function \code{\link{sdi}} also implements an adjusted scoring protocol
#' variant of this measure.
#'
#' The package \pkg{SDT} is implemented based on the S3 system. It comes with a
#' namespace, and consists of three main functions:
#' \code{\link{internalization}}, \code{\link{sdi}}, and \code{\link{simplex}}.
#' It also contains five functions, which are \code{plot}, \code{print}, and
#' \code{summary} methods for objects of the class \code{sdi}, and \code{plot}
#' and \code{print} methods for objects of the class \code{share}:
#' \code{\link{plot.sdi}}, \code{\link{print.sdi}}, and
#' \code{\link{summary.sdi}}, and \code{\link{plot.share}} and
#' \code{\link{print.share}}. The features of the package \pkg{SDT} are
#' illustrated with an accompanying dataset: \code{\link{learning_motivation}}.
#' @author Maintainer: Ali Uenlue <ali.uenlue@icloud.com>
#' @references
#' Deci, E. L. and Ryan, R. M. (1985) \emph{Intrinsic Motivation and
#' Self-Determination in Human Behavior}. New York, NY: Plenum. URL
#' \url{https://doi.org/10.1007/978-1-4899-2271-7}.
#'
#' Deci, E. L. and Ryan, R. M. (2000) The \dQuote{what} and \dQuote{why} of goal
#' pursuits: Human needs and the self-determination of behavior.
#' \emph{Psychological Inquiry}, \bold{11}(4), 227--268. URL
#' \url{https://doi.org/10.1207/S15327965PLI1104_01}.
#'
#' Deci, E. L. and Ryan, R. M. (Eds.) (2002) \emph{Handbook of
#' Self-Determination Research}. Rochester, NY: University of Rochester Press.
#'
#' Uenlue, A. (2016) Adjusting potentially confounded scoring protocols for
#' motivation aggregation in organismic integration theory: An exemplification
#' with the relative autonomy or self-determination index. \emph{Frontiers in
#' Educational Psychology}, \bold{7}(272), 1--4. URL
#' \url{https://doi.org/10.3389/fpsyg.2016.00272}.
#'
#' Uenlue, A. and Dettweiler, U. (2015) Motivation internalization and simplex
#' structure in self-determination theory. \emph{Psychological Reports},
#' \bold{117}(3), 675--691. URL \url{https://doi.org/10.2466/14.PR0.117c25z1}.
#'@keywords package
#'@docType package
#'@name SDT-package

NULL

################################################################################
#Extent of motivation internalization or externalization                      #
################################################################################

################################################################################
# This function computes the shares of internalization or externalization      #
# pertaining to the notion of "somewhat internal" and "somewhat external"      #
# on the self-determination theory subscales of identified regulation and      #
# introjected regulation, as the intermediate motivation regulation types;     #
# with intrinsic regulation and external regulation, the completely internal   #
# and completely external motivation poles of the theory, respectively,        #
# as the reference system spanned by these two base elements.                  #
################################################################################

#' Motivation Internalization or Externalization Shares
#'
#' \code{internalization} computes the internalization or externalization shares
#' of an intermediate motivation regulation type with respect to the poles of
#' intrinsic regulation and external regulation as the reference system.
#'
#' This function computes the shares of motivation internalization or
#' externalization pertaining to the notion of \emph{\dQuote{somewhat internal}}
#' and \emph{\dQuote{somewhat external}} on the self-determination theory
#' subscales of identified regulation and introjected regulation, as the
#' intermediate motivation regulation types---with respect to intrinsic
#' regulation and external regulation, the completely internal and completely
#' external motivation poles of the theory, respectively, as the reference
#' system spanned by these two base elements.
#'
#' The argument \code{intermediate_regulation}, the target variable of the
#' constrained regression analysis, can be either identified regulation or
#' introjected regulation aggregate subscale motivation scores, and the
#' arguments \code{intrinsic_regulation} and \code{external_regulation}, the
#' predictor variables of the constrained regression analysis, do represent
#' aggregate motivation scores for the intrinsic regulation and external
#' regulation subscales, respectively. The function
#' \code{\link[quadprog]{solve.QP}} of the package \pkg{quadprog} is applied in
#' \code{internalization} to solve the self-determination theory related
#' (convex) quadratic program. For details, see \cite{Uenlue and Dettweiler
#' (2015)}.
#'
#' @param intermediate_regulation A required numeric vector of intermediate,
#'   either identified or introjected, regulation subscale motivation scores. No
#'   \code{NA}, \code{NaN}, \code{Inf}, or \code{-Inf} values are allowed.
#' @param intrinsic_regulation,external_regulation Required numeric vectors of
#'   intrinsic regulation and external regulation subscale motivation scores,
#'   respectively. No \code{NA}, \code{NaN}, \code{Inf}, or \code{-Inf} values
#'   are allowed.
#' @return If the arguments \code{intermediate_regulation},
#'   \code{intrinsic_regulation}, and \code{external_regulation} are of required
#'   types, \code{internalization} returns a numeric vector containing the
#'   \eqn{2} named components \code{internal share} and \code{external share} of
#'   the \code{intermediate_regulation} type with respect to the extreme poles
#'   \code{intrinsic_regulation} and \code{external_regulation} of the theory.
#'   The returned object is of the class \code{share} and has the attribute
#'   \code{analysis} set to have the value \code{internalization}.
#' @author Ali Uenlue <ali.uenlue@icloud.com>
#' @references Uenlue, A. and Dettweiler, U. (2015) Motivation internalization
#'   and simplex structure in self-determination theory. \emph{Psychological
#'   Reports}, \bold{117}(3), 675--691. URL
#'   \url{https://doi.org/10.2466/14.PR0.117c25z1}.
#' @seealso The two other main functions of the package: \code{\link{simplex}}
#'   for motivation simplex structure analysis; \code{\link{sdi}} for the
#'   original and adjusted SDI or RAI index. See the methods associated with
#'   \code{internalization} as the constructor function:
#'   \code{\link{plot.share}}, the S3 method for plotting objects of the class
#'   \code{share}; \code{\link{print.share}}, the S3 method for printing objects
#'   of the class \code{share}. See also \code{\link{SDT-package}} for general
#'   information about this package.
#' @examples
#' ## attach dataset to search path (to use variable names)
#' attach(learning_motivation)
#'
#' ## internal share and external share of identified regulation
#' (idr <- internalization(identified, intrinsic, external))
#' ## attribute value and class
#' attr(idr, "analysis")
#' class(idr)
#'
#' ## internal share and external share of introjected regulation
#' (ijr <- internalization(introjected, intrinsic, external))
#' ## all attributes
#' attributes(ijr)
#' @keywords attribute classes models regression
#' @export

internalization <-
function(intermediate_regulation, intrinsic_regulation, external_regulation) {
  Y <- as.matrix(intermediate_regulation)  # target variable Y
  # the two base elements X1 and X2 of the reference system
  X <- cbind(intrinsic_regulation, external_regulation)

  D <- t(X) %*% X  # matrix D in the quadratic program
  d <- t(X) %*% Y  # vector d in the quadratic program

  # vector a and matrix A in the quadratic program
  # respectively for equality and inequality constraints
  a_A <- cbind(rep(1, 2), diag(2))
  # scalar b2 and vector b1 in the quadratic program
  # respectively for equality and inequality constraints
  b <- c(1, rep(0, 2))

  # main function to compute the internalization or externalization shares
  # first "meq = 1" constraint treated as equality constraint
  # all further, in this case 2, as inequality constraints
  est_internalization_parameter <-
  quadprog::solve.QP(Dmat = D, dvec = d, Amat = a_A, bvec = b,meq = 1)$solution
  names(est_internalization_parameter) <- c("internal share", "external share")

  # set a named object attribute indicating the internalization analysis
  attr(est_internalization_parameter, "analysis") <- "internalization"
  # introduce the class share, to provide plot and print methods
  # NOTE: objects of class share are assumed to have the attribute "analysis",
  # with values "internalization" or "simplex"
  class(est_internalization_parameter) <- "share"

  return(est_internalization_parameter)
}

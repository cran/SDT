################################################################################
# print method for objects of class share                                      #
################################################################################

#' Print Method for Objects of Class share
#'
#' S3 method to print objects of the class \code{share}.
#'
#' This \code{print} method prints the internalization, externalization, and
#' simplex structure shares of a target regulation with respect to a reference
#' system.
#'
#' @param x A required object of the class \code{share}, obtained from calls to
#'   the functions \code{\link{internalization}} and \code{\link{simplex}}.
#' @param \dots Further arguments to be passed are ignored in this function.
#' @return If the argument \code{x} is of required type, \code{print.share}
#'   prints the internalization, externalization, or simplex structure shares,
#'   and invisibly returns \code{x}, stripped off the \code{analysis} (\eqn{=}
#'   \code{internalization} or \code{simplex}) and \code{class} (\eqn{=}
#'   \code{share}) attributes.
#' @author Ali Uenlue <ali.uenlue@icloud.com>
#' @references Uenlue, A. and Dettweiler, U. (2015) Motivation internalization
#'   and simplex structure in self-determination theory. \emph{Psychological
#'   Reports}, \bold{117}(3), 675--691. URL
#'   \url{https://doi.org/10.2466/14.PR0.117c25z1}.
#' @seealso The two main functions of the package, which create objects of the
#'   class \code{share}: \code{\link{internalization}} for motivation
#'   internalization analysis; \code{\link{simplex}} for motivation simplex
#'   structure analysis. \code{\link{plot.share}}, the S3 method for plotting
#'   objects of the class \code{share}. See also \code{\link{SDT-package}} for
#'   general information about this package.
#' @examples
#' ## attach dataset to search path (to use variable names)
#' attach(learning_motivation)
#'
#' ## internalization print
#' pidr <- print(internalization(identified, intrinsic, external))
#' ## compactly displayed structure of the object pidr
#' str(pidr)
#'
#' ## simplex structure print
#' psimstr <- print(simplex(intrinsic, identified, introjected, external))
#' ## compactly displayed structure of the object psimstr
#' str(psimstr)
#' @keywords methods print
#' @export

print.share <-
function(x, ...) {
  # x: object of class share, obtained from calls to the functions
  #    internalization() and simplex()

  # internalization, externalization, and simplex structure shares are printed,
  # stripped off their "analysis" and "class" object attributes
  attr(x, "analysis") <- NULL
  attr(x, "class") <- NULL

  print(x)

  # return an invisible copy of the modified object x
  invisible(x)
}

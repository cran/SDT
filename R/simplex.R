################################################################################
# Simplex structure of motivation assessed based on optimal shares             #
################################################################################

################################################################################
# This function computes the simplex structure shares of a target motivation   #
# in a reference system consisting of at least three base regulation types,    #
# to illustrate the self-determination theory postulated simplex structure.    #
# For example, user can compute the shares of intrinsic regulation (target)    #
# with regard to the three extrinsic motivation regulation types of            #
# identified, introjected, and external regulations (base elements 1--3).      #
################################################################################

#' Motivation Simplex Structure Shares
#'
#' \code{simplex} computes the shares of a target regulation type in a reference
#' system consisting of three base regulation types to illustrate the
#' self-determination theory postulated simplex structure of motivation.
#'
#' This function computes the simplex structure shares of a target motivation
#' (e.g., intrinsic regulation) in a reference system consisting of three base
#' regulation types (e.g., identified regulation, introjected regulation, and
#' external regulation). With the function \code{simplex}, the
#' self-determination theory postulated simplex structure can be illustrated,
#' where the simplex structure of the theory means that motivation regulation
#' types theoretically closer to one another are more strongly
#' interrelated/correlated. From a theoretical viewpoint, the simplex structure
#' analysis can be viewed as a generalization of the problem of
#' \code{\link{internalization}}.
#'
#' The argument \code{target_regulation}, the dependent variable of the
#' constrained regression analysis, can be any regulation type, and the
#' arguments \code{base_regulation_1}, \code{base_regulation_2}, and
#' \code{base_regulation_3}, the independent variables of the constrained
#' regression analysis, do represent the remaining regulation types, with
#' respect to which the optimal shares of the target regulation are computed.
#' The function \code{\link[quadprog]{solve.QP}} of the package \pkg{quadprog}
#' is applied in \code{simplex} to solve the self-determination theory related
#' (convex) quadratic program. For details, see \cite{Uenlue and Dettweiler
#' (2015)}.
#'
#' @param target_regulation A required numeric vector of either intrinsic,
#'   identified, introjected, or external regulation subscale motivation scores.
#'   No \code{NA}, \code{NaN}, \code{Inf}, or \code{-Inf} values are allowed.
#' @param base_regulation_1,base_regulation_2,base_regulation_3 Required numeric
#'   vectors of, depending on the regulation type specified in
#'   \code{target_regulation}, the remaining three regulation subscale
#'   motivation scores. No \code{NA}, \code{NaN}, \code{Inf}, or \code{-Inf}
#'   values are allowed. For an example, see \sQuote{Details}.
#' @return If the arguments \code{target_regulation}, \code{base_regulation_1},
#'   \code{base_regulation_2}, and \code{base_regulation_3} are of required
#'   types, \code{simplex} returns a numeric vector containing the following
#'   \eqn{3} named components \code{base_regulation_1 share},
#'   \code{base_regulation_2 share}, and \code{base_regulation_3 share} of the
#'   \code{target_regulation} with respect to the remaining
#'   \code{base_regulation_1}, \code{base_regulation_2}, and
#'   \code{base_regulation_3} of the theory. The returned object is of the class
#'   \code{share} and has the attribute \code{analysis} set to have the value
#'   \code{simplex}.
#' @author Ali Uenlue <ali.uenlue@icloud.com>
#' @references Uenlue, A. and Dettweiler, U. (2015) Motivation internalization
#'   and simplex structure in self-determination theory. \emph{Psychological
#'   Reports}, \bold{117}(3), 675--691. URL
#'   \url{https://doi.org/10.2466/14.PR0.117c25z1}.
#' @seealso The two other main functions of the package:
#'   \code{\link{internalization}} for motivation internalization analysis;
#'   \code{\link{sdi}} for the original and adjusted SDI or RAI index. See the
#'   methods associated with \code{simplex} as the constructor function:
#'   \code{\link{plot.share}}, the S3 method for plotting objects of the class
#'   \code{share}; \code{\link{print.share}}, the S3 method for printing objects
#'   of the class \code{share}. See also \code{\link{SDT-package}} for general
#'   information about this package.
#' @examples
#' ## attach dataset to search path (to use variable names)
#' attach(learning_motivation)
#'
#' ## simplex structure analysis with intrinsic regulation as target variable
#' ## and identified, introjected, and external regulation as reference system
#' (simstr <- simplex(intrinsic, identified, introjected, external))
#'
#' ## numeric vector, attribute value, and class
#' mode(simstr)
#' attr(simstr, "analysis")
#' class(simstr)
#' @keywords attribute classes models regression
#' @export

simplex <-
function(target_regulation, base_regulation_1, base_regulation_2,
         base_regulation_3) {
  Y <- as.matrix(target_regulation)  # target variable Y
  # the three base elements X1, X2, and X3 of the reference system
  X <- cbind(base_regulation_1, base_regulation_2, base_regulation_3)

  D <- t(X) %*% X  # matrix D in the quadratic program
  d <- t(X) %*% Y  # vector d in the quadratic program

  # vector a and matrix A in the quadratic program
  a_A <- cbind(rep(1, 3), diag(3))
  # scalar b2 and vector b1 in the quadratic program
  b <- c(1, rep(0, 3))

  # main function to compute the simplex structure based on shares
  # first "meq = 1" constraint treated as equality constraint
  # all further, in this case 3, as inequality constraints
  est_simplex_parameter <-
  quadprog::solve.QP(Dmat = D, dvec = d, Amat = a_A, bvec = b, meq = 1)$solution
  names(est_simplex_parameter) <- c("base_regulation_1 share",
                                    "base_regulation_2 share",
                                    "base_regulation_3 share")

  # set a named object attribute indicating the simplex structure analysis
  attr(est_simplex_parameter, "analysis") <- "simplex"
  # introduce the class share, to provide plot and print methods
  # NOTE: objects of class share are assumed to have the attribute "analysis",
  # with values "internalization" or "simplex"
  class(est_simplex_parameter) <- "share"

  return(est_simplex_parameter)
}

################################################################################
# summary method for objects of class sdi                                      #
################################################################################

#' Summary Method for Objects of Class sdi
#'
#' S3 method to summarize objects of the class \code{sdi}.
#'
#' This \code{summary} method outlines the results obtained from the original or
#' adjusted SDI or RAI index computation by printing simple summary statistics
#' of the values obtained for the confounded or adjusted internal locus,
#' confounded or adjusted external locus, and for the original or adjusted SDI
#' or RAI overall index.
#'
#' @param object A required object of the class \code{sdi}, obtained from a call
#'   to the function \code{\link{sdi}}.
#' @param \dots Further arguments to be passed are ignored in this function.
#' @return If the argument \code{object} is of required type, \code{summary.sdi}
#'   prints simple summary statistics of the list components values, and
#'   invisibly returns \code{object}.
#' @author Ali Uenlue <ali.uenlue@icloud.com>
#' @references Uenlue, A. (2016) Adjusting potentially confounded scoring
#'   protocols for motivation aggregation in organismic integration theory: An
#'   exemplification with the relative autonomy or self-determination index.
#'   \emph{Frontiers in Educational Psychology}, \bold{7}(272), 1--4. URL
#'   \url{https://doi.org/10.3389/fpsyg.2016.00272}.
#' @seealso The main function of the package, which creates objects of the class
#'   \code{sdi}: \code{\link{sdi}} for the original and adjusted SDI or RAI
#'   index. \code{\link{plot.sdi}}, the S3 method for plotting objects of the
#'   class \code{sdi}; \code{\link{print.sdi}}, the S3 method for printing
#'   objects of the class \code{sdi}. See also \code{\link{SDT-package}} for
#'   general information about this package.
#' @examples
#' ## attach dataset to search path (for using variable names)
#' attach(learning_motivation)
#'
#' ## original and adjusted index summary
#' summary(sdi(intrinsic, identified, introjected, external, compute.adjusted = FALSE))
#' summary(sdi(intrinsic, identified, introjected, external))
#' @keywords methods univar
#' @export
#' @import stats

summary.sdi <-
function(object, ...) {
  # object: object of class sdi, obtained from a call to function sdi()

  # summary statistics of the values obtained for the confounded internal locus,
  # confounded external locus, and the original SDI (or RAI) index are printed
  if (attr(object, which = "variant", exact = TRUE) == "original") {
    cat("\n")
    cat("summary of confounded internal locus values:\n")
    print(summary(object$confounded_internal_locus))
    cat("\n")
    cat("summary of confounded external locus values:\n")
    print(summary(object$confounded_external_locus))
    cat("\n")
    cat("summary of original SDI scores:\n")
    print(summary(object$sdi_original))
    cat("\n")

    # return an invisible copy of the object (i.e., of all individual values)
    invisible(object)
  } else
    # summary statistics of the values obtained for the adjusted internal locus,
    # adjusted external locus, and the adjusted SDI (or RAI) index are printed
    if (attr(object, which = "variant", exact = TRUE) == "adjusted") {
      cat("\n")
      cat("summary of adjusted internal locus values:\n")
      print(summary(object$adjusted_internal_locus))
      cat("\n")
      cat("summary of adjusted external locus values:\n")
      print(summary(object$adjusted_external_locus))
      cat("\n")
      cat("summary of adjusted SDI scores:\n")
      print(summary(object$sdi_adjusted))
      cat("\n")

      # return an invisible copy of the object (i.e., of all individual values)
      invisible(object)
    } else
      stop("attribute \"variant\" must have value \"original\" or \"adjusted\"")
}

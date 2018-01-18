################################################################################
# print method for objects of class sdi                                        #
################################################################################

#' Print Method for Objects of Class sdi
#'
#' S3 method to print objects of the class \code{sdi}.
#'
#' This \code{print} method prints the main results obtained from calculating
#' the original or adjusted SDI or RAI index, which are the overall index values
#' or SDI or RAI scores. The output can be long, and thus, entries may be
#' omitted.
#'
#' @param x A required object of the class \code{sdi}, obtained from a call to
#'   the function \code{\link{sdi}}.
#' @param \dots Further arguments to be passed are ignored in this function.
#' @return If the argument \code{x} is of required type, \code{print.sdi} prints
#'   the original or adjusted SDI or RAI overall index values and returns an
#'   invisible copy of these scores.
#' @author Ali Uenlue <ali.uenlue@icloud.com>
#' @references Uenlue, A. (2016) Adjusting potentially confounded scoring
#'   protocols for motivation aggregation in organismic integration theory: An
#'   exemplification with the relative autonomy or self-determination index.
#'   \emph{Frontiers in Educational Psychology}, \bold{7}(272), 1--4. URL
#'   \url{https://doi.org/10.3389/fpsyg.2016.00272}.
#' @seealso The main function of the package, which creates objects of the class
#'   \code{sdi}: \code{\link{sdi}} for the original and adjusted SDI or RAI
#'   index. \code{\link{plot.sdi}}, the S3 method for plotting objects of the
#'   class \code{sdi}; \code{\link{summary.sdi}}, the S3 method for summarizing
#'   objects of the class \code{sdi}. See also \code{\link{SDT-package}} for
#'   general information about this package.
#' @examples
#' ## attach dataset to search path (to use variable names)
#' attach(learning_motivation)
#'
#' ## adjusted index print
#' padj <- print(sdi(intrinsic, identified, introjected, external))
#' ## compactly displayed structure of the object padj
#' str(padj)
#'
#' ## original index print
#' sdi(intrinsic, identified, introjected, external, compute.adjusted = FALSE)
#' @keywords methods print
#' @export

print.sdi <-
function(x, ...) {
  # x: object of class sdi, obtained from a call to function sdi()

  # the values of the original SDI (or RAI) index are printed
  if (attr(x, which = "variant", exact = TRUE) == "original") {
    cat("\n")
    cat("original SDI scores:\n")
    print(x$sdi_original)
    cat("\n")

    # return an invisible copy of the original SDI index values
    invisible(x$sdi_original)
  } else
    # the values of the adjusted SDI (or RAI) index are printed
    if (attr(x, which = "variant", exact = TRUE) == "adjusted") {
      cat("\n")
      cat("adjusted SDI scores:\n")
      print(x$sdi_adjusted)
      cat("\n")

      # return an invisible copy of the adjusted SDI index values
      invisible(x$sdi_adjusted)
    } else
      stop("attribute \"variant\" must have value \"original\" or \"adjusted\"")
}

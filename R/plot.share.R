################################################################################
# plot method for objects of class share                                       #
################################################################################

#' Plot Method for Objects of Class share
#'
#' S3 method to plot objects of the class \code{share}.
#'
#' This \code{plot} method produces stacked bar plots of the internalization,
#' externalization, or simplex structure shares of a target regulation with
#' respect to a reference system. Generic or user-specified convenient labeling
#' of the plot axes are provided.
#'
#' The default value \code{target = NULL} corresponds to generic labeling, which
#' is \dQuote{intermediate regulation} in the case of the attribute value
#' \code{analysis = internalization}, or \dQuote{target regulation} in the case
#' of \code{analysis = simplex}. If a character string is explicitly specified
#' instead, this is used to label the \eqn{x}-axis of the bar plot. See also
#' \sQuote{Examples}.
#'
#' The default value \code{reference = NULL} corresponds to generic labeling,
#' which is \dQuote{internalization share (dark)} and \dQuote{externalization
#' share (bright)} in the case of the attribute value \code{analysis =
#' internalization}, or \dQuote{base regulation 1 share (dark)}, \dQuote{base
#' regulation 2 share (medium)}, and \dQuote{base regulation 3 share (bright)}
#' in the case of \code{analysis = simplex}. If a character vector is explicitly
#' specified instead, which must be of length \eqn{2} for \code{analysis =
#' internalization} or of length \eqn{3} for \code{analysis = simplex}, this is
#' used to label the \eqn{y}-axis of the bar plot. See also \sQuote{Examples}.
#'
#' @param x A required object of the class \code{share}, obtained from calls to
#'   the functions \code{\link{internalization}} and \code{\link{simplex}}.
#' @param target An optional character string giving the label of the target
#'   regulation that is used. The default value \code{NULL} corresponds to
#'   generic labeling. See \sQuote{Details}.
#' @param reference An optional character vector giving the labels of the
#'   reference system base regulations that are used. The default value
#'   \code{NULL} corresponds to generic labeling. See \sQuote{Details}.
#' @param \dots Further arguments to be passed are ignored in this function.
#' @return If the arguments \code{x}, \code{target}, and \code{reference} are of
#'   required types, \code{plot.share} produces the plot and invisibly returns
#'   \code{NULL}.
#' @author Ali Uenlue <ali.uenlue@icloud.com>
#' @references Uenlue, A. and Dettweiler, U. (2015) Motivation internalization
#'   and simplex structure in self-determination theory. \emph{Psychological
#'   Reports}, \bold{117}(3), 675--691. URL
#'   \url{https://doi.org/10.2466/14.PR0.117c25z1}.
#' @seealso The two main functions of the package, which create objects of the
#'   class \code{share}: \code{\link{internalization}} for motivation
#'   internalization analysis; \code{\link{simplex}} for motivation simplex
#'   structure analysis. \code{\link{print.share}}, the S3 method for printing
#'   objects of the class \code{share}. See also \code{\link{SDT-package}} for
#'   general information about this package.
#' @examples
#' ## attach dataset to search path (to use variable names)
#' attach(learning_motivation)
#'
#' ## internalization plot
#' (ijr <- internalization(introjected, intrinsic, external))
#' ## with generic labels
#' plot(ijr)
#' ## with user-specified convenient labels
#' plot(ijr, target = "introjected regulation",
#'      reference = c("intrinsic regulation", "external regulation"))
#'
#' ## simplex structure plot 1
#' (simstr2 <- simplex(target_regulation = external, base_regulation_1 = intrinsic,
#'                     base_regulation_2 = identified, base_regulation_3 = introjected))
#' ## with generic labels
#' plot(simstr2)
#' ## with user-specified convenient labels
#' plot(simstr2, target = "external regulation",
#'      reference = c("intrinsic regulation", "identified regulation", "introjected regulation"))
#'
#' ## simplex structure plot 2
#' ## different target variable and reference system, conveniently labeled
#' plot(simplex(identified, intrinsic, introjected, external), target = "identified regulation",
#'      reference = c("intrinsic regulation", "introjected regulation", "external regulation"))
#' @keywords hplot methods
#' @export
#' @import graphics

plot.share <-
function(x, target = NULL, reference = NULL, ...) {
  # x: object of class share, obtained from calls to the functions
  #    internalization() and simplex()

  # stacked bar plot of the internalization or externalization shares
  # (in the case of internalization analysis)
  # stacked bar plot of the simplex structure shares
  # (in the case of simplex structure analysis)

  # the results for internalization analysis are plotted
  if (attr(x, which = "analysis", exact = TRUE) == "internalization") {
    x <- as.matrix(x)
    if (!is.null(target))
      colnames(x) <- target
    else
      colnames(x) <- "intermediate regulation"
    if (!is.null(reference)) {
      y_label <- paste(paste(reference[1], "share (dark)"),
                       paste("/", reference[2], "share (bright)"))
    } else
      y_label <- "internalization share (dark) / externalization share (bright)"

    # main bar plot
    barplot(x, main = "internalization analysis", ylab = y_label)

    # reference line y = 0.5
    abline(h = 0.5, col = "red")

    invisible(NULL)
  } else
    # the results for simplex structure analysis are plotted
    if (attr(x, which = "analysis", exact = TRUE) == "simplex") {
      x <- as.matrix(x)
      if (!is.null(target))
        colnames(x) <- target
      else
        colnames(x) <- "target regulation"
      if (!is.null(reference)) {
        y_label <- paste(paste(reference[1], "share (dark)"),
                         paste("/", reference[2], "share (medium)"),
                         paste("/", reference[3], "share (bright)"))
      } else
        y_label <- paste("base regulation 1 share (dark) /",
                         "base regulation 2 share (medium) /",
                         "base regulation 3 share (bright)")

      # main bar plot
      barplot(x, main = "simplex structure analysis", ylab = y_label)

      # reference line y = 0.5
      abline(h = 0.5, col = "red")

      invisible(NULL)
    } else
      stop(paste("attribute \"analysis\" must have value",
                 "\"internalization\" or \"simplex\""))
}

################################################################################
# plot method for objects of class sdi                                         #
################################################################################

#' Plot Method for Objects of Class sdi
#'
#' S3 method to plot objects of the class \code{sdi}.
#'
#' This \code{plot} method graphs the results obtained from calculating the
#' original or adjusted SDI or RAI index. It produces a scatterplot of the
#' confounded or adjusted external locus component values (\eqn{y}-axis) versus
#' the confounded or adjusted internal locus component values (\eqn{x}-axis),
#' respectively. In addition, for comparison, the reference line \eqn{y = x}
#' (red full line) for visual inspection of the deviation of the two types of
#' values and the admissible range (gray dashed lines) for the original or
#' adjusted SDI or RAI index component values are plotted. Points with higher
#' original or adjusted SDI or RAI overall index values are shown in darker gray
#' tone.
#'
#' To define and plot the admissible range, we do need the \code{minscore} and
#' \code{maxscore} arguments, where \code{minscore} is the minimum score used in
#' the scale procedure (typically 1), and \code{maxscore} is the maximum score
#' used in the scale procedure (typically 4, 5, or 7). Note that translation
#' with \sQuote{\eqn{-} \code{minscore}} and averaging are applied in the
#' definitions of the components of the adjusted SDI or RAI index. The
#' admissible range in the adjusted measure case is given by \eqn{[0, maxscore -
#' minscore]}, which yields \eqn{[0, 4]} for the default values. The admissible
#' range in the original index case is the interval \eqn{[(2 * minscore) +
#' minscore, (2 * maxscore) + maxscore]}, that is, \eqn{[3, 15]} for the default
#' values.
#'
#' The function \code{\link{gray}} of the package \pkg{grDevices} is used to
#' plot the points in the scatterplot at different gray levels determined by
#' their respective original or adjusted SDI or RAI overall index values.
#'
#' @param x A required object of the class \code{sdi}, obtained from a call to
#'   the function \code{\link{sdi}}.
#' @param minscore,maxscore Optional numerics, integer-valued, giving the
#'   minimum score (typically 1) or maximum score (typically 4, 5, or 7) used in
#'   the scale procedure, respectively. See also \sQuote{Details}.
#' @param \dots Further arguments to be passed are ignored in this function.
#' @return If the arguments \code{x}, \code{minscore}, and \code{maxscore} are
#'   of required types, \code{plot.sdi} produces the plot and invisibly returns
#'   \code{NULL}.
#' @author Ali Uenlue <ali.uenlue@icloud.com>
#' @references Uenlue, A. (2016) Adjusting potentially confounded scoring
#'   protocols for motivation aggregation in organismic integration theory: An
#'   exemplification with the relative autonomy or self-determination index.
#'   \emph{Frontiers in Educational Psychology}, \bold{7}(272), 1--4. URL
#'   \url{https://doi.org/10.3389/fpsyg.2016.00272}.
#' @seealso The main function of the package, which creates objects of the class
#'   \code{sdi}: \code{\link{sdi}} for the original and adjusted SDI or RAI
#'   index. \code{\link{print.sdi}}, the S3 method for printing objects of the
#'   class \code{sdi}; \code{\link{summary.sdi}}, the S3 method for summarizing
#'   objects of the class \code{sdi}. See also \code{\link{SDT-package}} for
#'   general information about this package.
#' @examples
#' ## attach dataset to search path (so a variable can be accessed by name)
#' attach(learning_motivation)
#'
#' ## adjusted index plot
#' plot(sdi(intrinsic, identified, introjected, external))
#'
#' ## original index plot
#' plot(sdi(intrinsic, identified, introjected, external, compute.adjusted = FALSE))
#' @keywords hplot methods
#' @export
#' @import graphics
#' @importFrom grDevices gray

plot.sdi <-
function(x, minscore = 1, maxscore = 5, ...) {
  # x: object of class sdi, obtained from a call to function sdi()
  # to define and plot the admissible range, we do need minscore and maxscore:
    # minscore: minimum score used in scale procedure (typically 1)
    # maxscore: maximum score used in scale procedure (typically 4, 5, or 7)

  # scatter plot of the confounded or adjusted external locus (y-axis)
  # versus the confounded or adjusted internal locus (x-axis)
  # in addition, for comparison, the reference line "y = x" (in red)
  # and the admissible range for the original or adjusted SDI component values
  # are plotted
  # points with higher original or adjusted SDI scores are shown in darker gray
  # tone

  # the results for the original SDI (or RAI) index are plotted
  if (attr(x, which = "variant", exact = TRUE) == "original") {
    # minimum and maximum locus values, the admissible range
    minlocus <- ((2 * minscore) + minscore)
    maxlocus <- ((2 * maxscore) + maxscore)

    # main scatter plot
    plot(x$confounded_internal_locus, x$confounded_external_locus,
         xlim = c(minlocus, maxlocus),
         ylim = c(minlocus, maxlocus),
         main = "original SDI index",
         xlab = "confounded internal locus",
         ylab = "confounded external locus",
         col = gray(abs(x$sdi_original - (maxlocus - minlocus))
                    / (2 * (maxlocus - minlocus) + 1)),
         pch = 20, cex = 2.5)

    # reference line y = x
    abline(a = 0, b = 1, col = "red")
    # marked-out admissible range
    abline(h = maxlocus, col = "darkgray", lwd = 2.25, lty = 2)
    abline(h = minlocus, col = "darkgray", lwd = 2.25, lty = 2)
    abline(v = maxlocus, col = "darkgray", lwd = 2.25, lty = 2)
    abline(v = minlocus, col = "darkgray", lwd = 2.25, lty = 2)

    invisible(NULL)
  } else
    # the results for the adjusted SDI (or RAI) index are plotted
    if (attr(x, which = "variant", exact = TRUE) == "adjusted") {
      # minimum and maximum locus values, the admissible range
      # Translation with "- minscore" and averaging are applied
      # in the definitions for the components of the adjusted SDI index!
      minlocus <- 0
      maxlocus <- (maxscore - minscore)

      # main scatter plot
      plot(x$adjusted_internal_locus, x$adjusted_external_locus,
           xlim = c(minlocus, maxlocus),
           ylim = c(minlocus, maxlocus),
           main = "adjusted SDI index",
           xlab = "adjusted internal locus",
           ylab = "adjusted external locus",
           col = gray(abs(x$sdi_adjusted - (maxlocus - minlocus))
                      / (2 * (maxlocus - minlocus) + 1)),
           pch = 20, cex = 2.5)

      # reference line y = x
      abline(a = 0, b = 1, col = "red")
      # marked-out admissible range
      abline(h = maxlocus, col = "darkgray", lwd = 2.25, lty = 2)
      abline(h = minlocus, col = "darkgray", lwd = 2.25, lty = 2)
      abline(v = maxlocus, col = "darkgray", lwd = 2.25, lty = 2)
      abline(v = minlocus, col = "darkgray", lwd = 2.25, lty = 2)

      invisible(NULL)
    } else
      stop("attribute \"variant\" must have value \"original\" or \"adjusted\"")
}

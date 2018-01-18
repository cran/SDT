################################################################################
# Original and Adjusted Variants of                                            #
# Self-Determination Index (SDI) or Relative Autonomy Index (RAI)              #
################################################################################

################################################################################
# This function computes the popular and original scoring protocol called the  #
# self-determination index (SDI), also known as relative autonomy index (RAI). #
# The version of this index used is for instruments assessing extrinsic        #
# motivation (excluding integrated regulation) and intrinsic motivation.       #
#                                                                              #
# Critique: The original SDI or RAI index does not allow one to account for    #
# the extent to which the identified and introjected regulation types are      #
# internal and external motivation. In particular, in the process of weighting #
# the subscale scores, the same weights are used (1 or -1, respectively).      #
# Correcting adaptations are proposed with the adjusted variant of the index,  #
# implemented in the function sdi() of the package, too.                       #
#                                                                              #
# This function also computes an adjusted variant of the original SDI or RAI   #
# index, which is weighted according to the extent to which the intermediate   #
# identified and introjected regulation types are internal and external.       #
# The used version is for instruments assessing extrinsic motivation           #
# (excluding integrated regulation) and intrinsic motivation.                  #
# This alternative adjusted SDI (or RAI) index provides correcting adaptations #
# to the original index definition, to accommodate for mixed or confounded     #
# internal and external motivation.                                            #
################################################################################

#' Original and Adjusted SDI or RAI Index
#'
#' \code{sdi} computes the original SDI or RAI scoring protocol and an adjusted
#' variant of it.
#'
#' This function provides the popular and original scoring protocol called the
#' \emph{self-determination index} (SDI), also known as the \emph{relative
#' autonomy index} (RAI). The version of the used index is for instruments
#' assessing extrinsic motivation (excluding integrated regulation) and
#' intrinsic motivation. With the SDI or RAI, the inventory scores are weighted
#' and combined to give a descriptive overall measure of the behavioral
#' self-regulatory style. The formula is, in respective regulation types:
#' \deqn{SDI = RAI = (2 * intrinsic + identified) - (2 * external +
#' introjected).}
#'
#' The original SDI or RAI index does not allow one to account for the extent to
#' which the identified and introjected regulation types are internal and
#' external motivation. In particular, in the process of weighting the subscale
#' scores, the same weights are used (1 or -1, respectively).
#'
#' Correcting adaptations are proposed to accommodate for mixed or confounded
#' internal and external motivation, implemented in the function \code{sdi}.
#' This function also computes an adjusted variant of the original SDI or RAI
#' index, which is weighted according to the extent to which the intermediate
#' identified and introjected regulation types are internal and external
#' motivation. For details, including the mathematical formula for the adjusted
#' measure, see \cite{Uenlue (2016)}.
#'
#' To compute the adjusted variant measure, \code{sdi} calls the function
#' \code{\link{internalization}}. The latter, in turn, uses the function
#' \code{\link[quadprog]{solve.QP}} of the package \pkg{quadprog} to solve the
#' corresponding constrained regression optimization problem.
#'
#' The arguments \code{intrinsic_regulation}, \code{identified_regulation},
#' \code{introjected_regulation}, and \code{external_regulation} do represent
#' aggregate subscale scores calculated by averaging the raw-data test items
#' associated with each of the four subscales (i.e., mean over the items that
#' make up a respective subscale). The four subscales are intrinsic regulation,
#' identified regulation, introjected regulation, and external regulation.
#'
#' The argument \code{minscore} only needs to be specified for the adjusted
#' index variant. Translation with \sQuote{\eqn{-} \code{minscore}} and
#' averaging are applied in the adjusted variant to ensure that the instrument
#' variables and the component and index values all range in the same interval
#' (from 0 to, e.g., 4).
#'
#' @param
#' intrinsic_regulation,identified_regulation,introjected_regulation,external_regulation
#' Required numeric vectors of intrinsic regulation, identified regulation,
#' introjected regulation, and external regulation subscale motivation scores,
#' respectively. No \code{NA}, \code{NaN}, \code{Inf}, or \code{-Inf} values are
#' allowed.
#' @param compute.adjusted An optional logical. The default value \code{TRUE}
#'   corresponds to computing the adjusted variant of the SDI or RAI index, and
#'   also allows for setting the argument \code{minscore}. If set to
#'   \code{FALSE}, the original index value is computed, and the argument
#'   \code{minscore} is irrelevant and ignored in this case.
#' @param minscore An optional numeric, integer-valued, giving the minimum score
#'   used in the scale procedure (typically 1). See also \sQuote{Details}.
#' @return If the arguments \code{intrinsic_regulation},
#'   \code{identified_regulation}, \code{introjected_regulation},
#'   \code{external_regulation}, \code{compute.adjusted}, and \code{minscore}
#'   are of required types, \code{sdi} returns a named list, of the class
#'   \code{sdi} and with the attribute \code{variant}, which consists of \eqn{3}
#'   components, independent of whether the original index computation
#'   (\code{variant} is then set to \code{original}) or the adjusted index
#'   computation (\code{variant} is then set to \code{adjusted}) was performed.
#'
#'   The original index computation list contains the following first \eqn{3}
#'   components, the adjusted index computation list the subsequent \eqn{3}
#'   components:
#'
#'   \item{confounded_internal_locus}{A numeric vector of the confounded
#'   internal locus original SDI or RAI component values.}
#'   \item{confounded_external_locus}{A numeric vector of the confounded
#'   external locus original SDI or RAI component values.} \item{sdi_original}{A
#'   numeric vector of the original SDI or RAI overall index values.}
#'   \item{adjusted_internal_locus}{A numeric vector of the adjusted internal
#'   locus adjusted SDI or RAI component values.}
#'   \item{adjusted_external_locus}{A numeric vector of the adjusted external
#'   locus adjusted SDI or RAI component values.} \item{sdi_adjusted}{A numeric
#'   vector of the adjusted SDI or RAI overall index values.}
#' @author Ali Uenlue <ali.uenlue@icloud.com>
#' @references Uenlue, A. (2016) Adjusting potentially confounded scoring
#'   protocols for motivation aggregation in organismic integration theory: An
#'   exemplification with the relative autonomy or self-determination index.
#'   \emph{Frontiers in Educational Psychology}, \bold{7}(272), 1--4. URL
#'   \url{https://doi.org/10.3389/fpsyg.2016.00272}.
#' @seealso The two other main functions of the package:
#'   \code{\link{internalization}} for motivation internalization analysis;
#'   \code{\link{simplex}} for motivation simplex structure analysis. See the
#'   methods associated with \code{sdi} as the constructor function:
#'   \code{\link{plot.sdi}}, the S3 method for plotting objects of the class
#'   \code{sdi}; \code{\link{print.sdi}}, the S3 method for printing objects of
#'   the class \code{sdi}; \code{\link{summary.sdi}}, the S3 method for
#'   summarizing objects of the class \code{sdi}. See also
#'   \code{\link{SDT-package}} for general information about this package.
#' @examples
#' ## attach dataset to search path (so a variable can be accessed by name)
#' attach(learning_motivation)
#'
#' ## adjusted index computation
#' adj <- sdi(intrinsic, identified, introjected, external)
#' ## first six elements of each list component vector and attributes
#' lapply(adj, head)
#' attributes(adj)
#'
#' ## original index computation
#' orig <- sdi(intrinsic, identified, introjected, external, compute.adjusted = FALSE)
#' lapply(orig, head)
#' attributes(orig)
#' @keywords attribute classes models univar
#' @export

sdi <-
function(intrinsic_regulation, identified_regulation,
         introjected_regulation, external_regulation,
         compute.adjusted = TRUE, minscore = 1) {
  # compute.adjusted: The default value TRUE corresponds to computing
  #                   the adjusted variant of the SDI index, and also allows
  #                   for setting the argument minscore (see below).
  #                   If compute.adjusted is set FALSE, the original SDI index
  #                   is computed, and the argument minscore is irrelevant and
  #                   ignored in this case.
  # minscore: minimum score used in scale procedure (typically 1)

  # minscore only needs to be specified for the adjusted index variant!:
    # to ensure that the instrument variables and component and index values
    # range in the same interval, translation with '- minscore' and averaging
    # are applied in the adjusted variant
    # in particular, we do need the minscore argument for the adjusted index

  # the results for the original SDI (or RAI) index are computed
  if (!compute.adjusted) {
    # confounded internal locus SDI component values
    internal_motivation <- ((2 * intrinsic_regulation) + identified_regulation)
    # confounded external locus SDI component values
    external_motivation <- ((2 * external_regulation) + introjected_regulation)

    # the original SDI index definition
    sdi_original <- (internal_motivation - external_motivation)

    results <- list(confounded_internal_locus = internal_motivation,
                    confounded_external_locus = external_motivation,
                    sdi_original = sdi_original)
  } else {
    # the results for the adjusted SDI (or RAI) index are computed

    # transform the minimum scale score, which is 'minscore,' to become 0
    intrinsic_regulation <- (intrinsic_regulation - minscore)
    identified_regulation <- (identified_regulation - minscore)
    introjected_regulation <- (introjected_regulation - minscore)
    external_regulation <- (external_regulation - minscore)

    # internal motivation share of identified regulation
    # obtained from a call to function internalization()
    int_identified <- internalization(identified_regulation, intrinsic_regulation,
                                      external_regulation)[1]
    # internal motivation share of introjected regulation
    # obtained from a call to function internalization()
    int_introjected <- internalization(introjected_regulation,intrinsic_regulation,
                                       external_regulation)[1]

    # adjusted internal locus index component values
    internal_motivation <- (intrinsic_regulation
                            + (int_identified * identified_regulation)
                            + (int_introjected * introjected_regulation)
                           ) / 3
    # adjusted external locus index component values
    external_motivation <- (external_regulation
                            + ((1 - int_identified) * identified_regulation)
                            + ((1 - int_introjected) * introjected_regulation)
                           ) / 3

    # the adjusted variant SDI or RAI index definition
    sdi_adjusted <- (internal_motivation - external_motivation)

    results <- list(adjusted_internal_locus = internal_motivation,
                    adjusted_external_locus = external_motivation,
                    sdi_adjusted = sdi_adjusted)
  }

  # set a named object attribute indicating whether the original or adjusted
  # variants of the SDI index were computed
  attr(results, "variant") <- ifelse(compute.adjusted, "adjusted", "original")
  # introduce the class sdi, to provide plot, print, and summary methods
  # NOTE: objects of class sdi are assumed to have the attribute "variant",
  # with values "adjusted" or "original"
  class(results) <- "sdi"

  return(results)
}

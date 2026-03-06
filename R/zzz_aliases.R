# Snake-case aliases for legacy camelCase exports
#
# This file provides snake_case aliases for functions that were originally
# exported with camelCase or dot-separated names. The original names remain
# available for backward compatibility.
#
# These are pure aliases (not wrappers), so they have zero performance cost.
# The original names will be deprecated in a future version.

# --- Endpoints.R ---

#' @rdname getEndpoint
#' @export
get_endpoint <- getEndpoint

#' @rdname getwilliamRes
#' @export
get_william_res <- getwilliamRes

#' @rdname contEndpoint
#' @export
cont_endpoint <- contEndpoint

#' @rdname getEC50
#' @export
get_ec50 <- getEC50

#' @rdname pavaMean
#' @export
pava_mean <- pavaMean

#' @rdname summaryZG
#' @export
summary_zg <- summaryZG

#' @rdname ECx_rating
#' @export
ecx_rating <- ECx_rating


# --- drc_Helper.R ---

#' @rdname addECxCI
#' @export
add_ecx_ci <- addECxCI

#' @rdname ED.plus
#' @export
ed_plus <- ED.plus

#' @rdname ED.ZG
#' @export
ed_zg <- ED.ZG

#' @rdname mselect.plus
#' @export
mselect_plus <- mselect.plus

#' @rdname mselect.ZG
#' @export
mselect_zg <- mselect.ZG

#' @rdname mselect.ED
#' @export
mselect_ed <- mselect.ED

#' @rdname drcCompare
#' @export
drc_compare <- drcCompare

#' @rdname getModelName
#' @export
get_model_name <- getModelName

#' @rdname calcSteepnessOverlap
#' @export
calc_steepness_overlap <- calcSteepnessOverlap

#' @rdname calcNW
#' @export
calc_nw <- calcNW


# --- dose_response_simulation.R ---

#' @rdname simDRdata
#' @export
sim_dr_data <- simDRdata


# --- stepdown_binom.R ---

#' @rdname cochranArmitageTrendTest
#' @export
cochran_armitage_trend_test <- cochranArmitageTrendTest

#' @rdname stepDownTrendTestBinom
#' @export
step_down_trend_test_binom <- stepDownTrendTestBinom


# --- preliminary.R ---

#' @rdname prelimPlot1
#' @export
prelim_plot_1 <- prelimPlot1

#' @rdname prelimPlot2
#' @export
prelim_plot_2 <- prelimPlot2

#' @rdname prelimPlot3
#' @export
prelim_plot_3 <- prelimPlot3

#' @rdname prelimSummary
#' @export
prelim_summary <- prelimSummary


# --- reshape_drc_data.R ---

#' @rdname reshape_drcData
#' @export
reshape_drc_data <- reshape_drcData


# --- data_helper.R ---

#' @rdname simplifyTreatment
#' @export
simplify_treatment <- simplifyTreatment


# --- ordinal.R ---

#' @rdname backCalcSE
#' @export
back_calc_se <- backCalcSE

#' @rdname dose.p.glmmPQL
#' @export
dose_p_glmm_pql <- dose.p.glmmPQL


# --- williams_JT.R ---

#' @rdname getComparison
#' @export
get_comparison <- getComparison


# --- quantal_tests.R ---

#' @rdname Tarone.test
#' @export
tarone_test <- Tarone.test


# --- overdispersion_binom.R ---

#' @rdname Tarone.trend.test
#' @export
tarone_trend_test <- Tarone.trend.test


# --- SK_TSK_tests_wrapper.R ---

#' @rdname SpearmanKarber_modified
#' @export
spearman_karber_modified <- SpearmanKarber_modified


# --- stepDownTrendTest_wrapper.R ---

#' @rdname stepDownTrendTest_NOEC
#' @export
step_down_trend_test_noec <- stepDownTrendTest_NOEC

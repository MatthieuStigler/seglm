#' Indicate the regime/segment in which each observation is
#'
#' @param object An object of class *seg_lm*
#' @param ... currently unused
#' @return A vector
#' @examples
#' data_thresh <- sim_thresh()
#' mod_seg <- seglm_lm(formula = y~x, data = data_thresh, th_var = "x", nthresh =1)
#' head(regime(mod_seg))
#' @export


regime <-  function(object, ...) UseMethod("regime")

#' @rdname regime
#' @export
regime.default <-  function(object, ...) object$regime

#' Extract the threshold (aka breakpoints) value(s)
#'
#' @param object An object of class *seg_lm*
#' @param ... currently unused
#' @return A vector
#' @examples
#' data_thresh <- sim_thresh()
#' mod_seg <- seglm_lm(formula = y~x, data = data_thresh, th_var = "x", nthresh =1)
#' get_th(mod_seg)
#' @export
get_th <-  function(object, ...) UseMethod("get_th")

#' @rdname get_th
#' @export
get_th.default <-  function(object, ...) object$th_val

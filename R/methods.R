#' Indicate the regime/segment in which each observation is
#'
#' @param object An object of class *seg_lm*
#' @param ... currently unused
#' @return A vector
#' @examples
#' data_thresh <- sim_thresh()
#' mod_seg <- seglm(formula = y~x, data = data_thresh, th_var = "x", nthresh =1)
#' head(regime(mod_seg))
#' @export


regime <-  function(object, ...) UseMethod("regime")

#' @rdname regime
#' @export
regime.default <-  function(object, ...) object$regime

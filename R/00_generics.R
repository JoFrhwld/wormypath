#' wormpredict
#' @import S7
#' @export
wormpredict <- S7::new_generic("wormpredict",
                           dispatch_args = c("model"))

get_model_dims <- S7::new_generic("get_model_dims",
                              dispatch_args = c("model"))

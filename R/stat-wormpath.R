#' @import ggplot2
#' @inheritParams layer
#' @inheritParams ggplot2::geom_point
#' @importFrom ggplot2 layer
#' @export
stat_wormpath <- function(
    mapping = NULL,
    data = NULL,
    geom = "path",
    position = "identity",
    lineend = "round",
    key_glyph = draw_key_wormpath,
    ...,
    n = 100,
    formula = NULL,
    method = NULL,
    method.args = list(),
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
){
  layer(
    data = data,
    mapping = mapping,
    stat = StatWormpath,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      n = n,
      formula = formula,
      method = method,
      method.args = list(),
      lineend = lineend,
      key_glyph = key_glyph,
      ...
    )
  )
}


#' @importFrom ggplot2 ggproto
StatWormpath <- ggproto("StatWormpath", Stat,
  setup_params = function(data, params){
    if(is.null(params$method)){
      params$method <- "gam"
    }

    if(is.null(params$formula)){
      if(identical(params$method, "gam")){
        params$formula <- list(x ~ s(z), y ~ s(z))
      }
    }

    if((!"family" %in% params$method.args) & identical(params$method, "gam")){
      params$method.args <- append(
        params$method.args,
        list(family = mvn(d = 2))
      )
    }

    if(identical(params$method, "gam")){
      params$method <- ggplot2:::gam_method()
    }

    params
  },

  compute_group = function(data, scales, n = 100,
                           method = NULL,
                           formula = NULL,
                           method.args = list(),
                           na.rm = FALSE){
    fit_worm(data = data, n = n, method = method, formula = formula,
             method.args = method.args)
  },

  required_aes = c("x", "y", "z"),
  optional_aes = c("colour_ramp", "color_ramp"),
  default_aes = aes(linewidth = after_stat(inv_speed))

)

#' @import mgcv
#' @import marginaleffects
fit_worm <- function(data, n, method, formula, method.args){
  if(is.character(method)){
    method = match.fun(method)
  }

  model <-inject(method(
    formula,
    data = data,
    !!!method.args
  ))

  ## Here is where a general
  ## wormfit(model) would go

  predgrid <- datagrid(
    model = model,
    z = seq(min(data$z), max(data$z), length.out = n)
  )

  predictions(
    model,
    newdata = predgrid
  ) |>
    as.data.frame() |>
    dplyr::select(rowid,
                  group,
                  estimate) |>
    dplyr::mutate(
      group = c("x", "y")[as.numeric(group)]
    ) |>
    tidyr::pivot_wider(
      names_from = group,
      values_from = estimate
    ) ->
    model_pred



  slopes(
    model,
    newdata = predgrid,
    variables = "z"
  ) |>
    as.data.frame() |>
    dplyr::select(rowid,
                  group,
                  z,
                  estimate) |>
    dplyr::mutate(
      group = c("x", "y")[as.numeric(group)]
    ) |>
    tidyr::pivot_wider(
      names_from = group,
      values_from = estimate
    ) |>
    dplyr::mutate(
      speed = sqrt(x^2 + y^2),
      inv_speed = 1/speed
    ) |>
    dplyr::select(-x, -y) ->
    model_slopes

  model_pred |>
    dplyr::left_join(model_slopes, by = dplyr::join_by(rowid == rowid))

}


#' @import grid
draw_key_wormpath <- function (data, params, size)
{
  if (is.null(data$linetype)) {
    data$linetype <- 0
  }
  else {
    data$linetype[is.na(data$linetype)] <- 0
  }
  segmentsGrob(
    0.1, 0.5, 0.9, 0.5,
    gp = gpar(col = alpha(data$colour %||%
                            data$fill %||% "black",
                          data$alpha),
              fill = alpha(params$arrow.fill %||%
                             data$colour %||%
                             data$fill %||% "black",
                           data$alpha),
              lwd = (data$linewidth %||% 0.5) * .pt,
              lty = data$linetype %||% 1,
              lineend = "butt"),
    arrow = params$arrow)
}

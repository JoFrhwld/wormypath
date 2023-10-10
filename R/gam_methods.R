#' @import S7
S7::method(wormpredict, gam_class) <- function(model, along, by = NULL, n = 10){

  along <- ensym(along)
  model_dim_name <- get_model_dims(model)

  if(!length(model_dim_name) == 2){
    cli::cli_abort(
      c("gam model must have two outcome variables.",
        "provided model has {length(model_dim_name)} outcome variables."
        )
    )
  }

  # setup argument list for datagrid
  list(
    model,
    function(x) modelr::seq_range(x, n = n)
  ) |>
    set_names(
      c(sym("model"), as_name(along))
    ) -> args

  # include any by args
  args <- append(args, by)

  # create datagrid
  call2(
    "datagrid",
    !!!args
  ) |>
    eval() ->
    newgrid

  #  model predictions
  predictions(
    model,
    newdata = newgrid
  ) |>
    tibble::as_tibble()->
    mod_preds


  # model slopes
  slopes(
    model,
    newdata = newgrid,
    variables = as_string(along)
  ) |>
    tibble::as_tibble() ->
    mod_slopes

  # distances from slope
  mod_slopes |>
    dplyr::select(rowid, group, estimate) |>
    tidyr::pivot_wider(
      names_from = group,
      values_from = estimate
    ) |>
    dplyr::mutate(
      .dist = sqrt(`1`^2 + `2`^2)
    ) |>
    dplyr::select(
      -`1`, -`2`
    )->
    mod_dists

    # output
    mod_preds |>
      dplyr::left_join(mod_dists) |>
      dplyr::select(rowid, group, estimate, .dist, names(newgrid)) |>
      dplyr::select(-all_of(model_dim_name))->
      out

    return(out)

}




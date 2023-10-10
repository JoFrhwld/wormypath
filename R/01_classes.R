#' @importFrom S7 as_class
gam_class <- S7::as_class(
  S7::new_S3_class("gam")
)

wormplot <- S7::new_class(
  "wormplot",
  properties = list(
    model = S7::new_union(gam_class)
  )
)

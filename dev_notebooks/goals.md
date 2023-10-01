# Project Goals

## Goals

A package to generate wormy paths.

## How will it work?

### From data -\> plot

``` r
# not run
data |> 
  ggplot(aes(x, y, z))+
    stat_wormpath()
```

``` r
# not run 
data |> 
  ggplot(aes(x, y, z))+
    stat_wormpath(
      aes(color = z, 
          linewidth = after_stat(inv_dist)),
      method = mgcv,
      formula = list(x ~ s(z), y ~ s(z)),
      method.args = list(family = mvn(d=2))
    )
```

``` r
# not run 
data |> 
  ggplot(aes(x, y, z))+
    stat_wormpath(
      aes(color = z,
          linewidth = after_stat(inv_dist)),
      method = brms,
      formula = mvbind(x,y) ~ s(z) + set_rescor(TRUE),
      method.args = list(file = "ggplotfit", cores = 4)
      ## the file argument would have to be augmented to 
      ## reflect the grouping...
    )
```

``` r
# not run 
pre_computed |> 
  ggplot(aes(x, y, z = z))+
    geom_wormpath(
      aes(color = z,
          linewidth = inv_dist)
    )
```

### From model -\> plot

``` r
gam(
  list(
    x ~ s(z),
    y ~ s(z)
  ),
  data = data,
  family = mvn(d=2)
)->model

wormplot(
  model,
  variable = "z",
  n = 100
)
```

``` r
gam(
  list(
    x ~ s(z) + g,
    y ~ s(z) + g
  ),
  data = data,
  family = mvn(d=2)
)->model

wormplot(
  model,
  variable = "z",
  by = "g",
  n = 100
)
```

## Ideas

``` r
library(palmerpenguins)
library(mgcv)
```

    Loading required package: nlme

    This is mgcv 1.9-0. For overview type 'help("mgcv-package")'.

``` r
library(ggplot2)
library(marginaleffects)
```

    Warning: package 'marginaleffects' was built under R version 4.3.1

### with mgcv

``` r
gam(
  list(
    bill_length_mm ~ s(body_mass_g),
    bill_depth_mm ~ s(body_mass_g)
  ),
  data = penguins,
  family = mvn(d = 2)
)->model
```

``` r
predictions(model, 
            newdata = datagrid(body_mass_g = \(x){
              modelr::seq_range(x, n = 100)
            }
            )
) |> 
  as.data.frame() |> 
  dplyr::select(rowid, 
                group, 
                estimate) |> 
  tidyr::pivot_wider(names_from = "group", 
                     values_from = "estimate") |> 
  ggplot(aes(`1`, `2`))+
  geom_point()+
    geom_path()
```

<img src="goals_files/figure-commonmark/unnamed-chunk-9-1.png"
class="quarto-discovered-preview-image" />

``` r
mod_preds <- predictions(model,
                         newdata = datagrid(body_mass_g = \(x) {
                           modelr::seq_range(x, n = 100)
                         }
                         )
)
mod_slopes <- slopes(model, 
                     newdata = datagrid(body_mass_g = \(x){
                       modelr::seq_range(x, n = 100)
                     }
                     )
)
```

``` r
mod_slopes |> 
  as.data.frame() |> 
  dplyr::select(rowid, 
                group, 
                body_mass_g, 
                estimate) |> 
  tidyr::pivot_wider(names_from = "group",
                     values_from = "estimate") |> 
  dplyr::mutate(
    dist = sqrt(`1`^2 + `2`^2)
  ) |> 
  dplyr::select(rowid, dist) ->
  distances
```

``` r
mod_preds |> 
  as.data.frame() |> 
  dplyr::select(rowid, 
                group, 
                body_mass_g, 
                estimate) |> 
  tidyr::pivot_wider(names_from = "group", 
                     values_from = "estimate") |>
  dplyr::left_join(distances) |> 
  ggplot(aes(`1`, `2`))+
    geom_point(aes(size = 1/dist, color = body_mass_g))+
    geom_path()
```

    Joining with `by = join_by(rowid)`

![](goals_files/figure-commonmark/unnamed-chunk-12-1.png)

``` r
mod_preds |> 
  as.data.frame() |> 
  dplyr::select(rowid, 
                group, 
                body_mass_g, 
                estimate) |> 
  tidyr::pivot_wider(names_from = "group",
                     values_from = "estimate") |>
  dplyr::left_join(distances) |> 
  ggplot(aes(`1`, `2`))+
    #geom_point(aes(size = 1/dist, color = body_mass_g))+
    geom_path(aes(linewidth = 1/dist,
                  color = body_mass_g),
              lineend = "round")+
    scale_color_viridis_c()
```

    Joining with `by = join_by(rowid)`

![](goals_files/figure-commonmark/unnamed-chunk-13-1.png)

### with brms

``` r
library(brms)
```

    Warning: package 'brms' was built under R version 4.3.1

    Loading required package: Rcpp

    Loading 'brms' package (version 2.20.4). Useful instructions
    can be found by typing help('brms'). A more detailed introduction
    to the package is available through vignette('brms_overview').


    Attaching package: 'brms'

    The following objects are masked from 'package:mgcv':

        s, t2

    The following object is masked from 'package:stats':

        ar

``` r
brm(
  mvbind(bill_length_mm, bill_depth_mm) ~ s(body_mass_g),
  data = penguins,
  backend = "cmdstanr",
  file = "brm0",
  cores = 4
) ->
  brm0_mod
```

``` r
predictions(brm0_mod, 
            newdata = datagrid(body_mass_g = \(x){
              modelr::seq_range(x, n = 100)
            }
            )
) |> 
  as.data.frame() |> 
  dplyr::select(rowid, 
                group, 
                estimate) |> 
  tidyr::pivot_wider(names_from = "group", 
                     values_from = "estimate") |> 
  ggplot(aes(billlengthmm, billdepthmm))+
  geom_point()+
  geom_path()
```

![](goals_files/figure-commonmark/unnamed-chunk-16-1.png)

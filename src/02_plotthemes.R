## `bayesplot` package settings and wrapper for parcoord plots
color_scheme_set('darkgray')
div_style <- parcoord_style_np(div_color = 'green',
                               div_size = 0.25,
                               div_alpha = 0.4)
stanfit_parcoord <- function(stanfit, transform = scale,
                             size = 0.25, alpha = 0.1,
                             np_style = div_style, ...) {
  draws <- as.array(stanfit)
  np <- nuts_params(stanfit)
  mcmc_parcoord(draws, transform = transform,
                size = size, alpha = alpha,
                np = np, np_style = np_style, ...)
}

theme_jkb <- function(base_size = 11,
                      title_family = "Arial",
                      base_family = "Arial") {
                      ## title_family = "Rubik",
                      ## base_family = "Montserrat") {
  sizes <- c(8, 9, 10, 11, 12, 14, 16, 18, 20, 24, 28, 32)
  base_idx <- which(base_size == sizes)
  theme(
    text = element_text(family = base_family,
                        size = base_size),
    plot.margin = unit(c(0.5, 0.75, 0.5, 0.25), "cm"),
    ## Title
    title = element_text(family = title_family,
                         size = sizes[base_idx + 3]),
    ## Panel
    panel.background = element_blank(),
    panel.grid = element_blank(),
    ## Axes
    axis.line = element_line(),
    axis.title = element_text(family = title_family,
                              size = sizes[base_idx + 1]),
    axis.text = element_text(),
    ## Facet labels
    strip.text = element_text(size = sizes[base_idx + 1]),
    strip.background = element_blank(),
    ## Legend labels
    legend.title = element_text(family = title_family,
                                size = sizes[base_idx + 1]),
    legend.text = element_text(),
    legend.key = element_blank())
}

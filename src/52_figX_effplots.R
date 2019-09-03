## Load diagnostics
fullPT_diagnostics <- readRDS("results/fullPT_diagnostics.Rds")
fixedPT_diagnostics <- readRDS("results/fixedPT_diagnostics.Rds")
Schaefer_diagnostics <- readRDS("results/Schaefer_diagnostics.Rds")

## Vector of model names in order for setting order as factor
model_levels <- c("Centered", "Noncentered", "Marginal q", "Explicit F",
                  "Explicit F marg q", "Constrained P")

## Function to produce the efficiency plots
make_effplot <- function(df_diagnostics) {
  models <- unique(df_diagnostics$model_name)
  if (length(models) == 5) {
    colors = PT_colors
    df_diagnostics$model_name <- factor(df_diagnostics$model_name,
                                        levels = model_levels[1:5])
  } else if (length(models) == 6) {
    colors <- Schaefer_colors
    df_diagnostics$model_name <- factor(df_diagnostics$model_name,
                                        levels = model_levels)
  } else {
    stop("Wrong number of different models")
  }

  ## Set breaks manually over orders of magnitude
  breaks <- 10^(-5:5)

  df_diagnostics %>%
    mutate(whiteout = ifelse(div, TRUE, NA)) %>%
    ggplot(aes(x = adapt_delta,
               y = ess_rate,
               color = model_name,
               group = model_name)) +
    geom_line(size = 0.5) +
    geom_point(data = filter(df_diagnostics, div),
               size = 2, shape = 1, stroke = 0.5) +
    geom_point(data = filter(df_diagnostics, !div),
               size = 2.5, shape = 19) +
               ## show.legend = FALSE) +
    scale_color_manual(#name = "Parameterization",
                       values = colors) +
    xlab("Target acceptance rate") +
    scale_y_log10(name = "Effectively independent samples per second",
                  breaks = breaks,
                  labels = breaks) +
    theme_jkb(base_size = 8) +
    theme(legend.position = "bottom",
          legend.title = element_blank())
}

## Save a TIFF for Word, and a PDF as a high quality vector image for publication
fullPT_effplot <- make_effplot(fullPT_diagnostics)
ggsave("figs/figX_fullPT_effplot.tiff", fullPT_effplot, width = 6, height = 4)
ggsave("figs/figX_fullPT_effplot.pdf", fullPT_effplot, device = cairo_pdf,
       width = 6, height = 4)

fixedPT_effplot <- make_effplot(fixedPT_diagnostics)
ggsave("figs/figX_fixedPT_effplot.tiff", fixedPT_effplot, width = 6, height = 4)
ggsave("figs/figX_fixedPT_effplot.pdf", fixedPT_effplot, device = cairo_pdf,
       width = 6, height = 4)

Schaefer_effplot <- make_effplot(Schaefer_diagnostics)
ggsave("figs/figX_Schaefer_effplot.tiff", Schaefer_effplot, width = 6, height = 4)
ggsave("figs/figX_Schaefer_effplot.pdf", Schaefer_effplot, device = cairo_pdf,
       width = 6, height = 4)


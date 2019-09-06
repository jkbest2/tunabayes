source("src/50_makefigs.R")

## Munge data frames for efficiency plots (figure 3)
reshape_effs <- function(df_diag) {
  df_diag %>%
    select(model_name, adapt_delta, ess_rate, div_total, div, summary)
}

## Get standardized data frames for each dynamics specification
fullPT_effs <- reshape_effs(fullPT_diagnostics) %>%
  mutate(dyn = factor("P-T, estimated m", levels = dyn_levels))
fixedPT_effs <- reshape_effs(fixedPT_diagnostics) %>%
  mutate(dyn = factor("P-T, fixed m", levels = dyn_levels))
Schaefer_effs <- reshape_effs(Schaefer_diagnostics) %>%
  mutate(dyn = factor("Schaefer", levels = dyn_levels))

## Put into a single data frame
all_effs <- bind_rows(fullPT_effs,
                       fixedPT_effs,
                       Schaefer_effs) %>%
  ## mutate(rhat = map(summary, ~ .x$summary[, 10]),
  ##        max_rhat = map_dbl(rhat, min, na.rm = TRUE),
  ##        min_rhat = map_dbl(rhat, max, na.rm = TRUE))
  filter(adapt_delta > 0.75)

## Set breaks manually over orders of magnitude
breaks <- 10^(-5:5)

## Plot it
effplot <- all_effs %>%
  filter(adapt_delta > 0.75) %>%
  ggplot(aes(x = adapt_delta, y = ess_rate,
             color = model_name,
             shape = div,
             group = model_name
             )) +
  geom_line(size = 0.5, alpha = 0.25) +
  geom_point(data = filter(all_effs, div),
             shape = 1, size = 1.5, stroke = 0.3) +
  geom_point(data = filter(all_effs, !div),
             shape = 19, size = 1.5) +
  scale_color_manual(values = param_colors) +
  xlab("Target acceptance rate") +
  guides(shape = FALSE,
         color = guide_legend(title = "", nrow = 1L)) +
  scale_x_continuous(breaks = ad_vals[1:9],
                     labels = c(ad_vals[1:6], "", "", ad_vals[9]),
                     minor_breaks = NULL) +
  scale_y_log10(name = "Effectively independent samples per second",
                breaks = breaks,
                labels = breaks,
                expand = expand_scale(),
                limits = c(0.007, 1000)) +
  facet_wrap(~ dyn) +
  theme_jkb(base_size = 8) +
  theme(plot.margin = margin(l = 1, r = 2),
        plot.title = element_blank(),
        axis.ticks = element_line(size = 0.2),
        axis.line = element_line(size = 0.2),
        strip.text.x = element_text(vjust = 0,
                                    margin = margin(b = 3)),
        legend.position = "bottom",
        legend.margin = margin(t = -10))
effplot

## Save a TIFF for Word, and a PDF as a high quality vector image for publication
ggsave("figs/fig3_effplot.tiff", effplot, width = 6, height = 4)
ggsave("figs/fig3_effplot.pdf", effplot, device = cairo_pdf,
       width = 6, height = 4)


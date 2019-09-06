source("src/50_makefigs.R")

## Load data frames of diagnostics
fullPT_diagnostics <- readRDS("results/fullPT_diagnostics.Rds") %>%
  mutate(dyn = factor("P-T, estimated m", levels = dyn_levels))
fixedPT_diagnostics <- readRDS("results/fixedPT_diagnostics.Rds") %>%
  mutate(dyn = factor("P-T, fixed m", levels = dyn_levels))
Schaefer_diagnostics <- readRDS("results/Schaefer_diagnostics.Rds") %>%
  mutate(dyn = factor("Schaefer", levels = dyn_levels))

## Get standardized data frames for each dynamics specification
## fullPT_diags <- reshape_diags(fullPT_diagnostics) %>%
## fixedPT_diags <- reshape_diags(fixedPT_diagnostics) %>%
## Schaefer_diags <- reshape_diags(Schaefer_diagnostics) %>%

## And in the darkness bind them
all_diagnostics <- bind_rows(fullPT_diagnostics,
                       fixedPT_diagnostics,
                       Schaefer_diagnostics) %>%
  filter(adapt_delta > 0.75) %>%
  select(dyn, model_name, adapt_delta, div_total, div)

## Extract to use for breaks and labels
ad_vals <- unique(all_diags$adapt_delta)

## Make the plot
diag_plot <- all_diagnostics %>%
  ggplot(aes(x = adapt_delta,
             y = div_total,
             color = model_name)) +
  geom_line(alpha = 0.25) +
  ## geom_point() +
  geom_point(data = filter(all_diagnostics, div),
             shape = 1, stroke = 0.25) +
  geom_point(data = filter(all_diagnostics, !div),
             shape = 19, size = 1) +
  geom_hline(yintercept = 0, size = 0.2) +
  scale_x_continuous(breaks = ad_vals[1:9],
                     labels = c(ad_vals[1:6], "", "", ad_vals[9]),
                     minor_breaks = NULL) +
  scale_y_continuous(expand = expand_scale(mul = c(0, 0.1))) +
  scale_color_manual(values = param_colors) +
  facet_grid(model_name ~ dyn, scales = "free_y") +
  theme_jkb(8) +
  labs(x = "Target acceptance rate",
       y = "Number of divergent transitions",
       color = "", fill = "") +
  theme(legend.position = "none",
        plot.margin = margin(l = 1),
        axis.ticks = element_line(size = 0.2),
        axis.line.x = element_line(size = 0.2),
        axis.line.y = element_line(size = 0.2),
        strip.text.y = element_text(angle = 0,
                                    size = 7,
                                    hjust = 0,
                                    margin = margin()),
        strip.text.x = element_text(vjust = 0,
                                    margin = margin(b = 3)),
        plot.title = element_blank())

## Save as PDF for evaluation
ggsave("figs/fig2_diagplot.pdf", diag_plot, device = cairo_pdf,
       width = 6, height = 4)
## Save as SVG for final editing
ggsave("figs/fig2_diagplot.svg", diag_plot, width = 6, height = 4)


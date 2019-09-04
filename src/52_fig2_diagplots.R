source("src/50_makefigs.R")

## Load data frames of diagnostics
fullPT_diagnostics <- readRDS("results/fullPT_diagnostics.Rds")
fixedPT_diagnostics <- readRDS("results/fixedPT_diagnostics.Rds")
Schaefer_diagnostics <- readRDS("results/Schaefer_diagnostics.Rds")

## plot_diagnostics <- function(df_diag) {
## }

## Get standardized data frames for each dynamics specification
fullPT_diags <- reshape_diags(fullPT_diagnostics) %>%
  mutate(dyn = factor("P-T, estimated m", levels = dyn_levels))
fixedPT_diags <- reshape_diags(fixedPT_diagnostics) %>%
  mutate(dyn = factor("P-T, fixed m", levels = dyn_levels))
Schaefer_diags <- reshape_diags(Schaefer_diagnostics) %>%
  mutate(dyn = factor("Schaefer", levels = dyn_levels))

## And in the darkness bind them
all_diags <- bind_rows(fullPT_diags,
                       fixedPT_diags,
                       Schaefer_diags) %>%
  mutate(dyn = factor(dyn, levels = dyn_levels))

## Extract to use for breaks and labels
ad_vals <- unique(all_diags$adapt_delta)

## Make the plot
diag_plot <- all_diags %>%
  filter(adapt_delta > 0.75) %>%
  filter(diagnostic == "div") %>%
  mutate(div0 = number > 0) %>%
  ggplot(aes(x = adapt_delta, y = number,
             color = model_name,
             fill = model_name,
             shape = div0,
             linetype = diagnostic
             ## group = diagnostic
             ## shape = diagnostic, linetype = diagnostic)) +
             )) +
  geom_line(alpha = 0.25) +
  geom_point() +
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

## Save as SVG for final editing
ggsave("figs/fig2_diagplot.svg", diag_plot, width = 6, height = 4)


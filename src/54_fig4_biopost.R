source("src/50_makefigs.R")

## Load summaries of each dynamics
fullPT_summaries <- readRDS("results/fullPT_summaries.Rds") %>%
  mutate(dyn = factor("P-T, estimated m", levels = dyn_levels),
         model_name = factor(model_name, levels = param_levels)) %>%
  left_join(readRDS("results/fullPT_diagnostics.Rds"),
            by = c("model_name", "adapt_delta"))

fixedPT_summaries <- readRDS("results/fixedPT_summaries.Rds") %>%
  mutate(dyn = factor("P-T, fixed m", levels = dyn_levels),
         model_name = factor(model_name, levels = param_levels)) %>%
  left_join(readRDS("results/fixedPT_diagnostics.Rds"),
            by = c("model_name", "adapt_delta"))

Schaefer_summaries <- readRDS("results/Schaefer_summaries.Rds") %>%
  mutate(dyn = factor("Schaefer", levels = dyn_levels),
         model_name = factor(model_name, levels = param_levels)) %>%
  left_join(readRDS("results/Schaefer_diagnostics.Rds"),
            by = c("model_name", "adapt_delta"))

all_summaries <- cross_df(list(dyn = factor(dyn_levels,
                                            levels = dyn_levels),
                               model_name = factor(param_levels,
                                                   levels = param_levels)),
                          .filter = function(dyn, model_name)
                            model_name == "Constrained P" && grepl("P-T", dyn)) %>%
  left_join(bind_rows(fullPT_summaries,
                      fixedPT_summaries,
                      Schaefer_summaries),
            by = c("dyn", "model_name")) %>%
  select(dyn, model_name, adapt_delta, post_qtiles,
         min_ess, ess_rate, div_total)

post_summaries <- all_summaries %>%
  group_by(dyn, model_name) %>%
  arrange(div_total, desc(ess_rate), .by_group = TRUE) %>%
  slice(1)

nr <- nrow(post_summaries)

extract_biomass <- function(lst) {
  idx <- grepl("Biomass.*", names(lst))
  lst[idx]
}

bio_summaries <- post_summaries %>%
  mutate(bio_qtiles = map(post_qtiles, extract_biomass)) %>%
  unnest(bio_qtiles) %>%
  mutate(year = rep(1967:1990)) %>%
  mutate(p025 = map_dbl(bio_qtiles, 1),
         p05 = map_dbl(bio_qtiles, 2),
         p10 = map_dbl(bio_qtiles, 3),
         p25 = map_dbl(bio_qtiles, 4),
         p50 = map_dbl(bio_qtiles, 5),
         p75 = map_dbl(bio_qtiles, 6),
         p90 = map_dbl(bio_qtiles, 7),
         p95 = map_dbl(bio_qtiles, 8),
         p975 = map_dbl(bio_qtiles, 9)) %>%
  mutate(divd = div_total == 0,
         divc = as.numeric(divd)) %>%
  select(dyn, model_name, divd, divc, year,
         starts_with("p"))


bp_dodge <- position_dodge(width = 0.75)

biopost_plot <- bio_summaries %>%
  ggplot(aes(x = year, y = p50,
             color = model_name,
             group = model_name,
             alpha = divc)) +
  facet_grid(dyn ~ .) +
  geom_linerange(aes(ymin = p05, ymax = p25),
                 position = bp_dodge, size = 0.5) +
  geom_linerange(aes(ymin = p25, ymax = p75),
                 position = bp_dodge, size = 0.75) +
  geom_linerange(aes(ymin = p75, ymax = p95),
                 position = bp_dodge, size = 0.5) +
  geom_point(position = bp_dodge,
             size = 2.5, shape = "-",
             color = "black") +
  geom_hline(yintercept = -1) +
  scale_color_manual(values = param_colors) +
  scale_alpha_continuous(range = c(0.3, 1)) +
  guides(color = guide_legend(nrow = 1, title = ""),
         alpha = FALSE) +
  labs(x = "Year", y = "Biomass (1000t)", color = "Parameterization") +
  coord_cartesian(ylim = c(0, 400),
                  xlim = c(1966.25, 1990.75),
                  expand = FALSE) +
  theme_jkb(base_size = 8) +
  theme(legend.position = "bottom") +
  theme(plot.margin = margin(t = 3, l = 1, r = 2),
        plot.title = element_blank(),
        panel.spacing.y = unit(10, "pt"),
        axis.ticks = element_line(size = 0.2),
        axis.line = element_line(size = 0.2),
        strip.text.x = element_text(vjust = 0,
                                    margin = margin(b = 0, l = 3, r = 0)),
        legend.position = "bottom",
        legend.margin = margin(t = -10))

## ggsave("figs/fig4_biopost.png", biopost_plot, width = 6, height = 4)
ggsave("figs/fig4_biopost.tiff", biopost_plot, width = 6, height = 4)
ggsave("figs/fig4_biopost.pdf", biopost_plot, device = cairo_pdf,
       width = 6, height = 4)

##-Figure X: performance and diagnostics----------------------------------------
fullPT_effplot <- fullPT_diags %>%
  mutate(div = div_total > 0)
  ggplot(aes(x = adapt_delta,
             y = ess_rate,
             color = model_name,
             group = model_name)) +
  geom_line(size = 1) +
  geom_point(size = 6, shape = 19) +
  geom_point(aes(alpha = as.numeric(div)), size = 5, color = "white",
             show.legend = FALSE) +
  geom_point(aes(color = div_fill), size = 4, shape = 19) +
  scale_color_manual(name = "Parameterization",
                     values = PT_colors,
                     na.value = "#ffffff",
                     drop = FALSE) +
  xlab("Target acceptance rate") +
  scale_y_log10(name = "Effectively independent samples per second",
                breaks = 10^(-1:3),
                labels = 10^(-1:3)) +
  theme_jkb(base_size = 16)
## Save a TIFF for Word, and a PDF as a high quality vector image for publication
ggsave("figs/figX_fullPT_effplot.tiff", fullPT_effplot, width = 6, height = 4)
ggsave("figs/figX_fullPT_effplot.pdf", fullPT_effplot, device = cairo_pdf,
       width = 6, height = 4)

fixedPT_effplot <- fixedPT_diags %>%
  mutate(div = div_total > 0)
ggplot(aes(x = adapt_delta,
           y = ess_rate,
           color = model_name,
           group = model_name)) +
  geom_line(size = 1) +
  geom_point(size = 6, shape = 19) +
  geom_point(aes(alpha = as.numeric(div)), size = 5, color = "white",
             show.legend = FALSE) +
  geom_point(aes(color = div_fill), size = 4, shape = 19) +
  scale_color_manual(name = "Parameterization",
                     values = PT_colors,
                     na.value = "#ffffff",
                     drop = FALSE) +
  xlab("Target acceptance rate") +
  scale_y_log10(name = "Effectively independent samples per second",
                breaks = 10^(-1:3),
                labels = 10^(-1:3)) +
  theme_jkb(base_size = 16)
Schaefer_diags %>%
  mutate(div = div_total > 0)
## Save a TIFF for Word, and a PDF as a high quality vector image for publication
ggsave("figs/figX_fixedPT_effplot.tiff", fixedPT_effplot, width = 6, height = 4)
ggsave("figs/figX_fixedPT_effplot.pdf", fixedPT_effplot, device = cairo_pdf,
       width = 6, height = 4)

Schaefer_effplot <- ggplot(aes(x = adapt_delta,
           y = ess_rate,
           color = model_name,
           group = model_name)) +
  geom_line(size = 1) +
  geom_point(size = 6, shape = 19) +
  geom_point(aes(alpha = as.numeric(div)), size = 5, color = "white",
             show.legend = FALSE) +
  geom_point(aes(color = div_fill), size = 4, shape = 19) +
  scale_color_manual(name = "Parameterization",
                     values = Schaefer_colors,
                     na.value = "#ffffff",
                     drop = FALSE) +
  xlab("Target acceptance rate") +
  scale_y_log10(name = "Effectively independent samples per second",
                breaks = 10^(-1:3),
                labels = 10^(-1:3)) +
  theme_jkb(base_size = 16)
## Save a TIFF for Word, and a PDF as a high quality vector image for publication
ggsave("figs/figX_Schaefer_effplot.tiff", Schaefer_effplot, width = 6, height = 4)
ggsave("figs/figX_Schaefer_effplot.pdf", Schaefer_effplot, device = cairo_pdf,
       width = 6, height = 4)

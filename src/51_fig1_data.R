##-Figure 1: CPUE and catch series plots----------------------------------------
cpue_catch <- tibble(year = 1967:1989,
                     catch = tuna_data$C,
                     index = tuna_data$I)
catch_plot <- ggplot(cpue_catch, aes(x = year, y = catch)) +
  geom_line(size = 1) +
  labs(# title = "South Atlantic albacore catch",
    x = "Year", y = "Catch biomass (Gg)") +
  theme_jkb(10)
cpue_plot <- ggplot(cpue_catch, aes(x = year, y = index)) +
  geom_line(size = 1) +
  labs(# title = "South Atlantic albacore catch per unit effort",
    x = "Year", y = "CPUE (kg per 100 hooks)") +
  theme_jkb(10)
cc_plot <- grid.arrange(catch_plot, cpue_plot, nrow = 2L)

## Save a TIFF for Word, and a PDF as a high quality vector image for publication
ggsave("figs/fig1_catch_cpue.tiff", cc_plot, width = 6, height = 4)
ggsave("figs/fig1_catch_cpue.pdf", cc_plot, device = cairo_pdf,
       width = 6, height = 4)

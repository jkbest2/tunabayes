fullPT_diagplot <- fullPT_diags %>%
  ggplot(aes(x = adapt_delta, y = div_total)) +
  geom_bar()
## Save a TIFF for Word, and a PDF as a high quality vector image for publication
ggsave("figs/figX_fullPT_diagplot.tiff", fullPT_diagplot, width = 6, height = 4)
ggsave("figs/figX_fullPT_diagplot.pdf", fullPT_diagplot, device = cairo_pdf,
       width = 6, height = 4)

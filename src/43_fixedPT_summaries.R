source("src/40_postprocess.R")

load("results/fixedPT_fits.Rdata")

fixedPT_summaries <- summarize_posteriors(fixedPT_fits)

saveRDS("results/fixedPT_summaries.rda")


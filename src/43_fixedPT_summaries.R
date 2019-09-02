source("src/40_postprocess.R")

fixedPT_fits <- readRDS("results/fixedPT_fits.Rds")

fixedPT_summaries <- summarize_posteriors(fixedPT_fits)

saveRDS(fixedPT_summaries, "results/fixedPT_summaries.Rds")


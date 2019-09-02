source("src/40_postprocess.R")

fullPT_fits <- readRDS("results/fullPT_fits.Rds")

fullPT_summaries <- summarize_posteriors(fullPT_fits)

saveRDS(fullPT_summaries, "results/fullPT_summaries.Rds")


source("src/40_postprocess.R")

fullPT_fits <- readRDS("results/fullPT_fits.Rds")

fullPT_summaries <- summarize_posteriors(fullPT_fits)
saveRDS(fullPT_summaries, "results/fullPT_summaries.Rds")

fullPT_diagnostics <- diagnose_fits(fullPT_fits)
saveRDS(fullPT_diagnostics, "results/fullPT_diagnostics.Rds")


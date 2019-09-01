source("src/40_postprocess.R")

load("results/fullPT_fits.Rdata")

fullPT_summaries <- posterior_summaries(fullPT_fits)

saveRDS(fullPT_summaries, "results/fullPT_summaries.rds")


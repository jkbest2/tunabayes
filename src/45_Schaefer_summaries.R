source("src/40_postprocess.R")

load("results/Schaefer_fits.Rdata")

Schaefer_summaries <- posterior_summaries(Schaefer_fits)

saveRDS(Schaefer_summaries, "results/Schaefer_summaries.rds")

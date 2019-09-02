source("src/40_postprocess.R")

fixedPT_fits <- readRDS("results/fixedPT_fits.Rds")

fixedPT_diagnostics <- diagnose_fits(fixedPT_fits)

saveRDS(fixedPT_diagnostics, "results/fixedPT_diagnostics.Rds")


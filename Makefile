fullPT_models := src/models/001_centered.stan\
								 src/models/010_ncproc.stan\
								 src/models/020_margq.stan\
								 src/models/030_exF.stan\
								 src/models/032_exF_margq.stan

fixedPT_models := src/models/101_centered.stan\
		 						  src/models/110_ncproc.stan\
		 						  src/models/120_margq.stan\
		 						  src/models/130_exF.stan\
		 						  src/models/132_exF_margq.stan\
									src/models/140_constrainedP.stan

fit_results := results/fullPT_fits.Rdata\
							 results/fixedPT_fits.Rdata\
							 results/Schaefer_fits.Rdata

fit_summaries := results/fullPT_summaries.rda\
								 results/fixedPT_summaries.rda\
								 results/Schaefer_summaries.rda

fit_diagnostics := results/fullPT_diagnostics.rda\
									 results/fixedPT_diagnostics.rda\
									 results/Schaefer_diagnostics.rda

fits: $(fit_results)

results/fullPT_fits.Rdata: $(fullPT_models)\
													 src/30_fitall.R\
													 src/31_fitfullPT.R
	Rscript src/31_fitfullPT.R

results/fixedPT_fits.Rdata: $(fullPT_models)\
													 	src/30_fitall.R\
													 	src/31_fitfullPT.R
	Rscript src/32_fitfixedPT.R

results/Schaefer_fits.Rdata: $(fullPT_models)\
													 	 src/30_fitall.R\
													 	 src/31_fitfullPT.R
	Rscript src/33_fitSchaefer.R

results: $(fit_summaries) $(fit_diagnostics)

results/fullPT_summaries.rda: results/fullPT_fits.Rdata\
															src/40_postprocess.R
	Rscript src/41_fullPT_summaries.R

results/fixedPT_summaries.rda: results/fixedPT_fits.Rdata
	Rscript src/43_fixedPT_summaries.R

results/Schaefer_summaries.rda: results/Schaefer_fits.Rdata
	Rscript src/45_Schaefer_summaries.R

results/fullPT_diagnostics.rda: results/fullPT_fits.Rdata
	Rscript src/42_fullPT_diagnostics.R

results/fixedPT_diagnostics.rda: results/fixedPT_fits.Rdata
	Rscript src/44_fixedPT_diagnostics.R

results/Schaefer_diagnostics.rda: results/Schaefer_fits.Rdata
	Rscript src/46_Schaefer_diagnostics.R

ch4.pdf: notes/ch4.md notes/ch4.bib
	pandoc --filter=pandoc-fignos --filter=pandoc-tablenos --filter=pandoc-eqnos \
	--filter=pandoc-citeproc --bibliography="notes/ch4.bib" \
	--csl=notes/fisheries-research.csl \
	-o notes/ch4.pdf notes/ch4.md

ch4_draft.pdf: notes/ch4.md notes/ch4.bib
	pandoc --filter=pandoc-fignos --filter=pandoc-tablenos --filter=pandoc-eqnos \
	--filter=pandoc-citeproc --bibliography="notes/ch4.bib" \
	--csl=notes/fisheries-research.csl \
	--variable=classoption:draft \
	--include-in-header=notes/draft_header.tex \
	-o notes/ch4_draft.pdf notes/ch4.md

ch4.docx: notes/ch4.md notes/ch4.bib
	pandoc --filter=pandoc-fignos --filter=pandoc-tablenos --filter=pandoc-eqnos \
	--filter=pandoc-citeproc --bibliography="notes/ch4.bib" \
	--csl=notes/fisheries-research.csl \
	--reference-doc=notes/ch4_ref.docx \
	-o notes/ch4.docx notes/ch4.md

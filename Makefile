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

fitted_models := results/fullPT_fits.Rds\
							 	 results/fixedPT_fits.Rds\
							 	 results/Schaefer_fits.Rds

fullPT_results := results/fullPT_summaries.Rds\
									results/fullPT_diagnostics.Rds

fixedPT_results := results/fixedPT_summaries.Rds\
									 results/fixedPT_diagnostics.Rds\

Schaefer_results := results/Schaefer_summaries.Rds\
										results/Schaefer_diagnostics.Rds

fits: $(fitted_models)

results/fullPT_fits.Rds: $(fullPT_models)\
												 src/30_fitall.R\
												 src/31_fitfullPT.R
	Rscript src/31_fitfullPT.R

results/fixedPT_fits.Rds: $(fullPT_models)\
													src/30_fitall.R\
													src/31_fitfullPT.R
	Rscript src/32_fitfixedPT.R

results/Schaefer_fits.Rds: $(fullPT_models)\
												 	 src/30_fitall.R\
													 src/31_fitfullPT.R
	Rscript src/33_fitSchaefer.R


results: $(fullPT_results) $(fixedPT_results) $(Schaefer_results)

$(fullPT_results): results/fullPT_fits.Rds\
									 src/40_postprocess.R\
									 src/41_fullPT_summaries.R
	Rscript src/41_fullPT_summaries.R

$(fixedPT_results): results/fixedPT_fits.Rds\
									  src/40_postprocess.R\
									  src/43_fixedPT_summaries.R
	Rscript src/43_fixedPT_summaries.R

$(Schaefer_results): results/Schaefer_fits.Rds\
									  src/40_postprocess.R\
									  src/45_Schaefer_summaries.R
	Rscript src/45_Schaefer_summaries.R

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

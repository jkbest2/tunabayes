fullPT_models := src/models/01_centered.stan\
								 src/models/10_ncproc.stan\
								 src/models/20_margq.stan\
								 src/models/30_exF.stan\
								 src/models/32_exF_margq.stan

fixedPT_models := src/models/101_centered.stan\
		 						  src/models/110_ncproc.stan\
		 						  src/models/120_margq.stan\
		 						  src/models/130_exF.stan\
		 						  src/models/132_exF_margq.stan\
									src/models/140_constrainedP.stan

fit_results := results/fullPT_fits.Rdata\
							 results/fixedPT_fits.Rdata\
							 results/Schaefer_fits.Rdata

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

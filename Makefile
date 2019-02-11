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
	-o notes/ch4.docx notes/ch4.md

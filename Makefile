ch4_pdf: notes/ch4.md
	pandoc --filter=pandoc-fignos --filter=pandoc-tablenos --filter=pandoc-eqnos \
	--filter=pandoc-citeproc --bibliography="notes/ch4.bib" \
	-o notes/ch4.pdf notes/ch4.md

# ch4_draft: notes/ch4.md
# 	pandoc --filter=pandoc-fignos --filter=pandoc-tablenos --filter=pandoc-eqnos \
# 	--filter=pandoc-citeproc --bibliography="notes/ch4.bib" \
# 	--include-in-header="\usepackage{setspace}" \
# 	--include-in-header="\doublespacing" \
# 	-o notes/ch4.pdf notes/ch4.md

ch4_docx: notes/ch4.md
	pandoc --filter=pandoc-fignos --filter=pandoc-tablenos --filter=pandoc-eqnos \
	--filter=pandoc-citeproc --bibliography="notes/ch4.bib" \
	-o notes/ch4.docx notes/ch4.md
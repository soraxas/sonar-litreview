OUTPUT_MD=output.md
OUTPUT_JIRA=output.jira
OUTPUT_PDF=output.pdf
OUTPUT_HTML=output.html
BIB=master.bib
CSL=assets/apa.csl
INPUT=paper.md

BIB_FLAGS=--bibliography=$(BIB) --csl=$(CSL) --citeproc
CROSSREF_FLAGS=-F pandoc-crossref

FIGURE_SEC_PREFIX={section:}\n{column:width=30%}\n{column}\n{column:width=40%}\n
FIGURE_SEC_SUFFIX=\n{column}\n{column:width=30%}\n{column}\n{section}

HTML_TAG=\n{html}\n


USE_KATEX_FOR_EQ=\
	$(HTML_TAG)\
	<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex@0.15.2/dist/katex.min.css" integrity="sha384-MlJdn/WNKDGXveldHDdyRP1R4CTHr3FeuDNfhsLPYrq2t0UBkUdK2jyTnXPEK1NQ" crossorigin="anonymous">\
	<script defer src="https://cdn.jsdelivr.net/npm/katex@0.15.2/dist/katex.min.js" integrity="sha384-VQ8d8WVFw0yHhCk5E8I86oOhv48xLpnDZx5T9GogA/Y84DcCKWXDmSDfn13bzFZY" crossorigin="anonymous"></script>\
	<script defer src="https://cdn.jsdelivr.net/npm/katex@0.15.2/dist/contrib/auto-render.min.js" integrity="sha384-+XBljXPPiv+OzfbB3cVmLHf4hdUFHlWNZN5spNQ7rmHTXpd7WvJum6fIACpNNfIR" crossorigin="anonymous" onload="renderMathInElement(document.body);"></script>\
	$(HTML_TAG)



.PHONY=pdf jira md clean

jira: $(OUTPUT_JIRA)
pdf: $(OUTPUT_PDF)
md: $(OUTPUT_MD)

$(OUTPUT_JIRA): $(INPUT) $(BIB)
	pandoc --number-sections -s  -t jira $(CROSSREF_FLAGS) $(BIB_FLAGS) -o $@ $<

	@# ensure anchor tag has newlines (to avoid messed up parsing by jira)
	sed '/{anchor:sec/b; s#\({anchor:[^}]*}\)#\1\n\n#g' -i $@

	@# clean anchor tag as we cannot use colon in jira (these tags are produces by pandoc-crossref)
	sed 's#\({anchor:[a-zA-Z]*\):#\1-#g' -i $@
	sed 's/\(|#[a-zA-Z]*\):/\1-/g' -i $@

	@# somehow the produced code has invisible characters (what??) that messes with display
	@# their unicode is U+2004 and U+2005 what corresponds to (0xe2 0x80 0x84 THREE-PER-EM SPACE), (0xe2 0x80 0x85 FOUR-PER-EM SPACE)
	@# we will strip them and replace them with whitespace
	sed 's/\xe2\x80\x84/ /g ; s/\xe2\x80\x85/ /g' -i $@

	@# we inject the image figure inside a custom width "section column" to control width
	perl -i~ -0777 -pe 's/({anchor:fig.*?Figure[^\n]*)/$(FIGURE_SEC_PREFIX)\1$(FIGURE_SEC_SUFFIX)/gs' $@

	@#inject katex script
	@#sed 's#\(h1[.].*Introduction\)#$(USE_KATEX_FOR_EQ)\1#' -i $@
	echo -e '$(USE_KATEX_FOR_EQ)' >> $@

	@#replace math with html block (two dollarsign to escape from makefile, then backslash for perl)
	perl -i~ -0777 -pe  's#1994 [^(]*\(([0-9]+)\)\s*{noformat}\s*\$$\$$([^\$$]*)\$$\$$\s*{noformat}#$(HTML_TAG)\$$\$$\2 \\tag{\1}\$$\$$$(HTML_TAG)#gs' $@



$(OUTPUT_PDF): $(INPUT)
	pandoc -s --pdf-engine=xelatex -t pdf $(CROSSREF_FLAGS) $(BIB_FLAGS) -o $@ $<

$(OUTPUT_HTML): $(INPUT)
	pandoc -s  -t html $(CROSSREF_FLAGS) $(BIB_FLAGS) -o $@ $<

$(OUTPUT_MD): $(INPUT)
	pandoc -s -t markdown_strict $(CROSSREF_FLAGS) $(BIB_FLAGS) -o $@ $<


clean:
	rm -rf $(OUTPUT_MD) $(OUTPUT_PDF) $(OUTPUT_JIRA)

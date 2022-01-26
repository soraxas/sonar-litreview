OUTPUT_MD=output.md
OUTPUT_JIRA=output.jira
OUTPUT_PDF=output.pdf
BIB=master.bib
CSL=apa.csl
INPUT=paper.md

BIB_FLAGS=--bibliography=$(BIB) --csl=$(CSL) --citeproc
CROSSREF_FLAGS=-F pandoc-crossref

.PHONY=pdf jira md clean

jira: $(OUTPUT_JIRA)
pdf: $(OUTPUT_PDF)
md: $(OUTPUT_MD)

$(OUTPUT_JIRA): $(INPUT)
	pandoc -s  -t jira $(CROSSREF_FLAGS) $(BIB_FLAGS) -o $@ $<

	@# ensure anchor tag has newlines (to avoid messed up parsing by jira)
	sed '/{anchor:sec/b; s#\({anchor:[^}]*}\)#\1\n\n#g' -i $@

	@# clean anchor tag as we cannot use colon in jira (these tags are produces by pandoc-crossref)
	sed 's#\({anchor:[a-zA-Z]*\):#\1-#g' -i $@
	sed 's/\(|#[a-zA-Z]*\):/\1-/g' -i $@

$(OUTPUT_PDF): $(INPUT)
	pandoc -s --pdf-engine=xelatex -t pdf $(CROSSREF_FLAGS) $(BIB_FLAGS) -o $@ $<

$(OUTPUT_MD): $(INPUT)
	pandoc -s -t markdown_strict $(CROSSREF_FLAGS) $(BIB_FLAGS) -o $@ $<


clean:
	rm -rf $(OUTPUT_MD) $(OUTPUT_PDF) $(OUTPUT_JIRA)

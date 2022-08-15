SHELL=/bin/sh

.PHONY: tex zip all

CODEDIRS = elemente fallunterscheidungen zusammengesetzte-daten gemischte-daten \
	selbstbezuege listen natuerliche-zahlen higher-order akkumulatoren videospiele \
	baeume eigenschaften secd
ZIP = sdp-code.zip

all: tex zip

includeonly.tex:
	touch includeonly.tex

tex: includeonly.tex
	pdflatex -interaction=nonstopmode -halt-on-error i1
	bibtex i1
	pdflatex -interaction=nonstopmode -halt-on-error i1
# 	https://singhkays.com/blog/sed-error-i-expects-followed-by-text/
	sed -i'' -e 's/\\"/"/g' i1.idx
	sed -i'' -e 's/\\eingebaut/eingebaut/g' i1.idx
	makeindex -c -s i1.mst -g i1
	pdflatex -interaction=nonstopmode -halt-on-error i1

zip:
	rm -f $(ZIP) && for f in $(CODEDIRS); do zip $(ZIP) $$f/*.rkt; done


annals: template.tex template_co.tex submissoes.csv cover.svg

	Rscript -e 'source("merge.R")'
	xelatex annals.tex
	xelatex annals.tex

clean:
	rm -f *.aux *.bbl *.blg *.brf *.idx *.ilg *.ind *.lof *.log \
	.*lol *.lot *.out *.toc *.synctex.gz annals.tex

lhs2tex:
	/home/joao/.cabal/bin/lhs2TeX -o LFCFDTypes.tex LFCFDTypes.lhs

pdf: lhs2tex
	pdflatex LFCFDTypes.tex
	biber LFCFDTypes
	pdflatex LFCFDTypes.tex

clean:
	rm LFCFDTypes.log LFCFDTypes.pdf LFCFDTypes.aux LFCFDTypes.tex LFCFDTypes.ptb LFCFDTypes.bcf LFCFDTypes.bbl LFCFDTypes.blg LFCFDTypes.run.xml

lhs2tex:
	/home/aeron/.cabal/bin/lhs2TeX -o LFCFDTypes.tex LFCFDTypes.lhs

pdf: lhs2tex
	pdflatex LFCFDTypes.tex

clean:
	rm LFCFDTypes.log LFCFDTypes.pdf LFCFDTypes.aux LFCFDTypes.tex LFCFDTypes.ptb

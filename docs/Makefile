# Build the literate-Haskell report.
#
# Layout assumed:
#   project/
#     haskell-lib/   <- your library's .hs modules
#     report/        <- this folder
#
# Adjust LIBDIR below if your library folder is named/located differently.

MAIN   = report
LIBDIR = ../haskell-lib

.PHONY: all clean check

all: $(MAIN).pdf

SECTIONS = $(wildcard sections/*.lhs)

$(MAIN).tex: $(MAIN).lhs ap-format.fmt ap-symbols.sty ap-report.sty \
             cover.tex abstract.tex nomenclature.tex preamble.tex appendix.tex $(SECTIONS)
	lhs2TeX --poly $(MAIN).lhs -o $(MAIN).tex

$(MAIN).pdf: $(MAIN).tex report.bib
	pdflatex -interaction=nonstopmode $(MAIN).tex
	-bibtex $(MAIN)
	-makeindex $(MAIN)
	pdflatex -interaction=nonstopmode $(MAIN).tex
	pdflatex -interaction=nonstopmode $(MAIN).tex

# Type-check the code blocks against the real library, without building the PDF.
check:
	ghc -fno-code -i$(LIBDIR) $(MAIN).lhs

clean:
	rm -f $(MAIN).tex *.aux *.log *.lot *.lof *.listing *.ptb *.toc *.idx *.ilg *.ind *.bbl *.blg *.out $(MAIN).pdf

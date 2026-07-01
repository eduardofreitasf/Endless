# Algebra of Programming — Haskell Library Report

A literate-Haskell report documenting a personal Haskell library built on the
algebra of programming: catamorphisms, anamorphisms, hylomorphisms, and their
associated equational laws.

---

## Directory layout

```
project/
├── haskell-lib/   ← your .hs library modules (sibling folder)
└── report/        ← this folder
```

The Makefile type-checks the literate code against `../haskell-lib` via GHC.
Change `LIBDIR` in the Makefile if your library folder has a different name or
location.

---

## Dependencies

### Haskell toolchain

| Tool    | How to get it                                                                                                           |
| ------- | ----------------------------------------------------------------------------------------------------------------------- |
| GHC     | Via [ghcup](https://www.haskell.org/ghcup/): `curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org \| sh` |
| cabal   | Bundled with ghcup                                                                                                      |
| lhs2TeX | `cabal install lhs2tex` — installs to `~/.cabal/bin`; add that to your `PATH`                                           |

Make sure `~/.cabal/bin` (or `~/.local/bin` on newer cabal) is on your `PATH`:

```bash
export PATH="$HOME/.cabal/bin:$HOME/.ghcup/bin:$PATH"   # add to ~/.bashrc
```

### LaTeX

A full TeX Live installation covers everything:

```bash
sudo apt install texlive-full          # Ubuntu/Debian — large but complete
```

If you prefer a leaner install, these individual packages are the minimum:

```bash
sudo apt install \
  texlive-latex-base \       # pdflatex, bibtex, makeindex
  texlive-latex-extra \      # tcolorbox, booktabs, titlesec, caption, parskip
  texlive-latex-recommended \# geometry, fancyhdr, hyperref, listings
  texlive-pictures \         # tikz/pgf
  texlive-science \          # xy (commutative diagrams)
  texlive-fonts-recommended  # Latin Modern
```

---

## Building

```bash
make          # produces report.pdf
make check    # type-checks the literate code against ../haskell-lib (no PDF)
make clean    # removes all build artifacts
```

The full build sequence is: `lhs2TeX` → `pdflatex` → `bibtex` → `makeindex`
→ `pdflatex` × 2 (to resolve cross-references and the table of contents).

---

## File reference

| File                       | Purpose                                                                                                                |
| -------------------------- | ---------------------------------------------------------------------------------------------------------------------- |
| `report.lhs`               | Main file: loads packages, assembles all sections.                                                                     |
| `cover.tex`                | Title page. Edit the `REPLACE_WITH_*` placeholders.                                                                    |
| `abstract.tex`             | Abstract and version history table.                                                                                    |
| `preamble.tex`             | Introduction section (§1).                                                                                             |
| `nomenclature.tex`         | Notation quick-reference and acronym tables.                                                                           |
| `appendix.tex`             | Template usage guide (remove before final submission).                                                                 |
| `report.bib`               | BibTeX bibliography.                                                                                                   |
| `Makefile`                 | Build rules.                                                                                                           |
| **`ap-report.sty`**        | Page layout, fonts, colors, and structural environments (`property`, `unittest`, `calculation`, `Topic`, `lcbr`).      |
| **`ap-symbols.sty`**       | Math macros for prose and equations (`\cata{}`, `\conj{}{}`, `\N`, …).                                                 |
| **`ap-format.fmt`**        | lhs2TeX `%format` rules: how literal Haskell tokens inside code blocks are typeset (e.g. `.` → `∘`, `cata g` → `⦇g⦈`). |
| `sections/`                | One `.lhs` file per documented module or topic.                                                                        |
| `sections/00-template.lhs` | Blank section template — copy to start a new topic.                                                                    |

---

## Adding notation

There are **two separate extension points**, for two different mechanisms:

- **Math you write yourself** — in prose, inside `eqnarray*`, or in `xymatrix` diagrams — add a `\newcommand` to **`ap-symbols.sty`**, in the numbered section that best matches, or in section 6 ("your own symbols") at the bottom.

- **How literal Haskell renders** — inside `\begin{code}` blocks or `|inline code|` — add a `%format` rule to **`ap-format.fmt`**, again in the matching section or at the bottom.

Both files have banner-commented sections and a template comment showing the syntax to copy.

---

## Adding a section

1. Copy `sections/00-template.lhs` → `sections/NN-yourtopic.lhs`.
2. Fill in `\Topic{}`, the commutative diagram (if applicable), the code, and any `property` / `calculation` / `unittest` blocks.
3. Add `%include sections/NN-yourtopic.lhs` to `report.lhs` in the order you want it to appear.

---

## Key environment syntax

**Equational proof:**

```latex
\begin{calculation}
  expr_1
\jstep{=}{justification text}
  expr_2
\qedstep
\end{calculation}
```

**GHCi unit test** (content written directly, no inner lstlisting):

```latex
\begin{unittest}
ghci> myFunction arg
result
\end{unittest}
```

**Property:**

```latex
\begin{property}
  Statement of the property in math or prose.
\end{property}
```

**Commutative diagram:**

```latex
\begin{eqnarray*}
\xymatrix@@C=3cm@@R=2cm{
  |A| \ar[d]_-{|cata g|} \ar[r]^-{|out|}
  & |F A| \ar[d]^{|F (cata g)|}
  \\
  |B| & |F B| \ar[l]^-{|g|}
}
\end{eqnarray*}
```

Note: use `@@` for a literal `@` inside `xymatrix` (lhs2TeX escaping rule).

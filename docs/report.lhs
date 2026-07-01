\documentclass[11pt,a4paper]{article}
\usepackage{ap-report}

%%==============================================================================
%% lhs2TeX setup
%%==============================================================================
%include ap-format.fmt

%%==============================================================================
%% Hidden module header (does not appear in the PDF, but lets this file be
%% type-checked by GHC against the real library in ../haskell-lib — see the
%% Makefile, which passes -i../haskell-lib).
%%==============================================================================
%if False
\begin{code}
module Main where
import Cp   -- replace/extend with your library's modules as needed

main :: IO ()
main = return ()
\end{code}
%endif

\title{Algebra of Programming --- Haskell Library}
\author{}
\date{}

\begin{document}

%% ---------------------------------------------------------------------------
%% Front matter (roman numerals)
%% ---------------------------------------------------------------------------
\input{cover}
\pagenumbering{roman}

\input{abstract}

\tableofcontents
\listoffigures
\listoftables

\newpage
\input{nomenclature}

%% ---------------------------------------------------------------------------
%% Main matter (arabic numerals restart at 1)
%% ---------------------------------------------------------------------------
\pagenumbering{arabic}
\setcounter{page}{1}

\input{preamble}

%% One %include per section file. Add a line here each time you add a file
%% under sections/ (copy sections/00-template.lhs to start a new one).
%include sections/01-example-lists.lhs
%include sections/02-example-streams.lhs

%% ---------------------------------------------------------------------------
%% Appendix
%% ---------------------------------------------------------------------------
\appendix
\input{appendix}

%% ---------------------------------------------------------------------------
%% Index and bibliography (remove either if unused)
%% ---------------------------------------------------------------------------
\printindex

\bibliographystyle{plain}
\bibliography{report}

\end{document}

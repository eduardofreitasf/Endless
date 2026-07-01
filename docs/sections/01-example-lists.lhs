%%==============================================================================
%% sections/01-example-lists.lhs
%% Worked example showing the intended structure for a documented module.
%% Delete or replace once you have real content.
%%==============================================================================

\Topic{Lists (worked example)}

This section documents the |List| module as a usage example for the
template: a short description, the recursion-pattern diagram, the code
(type-checked against |../haskell-lib|), a property, and an equational proof.

%if False
\begin{code}
module Sections.ExampleLists where
import Cp
\end{code}
%endif

The list functor has the usual base functor |1 + A >< Seq(A)|, with the
catamorphism characterised by the following diagram:

\begin{eqnarray*}
\xymatrix@@C=3cm@@R=2cm{
  |Seq(A)| \ar[d]_-{|cata g|} \ar@@/^1pc/[r]^-{|outList|}
  &
  |1 + A >< Seq(A)| \ar[d]^{|id + id >< cata g|} \ar@@/^1pc/[l]^-{|inList|}
  \\
  |B|
  &
  |1 + A >< B| \ar[l]^-{|g|}
}
\end{eqnarray*}

\begin{code}
sumList :: [Int] -> Int
sumList = cata (either (const 0) (\(x,y) -> x + y))
\end{code}

\begin{property}
|sumList| distributes over list concatenation:
\[ |sumList (xs ++ ys) = sumList xs + sumList ys| \]
\end{property}

A short equational justification:

\begin{calculation}
|sumList (xs ++ ys)|
\jstep{=}{definition of |sumList| as a catamorphism}
|cata g (xs ++ ys)|
\jstep{=}{cata-fusion / list-append law}
|sumList xs + sumList ys|
\qedstep
\end{calculation}

\begin{unittest}
ghci> sumList [1,2,3]
6
\end{unittest}

This catamorphic style of definition is standard in the algebra-of-programming
tradition \cite{bird1997algebra,meijer1991functional}; see
\autoref{fig:example-tree} for the corresponding shape when the same idea is
applied to a binary tree rather than a list, and \autoref{tab:patterns} for a
comparison of the three recursion patterns used throughout this report.

\begin{figure}[h]
\centering
\begin{tikzpicture}[
  level distance=14mm,
  every node/.style={circle, draw=cpaccent, minimum size=7mm, inner sep=0pt},
  level 1/.style={sibling distance=24mm},
  level 2/.style={sibling distance=12mm}
]
\node {a}
  child { node {b}
    child { node {d} }
    child { node {e} }
  }
  child { node {c} };
\end{tikzpicture}
\caption{A binary tree, the kind of structure a |cataTree|-style
catamorphism folds over.}
\label{fig:example-tree}
\end{figure}

\begin{table}[h]
\centering
\begin{tabular}{lll}
\toprule
\textbf{Pattern} & \textbf{Direction} & \textbf{Typical use} \\
\midrule
catamorphism ($\cata{g}$) & structure $\to$ value & folds, consumers \\
anamorphism ($\ana{g}$)   & value $\to$ structure & unfolds, generators \\
hylomorphism ($\hylo{g}{h}$) & value $\to$ value & generate then consume \\
\bottomrule
\end{tabular}
\caption{The three core recursion patterns used in this report.}
\label{tab:patterns}
\end{table}


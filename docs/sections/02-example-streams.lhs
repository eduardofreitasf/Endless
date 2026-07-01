%%==============================================================================
%% sections/02-example-streams.lhs
%% Second worked example: anamorphism + hylomorphism.
%% Delete or replace once you have real content.
%%==============================================================================

\Topic{Streams (worked example)}

While \autoref{fig:example-tree} showed a catamorphism consuming a
structure, this section documents the dual case: an anamorphism that
\emph{generates} one, and a hylomorphism that fuses generation with
consumption without ever materialising the intermediate structure.

%if False
\begin{code}
module Sections.ExampleStreams where
import Cp
\end{code}
%endif

\begin{code}
countdown :: Int -> [Int]
countdown = ana (\n -> if n == 0 then i1 () else i2 (n, n - 1))
\end{code}

\begin{property}
|countdown n| has length |n + 1| for all |n >= 0|.
\end{property}

Fusing the anamorphism above with the |sumList| catamorphism from
\autoref{tab:patterns} gives a hylomorphism that sums down from |n| to
|0| without building the intermediate list:

\begin{code}
sumCountdown :: Int -> Int
sumCountdown = hylo (either (const 0) (\(x,y) -> x + y))
                     (\n -> if n == 0 then i1 () else i2 (n, n - 1))
\end{code}

\begin{calculation}
|sumCountdown n|
\jstep{=}{hylomorphism fusion law}
|sumList (countdown n)|
\jstep{=}{arithmetic series}
|n * (n + 1) `div` 2|
\qedstep
\end{calculation}

\begin{unittest}
ghci> sumCountdown 5
15
\end{unittest}

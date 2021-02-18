---
title: Probability of 22
---

.. role:: haskell(code)
    :language: haskell

Imagine a random process generating a sequence of numbers, picking $0,1,2$ in each place with uniform
probability. What is the probability $p_n$ that two consecutive 2 appear in a sequence $n$ numbers long?

First solution
##############


Firstly, when the sequence is empty or has one element, the probability is 0. Now let's say the sequence has
$k > 1$ elements.

If the $k$-th element is different from 2 (with probability 2/3), then nothing changes here and thus
$p_k = \\frac{2}{3} p _{k-1}$.

If the $k$-th element is 2 (with probability 1/3), then

* if $(k-1)$-st is different from 2 (with probability $1/3 \\cdot 2/3$),
  then nothing changes here and thus $p_k = \\frac{1}{3} \\frac{2}{3} p _{k-2}$, or
* if $(k-1)$-st is 2 (with probability $1/3 \\cdot 1/3$), then we found 22.

.. code-block:: haskell

    p :: Int -> Double
    p 0 = 0
    p 1 = 0
    p k = 2/3 * p (k-1)
          + -- k-th element being = 2 or /= 2 is mutually exclusive
          1/3 *
                (1/3
                 + -- (k-1)-st element being = 2 or /= 2 is exclusive
                 2/3 * p (k-2))


This method of computing $p_n$ grows exponentially slower with increasing $n$. Thankfully, the recurring computations are overlapping
and dynamic programming can be employed, either using ``Data.Function.Memoize`` from the ``memoize``
package, or :python:`functools.lru_cache` if you're in Python, or maybe with bottom-up pre-computing.

.. source
    # stack exec ghci --package matrix --package memoize
    import Data.Function.Memoize
    import Data.Function (fix)

    p f 0 = 0
    p f 1 = 0
    p f k = 2/3 * f (k-1)  +  1/3 * (1/3 + 2/3 * f (k-2))

    memoizedp :: Int -> Double
    memoizedp = fix (memoize . p)


Markov solution
###############

However,
since the generating process is a discrete-time Markov chain,
there's a nicer way to solve the problem.
The Markov chain has three states,

* two for when 22 has not been seen
  * and the last digit is 0 or 1 (for the first digit take $x$ to be empty),
  * and the last digit is 2,
* and one for when 22 was seen.

.. image:: /images/2020-05-25-probability-of-22/markov-chain.svg
    :width: 80%
    :align: center
    :alt: Markov chain with states "Neither", "Last is 2", "Seen 22"

.. source
    % pdflatex -shell-escape mc.tex # produces mc.svg
    \documentclass[crop,tikz,convert=pdf2svg]{standalone}[]
    \usepackage{tikz}
    \usetikzlibrary{automata, positioning}
    \begin{document}
        \begin{tikzpicture}[font=\sffamily]
            \node[state] (n) {Neither};
            \node[state, right=2cm of n] (l) {Last is 2};
            \node[state, right=2cm of l] (s) {Seen 22};
            \draw[every loop, auto=right, >=latex]
                (n) edge[loop above] node {2/3} (n)
                (n) edge[bend left, auto=left] node {1/3} (l)
                (l) edge[bend left, auto=left] node {2/3} (n)
                (l) edge[bend left, auto=left] node {1/3} (s)
                (s) edge[loop above] node {1} (s);
        \end{tikzpicture}
    \end{document}

If we represent the probability of the three states with a triplet/vector (in the same order as above)
then the initial state is $b$ and we can also translate the graph into a transition matrix $P$.

$$
b =
\\begin{pmatrix}
1, 0, 0
\\end{pmatrix},
P =
\\begin{pmatrix}
2/3 & 1/3 & 0   \\\\
2/3 &   0 & 1/3 \\\\
0   &   0 & 1
\\end{pmatrix}
$$

And now $p_k$ is the rightmost element of $b \\cdot P^k$.


.. code-block:: haskell

    import Data.Matrix

    b :: Matrix Double
    b = fromLists [ [ 1, 0, 0 ] ]

    matP :: Matrix Double
    matP = fromLists
             [ [2/3, 1/3,   0]
             , [2/3,   0, 1/3]
             , [  0,   0,   1]
             ]

    p' :: Int -> Double
    p' 0 = 0
    p' k = getElem 1 3 (b * matP^k)


Exponentiation by squaring
##########################

To make sure `exponentiation by squaring <https://en.wikipedia.org/wiki/Exponentiation_by_squaring>`_
is used (it should be since matrices with multiplication form a semigroup) I added a quick naive implementation
which surely escapes this optimisation and then compared them in GHCi (a disclaimer applies that the usual GHC
optimisations are missing too).

.. code-block:: haskell

    -- slowp' k = getElem 1 3 (b * (foldr1 multStd (replicate k matP)))
    *Main> :set +s
    *Main> p' 100000
    0.9999999999999998
    it :: Double
    (0.02 secs, 223,744 bytes)
    *Main> slowp' 100000
    0.9999999999999982
    it :: Double
    (0.50 secs, 339,976,496 bytes)
    -- and also
    *Main> memoizedp 100000
    0.9999999999999982
    it :: Double
    (0.78 secs, 1,129,307,904 bytes)


In fact, exponentiation by squaring is so good that computation is quick even for very big $n$
and floating point error becomes the main concern.

.. code-block:: haskell

    *Main> p' (10^10000)
    0.9999999999999998
    it :: Double
    (0.37 secs, 357,373,144 bytes)


Final trick
###########

But since we have a matrix we can try diagonalising it.  Once we have a diagonalisation $P = S \\cdot J \\cdot
S^{-1}$, calculating $P^k$ reduces to calculating powers of real numbers thanks to diagonal $J$. Multiplying
$P^k$ by $b$ from the left and then picking the rightmost item yields a formula which uses only simple
operations. I `let Wolfram Alpha do this
<https://www.wolframalpha.com/input/?i=%281%2C0%2C0%29+%7B%282%2F3%2C+1%2F3%2C+0%29%2C+%282%2F3%2C+0%2C+1%2F3%29%2C+%280%2C+0%2C+1%29%7D%5Ek>`_
for me and modified the result just enough to make it valid Haskell.

.. code-block:: haskell

    p'' :: Double -> Double
    p'' 0 = 0
    p'' 1 = 0
    p'' k =   1/4 * (sqrt(3) - 1)^2 * 3**(-k - 1/2) * (1 - sqrt(3))**k
            - 1/4 * 3**(-k - 1/2) * (1 + sqrt(3))**(k + 2) + 1


This solution has an interesting property:

.. code-block:: haskell

    *Main> p'' 10
    0.5773002083015804
    it :: Double
    *Main> p'' 500
    1.0
    it :: Double
    *Main> p'' 1000
    NaN
    it :: Double

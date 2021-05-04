---
title: Lovász Local Lemma
---

The 2021 Abel Prize has been awarded to László Lovász and Avi Wigderson which is a great opportunity to revisit
one of my favourite tricks, the probabilistic method. This method was developed by Paul Erdős (see *My Brain is
Open* by Schechter for a biography) originally as a way to prove existence of combinatorial objects but it has
enjoyed broad applications in other areas, including algorithm design.

In this post I present the probabilistic method, the titular lemma which is a mighty weapon when using the
method, its application in an existential proof, and finally an algorithm supported by the lemma. What you read
here is based on lectures *Randomized Algorithms and Computing* by `Jozef
Gruska<https://www.fi.muni.cz/usr/gruska>`_ (which I attended) and the book *Probability and Computing* by
Mitzenmacher and Upfal, which contains a whole chapter on the topic.


The probabilistic method
########################

The idea of the probabilistic method is both very original and simple in its principle. If you want to prove an
object exists, just prove it has a non-zero probability of occurring in some sample space. One approach is to
directly show the non-zero probability.

Another approach is to consider the expected value of a random variable $X$. If the expected value of $X$ is
$\mu$ then you're guaranteed there exists an object for which $X$ is at least $\mu$ and an object for which $X$ is
at most $\mu$ (possibly the same object). Stated formally as a lemma that follows from a short argument based on
contradiction:

**Lemma.** *Let $X$ be a random variable with $\\mathbb{E}(X) = μ$. Then $Pr(X \\geq μ) > 0$ and $Pr(X \\leq μ) > 0$.*


For an example of this approach take the MAXSAT problem, where you try to maximise as many clauses of a given
formula as possible.  Recall that formulas are built on variables ($x_i$) that make up literals (either $x_i$ or
$¬x_i$) which are joined by disjunction ("or") into clauses (e.g. $x_1 ∨ ¬x_2$) which are joined by conjunction
("and") into a formula (e.g. $(x_1 ∨ ¬x_2) ∧ (x_2 ∨ x_3)$).

To make our investigation a bit simpler let's assume MAXSAT does not contain formulas where a single clause
contains both a variable and its negation. We can omit these without loss of generality as such clauses are
clearly satisfied.

**Theorem.** *Given a formula with $m$ clauses and $k_i$ literals in the $i$-th clause.
Let $k$ be the minimum of all $k_i$. Then there is a truth assignment that satisfies at least*
$$
\\sum _{i=1}^m (1-2^{-k_i}) \\geq m(1-2^{-k})
$$
*clauses.*

Note that $1-2^{-k}$ approaches 1 very quickly with growing $k$.

**Proof.** Pick a true/false assignment for each variable with an (unbiased) coin toss.
Define the random variable $I_i$ to be 1 if $i$-th clause is satisfied and 0 otherwise.
Let $X$ be the random variable representing the number of satisfied clauses.

Then $Pr(I_i = 1) = 1-2^{-k_i}$ and, by linearity of expectation,

$$
\\mathbb{E}(X)
= \\mathbb{E}\\bigl(\\sum I_i\\bigr)
= \\sum \\mathbb{E}(I_i)
= \\sum 1-2^{-k_i}
\\geq m(1-2^{k})
$$

Finally, by the lemma above, the theorem follows. ∎


Independent and only locally dependent events
#############################################

That was maximising the number of satisfied clauses (MAXSAT).  Now consider formulas again but restrict the
number of literals in every clause to be exactly $k$ and check whether the whole formula is satisfiable. This is
the k-SAT problem. When a clause has fewer than $k$ literals it can be padded with new variables so the problem
isn't as constrained as it might seem at first.

Here's an example instance:

$$
(x_1 ∨ ¬x_2 ∨ x_3) ∧ (¬x_4 ∨ x_5 ∨ x_6) ∧ (x_7 ∨ x_8 ∨ ¬x_9)
$$

One interesting property of this example is that no variable appears in two formulas.
That's a convenient setting for a probabilistic approach: we can consider the satisfiability of each clause
separately and then calculate the product of **independent** events.

We can call $E_i$ the event that $i$-th clause is *not* satisfied. When we assign values to variables by an
unbiased coin toss again then $Pr(E_i) = 1 - 2^k$.
We would like to reason about the probability of all clauses being satisfied, $\\bigcap \\neg E_i$,
and in this particular example we know all $E_i$ are
independent so we can conclude $Pr(\\bigcap \\neg E_i) = \\prod Pr(\\neg E_i) > 0$.

What we've just done here -- proved a formula of the shape $Pr(\\bigcap \\neg E_i) > 0$ --
is the third approach to probabilistic method.

But that was a lucky example. To visualise the independence we used, imagine a graph, where clauses are vertices
and an edge exists between two clauses if and only if they share a variable. For our formula (copied below) there
are no edges:

$$
(x_1 ∨ ¬x_2 ∨ x_3) ∧ (¬x_4 ∨ x_5 ∨ x_6) ∧ (x_7 ∨ x_8 ∨ ¬x_9)
$$

.. image:: /assets/2021-04-02-lovasz-local-lemma/independent_formula.svg
    :width: 100%
    :align: center
    :alt: Formula


Now consider the following formula and the corresponding dependency graph:

$$
(¬x_1 ∨ x_2 ∨ x_3) ∧ (¬x_2 ∨ ¬x_4 ∨ x_6) ∧ (x_7 ∨ x_6 ∨ x_1)
$$

.. image:: /assets/2021-04-02-lovasz-local-lemma/dependent_formula.svg
    :width: 100%
    :align: center
    :alt: Formula

Suddenly the jump straight from $Pr(\\bigcap \\ldots)$ to $\\prod Pr(\\ldots)$ is not possible.

But notice there are only a few edges. And in bigger graphs edges could possibly be rare enough to actually make
the problem resemble many small independent problems.

This is where the genius of Lovász and his **Local** Lemma appears: if the dependencies between events are only
of a local character then the probabilty of them occuring simultaneously can still be bounded by an inequality
of the shape $Pr(\\bigcap \\ldots) \\geq \\prod Pr(\\ldots)$.  Here's a formal statement of a variant that
arrives to a simpler inequality of the form $Pr(\\bigcap \\ldots) > 0$:

**Theorem.** *Let $E_1,...,E_n$ be events and assume there are $p,d$ such that all of the following hold:*

1. *for all $i$, $Pr(E_i) \\leq p$*,
2. *the degree of the dependency graph of $E_1,...,E_n$ is at most $d$*,
3. $4dp \\leq 1$.

*Then*
$$
Pr \\Bigl(\\bigcap _{i=1}^{n} \\neg E_i \\Bigr) > 0.
$$

For a proof I'll refer the reader to *Probability and Computating* mentioned above. Let's again assume without
loss of generality there are no formulas where a single clause contains both a variable and its negation, and
now let's just apply the lemma to kSAT.

**Theorem.** *If no variable in a k-SAT formula appears in more than $T=2^k/4k$ clauses, then the formula has a
satisfying assignment.*

**Proof.**
Let's pick a true/false assignment for each variable with an (unbiased) coin toss.

Consider $E_i$, the event $i$-th clause is *not* satisfied. We want to prove that the probability of all
opposite events ($\\neg E_i$, $i$-th clause *is* satisfied) occuring together is greater than 0, that is $Pr
\\bigl(\\bigcap _{i=1}^{n} \\neg E_i \\bigr) > 0$.

We check the conditions of Lovász local lemma:

1. Each clause has $k$ literals, so each clause is satisfied with probability $Pr(E_i) = 2^{-k}$, and so
   $p = 2^{-k}$.
2. Each literal appears in at most $T=2^k/4k$ clauses and each clause has exactly $k$ literals, so the degree
   $d$ is at most $kT = 2^k/4$.
3. $4dp = 4 \\cdot \\frac{2^k}{4} \\cdot 2^{-k} = 1 \\leq 1$.

All the conditions are satisfied and thus the theorem follows. ∎

Towards an algortihm for k-SAT
##############################

There is an obvious algorithm that finds a satisfying assignment: pick an assignment at random and check if the
formula is satisfied, repeat if not. But even though this algorithm is guaranteed by the theorem to find a
solution, the time it takes might be exponential in the problem size.

Assumptions for a faster algorithm
----------------------------------

Let's investigate an algorithm that is faster and still simple to state but the proof of its correctness
and complexity is significantly more involved, even when we add further assumptions about the problem that
alleviate two sources of slowness:

1. $k$ has to be fixed,
2. in each formula, each variable appears in no more than $T = 2^{\\alpha k}$ clauses, for a sufficiently small
   $\\alpha > 0$.

**Theorem.** *There is an algorithm finding satisfying assignments for k-SAT formulas that meet the above
conditions, which runs in expected polynomial time.*

The first condition is restrictive but even for a fixed $k \\geq 3$ the corresponding unrestricted decision variant
of k-SAT is NP-hard and so this condition does not make the problem trivial.

The second condition is needed to guarantee locality of dependency and its value depends on the structure
of the problem. We will arrive at one formula bounding $\\alpha$ but *Probability and Computing* has the full
proof, including all constraints on $\\alpha$.


The algorithm
-------------

Firstly, assume without loss of generality that $k$ is even. Let $l$ be the number of variables and $m$ the
number of clauses in the given formula. Further, we call a clause *dangerous* if $k/2$ of its literals have been
fixed and it is still not satisfied.

The full algorithm is as follows:

1. a. Iterate through variables $x_i$ for $i = 1 \\ldots l$, if $x_i$ is not in any *dangerous* clause pick the
      value for $x_i$ with a coin toss.
   b. Define $H$ to be a graph where each vertex is a *surviving* clause, one that has not been satisfied in
      1.a, and where edge exists if and only if clauses share a *deferred* variable, one that has not been
      assigned a value in 1.a.

      If each of up to $m$ connected components in $H$ has $\\mathcal{O}(\\log{} m)$ vertices,
      then proceed to **2**, else repeat **1.a**.
2. Exhaustively search assignments for the remaining variables, independently in every component.

For a proof of the theorem we are left with analysing complexity and correctness.

*Complexity.* Recall the claim is the algorithm runs in expected polynomial time.

* Part 1.a and checking the condition in 1.b are clearly polynomial time.
* Part 1.b is *expected* to make the algorithm start over only a constant number of times.
  That's not obvious but I will skip the proof.
* In part 2 there are as many exhaustive searches as components; at most $m$.
* In part 2 each exhaustive search is exponential in the size of its own problem but that size is
  $\\mathcal{O}(\\log{} m)$, making each search polynomial time in the size of the whole problem.

  The size of each subproblem is $\\mathcal{O}(\\log{} m)$ because $\\log m$ clauses have $k \\log m$ literals
  and recall $k$ is fixed and thus hidden in $\\mathcal{O}$.

*Intermezzo*. Note that for implementation the size requirement for components can be made concrete
from `the definition of $\\mathcal{O}$<https://en.wikipedia.org/wiki/Big_O_notation#Formal_definition>`_. When
$m$ is less than, say, 16 every component can be considered small (that's the $x < x_0$ case) and otherwise consider
it small if it has fewer than $M \\log{} m$ clauses, for some arbitrarily chosen $M$, say, 4.

*Correctness.* The second phase correctly finds a solution as it's an exhaustive search but only if it was given
a partial assignment that can be extended to a complete one.  We prove this condition is always satisfied using
Lovász's lemma and thus complete the argument in support of the theorem.

**Lemma.** *There is an assignment to the deferred variables such that all the surviving clauses are satisfied.*

**Proof.**
Let $H$ be the dependency graph as defined in the algorithm.
Let's pick a true/false assignment for each deferred variable with an (unbiased) coin toss.

Consider $E_i$, the event $i$-th clause (and a vertex in $H$) is *not* satisfied. We want to prove that the
probability of all opposite events ($\\neg E_i$, $i$-th clause *is* satisfied) occuring together is greater than
0, that is $Pr \\bigl(\\bigcap _{i=1}^{n} \\neg E_i \\bigr) > 0$.

We check the conditions of Lovász local lemma:

1. $Pr(E_i) \\leq 2^{-k/2}$ since a surviving clause has at least $k/2$ deferred variables, and so $p$ can be
   equal to $2^{-k/2}$.
2. Each literal appears in at most $T = 2^{\\alpha k}$ clauses and each clause has exactly $k$ literals, so the
   degree $d$ is at most $kT = k2^{\\alpha k}$.
3. $4dp = 4 \\cdot k2^{\\alpha k} \\cdot 2^{-k/2} \\leq 1$ if $\\alpha$ is sufficiently small (which is
   assumed).

All the conditions are satisfied and thus the lemma follows. ∎

The proof also gives one constraint for $\\alpha$. From the $4dp \\leq 1$ condition of the lemma, $\\alpha$ has to be
small enough to make sure $H$ does not have large components with surviving clauses that could turn out to be
unsatisfiable.  Chances of large components increase with $d$ which grows with $k$ and $\\alpha$, whereas
chances of *not* satisfying a clause decrease with growing $k$.  Thus $\\alpha$ can nearly approach $1/2$ for
large $k$.

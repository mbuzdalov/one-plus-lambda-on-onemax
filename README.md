# (1+λ) EA on OneMax

This repository contributes to parameter analysis
of various flavours of the (1+λ) EA on the OneMax problem.
It accompanies the following papers:

Buzdalov M., Doerr C. Optimal Mutation Rates for the (1+λ) EA on OneMax
// Parallel Problem Solving from Nature XVI -- 2020 -- P.574-587 -- Lecture Notes in Computer Science, no. 12270.

Buzdalov M., Doerr C. Optimal Static Mutation Strength Distributions for the (1+λ) Evolutionary Algorithm on OneMax
// Proceedings of Genetic and Evolutionary Computation Conference -- 2021 -- P.660-668.

Vinokurov D., Buzdalov M. On Optimal Static and Dynamic Parameter Choices for Fixed-Target Optimization.
// Proceedings of Genetic and Evolutionary Computation Conference -- 2022.

# Motivation

Although the framework of (1+λ) evolutionary algorithms
is only marginally more complicated than purely local optimizers,
and OneMax is the very easy and very frequently studied problem,
their combination is still capable of producing surprises to researchers.

The focus of this particular repository is to study the optimal
values of parameters other than the population size λ.
For instance, the standard (1+λ) evolutionary algorithm
for bit strings has the parameter called *mutation probability*,
and its local-search counterpart, the so-called (1+λ) randomized local search
(or RLS for short), has *mutation strength* which is the number of bits to flip
in a single mutation. 

# Highlights

1. The best number of bit flips, as a function of distance to the optimum, is **NOT** monotone for large enough population sizes, unlike the standard (1+1) randomized local search.
2. The runtime as a function of the mutation probability is **NOT** unimodal in the (1+λ) EA with the shift bit mutation.

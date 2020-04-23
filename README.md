# (1+λ) EA on OneMax

This repository contributes to parameter analysis
of various flavours of the (1+λ) EA on the OneMax problem.

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


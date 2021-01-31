package com.github.mbuzdalov.opl.cma;

import java.util.Arrays;
import java.util.function.BiConsumer;

import com.github.mbuzdalov.opl.util.FastRandom;

/**
 * This class implements a separable CMA-ES optimizer derived from the Apache Math version
 * and refactored to suit the needs of this project.
 */
public final class CMAESDistributionOptimizer {
    // Variables to store the results.
    private double[] bestRunIndividual;
    private double bestRunFitness;

    public CMAESDistributionOptimizer(int maxIterations,
                                       int nResamplingUntilFeasible,
                                       int dimension,
                                       int populationSize,
                                       BiConsumer<double[][], double[]> function) {
        if (populationSize <= 0) {
            throw new IllegalArgumentException("Population size must be positive, found " + populationSize);
        }

        // Initialize the common step size.
        double sigma = 1;

        // Initialize constants for internal termination criteria.
        double stopTolUpX = 1e3;
        double stopTolX = 1e-11;
        double stopTolFun = 1e-12;
        double stopTolHistFun = 1e-13;

        // Initialize weights.
        int mu = populationSize / 2;
        double[] weights = new double[mu];
        double sumW = 0;
        double sumWQ = 0;
        for (int i = 0; i < mu; ++i) {
            double w = Math.log(mu + 0.5) - Math.log(i + 1);
            weights[i] = w;
            sumW += w;
            sumWQ += w * w;
        }
        for (int i = 0; i < mu; ++i) {
            weights[i] /= sumW;
        }
        double muEff = sumW * sumW / sumWQ;

        // Initialize pre-tuned parameters and constants.
        double cc = (4 + muEff / dimension) / (dimension + 4 + 2 * muEff / dimension);
        double cs = (muEff + 2) / (dimension + muEff + 3);
        double qCS = Math.sqrt(cs * (2 - cs) * muEff);
        double damps = (1 + 2 * Math.max(0, Math.sqrt((muEff - 1) / (dimension + 1)) - 1)) *
                Math.max(0.3, 1 - dimension / (1e-6 + maxIterations)) + cs;
        double cCov1 = 2 / ((dimension + 1.3) * (dimension + 1.3) + muEff);
        double cCovMu = Math.min(1 - cCov1, 2 * (muEff - 2 + 1 / muEff) / ((dimension + 2) * (dimension + 2) + muEff));
        double cCov1Sep = Math.min(1, cCov1 * (dimension + 1.5) / 3);
        double cCovMuSep = Math.min(1 - cCov1, cCovMu * (dimension + 1.5) / 3);
        double chiN = Math.sqrt(dimension) * (1 - 1 / (4.0 * dimension) + 1 / (21.0 * dimension * dimension));

        // Initialize matrices and step sizes.
        double[] D = new double[dimension];
        double[] C = new double[dimension];
        double[] pc = new double[dimension];
        double[] ps = new double[dimension];
        Arrays.fill(D, 1 / sigma);
        Arrays.fill(C, 1 / sigma / sigma);

        // Initialize fitness history to track stagnation.
        int historySize = 10 + (int) (30.0 * dimension / populationSize);
        FitnessHistory fitnessHistory = new FitnessHistory(historySize);

        // Fetch the thread-local random number generator.
        FastRandom random = FastRandom.threadLocal();

        // Create and evaluate the initial guess.
        double[] xMean = new double[dimension];
        {
            double sum = 0;
            for (int i = 0; i < dimension; ++i) {
                xMean[i] = random.nextDouble();
                sum += xMean[i];
            }
            for (int i = 0; i < dimension; ++i) {
                xMean[i] /= sum;
            }
            double[][] wrappedGuess = { xMean };
            double[] fitnessHolder = new double[1];
            function.accept(wrappedGuess, fitnessHolder);
            bestRunFitness = fitnessHolder[0];
            bestRunIndividual = xMean.clone();
        }
        fitnessHistory.push(bestRunFitness);

        // Allocate all the memory for individuals and auxiliary fitness in/out arrays.
        Individual[] individuals = new Individual[populationSize];
        double[][] exportedGenomes = new double[populationSize][];
        double[] importedFitnessValues = new double[populationSize];
        for (int i = 0; i < populationSize; ++i) {
            individuals[i] = new Individual(dimension);
        }

        // Do the iterations.
        for (int iterations = 1; iterations <= maxIterations; iterations++) {
            // Initialize the current population.
            for (int i = 0; i < populationSize; ++i) {
                Individual ind = individuals[i];
                ind.initialize(random, xMean, D, sigma, nResamplingUntilFeasible);
                exportedGenomes[i] = ind.getFixedX();
            }
            // Run the fitness evaluation on all the fixed individuals.
            function.accept(exportedGenomes, importedFitnessValues);
            // Assign violation-discounted fitness to the individuals.
            double valueRange = max(importedFitnessValues) - min(importedFitnessValues);
            for (int i = 0; i < populationSize; ++i) {
                individuals[i].setRawFitness(importedFitnessValues[i], valueRange);
            }
            // Sort the individuals. Better ones come first.
            Arrays.sort(individuals);

            // Update the before-the-matrix step size (commonly known as ps).
            double normPS = 0;
            for (int i = 0; i < dimension; ++i) {
                double zMeanI = 0;
                for (int j = 0; j < mu; ++j) {
                    zMeanI += weights[j] * individuals[j].getZ(i);
                }
                ps[i] = ps[i] * (1 - cs) + zMeanI * qCS;
                normPS += ps[i] * ps[i];
            }
            normPS = Math.sqrt(normPS);

            // Compute normalization coefficients for the rest of the updates.
            boolean hSig = normPS / Math.sqrt(1 - Math.pow(1 - cs, 2 * iterations)) / chiN < 1.4 + 2 / (dimension + 1.0);
            double q2 = hSig ? Math.sqrt(cc * (2 - cc) * muEff) / sigma : 0;
            double oldFac = (1 - cCov1Sep - cCovMuSep) + (hSig ? 0 : cCov1Sep * cc * (2 - cc));

            // Do all the updates in a single run to avoid storage of temporary variables.
            for (int i = 0; i < dimension; ++i) {
                double xOldI = xMean[i];
                xMean[i] = 0;
                double weighedSquare = 0;
                for (int j = 0; j < mu; ++j) {
                    double xi = individuals[j].getX(i);
                    double zi = individuals[j].getZ(i);
                    double wj = weights[j];
                    xMean[i] += wj * xi;
                    weighedSquare += wj * zi * zi;
                }
                pc[i] = pc[i] * (1 - cc) + q2 * (xMean[i] - xOldI);
                C[i] = C[i] * oldFac + pc[i] * pc[i] * cCov1Sep + C[i] * weighedSquare * cCovMuSep;
                D[i] = Math.sqrt(C[i]);
            }

            // Adapt the common step size.
            sigma *= Math.exp(Math.min(1, (normPS / chiN - 1) * cs / damps));

            // Update the best individual, and also check up the worst fitness across the iteration.
            double bestFitness = individuals[0].getFitness();
            double worstFitness = individuals[populationSize - 1].getFitness();
            if (bestRunFitness > bestFitness) {
                bestRunFitness = bestFitness;
                bestRunIndividual = individuals[0].getFixedX().clone();
            }

            double maxD = max(D);
            double minD = min(D);
            double historyBest = fitnessHistory.getMinimum();
            double historyWorst = fitnessHistory.getMaximum();

            if (maxD / minD > 1e7
                || sigma * maxD > stopTolUpX
                || iterations > 2 && Math.max(historyWorst, worstFitness) - Math.min(historyBest, bestFitness) < stopTolFun
                || iterations > fitnessHistory.getCapacity() && historyWorst - historyBest < stopTolHistFun) {
                return;
            }

            // Check termination degeneration-based criterion 2.
            boolean validated = false;
            for (int i = 0; i < dimension; i++) {
                if (sigma * Math.max(Math.abs(pc[i]), D[i]) > stopTolX) {
                    validated = true;
                    break;
                }
            }
            if (!validated) {
                return;
            }

            // Adjust step size in the case of equal function values, the case of plain population.
            if (bestRunFitness == individuals[(int) (0.1 + populationSize / 4.0)].getFitness()) {
                sigma *= Math.exp(0.2 + cs / damps);
            }

            // Adjust step size in the case of equal fitness values, the case of plain history.
            if (iterations > 2 && Math.max(historyWorst, bestFitness) - Math.min(historyBest, bestFitness) == 0) {
                sigma *= Math.exp(0.2 + cs / damps);
            }

            // Update the fitness history.
            fitnessHistory.push(bestFitness);
        }
    }

    public double[] getBestIndividual() {
        return bestRunIndividual;
    }

    public double getBestFitness() {
        return bestRunFitness;
    }

    private static double max(final double[] m) {
        double max = Double.MIN_VALUE;
        for (double v : m) {
            if (max < v) {
                max = v;
            }
        }
        return max;
    }

    private static double min(final double[] m) {
        double min = Double.MAX_VALUE;
        for (double v : m) {
            if (min > v) {
                min = v;
            }
        }
        return min;
    }
}

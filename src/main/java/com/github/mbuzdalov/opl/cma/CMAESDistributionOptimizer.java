package com.github.mbuzdalov.opl.cma;

import java.util.Arrays;
import java.util.function.BiConsumer;

import org.apache.commons.math3.optim.PointValuePair;
import org.apache.commons.math3.random.RandomGenerator;

/**
 * This class implements a separable CMA-ES optimizer derived from the Apache Math version
 * and refactored to suit the needs of this project.
 */
public class CMAESDistributionOptimizer {
    // Main user input parameters.
    private final int populationSize;
    private final int dimension;
    private final BiConsumer<double[][], double[]> function;

    // Other configuration parameters.
    private final int nResamplingUntilFeasible;
    private final int maxIterations;

    // Internal termination criteria.
    private final double stopTolUpX;
    private final double stopTolX;
    private final double stopTolFun;
    private final double stopTolHistFun;

    // Fixed auto-inferred parameters.
    private final int mu;
    private final double[] weights;
    private final double muEff;
    private final double cc;
    private final double cs;
    private final double damps;
    private final double chiN;
    private final double cCov1Sep;
    private final double cCovMuSep;

    // Varying auto-inferred parameters.
    private double sigma;

    // Vectors and matrices.
    private final double[] pc;
    private final double[] ps;
    private final double[] D;
    private final double[] C;

    /** History queue of best values. */
    private final FitnessHistory fitnessHistory;

    /** Random generator. */
    private final RandomGenerator random;

    public CMAESDistributionOptimizer(int maxIterations,
                                       int nResamplingUntilFeasible,
                                       RandomGenerator random,
                                       int dimension,
                                       int populationSize,
                                       BiConsumer<double[][], double[]> function) {
        if (populationSize <= 0) {
            throw new IllegalArgumentException("Population size must be positive, found " + populationSize);
        }

        this.maxIterations = maxIterations;
        this.nResamplingUntilFeasible = nResamplingUntilFeasible;
        this.random = random;
        this.dimension = dimension;
        this.populationSize = populationSize;
        this.function = function;

        this.sigma = 1;

        // initialize termination criteria
        this.stopTolUpX = 1e3;
        this.stopTolX = 1e-11;
        this.stopTolFun = 1e-12;
        this.stopTolHistFun = 1e-13;

        // initialize selection strategy parameters
        this.mu = populationSize / 2;
        this.weights = new double[mu];
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
        this.muEff = sumW * sumW / sumWQ;

        // initialize parameters and constants
        this.cc = (4 + muEff / dimension) / (dimension + 4 + 2 * muEff / dimension);
        this.cs = (muEff + 2) / (dimension + muEff + 3);
        this.damps = (1 + 2 * Math.max(0, Math.sqrt((muEff - 1) / (dimension + 1)) - 1)) *
                Math.max(0.3, 1 - dimension / (1e-6 + maxIterations)) + cs;
        double cCov1 = 2 / ((dimension + 1.3) * (dimension + 1.3) + muEff);
        double cCovMu = Math.min(1 - cCov1, 2 * (muEff - 2 + 1 / muEff) / ((dimension + 2) * (dimension + 2) + muEff));
        this.cCov1Sep = Math.min(1, cCov1 * (dimension + 1.5) / 3);
        this.cCovMuSep = Math.min(1 - cCov1, cCovMu * (dimension + 1.5) / 3);
        this.chiN = Math.sqrt(dimension) * (1 - 1 / (4.0 * dimension) + 1 / (21.0 * dimension * dimension));

        // initialize matrices and vectors that change
        D = new double[dimension];
        C = new double[dimension];
        pc = new double[dimension];
        ps = new double[dimension];

        Arrays.fill(D, 1 / sigma);
        Arrays.fill(C, 1 / sigma / sigma);

        int historySize = 10 + (int) (30.0 * dimension / populationSize);
        this.fitnessHistory = new FitnessHistory(historySize);
    }

    public PointValuePair optimize() {
        final double[] xMean = new double[dimension];
        double bestValue;
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
            bestValue = fitnessHolder[0];
        }
        fitnessHistory.push(bestValue);
        PointValuePair optimum = new PointValuePair(xMean.clone(), bestValue);

        // -------------------- Generation Loop --------------------------------

        final Individual[] individuals = new Individual[populationSize];
        final double[][] exportedGenomes = new double[populationSize][];
        final double[] importedFitnessValues = new double[populationSize];
        for (int i = 0; i < populationSize; ++i) {
            individuals[i] = new Individual(dimension);
        }

        for (int iterations = 1; iterations <= maxIterations; iterations++) {
            for (int i = 0; i < populationSize; ++i) {
                Individual ind = individuals[i];
                for (int t = 0; t <= nResamplingUntilFeasible; ++t) {
                    ind.initZ(random);
                    ind.populateX(xMean, D, sigma);
                    if (ind.isFeasible()) {
                        break;
                    }
                }
                exportedGenomes[i] = ind.fixedX;
            }
            function.accept(exportedGenomes, importedFitnessValues);
            double valueRange = max(importedFitnessValues) - min(importedFitnessValues);
            for (int i = 0; i < populationSize; ++i) {
                individuals[i].setRawFitness(importedFitnessValues[i], valueRange);
            }

            Arrays.sort(individuals);

            double q1 = Math.sqrt(cs * (2 - cs) * muEff);
            double normPS = 0;
            for (int i = 0; i < dimension; ++i) {
                double zMeanI = 0;
                for (int j = 0; j < mu; ++j) {
                    zMeanI += weights[j] * individuals[j].z[i];
                }
                ps[i] = ps[i] * (1 - cs) + zMeanI * q1;
                normPS += ps[i] * ps[i];
            }
            normPS = Math.sqrt(normPS);

            boolean hSig = normPS / Math.sqrt(1 - Math.pow(1 - cs, 2 * iterations)) / chiN < 1.4 + 2 / (dimension + 1.0);
            double q2 = hSig ? Math.sqrt(cc * (2 - cc) * muEff) / sigma : 0;
            double oldFac = (1 - cCov1Sep - cCovMuSep) + (hSig ? 0 : cCov1Sep * cc * (2 - cc));

            for (int i = 0; i < dimension; ++i) {
                double xOldI = xMean[i];
                xMean[i] = 0;
                double weighedSquare = 0;
                for (int j = 0; j < mu; ++j) {
                    double xi = individuals[j].x[i];
                    double zi = individuals[j].z[i];
                    double wj = weights[j];
                    xMean[i] += wj * xi;
                    weighedSquare += wj * zi * zi;
                }
                pc[i] = pc[i] * (1 - cc) + q2 * (xMean[i] - xOldI);
                C[i] = C[i] * oldFac + pc[i] * pc[i] * cCov1Sep + C[i] * weighedSquare * cCovMuSep;
                D[i] = Math.sqrt(C[i]);
            }

            // Adapt step size sigma - Eq. (5)
            sigma *= Math.exp(Math.min(1, (normPS / chiN - 1) * cs / damps));
            final double bestFitness = individuals[0].fitness;
            final double worstFitness = individuals[populationSize - 1].fitness;
            if (bestValue > bestFitness) {
                bestValue = bestFitness;
                optimum = new PointValuePair(individuals[0].fixedX.clone(), bestFitness);
            }
            // handle termination criteria
            if (shallExitBySigmaTolerance()) {
                break;
            }
            final double historyBest = fitnessHistory.getMinimum();
            final double historyWorst = fitnessHistory.getMaximum();
            if (iterations > 2 && Math.max(historyWorst, worstFitness) - Math.min(historyBest, bestFitness) < stopTolFun) {
                break;
            }
            if (iterations > fitnessHistory.getCapacity() && historyWorst - historyBest < stopTolHistFun) {
                break;
            }
            // condition number of the covariance matrix exceeds 1e14
            if (max(D) / min(D) > 1e7) {
                break;
            }
            // Adjust step size in case of equal function values (flat fitness)
            if (bestValue == individuals[(int) (0.1 + populationSize / 4.0)].fitness) {
                sigma *= Math.exp(0.2 + cs / damps);
            }
            if (iterations > 2 && Math.max(historyWorst, bestFitness) - Math.min(historyBest, bestFitness) == 0) {
                sigma *= Math.exp(0.2 + cs / damps);
            }
            // store best in history
            fitnessHistory.push(bestFitness);
        }
        return optimum;
    }

    private static class Individual implements Comparable<Individual> {
        private final double[] x, fixedX, z;
        private double fitness, penalty;

        Individual(int dimension) {
            x = new double[dimension];
            fixedX = new double[dimension];
            z = new double[dimension];
        }

        void initZ(RandomGenerator generator) {
            for (int i = 0; i < z.length; ++i) {
                z[i] = generator.nextGaussian();
            }
        }

        void populateX(double[] xMean, double[] diagD, double sigma) {
            penalty = 0;
            for (int i = 0; i < x.length; ++i) {
                x[i] = xMean[i] + diagD[i] * z[i] * sigma;
                fixedX[i] = Math.min(1, Math.max(0, x[i]));
                penalty += Math.abs(x[i] - fixedX[i]);
            }
        }

        void setRawFitness(double rawFitness, double penaltyScale) {
            this.fitness = rawFitness + penalty * penaltyScale;
        }

        boolean isFeasible() {
            return penalty == 0;
        }

        @Override
        public int compareTo(Individual o) {
            return Double.compare(fitness, o.fitness);
        }
    }

    private boolean shallExitBySigmaTolerance() {
        for (int i = 0; i < dimension; i++) {
            if (sigma * D[i] > stopTolUpX) {
                return true;
            }
        }
        for (int i = 0; i < dimension; i++) {
            if (sigma * Math.max(Math.abs(pc[i]), D[i]) > stopTolX) {
                return false;
            }
        }
        return true;
    }

    private static class FitnessHistory {
        private final double[] inputStack, outputMin, outputMax;
        private int nInputs, nOutputs;
        private double inputMin, inputMax;

        FitnessHistory(int length) {
            inputStack = new double[length];
            outputMin = new double[length];
            outputMax = new double[length];
            inputMin = Double.POSITIVE_INFINITY;
            inputMax = Double.NEGATIVE_INFINITY;
        }

        void push(double value) {
            if (nInputs + nOutputs == getCapacity()) {
                if (nOutputs == 0) {
                    double tailMin = Double.POSITIVE_INFINITY;
                    double tailMax = Double.NEGATIVE_INFINITY;
                    while (nInputs > 0) {
                        --nInputs;
                        double popped = inputStack[nInputs];
                        tailMin = Math.min(tailMin, popped);
                        tailMax = Math.max(tailMax, popped);
                        outputMin[nOutputs] = tailMin;
                        outputMax[nOutputs] = tailMax;
                        ++nOutputs;
                    }
                    inputMin = Double.POSITIVE_INFINITY;
                    inputMax = Double.NEGATIVE_INFINITY;
                }
                --nOutputs;
            }
            inputStack[nInputs] = value;
            ++nInputs;
            inputMin = Math.min(inputMin, value);
            inputMax = Math.max(inputMax, value);
        }

        int getCapacity() {
            return inputStack.length;
        }

        double getMinimum() {
            return nOutputs == 0 ? inputMin : Math.min(inputMin, outputMin[nOutputs - 1]);
        }

        double getMaximum() {
            return nOutputs == 0 ? inputMax : Math.max(inputMax, outputMax[nOutputs - 1]);
        }
    }

    // -----Matrix utility functions similar to the Matlab build in functions------

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

package com.github.mbuzdalov.opl.cma;

import java.util.Arrays;
import java.util.function.BiConsumer;

import org.apache.commons.math3.exception.NotStrictlyPositiveException;
import org.apache.commons.math3.linear.Array2DRowRealMatrix;
import org.apache.commons.math3.linear.MatrixUtils;
import org.apache.commons.math3.linear.RealMatrix;
import org.apache.commons.math3.optim.PointValuePair;
import org.apache.commons.math3.random.RandomGenerator;
import org.apache.commons.math3.util.MathArrays;

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
    private final RealMatrix weights;
    private final double mueff;
    private final double cc;
    private final double cs;
    private final double damps;
    private final double chiN;
    private final double ccov1Sep;
    private final double ccovmuSep;

    // Varying auto-inferred parameters.
    private double sigma;

    private RealMatrix pc;
    private RealMatrix ps;
    private final RealMatrix B;
    private RealMatrix diagD;
    private RealMatrix diagC;

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
            throw new NotStrictlyPositiveException(populationSize);
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
        RealMatrix rawWeights = log(naturals(mu)).scalarMultiply(-1).scalarAdd(Math.log(mu + 0.5));
        double sumW = 0;
        double sumWQ = 0;
        for (int i = 0; i < mu; i++) {
            double w = rawWeights.getEntry(i, 0);
            sumW += w;
            sumWQ += w * w;
        }
        this.weights = rawWeights.scalarMultiply(1 / sumW);
        this.mueff = sumW * sumW / sumWQ;

        // initialize parameters and constants
        this.cc = (4 + mueff / dimension) / (dimension + 4 + 2 * mueff / dimension);
        this.cs = (mueff + 2) / (dimension + mueff + 3);
        this.damps = (1 + 2 * Math.max(0, Math.sqrt((mueff - 1) / (dimension + 1)) - 1)) *
                Math.max(0.3, 1 - dimension / (1e-6 + maxIterations)) + cs;
        double ccov1 = 2 / ((dimension + 1.3) * (dimension + 1.3) + mueff);
        double ccovmu = Math.min(1 - ccov1, 2 * (mueff - 2 + 1 / mueff) / ((dimension + 2) * (dimension + 2) + mueff));
        this.ccov1Sep = Math.min(1, ccov1 * (dimension + 1.5) / 3);
        this.ccovmuSep = Math.min(1 - ccov1, ccovmu * (dimension + 1.5) / 3);
        this.chiN = Math.sqrt(dimension) * (1 - 1 / (4.0 * dimension) + 1 / (21.0 * dimension * dimension));

        // initialize matrices and vectors that change
        diagD = columnOfOnes(dimension).scalarMultiply(1 / sigma);
        diagC = square(diagD);
        pc = zeros(dimension, 1);
        ps = zeros(dimension, 1);
        B = eye(dimension, dimension);

        int historySize = 10 + (int) (3 * 10 * dimension / (double) populationSize);
        this.fitnessHistory = new FitnessHistory(historySize);
    }

    public PointValuePair optimize() {
        double[] guess = generateNormalizedRandomVector();
        double bestValue;
        {
            double[] fixedGuess = repair(guess);
            double[] fitnessHolder = new double[1];
            function.accept(new double[][] {fixedGuess}, fitnessHolder);
            bestValue = fitnessHolder[0] + penalty(guess, fixedGuess);
        }
        // Vectors and matrices.
        RealMatrix xMean = MatrixUtils.createColumnRealMatrix(guess);
        fitnessHistory.push(bestValue);
        PointValuePair optimum = new PointValuePair(guess, bestValue);

        // -------------------- Generation Loop --------------------------------

        for (int iterations = 1; iterations <= maxIterations; ++iterations) {
            // Generate and evaluate lambda offspring
            final RealMatrix arz = gaussianMatrix(dimension, populationSize);
            final RealMatrix arx = zeros(dimension, populationSize);
            final double[] fitness = new double[populationSize];
            final double[][] fixedIndividuals = new double[populationSize][];
            final double[] penalties = new double[populationSize];

            // generate random offspring
            for (int k = 0; k < populationSize; k++) {
                for (int i = 0; i <= nResamplingUntilFeasible; i++) {
                    arx.setColumnMatrix(k, xMean.add(times(diagD, arz.getColumnMatrix(k)).scalarMultiply(sigma)));
                    if (i >= nResamplingUntilFeasible || isFeasible(arx.getColumn(k))) {
                        break;
                    }
                    // regenerate random arguments for row
                    arz.setColumn(k, gaussianArray(dimension));
                }
                double[] rawIndividual = arx.getColumn(k);
                fixedIndividuals[k] = repair(rawIndividual);
                penalties[k] = penalty(rawIndividual, fixedIndividuals[k]);
            }

            double[] rawFitness = new double[populationSize];
            function.accept(fixedIndividuals, rawFitness);
            double valueRange = max(rawFitness) - min(rawFitness);
            for (int i = 0; i < populationSize; ++i) {
                fitness[i] = rawFitness[i] + penalties[i] * valueRange;
            }

            // Sort by fitness and compute weighted mean into xMean
            final int[] arindex = sortedIndices(fitness);
            // Calculate new xMean, this is selection and recombination
            final RealMatrix xOld = xMean; // for speed up of Eq. (2) and (3)
            final RealMatrix bestArx = selectColumns(arx, MathArrays.copyOf(arindex, mu));
            xMean = bestArx.multiply(weights);
            final RealMatrix bestArz = selectColumns(arz, MathArrays.copyOf(arindex, mu));
            final RealMatrix zMean = bestArz.multiply(weights);

            double q1 = Math.sqrt(cs * (2 - cs) * mueff);
            ps = ps.scalarMultiply(1 - cs).add(B.multiply(zMean).scalarMultiply(q1));
            double normPS = ps.getFrobeniusNorm();
            boolean hSig = normPS / Math.sqrt(1 - Math.pow(1 - cs, 2 * iterations)) / chiN < 1.4 + 2 / (dimension + 1.0);
            double q2 = hSig ? Math.sqrt(cc * (2 - cc) * mueff) / sigma : 0;
            pc = pc.scalarMultiply(1 - cc).add(xMean.subtract(xOld).scalarMultiply(q2));
            double oldFac = (1 - ccov1Sep - ccovmuSep) + (hSig ? 0 : ccov1Sep * cc * (2 - cc));
            diagC = diagC.scalarMultiply(oldFac) // regard old matrix
                    .add(square(pc).scalarMultiply(ccov1Sep)) // plus rank one update
                    .add((times(diagC, square(bestArz).multiply(weights))) // plus rank mu update
                            .scalarMultiply(ccovmuSep));
            diagD = sqrt(diagC); // replaces eig(C)
            // Adapt step size sigma - Eq. (5)
            sigma *= Math.exp(Math.min(1, (normPS/chiN - 1) * cs / damps));
            final double bestFitness = fitness[arindex[0]];
            final double worstFitness = fitness[arindex[arindex.length - 1]];
            if (bestValue > bestFitness) {
                bestValue = bestFitness;
                optimum = new PointValuePair(repair(bestArx.getColumn(0)), bestFitness);
            }
            // handle termination criteria
            if (shallExitBySigmaTolerance(pc.getColumn(0), diagD.getColumn(0))) {
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
            if (max(diagD) / min(diagD) > 1e7) {
                break;
            }
            // Adjust step size in case of equal function values (flat fitness)
            if (bestValue == fitness[arindex[(int) (0.1 + populationSize / 4.0)]]) {
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

    private boolean shallExitBySigmaTolerance(double[] pcCol, double[] diagD) {
        for (int i = 0; i < dimension; i++) {
            if (sigma * diagD[i] > stopTolUpX) {
                return true;
            }
        }
        for (int i = 0; i < dimension; i++) {
            if (sigma * Math.max(Math.abs(pcCol[i]), diagD[i]) > stopTolX) {
                return false;
            }
        }
        return true;
    }

    private double[] generateNormalizedRandomVector() {
        final double[] guess = new double[dimension];
        double guessSum = 0;
        for (int i = 0; i < dimension; ++i) {
            guess[i] = random.nextDouble();
            guessSum += guess[i];
        }
        for (int i = 0; i < dimension; ++i) {
            guess[i] /= guessSum;
        }
        return guess;
    }

    private static int[] sortedIndices(final double[] doubles) {
        final DoubleIndex[] dis = new DoubleIndex[doubles.length];
        for (int i = 0; i < doubles.length; i++) {
            dis[i] = new DoubleIndex(doubles[i], i);
        }
        Arrays.sort(dis);
        final int[] indices = new int[doubles.length];
        for (int i = 0; i < doubles.length; i++) {
            indices[i] = dis[i].index;
        }
        return indices;
    }

    private static class DoubleIndex implements Comparable<DoubleIndex> {
        private final double value;
        private final int index;

        DoubleIndex(double value, int index) {
            this.value = value;
            this.index = index;
        }

        public int compareTo(DoubleIndex o) {
            return Double.compare(value, o.value);
        }
    }

    private static boolean isFeasible(final double[] x) {
        for (double v : x) {
            if (v < 0 || v > 1) {
                return false;
            }
        }
        return true;
    }

    private static double[] repair(final double[] x) {
        final double[] repaired = x.clone();
        for (int i = 0; i < repaired.length; i++) {
            if (repaired[i] < 0) {
                repaired[i] = 0;
            } else if (repaired[i] > 1) {
                repaired[i] = 1;
            }
        }
        return repaired;
    }

    private static double penalty(final double[] x, final double[] repaired) {
        double penalty = 0;
        for (int i = 0; i < x.length; i++) {
            double diff = Math.abs(x[i] - repaired[i]);
            penalty += diff;
        }
        return penalty;
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

    private static RealMatrix log(final RealMatrix m) {
        final double[][] d = new double[m.getRowDimension()][m.getColumnDimension()];
        for (int r = 0; r < m.getRowDimension(); r++) {
            for (int c = 0; c < m.getColumnDimension(); c++) {
                d[r][c] = Math.log(m.getEntry(r, c));
            }
        }
        return new Array2DRowRealMatrix(d, false);
    }

    private static RealMatrix sqrt(final RealMatrix m) {
        final double[][] d = new double[m.getRowDimension()][m.getColumnDimension()];
        for (int r = 0; r < m.getRowDimension(); r++) {
            for (int c = 0; c < m.getColumnDimension(); c++) {
                d[r][c] = Math.sqrt(m.getEntry(r, c));
            }
        }
        return new Array2DRowRealMatrix(d, false);
    }

    private static RealMatrix square(final RealMatrix m) {
        final double[][] d = new double[m.getRowDimension()][m.getColumnDimension()];
        for (int r = 0; r < m.getRowDimension(); r++) {
            for (int c = 0; c < m.getColumnDimension(); c++) {
                double e = m.getEntry(r, c);
                d[r][c] = e * e;
            }
        }
        return new Array2DRowRealMatrix(d, false);
    }

    private static RealMatrix times(final RealMatrix m, final RealMatrix n) {
        final double[][] d = new double[m.getRowDimension()][m.getColumnDimension()];
        for (int r = 0; r < m.getRowDimension(); r++) {
            for (int c = 0; c < m.getColumnDimension(); c++) {
                d[r][c] = m.getEntry(r, c) * n.getEntry(r, c);
            }
        }
        return new Array2DRowRealMatrix(d, false);
    }

    private static RealMatrix selectColumns(final RealMatrix m, final int[] cols) {
        final double[][] d = new double[m.getRowDimension()][cols.length];
        for (int r = 0; r < m.getRowDimension(); r++) {
            for (int c = 0; c < cols.length; c++) {
                d[r][c] = m.getEntry(r, cols[c]);
            }
        }
        return new Array2DRowRealMatrix(d, false);
    }

    private static RealMatrix columnOfOnes(int n) {
        final double[][] d = new double[n][1];
        for (int r = 0; r < n; r++) {
            Arrays.fill(d[r], 1);
        }
        return new Array2DRowRealMatrix(d, false);
    }

    private static RealMatrix eye(int n, int m) {
        final double[][] d = new double[n][m];
        for (int r = 0; r < n; r++) {
            if (r < m) {
                d[r][r] = 1;
            }
        }
        return new Array2DRowRealMatrix(d, false);
    }

    private static RealMatrix zeros(int n, int m) {
        return new Array2DRowRealMatrix(n, m);
    }

    private static RealMatrix naturals(int size) {
        final double[][] d = new double[size][1];
        double value = 1;
        for (int r = 0; r < size; r++) {
            d[r][0] = value;
            value += 1;
        }
        return new Array2DRowRealMatrix(d, false);
    }

    private static double max(final RealMatrix m) {
        double max = -Double.MAX_VALUE;
        for (int r = 0; r < m.getRowDimension(); r++) {
            for (int c = 0; c < m.getColumnDimension(); c++) {
                double e = m.getEntry(r, c);
                if (max < e) {
                    max = e;
                }
            }
        }
        return max;
    }

    private static double min(final RealMatrix m) {
        double min = Double.MAX_VALUE;
        for (int r = 0; r < m.getRowDimension(); r++) {
            for (int c = 0; c < m.getColumnDimension(); c++) {
                double e = m.getEntry(r, c);
                if (min > e) {
                    min = e;
                }
            }
        }
        return min;
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

    private double[] gaussianArray(int size) {
        final double[] result = new double[size];
        for (int i = 0; i < size; i++) {
            result[i] = random.nextGaussian();
        }
        return result;
    }

    private RealMatrix gaussianMatrix(int size, int popSize) {
        final double[][] d = new double[size][popSize];
        for (int r = 0; r < size; r++) {
            for (int c = 0; c < popSize; c++) {
                d[r][c] = random.nextGaussian();
            }
        }
        return new Array2DRowRealMatrix(d, false);
    }
}

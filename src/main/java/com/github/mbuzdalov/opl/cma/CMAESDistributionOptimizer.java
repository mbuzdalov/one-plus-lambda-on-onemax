package com.github.mbuzdalov.opl.cma;

import java.util.Arrays;
import java.util.function.Function;

import org.apache.commons.math3.exception.NotStrictlyPositiveException;
import org.apache.commons.math3.linear.Array2DRowRealMatrix;
import org.apache.commons.math3.linear.EigenDecomposition;
import org.apache.commons.math3.linear.MatrixUtils;
import org.apache.commons.math3.linear.RealMatrix;
import org.apache.commons.math3.optim.PointValuePair;
import org.apache.commons.math3.random.RandomGenerator;
import org.apache.commons.math3.util.FastMath;
import org.apache.commons.math3.util.MathArrays;

public class CMAESDistributionOptimizer {
    // Main user input parameters.
    private final int populationSize;
    private final int dimension;
    private final Function<double[][], double[]> function;

    // Other configuration parameters.
    private final boolean isActiveCMA;
    private final int nResamplingUntilFeasible;
    private final int nDiagonalOnlyIterations;
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
    private final double ccov1;
    private final double ccovmu;
    private final double chiN;
    private final double ccov1Sep;
    private final double ccovmuSep;

    // Number of iterations done so far.
    private int iterations;

    // Varying auto-inferred parameters.
    private double sigma;
    private double normps;

    // Vectors and matrices.
    private RealMatrix xmean;
    private RealMatrix pc;
    private RealMatrix ps;
    private RealMatrix B;
    private RealMatrix D;
    private RealMatrix BD;
    private RealMatrix diagD;
    private RealMatrix C;
    private RealMatrix diagC;

    /** History queue of best values. */
    private final double[] fitnessHistory;

    /** Random generator. */
    private final RandomGenerator random;

    public CMAESDistributionOptimizer(int maxIterations,
                                      boolean isActiveCMA,
                                      int nDiagonalOnlyIterations,
                                      int nResamplingUntilFeasible,
                                      RandomGenerator random,
                                      int dimension,
                                      int populationSize,
                                      Function<double[][], double[]> function) {
        if (populationSize <= 0) {
            throw new NotStrictlyPositiveException(populationSize);
        }

        this.maxIterations = maxIterations;
        this.isActiveCMA = isActiveCMA;
        this.nDiagonalOnlyIterations = nDiagonalOnlyIterations;
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
        RealMatrix rawWeights = log(sequence(1, mu, 1)).scalarMultiply(-1).scalarAdd(FastMath.log(mu + 0.5));
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
        this.damps = (1 + 2 * FastMath.max(0, FastMath.sqrt((mueff - 1) / (dimension + 1)) - 1)) *
                FastMath.max(0.3, 1 - dimension / (1e-6 + maxIterations)) + cs;
        this.ccov1 = 2 / ((dimension + 1.3) * (dimension + 1.3) + mueff);
        this.ccovmu = FastMath.min(1 - ccov1, 2 * (mueff - 2 + 1 / mueff) / ((dimension + 2) * (dimension + 2) + mueff));
        this.ccov1Sep = FastMath.min(1, ccov1 * (dimension + 1.5) / 3);
        this.ccovmuSep = FastMath.min(1 - ccov1, ccovmu * (dimension + 1.5) / 3);
        this.chiN = FastMath.sqrt(dimension) * (1 - 1 / (4.0 * dimension) + 1 / (21.0 * dimension * dimension));

        // initialize matrices and vectors that change
        diagD = ones(dimension, 1).scalarMultiply(1 / sigma);
        diagC = square(diagD);
        pc = zeros(dimension, 1);
        ps = zeros(dimension, 1);
        normps = ps.getFrobeniusNorm();
        B = eye(dimension, dimension);
        D = ones(dimension, 1);
        BD = times(B, repmat(diagD.transpose(), dimension, 1));
        C = B.multiply(diag(square(D)).multiply(B.transpose()));

        int historySize = 10 + (int) (3 * 10 * dimension / (double) populationSize);
        this.fitnessHistory = new double[historySize];
        for (int i = 0; i < historySize; i++) {
            this.fitnessHistory[i] = Double.MAX_VALUE;
        }
    }

    public PointValuePair optimize() {
        // -------------------- Initialization --------------------------------
        initializeCMA();

        iterations = 0;
        double[] guess = generateNormalizedRandomVector();
        double[] fixedGuess = repair(guess);
        double bestValue = function.apply(new double[][] {fixedGuess})[0] + penalty(guess, fixedGuess);
        xmean = MatrixUtils.createColumnRealMatrix(guess);
        push(fitnessHistory, bestValue);
        PointValuePair optimum = new PointValuePair(guess, bestValue);

        // -------------------- Generation Loop --------------------------------

        generationLoop:
        for (iterations = 1; iterations <= maxIterations; iterations++) {
            // Generate and evaluate lambda offspring
            final RealMatrix arz = randn1(dimension, populationSize);
            final RealMatrix arx = zeros(dimension, populationSize);
            final double[] fitness = new double[populationSize];
            final double[][] fixedIndividuals = new double[populationSize][];
            final double[] penalties = new double[populationSize];

            // generate random offspring
            for (int k = 0; k < populationSize; k++) {
                RealMatrix arxk = null;
                for (int i = 0; i <= nResamplingUntilFeasible; i++) {
                    if (nDiagonalOnlyIterations <= iterations) {
                        arxk = xmean.add(BD.multiply(arz.getColumnMatrix(k))
                                .scalarMultiply(sigma)); // m + sig * Normal(0,C)
                    } else {
                        arxk = xmean.add(times(diagD,arz.getColumnMatrix(k))
                                .scalarMultiply(sigma));
                    }
                    if (i >= nResamplingUntilFeasible || isFeasible(arxk.getColumn(0))) {
                        break;
                    }
                    // regenerate random arguments for row
                    arz.setColumn(k, randn(dimension));
                }
                copyColumn(arxk, 0, arx, k);
                double[] rawIndividual = arx.getColumn(k);
                fixedIndividuals[k] = repair(rawIndividual);
                penalties[k] = penalty(rawIndividual, fixedIndividuals[k]);
            }

            double[] rawFitness = function.apply(fixedIndividuals);
            double valueRange = max(rawFitness) - min(rawFitness);
            for (int i = 0; i < populationSize; ++i) {
                fitness[i] = rawFitness[i] + penalties[i] * valueRange;
            }

            // Sort by fitness and compute weighted mean into xmean
            final int[] arindex = sortedIndices(fitness);
            // Calculate new xmean, this is selection and recombination
            final RealMatrix xold = xmean; // for speed up of Eq. (2) and (3)
            final RealMatrix bestArx = selectColumns(arx, MathArrays.copyOf(arindex, mu));
            xmean = bestArx.multiply(weights);
            final RealMatrix bestArz = selectColumns(arz, MathArrays.copyOf(arindex, mu));
            final RealMatrix zmean = bestArz.multiply(weights);
            final boolean hsig = updateEvolutionPaths(zmean, xold);
            if (nDiagonalOnlyIterations <= iterations) {
                updateCovariance(hsig, bestArx, arz, arindex, xold);
            } else {
                updateCovarianceDiagonalOnly(hsig, bestArz);
            }
            // Adapt step size sigma - Eq. (5)
            sigma *= FastMath.exp(FastMath.min(1, (normps/chiN - 1) * cs / damps));
            final double bestFitness = fitness[arindex[0]];
            final double worstFitness = fitness[arindex[arindex.length - 1]];
            if (bestValue > bestFitness) {
                bestValue = bestFitness;
                optimum = new PointValuePair(repair(bestArx.getColumn(0)), bestFitness);
            }
            // handle termination criteria
            final double[] sqrtDiagC = sqrt(diagC).getColumn(0);
            final double[] pcCol = pc.getColumn(0);
            for (int i = 0; i < dimension; i++) {
                if (sigma * FastMath.max(FastMath.abs(pcCol[i]), sqrtDiagC[i]) > stopTolX) {
                    break;
                }
                if (i >= dimension - 1) {
                    break generationLoop;
                }
            }
            for (int i = 0; i < dimension; i++) {
                if (sigma * sqrtDiagC[i] > stopTolUpX) {
                    break generationLoop;
                }
            }
            final double historyBest = min(fitnessHistory);
            final double historyWorst = max(fitnessHistory);
            if (iterations > 2 &&
                    FastMath.max(historyWorst, worstFitness) -
                            FastMath.min(historyBest, bestFitness) < stopTolFun) {
                break generationLoop;
            }
            if (iterations > fitnessHistory.length &&
                    historyWorst - historyBest < stopTolHistFun) {
                break generationLoop;
            }
            // condition number of the covariance matrix exceeds 1e14
            if (max(diagD) / min(diagD) > 1e7) {
                break generationLoop;
            }
            // Adjust step size in case of equal function values (flat fitness)
            if (bestValue == fitness[arindex[(int)(0.1+ populationSize /4.)]]) {
                sigma *= FastMath.exp(0.2 + cs / damps);
            }
            if (iterations > 2 && FastMath.max(historyWorst, bestFitness) -
                    FastMath.min(historyBest, bestFitness) == 0) {
                sigma *= FastMath.exp(0.2 + cs / damps);
            }
            // store best in history
            push(fitnessHistory,bestFitness);
        }
        return optimum;
    }

    private void initializeCMA() {
        // intialize CMA internal values - updated each generation
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

    /**
     * Update of the evolution paths ps and pc.
     *
     * @param zmean Weighted row matrix of the gaussian random numbers generating
     * the current offspring.
     * @param xold xmean matrix of the previous generation.
     * @return hsig flag indicating a small correction.
     */
    private boolean updateEvolutionPaths(RealMatrix zmean, RealMatrix xold) {
        ps = ps.scalarMultiply(1 - cs).add(
                B.multiply(zmean).scalarMultiply(
                        FastMath.sqrt(cs * (2 - cs) * mueff)));
        normps = ps.getFrobeniusNorm();
        final boolean hsig = normps /
                FastMath.sqrt(1 - FastMath.pow(1 - cs, 2 * iterations)) /
                chiN < 1.4 + 2 / ((double) dimension + 1);
        pc = pc.scalarMultiply(1 - cc);
        if (hsig) {
            pc = pc.add(xmean.subtract(xold).scalarMultiply(FastMath.sqrt(cc * (2 - cc) * mueff) / sigma));
        }
        return hsig;
    }

    /**
     * Update of the covariance matrix C for diagonalOnly > 0
     *
     * @param hsig Flag indicating a small correction.
     * @param bestArz Fitness-sorted matrix of the gaussian random values of the
     * current offspring.
     */
    private void updateCovarianceDiagonalOnly(boolean hsig,
                                              final RealMatrix bestArz) {
        // minor correction if hsig==false
        double oldFac = hsig ? 0 : ccov1Sep * cc * (2 - cc);
        oldFac += 1 - ccov1Sep - ccovmuSep;
        diagC = diagC.scalarMultiply(oldFac) // regard old matrix
                .add(square(pc).scalarMultiply(ccov1Sep)) // plus rank one update
                .add((times(diagC, square(bestArz).multiply(weights))) // plus rank mu update
                        .scalarMultiply(ccovmuSep));
        diagD = sqrt(diagC); // replaces eig(C)
        if (iterations > nDiagonalOnlyIterations) {
            // full covariance matrix from now on
            B = eye(dimension, dimension);
            BD = diag(diagD);
            C = diag(diagC);
        }
    }

    /**
     * Update of the covariance matrix C.
     *
     * @param hsig Flag indicating a small correction.
     * @param bestArx Fitness-sorted matrix of the argument vectors producing the
     * current offspring.
     * @param arz Unsorted matrix containing the gaussian random values of the
     * current offspring.
     * @param arindex Indices indicating the fitness-order of the current offspring.
     * @param xold xmean matrix of the previous generation.
     */
    private void updateCovariance(boolean hsig, final RealMatrix bestArx,
                                  final RealMatrix arz, final int[] arindex,
                                  final RealMatrix xold) {
        double negccov = 0;
        if (ccov1 + ccovmu > 0) {
            final RealMatrix arpos = bestArx.subtract(repmat(xold, 1, mu))
                    .scalarMultiply(1 / sigma); // mu difference vectors
            final RealMatrix roneu = pc.multiply(pc.transpose())
                    .scalarMultiply(ccov1); // rank one update
            // minor correction if hsig==false
            double oldFac = hsig ? 0 : ccov1 * cc * (2 - cc);
            oldFac += 1 - ccov1 - ccovmu;
            if (isActiveCMA) {
                // Adapt covariance matrix C active CMA
                negccov = (1 - ccovmu) * 0.25 * mueff /
                        (FastMath.pow(dimension + 2, 1.5) + 2 * mueff);
                // keep at least 0.66 in all directions, small popsize are most
                // critical
                final double negminresidualvariance = 0.66;
                // where to make up for the variance loss
                final double negalphaold = 0.5;
                // prepare vectors, compute negative updating matrix Cneg
                final int[] arReverseIndex = reverse(arindex);
                RealMatrix arzneg = selectColumns(arz, MathArrays.copyOf(arReverseIndex, mu));
                RealMatrix arnorms = sqrt(sumRows(square(arzneg)));
                final int[] idxnorms = sortedIndices(arnorms.getRow(0));
                final RealMatrix arnormsSorted = selectColumns(arnorms, idxnorms);
                final int[] idxReverse = reverse(idxnorms);
                final RealMatrix arnormsReverse = selectColumns(arnorms, idxReverse);
                arnorms = divide(arnormsReverse, arnormsSorted);
                final int[] idxInv = inverse(idxnorms);
                final RealMatrix arnormsInv = selectColumns(arnorms, idxInv);
                // check and set learning rate negccov
                final double negcovMax = (1 - negminresidualvariance) /
                        square(arnormsInv).multiply(weights).getEntry(0, 0);
                if (negccov > negcovMax) {
                    negccov = negcovMax;
                }
                arzneg = times(arzneg, repmat(arnormsInv, dimension, 1));
                final RealMatrix artmp = BD.multiply(arzneg);
                final RealMatrix Cneg = artmp.multiply(diag(weights)).multiply(artmp.transpose());
                oldFac += negalphaold * negccov;
                C = C.scalarMultiply(oldFac)
                        .add(roneu) // regard old matrix
                        .add(arpos.scalarMultiply( // plus rank one update
                                ccovmu + (1 - negalphaold) * negccov) // plus rank mu update
                                .multiply(times(repmat(weights, 1, dimension),
                                        arpos.transpose())))
                        .subtract(Cneg.scalarMultiply(negccov));
            } else {
                // Adapt covariance matrix C - nonactive
                C = C.scalarMultiply(oldFac) // regard old matrix
                        .add(roneu) // plus rank one update
                        .add(arpos.scalarMultiply(ccovmu) // plus rank mu update
                                .multiply(times(repmat(weights, 1, dimension),
                                        arpos.transpose())));
            }
        }
        updateBD(negccov);
    }

    /**
     * Update B and D from C.
     *
     * @param negccov Negative covariance factor.
     */
    private void updateBD(double negccov) {
        if (ccov1 + ccovmu + negccov > 0 &&
                (iterations % 1. / (ccov1 + ccovmu + negccov) / dimension / 10.) < 1) {
            // to achieve O(N^2)
            C = triu(C, 0).add(triu(C, 1).transpose());
            // enforce symmetry to prevent complex numbers
            final EigenDecomposition eig = new EigenDecomposition(C);
            B = eig.getV(); // eigen decomposition, B==normalized eigenvectors
            D = eig.getD();
            diagD = diag(D);
            if (min(diagD) <= 0) {
                for (int i = 0; i < dimension; i++) {
                    if (diagD.getEntry(i, 0) < 0) {
                        diagD.setEntry(i, 0, 0);
                    }
                }
                final double tfac = max(diagD) / 1e14;
                C = C.add(eye(dimension, dimension).scalarMultiply(tfac));
                diagD = diagD.add(ones(dimension, 1).scalarMultiply(tfac));
            }
            if (max(diagD) > 1e14 * min(diagD)) {
                final double tfac = max(diagD) / 1e14 - min(diagD);
                C = C.add(eye(dimension, dimension).scalarMultiply(tfac));
                diagD = diagD.add(ones(dimension, 1).scalarMultiply(tfac));
            }
            diagC = diag(C);
            diagD = sqrt(diagD); // D contains standard deviations now
            BD = times(B, repmat(diagD.transpose(), dimension, 1)); // O(n^2)
        }
    }

    /**
     * Pushes the current best fitness value in a history queue.
     *
     * @param vals History queue.
     * @param val Current best fitness value.
     */
    private static void push(double[] vals, double val) {
        for (int i = vals.length-1; i > 0; i--) {
            vals[i] = vals[i-1];
        }
        vals[0] = val;
    }

    /**
     * Sorts fitness values.
     *
     * @param doubles Array of values to be sorted.
     * @return a sorted array of indices pointing into doubles.
     */
    private int[] sortedIndices(final double[] doubles) {
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

    /**
     * Used to sort fitness values. Sorting is always in lower value first
     * order.
     */
    private static class DoubleIndex implements Comparable<DoubleIndex> {
        /** Value to compare. */
        private final double value;
        /** Index into sorted array. */
        private final int index;

        /**
         * @param value Value to compare.
         * @param index Index into sorted array.
         */
        DoubleIndex(double value, int index) {
            this.value = value;
            this.index = index;
        }

        /** {@inheritDoc} */
        public int compareTo(DoubleIndex o) {
            return Double.compare(value, o.value);
        }

        /** {@inheritDoc} */
        @Override
        public boolean equals(Object other) {

            if (this == other) {
                return true;
            }

            if (other instanceof DoubleIndex) {
                return Double.compare(value, ((DoubleIndex) other).value) == 0;
            }

            return false;
        }

        /** {@inheritDoc} */
        @Override
        public int hashCode() {
            long bits = Double.doubleToLongBits(value);
            return (int) (1438542 ^ bits >>> 32 ^ bits);
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
            double diff = FastMath.abs(x[i] - repaired[i]);
            penalty += diff;
        }
        return penalty;
    }

    // -----Matrix utility functions similar to the Matlab build in functions------

    /**
     * @param m Input matrix
     * @return Matrix representing the element-wise logarithm of m.
     */
    private static RealMatrix log(final RealMatrix m) {
        final double[][] d = new double[m.getRowDimension()][m.getColumnDimension()];
        for (int r = 0; r < m.getRowDimension(); r++) {
            for (int c = 0; c < m.getColumnDimension(); c++) {
                d[r][c] = FastMath.log(m.getEntry(r, c));
            }
        }
        return new Array2DRowRealMatrix(d, false);
    }

    /**
     * @param m Input matrix.
     * @return Matrix representing the element-wise square root of m.
     */
    private static RealMatrix sqrt(final RealMatrix m) {
        final double[][] d = new double[m.getRowDimension()][m.getColumnDimension()];
        for (int r = 0; r < m.getRowDimension(); r++) {
            for (int c = 0; c < m.getColumnDimension(); c++) {
                d[r][c] = FastMath.sqrt(m.getEntry(r, c));
            }
        }
        return new Array2DRowRealMatrix(d, false);
    }

    /**
     * @param m Input matrix.
     * @return Matrix representing the element-wise square of m.
     */
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

    /**
     * @param m Input matrix 1.
     * @param n Input matrix 2.
     * @return the matrix where the elements of m and n are element-wise multiplied.
     */
    private static RealMatrix times(final RealMatrix m, final RealMatrix n) {
        final double[][] d = new double[m.getRowDimension()][m.getColumnDimension()];
        for (int r = 0; r < m.getRowDimension(); r++) {
            for (int c = 0; c < m.getColumnDimension(); c++) {
                d[r][c] = m.getEntry(r, c) * n.getEntry(r, c);
            }
        }
        return new Array2DRowRealMatrix(d, false);
    }

    /**
     * @param m Input matrix 1.
     * @param n Input matrix 2.
     * @return Matrix where the elements of m and n are element-wise divided.
     */
    private static RealMatrix divide(final RealMatrix m, final RealMatrix n) {
        final double[][] d = new double[m.getRowDimension()][m.getColumnDimension()];
        for (int r = 0; r < m.getRowDimension(); r++) {
            for (int c = 0; c < m.getColumnDimension(); c++) {
                d[r][c] = m.getEntry(r, c) / n.getEntry(r, c);
            }
        }
        return new Array2DRowRealMatrix(d, false);
    }

    /**
     * @param m Input matrix.
     * @param cols Columns to select.
     * @return Matrix representing the selected columns.
     */
    private static RealMatrix selectColumns(final RealMatrix m, final int[] cols) {
        final double[][] d = new double[m.getRowDimension()][cols.length];
        for (int r = 0; r < m.getRowDimension(); r++) {
            for (int c = 0; c < cols.length; c++) {
                d[r][c] = m.getEntry(r, cols[c]);
            }
        }
        return new Array2DRowRealMatrix(d, false);
    }

    /**
     * @param m Input matrix.
     * @param k Diagonal position.
     * @return Upper triangular part of matrix.
     */
    private static RealMatrix triu(final RealMatrix m, int k) {
        final double[][] d = new double[m.getRowDimension()][m.getColumnDimension()];
        for (int r = 0; r < m.getRowDimension(); r++) {
            for (int c = 0; c < m.getColumnDimension(); c++) {
                d[r][c] = r <= c - k ? m.getEntry(r, c) : 0;
            }
        }
        return new Array2DRowRealMatrix(d, false);
    }

    /**
     * @param m Input matrix.
     * @return Row matrix representing the sums of the rows.
     */
    private static RealMatrix sumRows(final RealMatrix m) {
        final double[][] d = new double[1][m.getColumnDimension()];
        for (int c = 0; c < m.getColumnDimension(); c++) {
            double sum = 0;
            for (int r = 0; r < m.getRowDimension(); r++) {
                sum += m.getEntry(r, c);
            }
            d[0][c] = sum;
        }
        return new Array2DRowRealMatrix(d, false);
    }

    /**
     * @param m Input matrix.
     * @return the diagonal n-by-n matrix if m is a column matrix or the column
     * matrix representing the diagonal if m is a n-by-n matrix.
     */
    private static RealMatrix diag(final RealMatrix m) {
        if (m.getColumnDimension() == 1) {
            final double[][] d = new double[m.getRowDimension()][m.getRowDimension()];
            for (int i = 0; i < m.getRowDimension(); i++) {
                d[i][i] = m.getEntry(i, 0);
            }
            return new Array2DRowRealMatrix(d, false);
        } else {
            final double[][] d = new double[m.getRowDimension()][1];
            for (int i = 0; i < m.getColumnDimension(); i++) {
                d[i][0] = m.getEntry(i, i);
            }
            return new Array2DRowRealMatrix(d, false);
        }
    }

    /**
     * Copies a column from m1 to m2.
     *
     * @param m1 Source matrix.
     * @param col1 Source column.
     * @param m2 Target matrix.
     * @param col2 Target column.
     */
    private static void copyColumn(final RealMatrix m1, int col1, RealMatrix m2, int col2) {
        for (int i = 0; i < m1.getRowDimension(); i++) {
            m2.setEntry(i, col2, m1.getEntry(i, col1));
        }
    }

    /**
     * @param n Number of rows.
     * @param m Number of columns.
     * @return n-by-m matrix filled with 1.
     */
    private static RealMatrix ones(int n, int m) {
        final double[][] d = new double[n][m];
        for (int r = 0; r < n; r++) {
            Arrays.fill(d[r], 1);
        }
        return new Array2DRowRealMatrix(d, false);
    }

    /**
     * @param n Number of rows.
     * @param m Number of columns.
     * @return n-by-m matrix of 0 values out of diagonal, and 1 values on
     * the diagonal.
     */
    private static RealMatrix eye(int n, int m) {
        final double[][] d = new double[n][m];
        for (int r = 0; r < n; r++) {
            if (r < m) {
                d[r][r] = 1;
            }
        }
        return new Array2DRowRealMatrix(d, false);
    }

    /**
     * @param n Number of rows.
     * @param m Number of columns.
     * @return n-by-m matrix of zero values.
     */
    private static RealMatrix zeros(int n, int m) {
        return new Array2DRowRealMatrix(n, m);
    }

    /**
     * @param mat Input matrix.
     * @param n Number of row replicates.
     * @param m Number of column replicates.
     * @return a matrix which replicates the input matrix in both directions.
     */
    private static RealMatrix repmat(final RealMatrix mat, int n, int m) {
        final int rd = mat.getRowDimension();
        final int cd = mat.getColumnDimension();
        final double[][] d = new double[n * rd][m * cd];
        for (int r = 0; r < n * rd; r++) {
            for (int c = 0; c < m * cd; c++) {
                d[r][c] = mat.getEntry(r % rd, c % cd);
            }
        }
        return new Array2DRowRealMatrix(d, false);
    }

    /**
     * @param start Start value.
     * @param end End value.
     * @param step Step size.
     * @return a sequence as column matrix.
     */
    private static RealMatrix sequence(double start, double end, double step) {
        final int size = (int) ((end - start) / step + 1);
        final double[][] d = new double[size][1];
        double value = start;
        for (int r = 0; r < size; r++) {
            d[r][0] = value;
            value += step;
        }
        return new Array2DRowRealMatrix(d, false);
    }

    /**
     * @param m Input matrix.
     * @return the maximum of the matrix element values.
     */
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

    /**
     * @param m Input matrix.
     * @return the minimum of the matrix element values.
     */
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

    /**
     * @param m Input array.
     * @return the maximum of the array values.
     */
    private static double max(final double[] m) {
        double max = Double.MIN_VALUE;
        for (double v : m) {
            if (max < v) {
                max = v;
            }
        }
        return max;
    }

    /**
     * @param m Input array.
     * @return the minimum of the array values.
     */
    private static double min(final double[] m) {
        double min = Double.MAX_VALUE;
        for (double v : m) {
            if (min > v) {
                min = v;
            }
        }
        return min;
    }

    /**
     * @param indices Input index array.
     * @return the inverse of the mapping defined by indices.
     */
    private static int[] inverse(final int[] indices) {
        final int[] inverse = new int[indices.length];
        for (int i = 0; i < indices.length; i++) {
            inverse[indices[i]] = i;
        }
        return inverse;
    }

    /**
     * @param indices Input index array.
     * @return the indices in inverse order (last is first).
     */
    private static int[] reverse(final int[] indices) {
        final int[] reverse = new int[indices.length];
        for (int i = 0; i < indices.length; i++) {
            reverse[i] = indices[indices.length - i - 1];
        }
        return reverse;
    }

    /**
     * @param size Length of random array.
     * @return an array of Gaussian random numbers.
     */
    private double[] randn(int size) {
        final double[] randn = new double[size];
        for (int i = 0; i < size; i++) {
            randn[i] = random.nextGaussian();
        }
        return randn;
    }

    /**
     * @param size Number of rows.
     * @param popSize Population size.
     * @return a 2-dimensional matrix of Gaussian random numbers.
     */
    private RealMatrix randn1(int size, int popSize) {
        final double[][] d = new double[size][popSize];
        for (int r = 0; r < size; r++) {
            for (int c = 0; c < popSize; c++) {
                d[r][c] = random.nextGaussian();
            }
        }
        return new Array2DRowRealMatrix(d, false);
    }
}

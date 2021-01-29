package com.github.mbuzdalov.opl.cma;

import java.util.Arrays;

import org.apache.commons.math3.analysis.MultivariateFunction;
import org.apache.commons.math3.exception.*;
import org.apache.commons.math3.linear.Array2DRowRealMatrix;
import org.apache.commons.math3.linear.EigenDecomposition;
import org.apache.commons.math3.linear.MatrixUtils;
import org.apache.commons.math3.linear.RealMatrix;
import org.apache.commons.math3.optim.InitialGuess;
import org.apache.commons.math3.optim.OptimizationData;
import org.apache.commons.math3.optim.PointValuePair;
import org.apache.commons.math3.optim.SimpleBounds;
import org.apache.commons.math3.optim.nonlinear.scalar.ObjectiveFunction;
import org.apache.commons.math3.random.RandomGenerator;
import org.apache.commons.math3.util.FastMath;
import org.apache.commons.math3.util.MathArrays;

public class CMAESOptimizer {
    // from BaseOptimizer
    public PointValuePair optimize(OptimizationData... optData) {
        parseOptimizationData(optData);
        return doOptimize();
    }

    // fields from BaseMultivariateOptimizer
    private double[] start;
    private double[] lowerBound;
    private double[] upperBound;

    // fields from MultivariateOptimizer
    private MultivariateFunction function;

    // global search parameters
    private final int populationSize;

    /**
     * Covariance update mechanism, default is active CMA. isActiveCMA = true
     * turns on "active CMA" with a negative update of the covariance matrix and
     * checks for positive definiteness. OPTS.CMA.active = 2 does not check for
     * pos. def. and is numerically faster. Active CMA usually speeds up the
     * adaptation.
     */
    private final boolean isActiveCMA;
    /**
     * Determines how often a new random offspring is generated in case it is
     * not feasible / beyond the defined limits, default is 0.
     */
    private final int nResamplingUntilFeasble;
    /**
     * @see CMAESOptimizer.Sigma
     */
    private double[] inputSigma;
    /** Number of objective variables/problem dimension */
    private int dimension;
    /**
     * Defines the number of initial iterations, where the covariance matrix
     * remains diagonal and the algorithm has internally linear time complexity.
     * diagonalOnly = 1 means keeping the covariance matrix always diagonal and
     * this setting also exhibits linear space complexity. This can be
     * particularly useful for dimension > 100.
     * @see <a href="http://hal.archives-ouvertes.fr/inria-00287367/en">A Simple Modification in CMA-ES</a>
     */
    private int nDiagonalOnlyIterations;

    // termination criteria
    /** Maximal number of iterations allowed. */
    private final int maxIterations;
    /** Stop if x-changes larger stopTolUpX. */
    private double stopTolUpX;
    /** Stop if x-change smaller stopTolX. */
    private double stopTolX;
    /** Stop if fun-changes smaller stopTolFun. */
    private double stopTolFun;
    /** Stop if back fun-changes smaller stopTolHistFun. */
    private double stopTolHistFun;

    // selection strategy parameters
    /** Number of parents/points for recombination. */
    private int mu; //
    /** Array for weighted recombination. */
    private RealMatrix weights;
    /** Variance-effectiveness of sum w_i x_i. */
    private double mueff; //

    // dynamic strategy parameters and constants
    /** Overall standard deviation - search volume. */
    private double sigma;
    /** Cumulation constant. */
    private double cc;
    /** Cumulation constant for step-size. */
    private double cs;
    /** Damping for step-size. */
    private double damps;
    /** Learning rate for rank-one update. */
    private double ccov1;
    /** Learning rate for rank-mu update' */
    private double ccovmu;
    /** Expectation of ||N(0,I)|| == norm(randn(N,1)). */
    private double chiN;
    /** Learning rate for rank-one update - diagonalOnly */
    private double ccov1Sep;
    /** Learning rate for rank-mu update - diagonalOnly */
    private double ccovmuSep;

    // CMA internal values - updated each generation
    /** Objective variables. */
    private RealMatrix xmean;
    /** Evolution path. */
    private RealMatrix pc;
    /** Evolution path for sigma. */
    private RealMatrix ps;
    /** Norm of ps, stored for efficiency. */
    private double normps;
    /** Coordinate system. */
    private RealMatrix B;
    /** Scaling. */
    private RealMatrix D;
    /** B*D, stored for efficiency. */
    private RealMatrix BD;
    /** Diagonal of sqrt(D), stored for efficiency. */
    private RealMatrix diagD;
    /** Covariance matrix. */
    private RealMatrix C;
    /** Diagonal of C, used for diagonalOnly. */
    private RealMatrix diagC;
    /** Number of iterations already performed. */
    private int iterations;

    /** History queue of best values. */
    private double[] fitnessHistory;

    /** Random generator. */
    private final RandomGenerator random;

    /**
     * @param maxIterations Maximal number of iterations.
     * @param isActiveCMA Chooses the covariance matrix update method.
     * @param nDiagonalOnlyIterations Number of initial iterations, where the covariance matrix remains diagonal.
     * @param nResamplingUntilFeasible Determines how often new random objective variables are generated in case they are out of bounds.
     * @param random Random generator.
     *
     * @since 3.1
     */
    public CMAESOptimizer(int maxIterations,
                          boolean isActiveCMA,
                          int nDiagonalOnlyIterations,
                          int nResamplingUntilFeasible,
                          RandomGenerator random,
                          int populationSize) {
        this.maxIterations = maxIterations;
        this.isActiveCMA = isActiveCMA;
        this.nDiagonalOnlyIterations = nDiagonalOnlyIterations;
        this.nResamplingUntilFeasble = nResamplingUntilFeasible;
        this.random = random;
        this.populationSize = populationSize;
    }

    /**
     * Input sigma values.
     * They define the initial coordinate-wise standard deviations for
     * sampling new search points around the initial guess.
     * It is suggested to set them to the estimated distance from the
     * initial to the desired optimum.
     * Small values induce the search to be more local (and very small
     * values are more likely to find a local optimum close to the initial
     * guess).
     * Too small values might however lead to early termination.
     */
    public static class Sigma implements OptimizationData {
        /** Sigma values. */
        private final double[] sigma;

        /**
         * @param s Sigma values.
         * @throws NotPositiveException if any of the array entries is smaller
         * than zero.
         */
        public Sigma(double[] s)
                throws NotPositiveException {
            for (double v : s) {
                if (v < 0) {
                    throw new NotPositiveException(v);
                }
            }

            sigma = s.clone();
        }

        /**
         * @return the sigma values.
         */
        public double[] getSigma() {
            return sigma.clone();
        }
    }

    protected PointValuePair doOptimize() {
        // -------------------- Initialization --------------------------------
        final FitnessFunction fitfun = new FitnessFunction();
        final double[] guess = start;
        // number of objective variables/problem dimension
        dimension = guess.length;
        initializeCMA(guess);
        iterations = 0;
        ValuePenaltyPair valuePenalty = fitfun.value(guess);
        double bestValue = valuePenalty.value+valuePenalty.penalty;
        push(fitnessHistory, bestValue);
        PointValuePair optimum = new PointValuePair(start, bestValue);

        // -------------------- Generation Loop --------------------------------

        generationLoop:
        for (iterations = 1; iterations <= maxIterations; iterations++) {
            // Generate and evaluate lambda offspring
            final RealMatrix arz = randn1(dimension, populationSize);
            final RealMatrix arx = zeros(dimension, populationSize);
            final double[] fitness = new double[populationSize];
            final ValuePenaltyPair[] valuePenaltyPairs = new ValuePenaltyPair[populationSize];
            // generate random offspring
            for (int k = 0; k < populationSize; k++) {
                RealMatrix arxk = null;
                for (int i = 0; i <= nResamplingUntilFeasble; i++) {
                    if (nDiagonalOnlyIterations <= 0) {
                        arxk = xmean.add(BD.multiply(arz.getColumnMatrix(k))
                                .scalarMultiply(sigma)); // m + sig * Normal(0,C)
                    } else {
                        arxk = xmean.add(times(diagD,arz.getColumnMatrix(k))
                                .scalarMultiply(sigma));
                    }
                    if (i >= nResamplingUntilFeasble || fitfun.isFeasible(arxk.getColumn(0))) {
                        break;
                    }
                    // regenerate random arguments for row
                    arz.setColumn(k, randn(dimension));
                }
                copyColumn(arxk, 0, arx, k);
                try {
                    valuePenaltyPairs[k] = fitfun.value(arx.getColumn(k)); // compute fitness
                } catch (TooManyEvaluationsException e) {
                    break generationLoop;
                }
            }

            // Compute fitnesses by adding value and penalty after scaling by value range.
            double valueRange = valueRange(valuePenaltyPairs);
            for (int iValue=0;iValue<valuePenaltyPairs.length;iValue++) {
                fitness[iValue] = valuePenaltyPairs[iValue].value + valuePenaltyPairs[iValue].penalty*valueRange;
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
            if (nDiagonalOnlyIterations <= 0) {
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
                optimum = new PointValuePair(fitfun.repair(bestArx.getColumn(0)), bestFitness);
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

    protected void parseOptimizationData(OptimizationData... optData) {
        // Allow base class to register its own data.

        // The existing values (as set by the previous call) are reused if
        // not provided in the argument list.
        for (OptimizationData data : optData) {
            if (data instanceof InitialGuess) {
                start = ((InitialGuess) data).getInitialGuess();
                continue;
            }
            if (data instanceof SimpleBounds) {
                final SimpleBounds bounds = (SimpleBounds) data;
                lowerBound = bounds.getLower();
                upperBound = bounds.getUpper();
                continue;
            }
            if (data instanceof ObjectiveFunction) {
                function = ((ObjectiveFunction) data).getObjectiveFunction();
                continue;
            }
            if (data instanceof Sigma) {
                inputSigma = ((Sigma) data).getSigma();
                continue;
            }
        }

        checkParameters();
    }

    private void checkParameters() {
        final int dim = start.length;
        if (lowerBound.length != dim) {
            throw new DimensionMismatchException(lowerBound.length, dim);
        }
        for (int i = 0; i < dim; i++) {
            if (start[i] < lowerBound[i]) {
                throw new NumberIsTooSmallException(start[i], lowerBound[i], true);
            }
        }
        if (upperBound.length != dim) {
            throw new DimensionMismatchException(upperBound.length, dim);
        }
        for (int i = 0; i < dim; i++) {
            if (start[i] > upperBound[i]) {
                throw new NumberIsTooLargeException(start[i], upperBound[i], true);
            }
        }
        if (inputSigma.length != dim) {
            throw new DimensionMismatchException(inputSigma.length, start.length);
        }
        for (int i = 0; i < dim; i++) {
            if (inputSigma[i] > upperBound[i] - lowerBound[i]) {
                throw new OutOfRangeException(inputSigma[i], 0, upperBound[i] - lowerBound[i]);
            }
        }
    }

    /**
     * Initialization of the dynamic search parameters
     *
     * @param guess Initial guess for the arguments of the fitness function.
     */
    private void initializeCMA(double[] guess) {
        if (populationSize <= 0) {
            throw new NotStrictlyPositiveException(populationSize);
        }
        // initialize sigma
        final double[][] sigmaArray = new double[guess.length][1];
        for (int i = 0; i < guess.length; i++) {
            sigmaArray[i][0] = inputSigma[i];
        }
        final RealMatrix insigma = new Array2DRowRealMatrix(sigmaArray, false);
        sigma = max(insigma); // overall standard deviation

        // initialize termination criteria
        stopTolUpX = 1e3 * max(insigma);
        stopTolX = 1e-11 * max(insigma);
        stopTolFun = 1e-12;
        stopTolHistFun = 1e-13;

        // initialize selection strategy parameters
        mu = populationSize / 2; // number of parents/points for recombination
        weights = log(sequence(1, mu, 1)).scalarMultiply(-1).scalarAdd(FastMath.log(mu + 0.5));
        double sumw = 0;
        double sumwq = 0;
        for (int i = 0; i < mu; i++) {
            double w = weights.getEntry(i, 0);
            sumw += w;
            sumwq += w * w;
        }
        weights = weights.scalarMultiply(1 / sumw);
        mueff = sumw * sumw / sumwq; // variance-effectiveness of sum w_i x_i

        // initialize dynamic strategy parameters and constants
        cc = (4 + mueff / dimension) /
                (dimension + 4 + 2 * mueff / dimension);
        cs = (mueff + 2) / (dimension + mueff + 3.);
        damps = (1 + 2 * FastMath.max(0, FastMath.sqrt((mueff - 1) /
                (dimension + 1)) - 1)) *
                FastMath.max(0.3,
                        1 - dimension / (1e-6 + maxIterations)) + cs; // minor increment
        ccov1 = 2 / ((dimension + 1.3) * (dimension + 1.3) + mueff);
        ccovmu = FastMath.min(1 - ccov1, 2 * (mueff - 2 + 1 / mueff) /
                ((dimension + 2) * (dimension + 2) + mueff));
        ccov1Sep = FastMath.min(1, ccov1 * (dimension + 1.5) / 3);
        ccovmuSep = FastMath.min(1 - ccov1, ccovmu * (dimension + 1.5) / 3);
        chiN = FastMath.sqrt(dimension) *
                (1 - 1 / ((double) 4 * dimension) + 1 / ((double) 21 * dimension * dimension));
        // intialize CMA internal values - updated each generation
        xmean = MatrixUtils.createColumnRealMatrix(guess); // objective variables
        diagD = insigma.scalarMultiply(1 / sigma);
        diagC = square(diagD);
        pc = zeros(dimension, 1); // evolution paths for C and sigma
        ps = zeros(dimension, 1); // B defines the coordinate system
        normps = ps.getFrobeniusNorm();

        B = eye(dimension, dimension);
        D = ones(dimension, 1); // diagonal D defines the scaling
        BD = times(B, repmat(diagD.transpose(), dimension, 1));
        C = B.multiply(diag(square(D)).multiply(B.transpose())); // covariance

        int historySize = 10 + (int) (3 * 10 * dimension / (double) populationSize);
        fitnessHistory = new double[historySize]; // history of fitness values
        for (int i = 0; i < historySize; i++) {
            fitnessHistory[i] = Double.MAX_VALUE;
        }
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
        if (nDiagonalOnlyIterations > 1 &&
                iterations > nDiagonalOnlyIterations) {
            // full covariance matrix from now on
            nDiagonalOnlyIterations = 0;
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
     * Get range of values.
     *
     * @param vpPairs Array of valuePenaltyPairs to get range from.
     * @return a double equal to maximum value minus minimum value.
     */
    private double valueRange(final ValuePenaltyPair[] vpPairs) {
        double max = Double.NEGATIVE_INFINITY;
        double min = Double.MAX_VALUE;
        for (ValuePenaltyPair vpPair:vpPairs) {
            if (vpPair.value > max) {
                max = vpPair.value;
            }
            if (vpPair.value < min) {
                min = vpPair.value;
            }
        }
        return max-min;
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
    /**
     * Stores the value and penalty (for repair of out of bounds point).
     */
    private static class ValuePenaltyPair {
        /** Objective function value. */
        private final double value;
        /** Penalty value for repair of out out of bounds points. */
        private final double penalty;

        /**
         * @param value Function value.
         * @param penalty Out-of-bounds penalty.
         */
        ValuePenaltyPair(final double value, final double penalty) {
            this.value   = value;
            this.penalty = penalty;
        }
    }


    /**
     * Normalizes fitness values to the range [0,1]. Adds a penalty to the
     * fitness value if out of range.
     */
    private class FitnessFunction {
        /**
         * Flag indicating whether the objective variables are forced into their
         * bounds if defined
         */
        private final boolean isRepairMode;

        /** Simple constructor.
         */
        FitnessFunction() {
            isRepairMode = true;
        }

        /**
         * @param point Normalized objective variables.
         * @return the objective value + penalty for violated bounds.
         */
        public ValuePenaltyPair value(final double[] point) {
            double value;
            double penalty=0.0;
            if (isRepairMode) {
                double[] repaired = repair(point);
                value = function.value(repaired);
                penalty =  penalty(point, repaired);
            } else {
                value = function.value(point);
            }
            return new ValuePenaltyPair(value,penalty);
        }

        /**
         * @param x Normalized objective variables.
         * @return {@code true} if in bounds.
         */
        public boolean isFeasible(final double[] x) {
            final double[] lB = lowerBound;
            final double[] uB = upperBound;

            for (int i = 0; i < x.length; i++) {
                if (x[i] < lB[i]) {
                    return false;
                }
                if (x[i] > uB[i]) {
                    return false;
                }
            }
            return true;
        }

        /**
         * @param x Normalized objective variables.
         * @return the repaired (i.e. all in bounds) objective variables.
         */
        private double[] repair(final double[] x) {
            final double[] repaired = new double[x.length];
            for (int i = 0; i < x.length; i++) {
                if (x[i] < lowerBound[i]) {
                    repaired[i] = lowerBound[i];
                } else repaired[i] = Math.min(x[i], upperBound[i]);
            }
            return repaired;
        }

        /**
         * @param x Normalized objective variables.
         * @param repaired Repaired objective variables.
         * @return Penalty value according to the violation of the bounds.
         */
        private double penalty(final double[] x, final double[] repaired) {
            double penalty = 0;
            for (int i = 0; i < x.length; i++) {
                double diff = FastMath.abs(x[i] - repaired[i]);
                penalty += diff;
            }
            return penalty;
        }
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

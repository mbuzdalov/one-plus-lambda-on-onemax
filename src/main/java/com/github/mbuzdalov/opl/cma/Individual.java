package com.github.mbuzdalov.opl.cma;

import com.github.mbuzdalov.opl.util.FastRandom;

public final class Individual implements Comparable<Individual> {
    private final double[] x, fixedX, z;
    private double fitness, penalty;

    public Individual(int dimension) {
        x = new double[dimension];
        fixedX = new double[dimension];
        z = new double[dimension];
    }

    public void initialize(FastRandom rng, double[] xMean, double[] D, double sigma, int resampleWhileFeasible) {
        do {
            penalty = 0;
            for (int i = 0; i < z.length; ++i) {
                z[i] = rng.nextGaussian();
                x[i] = xMean[i] + D[i] * z[i] * sigma;
                fixedX[i] = Math.min(1, Math.max(0, x[i]));
                penalty += Math.abs(x[i] - fixedX[i]);
            }
        } while (penalty != 0 && --resampleWhileFeasible >= 0);
    }

    public double[] getFixedX() {
        return fixedX;
    }

    public double getX(int index) {
        return x[index];
    }

    public double getZ(int index) {
        return z[index];
    }

    public double getFitness() {
        return fitness;
    }

    public void setRawFitness(double rawFitness, double penaltyScale) {
        this.fitness = rawFitness + penalty * penaltyScale;
    }

    @Override
    public int compareTo(Individual o) {
        return Double.compare(fitness, o.fitness);
    }
}

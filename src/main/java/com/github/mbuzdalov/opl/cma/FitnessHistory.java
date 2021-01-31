package com.github.mbuzdalov.opl.cma;

public final class FitnessHistory {
    private final double[] inputStack, outputMin, outputMax;
    private int nInputs, nOutputs;
    private double inputMin, inputMax;

    public FitnessHistory(int length) {
        inputStack = new double[length];
        outputMin = new double[length];
        outputMax = new double[length];
        inputMin = Double.POSITIVE_INFINITY;
        inputMax = Double.NEGATIVE_INFINITY;
    }

    public void push(double value) {
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

    public int getCapacity() {
        return inputStack.length;
    }

    public double getMinimum() {
        return nOutputs == 0 ? inputMin : Math.min(inputMin, outputMin[nOutputs - 1]);
    }

    public double getMaximum() {
        return nOutputs == 0 ? inputMax : Math.max(inputMax, outputMax[nOutputs - 1]);
    }
}

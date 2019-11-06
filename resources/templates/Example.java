package learning;

import java.util.Arrays;

import za.ac.sun.cs.coastal.Symbolic;

public class Example {

  {{{target-fn}}}

  {{{candidate-fn}}}

	public static boolean check(char[] input) {
		boolean candidateResult = candidate(input);
		boolean targetResult = target(input);
		if (candidateResult != targetResult) {
			Symbolic.mark("<<Counter Example: " + Arrays.toString(input) + ">>");
			return false;
		} else {
			return true;
		}
	}

	public static void main(String[] args) {
    char[] inputValues = new char[] { {{{input}}} };

		boolean result = check(inputValues);
	}
}

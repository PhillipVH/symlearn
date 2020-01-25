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
        String ce = "[";
        for (char ch : input) {
            ce += ch + ",";
        }
        ce += "]";

			Symbolic.mark("<<Counter Example: " + ce + ">>");

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

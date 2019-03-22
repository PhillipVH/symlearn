package examples.tacas2017;

import za.ac.sun.cs.coastal.Symbolic;

/**
 * A version of the worked example from the
 * TACAS2017 paper "Learning Symbolic Automata"
 * @author Phillip van Heerden
 *
 */
public class Example {

	public static boolean check(int[] input) {
		boolean fVal = convertedAutomaton(input);
//		boolean gVal = candidate0(input);
		return true;
	}
	
	public static boolean candidate0 (int[] input) {
		if (input[0] >= 0 && input[0] <= 50) {
			Symbolic.mark(1);
			return true;
		} else {
			Symbolic.mark(0);
			return false;
		}
	}
	
	public static boolean convertedAutomaton(int[] input) {
		int state = 1;
		for (int idx = 0; idx < input.length; idx++) {
			switch (state) {
			case 1:
			if ((input[idx] >= 51 && input[idx] <= 100) || false) {
				state = 2;
				break;
			}
			if ((input[idx] >= 0 && input[idx] <= 50) || (input[idx] >= 101 && input[idx] <= 2147483647) || false) {
				state = 1;
				break;
			}
			case 2:
			if ((input[idx] >= 21 && input[idx] <= 2147483647) || false) {
				state = 3;
				break;
			}
			if ((input[idx] >= 0 && input[idx] <= 20) || false) {
				state = 4;
				break;
			}
			case 3:
			if ((input[idx] >= -2147483648 && input[idx] <= 2147483647) || false) {
				state = 3;
				break;
			}
			case 4:
			if ((input[idx] >= -2147483648 && input[idx] <= 20) || false) {
				state = 1;
				break;
			}
			if ((input[idx] >= 21 && input[idx] <= 2147483647) || false) {
				state = 3;
				break;
			}
		  }
		  }
		  if ((state == 1) || false) {
			Symbolic.mark(1);
		    return true;
		  } else { 
			Symbolic.mark(0);
			return false; 
		  }

		}
	
	public static boolean int_pred(int num, int bottom, int upper) {
		return (num >= bottom && num < upper);
	}
	
	public static boolean int_pred(int num, int bottom) {
		return num >= bottom;
	}
	
	public static void main(String[] args) {
		boolean result = check(new int[] {1});
	}

}

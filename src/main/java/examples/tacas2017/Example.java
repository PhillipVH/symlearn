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
		boolean fVal = f_array(input);
//		boolean gVal = g0(x, y, z);
		return (fVal);
	}
	
	public static boolean g0(int x, int y, int z) {
		return true;
	}
	
				// TODO Remind LExis about back exercises 
	
	public static boolean int_pred(int num, int bottom, int upper) {
		return (num >= bottom && num < upper);
	}
	
	public static boolean int_pred(int num, int bottom) {
		return num >= bottom;
	}
	
	// TODO Idea: Magical Bound Counterexamples Using Concolic Execution
	public static boolean f_array(int[] input) {
		int state = 0;
		for (int idx = 0; idx < input.length; idx++) {
			
			// Get the input symbol
			int num = input[idx];
			
			if (state == 0) {
				// Remain in state zero
				if (int_pred(num, 0, 51) || int_pred(num, 101)) {
					state = 0;
					continue;
				}
				
				// Move to state 51
				if (int_pred(num, 51, 101)) {
					state = 51;
					continue;
				}
			} else if (state == 51) {
				// Transition to state 51,21
				if (int_pred(num, 21)) {
					state = 5121;
					continue;
				} 
				
				// Transition to state 51,0
				if (int_pred(num, 0, 21)) {
					state = 510;
					continue;
				}
			} else if (state == 510) {
				// Transition to state 0
				if (int_pred(num, 0, 21)) {
					state = 0;
					continue;
				}
				
				// Transition to state 51,21
				if (int_pred(num, 21)) {
					state = 5121;
					continue;
				}
			}
		}
		
		// State 0 is our only final state
		if (state != 0) {
			Symbolic.mark(0);
		} else {
			Symbolic.mark(1);
		}
		
		return state == 0;
	}
	
	public static boolean f(int x, int y, int z) {
		if ((x >= 0 && x < 51) || (x >= 101)) {
			// Stay in epsilon
			if (y >= 51 && y < 101) {
				// Transition to 51
				if (z >= 21) {
					// Transition to 51,21
					Symbolic.mark(0);
					return false;
				} else if (z >= 0 && z < 21){
					// Transition to 51,0
					Symbolic.mark(0);
					return false;
				}
			} else if ((y >= 0 && y < 51) || (y >= 101)) {
				// Stay in epsilon
				if (z >= 51 && z < 101) {
					// Transition to 51
					Symbolic.mark(0);
					return false;
				} else if ((z >= 0 && z < 51) || (z >= 101)) {
					// Stay in epsilon
					Symbolic.mark(1);
					return true;
				}
			}
		} else if (x >= 51 && x < 101) {
			// Transition to 51
			if (y >= 0 && y < 21) {
				// Transition to 51,0
				if (z >= 0 && z < 21) {
					// Transition to epsilon
					// z = z / 0; TODO Coastal doesn't like uncaught exceptions very much, does it?
					Symbolic.mark(1);
					return true;
				} else if (z >= 21) {
					// Transition to 51,21
					Symbolic.mark(0);
					return false;
				}
			} else if (y >= 21) {
				// Transition to 51,21 (trap state)
				Symbolic.mark(0);
				return false;
			}
		}
		
		Symbolic.mark(1);
		return true;
	}
	
	public static void main(String[] args) {
		boolean result = check(new int[] {1, 2, 3});
	}

}

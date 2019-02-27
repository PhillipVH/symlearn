package examples.tacas2017;

import za.ac.sun.cs.coastal.Symbolic;

/**
 * A version of the worked example from the
 * TACAS2017 paper "Learning Symbolic Automata"
 * @author Phillip van Heerden
 *
 */
public class Example {

	public static boolean check(int x, int y, int z) {
		boolean fVal = f(x, y, z);
		boolean gVal = g0(x, y, z);
		return (fVal == gVal);
	}
	
	public static boolean g0(int x, int y, int z) {
		if (x >= 0) {
			if (y >= 0) {
				if (z >= 0) {
					Symbolic.mark(1);
					return true;
				}
			}
		}
		
		return false;
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
		boolean result = check(1, 2, 3);
	}

}

package examples.tacasfubar;

import za.ac.sun.cs.coastal.Symbolic;

public class TACASFUBAR {
	
//	static int inf = Integer.MAX_VALUE; // Let's pretend baby
	
//	public static Function<Integer, Boolean> int_pred(int lower, int upper) {
//		return (x -> (x >= lower && x <= upper));
//	}
	
	public static boolean int_pred (int x, int lower, int upper) {
		return x >= lower && x <= upper;
	}
	
	public static boolean parse(int[] A) {
		int inf = Integer.MAX_VALUE;
		
		int state = 0;
		int finalState = 0;
		
		
		for (int i = 0; i < A.length; i++) {
			switch (state) {
			case 0:
				if (int_pred(A[i], 21, inf)) { state = 0; break; }
				if (int_pred(A[i], 0, 20)) { state = 1; break; }
				break;
				
			case 1:
				if (int_pred(A[i], 0, 24) || int_pred(A[i], 100, inf) || int_pred(A[i], 31, 98)) { state = 1; break; }
				if (int_pred(A[i], 99, 99)) { state = 2; break; }
				if (int_pred(A[i], 25, 30)) { state = 3; break; }
				break;
				
			case 2:
				if (int_pred(A[i], 0, inf)) { state = 2; break; }
				break;
				
			case 3:
				if (int_pred(A[i], 11, inf)) { state = 3; break; }
				if (int_pred(A[i], 0, 10)) { state = 0; break; }
				break;
			}
		}
		
		if (state == finalState) {
			Symbolic.mark(1);
			System.out.println("Accept");
			return true;
		} else {
			Symbolic.mark(0);
			System.out.println("Reject");
			return false;
		}
	}


	public static void main(String[] args) {
		
		boolean res = parse(new int[] {0, 0, 0, 0, 0, 0});

	}
	
}

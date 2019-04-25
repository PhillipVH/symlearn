package examples.tacasfubar;

import za.ac.sun.cs.coastal.Symbolic;

public class TACASFUBAR {
	
//	static int inf = Integer.MAX_VALUE; // Let's pretend baby
	
//	public static Function<Integer, Boolean> int_pred(int lower, int upper) {
//		return (x -> (x >= lower && x <= upper));
//	}
	
//	public static boolean parse(int[] A) {
//		int state = 1;
//		for (int idx = 0; idx < A.length; idx++) {
//			switch (state) {
//			case 1:
//			if ((A[idx] >= 0 && A[idx] <= 0) || (A[idx] >= 2 && A[idx] <= 2147483647) || false) {
//				state = 3;
//				break;
//			}
//			if ((A[idx] >= 1 && A[idx] <= 1) || false) {
//				state = 2;
//				break;
//			}
//			case 2:
//			if ((A[idx] >= 0 && A[idx] <= 2147483647) || false) {
//				state = 2;
//				break;
//			}
//			case 3:
//			if ((A[idx] >= 0 && A[idx] <= 2147483647) || false) {
//				state = 3;
//				break;
//			}
//		  }
//		  }
//		  if ((state == 2) || false) {
//			  Symbolic.mark(1);
//		    return true;
//		  } else { 
//			  Symbolic.mark(0);
//			return false; 
//		  }
//
//		}
//
	
	public static boolean parse(int[] input) {
		int state = 0;
		for (int idx = 0; idx < input.length; idx++) {
			switch (state) {
			case 0:
			if ((input[idx] >= 0 && input[idx] <= 0) || (input[idx] >= 3 && input[idx] <= 2147483647) || false) {
				state = 4;
				break;
			}
			if ((input[idx] >= 2 && input[idx] <= 2) || false) {
				state = 0;
				break;
			}
			if ((input[idx] >= 1 && input[idx] <= 1) || false) {
				state = 3;
				break;
			}
			case 2:
			if ((input[idx] >= 3 && input[idx] <= 3) || false) {
				state = 0;
				break;
			}
			if ((input[idx] >= 1 && input[idx] <= 1) || false) {
				state = 3;
				break;
			}
			if ((input[idx] >= 0 && input[idx] <= 0) || (input[idx] >= 4 && input[idx] <= 2147483647) || false) {
				state = 4;
				break;
			}
			if ((input[idx] >= 2 && input[idx] <= 2) || false) {
				state = 2;
				break;
			}
			case 3:
			if ((input[idx] >= 0 && input[idx] <= 0) || (input[idx] >= 3 && input[idx] <= 2147483647) || false) {
				state = 4;
				break;
			}
			if ((input[idx] >= 1 && input[idx] <= 1) || false) {
				state = 2;
				break;
			}
			if ((input[idx] >= 2 && input[idx] <= 2) || false) {
				state = 3;
				break;
			}
			case 4:
			if ((input[idx] >= 0 && input[idx] <= 2147483647) || false) {
				state = 4;
				break;
			}
		  }
		  }
		  if ((state == 0) || false) {
			  Symbolic.mark(1);
		    return true;
		  } else { 
			  Symbolic.mark(0);
			return false; 
		  }

		}


	public static void main(String[] args) {
		
		boolean res = parse(new int[] {0, 0, 0, 0, 0, 0});

	}
	
}

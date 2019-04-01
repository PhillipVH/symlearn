package examples.oddeven;

import za.ac.sun.cs.coastal.Symbolic;

public class OddEven {
	
	public static boolean oddEvenParser(int[] A) {
		int state = 0;
		int finalState = 3;
		
		for (int i = 0; i < A.length; i++) {
			System.out.println(state);
			switch (state) {
			case 0:
				if (isOdd(A[i])) state = 1;
				if (isEven(A[i])) state = 0;
				break;
				
			case 1:
				if (isOdd(A[i])) state = 2;
				if (isEven(A[i])) state = 0;
				break;
				
			case 2:
				if (isEven(A[i])) state = 3;
				if (isOdd(A[i])) state = 0;
				break;
				
			case 3:
				if (isOdd(A[i])) state = 3;
				if (isEven(A[i])) state = 3;
				break;
			}
		}
		
		if (state == finalState) {
			Symbolic.mark(1);
			return true;
		} else {
			Symbolic.mark(0);
			System.out.println("Rejecting " + state);
			return false;
		}
	}
	
	public static boolean isEven(int n) {
		return n % 2 == 0;
	}
	
	public static boolean isOdd(int n) {
		return n % 2 != 0;
	}


	public static void main(String[] args) {
		
		boolean res = oddEvenParser(new int[] {1, 2, 3, 4});

	}
	
}

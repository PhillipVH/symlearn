package examples.tacas2017;

import za.ac.sun.cs.coastal.Symbolic;

public class LearnLarge {
	public static boolean parse(int[] A) {
		int state = 0;
		for (int idx = 0; idx < A.length; idx++) {
			switch (state) {
			case 0:
				if ((A[idx] >= 0 && A[idx] <= 27) || false) {
					state = 5;
					break;
				}
				if ((A[idx] >= 28 && A[idx] <= 86) || false) {
					state = 2;
					break;
				}
				if ((A[idx] >= 87 && A[idx] <= 2147483647) || false) {
					state = 3;
					break;
				}
			case 1:
				if ((A[idx] >= 65 && A[idx] <= 2147483647) || false) {
					state = 9;
					break;
				}
				if ((A[idx] >= 0 && A[idx] <= 58) || false) {
					state = 6;
					break;
				}
				if ((A[idx] >= 59 && A[idx] <= 64) || false) {
					state = 14;
					break;
				}
			case 2:
				if ((A[idx] >= 26 && A[idx] <= 44) || false) {
					state = 10;
					break;
				}
				if ((A[idx] >= 0 && A[idx] <= 25) || false) {
					state = 4;
					break;
				}
				if ((A[idx] >= 45 && A[idx] <= 67) || false) {
					state = 6;
					break;
				}
				if ((A[idx] >= 68 && A[idx] <= 2147483647) || false) {
					state = 1;
					break;
				}
			case 3:
				if ((A[idx] >= 47 && A[idx] <= 2147483647) || false) {
					state = 3;
					break;
				}
				if ((A[idx] >= 16 && A[idx] <= 16) || false) {
					state = 12;
					break;
				}
				if ((A[idx] >= 17 && A[idx] <= 46) || false) {
					state = 1;
					break;
				}
				if ((A[idx] >= 0 && A[idx] <= 15) || false) {
					state = 6;
					break;
				}
			case 4:
				if ((A[idx] >= 0 && A[idx] <= 65) || false) {
					state = 4;
					break;
				}
				if ((A[idx] >= 70 && A[idx] <= 2147483647) || false) {
					state = 3;
					break;
				}
				if ((A[idx] >= 66 && A[idx] <= 69) || false) {
					state = 1;
					break;
				}
			case 5:
				if ((A[idx] >= 79 && A[idx] <= 90) || false) {
					state = 10;
					break;
				}
				if ((A[idx] >= 0 && A[idx] <= 78) || false) {
					state = 7;
					break;
				}
				if ((A[idx] >= 91 && A[idx] <= 2147483647) || false) {
					state = 8;
					break;
				}
			case 6:
				if ((A[idx] >= 85 && A[idx] <= 2147483647) || false) {
					state = 4;
					break;
				}
				if ((A[idx] >= 0 && A[idx] <= 17) || false) {
					state = 13;
					break;
				}
				if ((A[idx] >= 18 && A[idx] <= 84) || false) {
					state = 12;
					break;
				}
			case 7:
				if ((A[idx] >= 43 && A[idx] <= 2147483647) || false) {
					state = 1;
					break;
				}
				if ((A[idx] >= 0 && A[idx] <= 34) || false) {
					state = 12;
					break;
				}
				if ((A[idx] >= 35 && A[idx] <= 42) || false) {
					state = 2;
					break;
				}
			case 8:
				if ((A[idx] >= 59 && A[idx] <= 69) || false) {
					state = 13;
					break;
				}
				if ((A[idx] >= 0 && A[idx] <= 58) || false) {
					state = 11;
					break;
				}
				if ((A[idx] >= 70 && A[idx] <= 2147483647) || false) {
					state = 8;
					break;
				}
			case 9:
				if ((A[idx] >= 9 && A[idx] <= 2147483647) || false) {
					state = 9;
					break;
				}
				if ((A[idx] >= 0 && A[idx] <= 0) || false) {
					state = 8;
					break;
				}
				if ((A[idx] >= 1 && A[idx] <= 8) || false) {
					state = 14;
					break;
				}
			case 10:
				if ((A[idx] >= 10 && A[idx] <= 2147483647) || false) {
					state = 7;
					break;
				}
				if ((A[idx] >= 0 && A[idx] <= 2) || false) {
					state = 10;
					break;
				}
				if ((A[idx] >= 3 && A[idx] <= 9) || false) {
					state = 5;
					break;
				}
			case 11:
				if ((A[idx] >= 0 && A[idx] <= 2147483647) || false) {
					state = 2;
					break;
				}
			case 12:
				if ((A[idx] >= 0 && A[idx] <= 21) || false) {
					state = 5;
					break;
				}
				if ((A[idx] >= 38 && A[idx] <= 2147483647) || false) {
					state = 11;
					break;
				}
				if ((A[idx] >= 22 && A[idx] <= 27) || false) {
					state = 3;
					break;
				}
				if ((A[idx] >= 28 && A[idx] <= 37) || false) {
					state = 7;
					break;
				}
			case 13:
				if ((A[idx] >= 92 && A[idx] <= 2147483647) || false) {
					state = 14;
					break;
				}
				if ((A[idx] >= 0 && A[idx] <= 91) || false) {
					state = 0;
					break;
				}
			case 14:
				if ((A[idx] >= 59 && A[idx] <= 2147483647) || false) {
					state = 12;
					break;
				}
				if ((A[idx] >= 0 && A[idx] <= 58) || false) {
					state = 13;
					break;
				}
			}
		}
		if ((state == 0) || (state == 2) || (state == 3) || (state == 4) || (state == 5) || (state == 6) || (state == 7)
				|| (state == 8) || (state == 9) || (state == 10) || (state == 11) || (state == 12) || (state == 13)
				|| false) {
			Symbolic.mark(1);
			return true;
		} else {
			Symbolic.mark(0);
			return false;
		}

	}
	
	public static void main(String[] args) {
		boolean result = parse(new int[]{0});
		System.out.println(result);
	}

}

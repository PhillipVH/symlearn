package examples.tacas2017;

import za.ac.sun.cs.coastal.Symbolic;

public class LearnLarge {
	public static boolean parse(int[] input) {
		int state = 0;
		for (int idx = 0; idx < input.length; idx++) {
			switch (state) {
			case 0:
				if ((input[idx] >= 0 && input[idx] <= 27) || false) {
					state = 5;
					break;
				}
				if ((input[idx] >= 28 && input[idx] <= 86) || false) {
					state = 2;
					break;
				}
				if ((input[idx] >= 87 && input[idx] <= 2147483647) || false) {
					state = 3;
					break;
				}
			case 1:
				if ((input[idx] >= 65 && input[idx] <= 2147483647) || false) {
					state = 9;
					break;
				}
				if ((input[idx] >= 0 && input[idx] <= 58) || false) {
					state = 6;
					break;
				}
				if ((input[idx] >= 59 && input[idx] <= 64) || false) {
					state = 14;
					break;
				}
			case 2:
				if ((input[idx] >= 26 && input[idx] <= 44) || false) {
					state = 10;
					break;
				}
				if ((input[idx] >= 0 && input[idx] <= 25) || false) {
					state = 4;
					break;
				}
				if ((input[idx] >= 45 && input[idx] <= 67) || false) {
					state = 6;
					break;
				}
				if ((input[idx] >= 68 && input[idx] <= 2147483647) || false) {
					state = 1;
					break;
				}
			case 3:
				if ((input[idx] >= 47 && input[idx] <= 2147483647) || false) {
					state = 3;
					break;
				}
				if ((input[idx] >= 16 && input[idx] <= 16) || false) {
					state = 12;
					break;
				}
				if ((input[idx] >= 17 && input[idx] <= 46) || false) {
					state = 1;
					break;
				}
				if ((input[idx] >= 0 && input[idx] <= 15) || false) {
					state = 6;
					break;
				}
			case 4:
				if ((input[idx] >= 0 && input[idx] <= 65) || false) {
					state = 4;
					break;
				}
				if ((input[idx] >= 70 && input[idx] <= 2147483647) || false) {
					state = 3;
					break;
				}
				if ((input[idx] >= 66 && input[idx] <= 69) || false) {
					state = 1;
					break;
				}
			case 5:
				if ((input[idx] >= 79 && input[idx] <= 90) || false) {
					state = 10;
					break;
				}
				if ((input[idx] >= 0 && input[idx] <= 78) || false) {
					state = 7;
					break;
				}
				if ((input[idx] >= 91 && input[idx] <= 2147483647) || false) {
					state = 8;
					break;
				}
			case 6:
				if ((input[idx] >= 85 && input[idx] <= 2147483647) || false) {
					state = 4;
					break;
				}
				if ((input[idx] >= 0 && input[idx] <= 17) || false) {
					state = 13;
					break;
				}
				if ((input[idx] >= 18 && input[idx] <= 84) || false) {
					state = 12;
					break;
				}
			case 7:
				if ((input[idx] >= 43 && input[idx] <= 2147483647) || false) {
					state = 1;
					break;
				}
				if ((input[idx] >= 0 && input[idx] <= 34) || false) {
					state = 12;
					break;
				}
				if ((input[idx] >= 35 && input[idx] <= 42) || false) {
					state = 2;
					break;
				}
			case 8:
				if ((input[idx] >= 59 && input[idx] <= 69) || false) {
					state = 13;
					break;
				}
				if ((input[idx] >= 0 && input[idx] <= 58) || false) {
					state = 11;
					break;
				}
				if ((input[idx] >= 70 && input[idx] <= 2147483647) || false) {
					state = 8;
					break;
				}
			case 9:
				if ((input[idx] >= 9 && input[idx] <= 2147483647) || false) {
					state = 9;
					break;
				}
				if ((input[idx] >= 0 && input[idx] <= 0) || false) {
					state = 8;
					break;
				}
				if ((input[idx] >= 1 && input[idx] <= 8) || false) {
					state = 14;
					break;
				}
			case 10:
				if ((input[idx] >= 10 && input[idx] <= 2147483647) || false) {
					state = 7;
					break;
				}
				if ((input[idx] >= 0 && input[idx] <= 2) || false) {
					state = 10;
					break;
				}
				if ((input[idx] >= 3 && input[idx] <= 9) || false) {
					state = 5;
					break;
				}
			case 11:
				if ((input[idx] >= 0 && input[idx] <= 2147483647) || false) {
					state = 2;
					break;
				}
			case 12:
				if ((input[idx] >= 0 && input[idx] <= 21) || false) {
					state = 5;
					break;
				}
				if ((input[idx] >= 38 && input[idx] <= 2147483647) || false) {
					state = 11;
					break;
				}
				if ((input[idx] >= 22 && input[idx] <= 27) || false) {
					state = 3;
					break;
				}
				if ((input[idx] >= 28 && input[idx] <= 37) || false) {
					state = 7;
					break;
				}
			case 13:
				if ((input[idx] >= 92 && input[idx] <= 2147483647) || false) {
					state = 14;
					break;
				}
				if ((input[idx] >= 0 && input[idx] <= 91) || false) {
					state = 0;
					break;
				}
			case 14:
				if ((input[idx] >= 59 && input[idx] <= 2147483647) || false) {
					state = 12;
					break;
				}
				if ((input[idx] >= 0 && input[idx] <= 58) || false) {
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

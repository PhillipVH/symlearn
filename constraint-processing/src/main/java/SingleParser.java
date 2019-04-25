public class SingleParser {
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
            return true;
        } else { 
            return false; 
        }

		}
}

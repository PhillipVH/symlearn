public class TacasParser {
    public static boolean parse(int[] input) {
        int state = 0;
        for (int idx = 0; idx < input.length; idx++) {
            switch (state) {
            case 0:
                if ((input[idx] >= 0 && input[idx] <= 20) || false) {
                    state = 1;
                    break;
                }
                if ((input[idx] >= 21 && input[idx] <= 2147483647) || false) {
                    state = 0;
                    break;
                }
            case 1:
                if ((input[idx] >= 25 && input[idx] <= 30) || false) {
                    state = 2;
                    break;
                }
                if ((input[idx] >= 0 && input[idx] <= 24) || (input[idx] >= 31 && input[idx] <= 98) || (input[idx] >= 100 && input[idx] <= 2147483647) || false) {
                    state = 1;
                    break;
                }
                if ((input[idx] >= 99 && input[idx] <= 99) || false) {
                    state = 3;
                    break;
                }
            case 2:
                if ((input[idx] >= 11 && input[idx] <= 2147483647) || false) {
                    state = 2;
                    break;
                }
                if ((input[idx] >= 0 && input[idx] <= 10) || false) {
                    state = 0;
                    break;
                }
            case 3:
                if ((input[idx] >= 0 && input[idx] <= 2147483647) || false) {
                    state = 3;
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

public class Parser {
    public static boolean parse(int[] A) {
        int state = 1;
        for (int idx = 0; idx < A.length; idx++) {
            switch (state) {
            case 1:
                if ((A[idx] >= 0 && A[idx] <= 0) || (A[idx] >= 2 && A[idx] <= 2147483647) || false) {
                    state = 3;
                    break;
                }
                if ((A[idx] >= 1 && A[idx] <= 1) || false) {
                    state = 2;
                    break;
                }
            case 2:
                if ((A[idx] >= 0 && A[idx] <= 2147483647) || false) {
                    state = 2;
                    break;
                }
            case 3:
                if ((A[idx] >= 0 && A[idx] <= 2147483647) || false) {
                    state = 3;
                    break;
                }
            }
        }
        if ((state == 2) || false) {
            return true;
        } else {
            return false;
        }
		}
}

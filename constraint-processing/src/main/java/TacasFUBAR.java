public class TacasFUBAR {

    public static boolean parse(int[] A) {
        int state = 0;
        for (int idx = 0; idx < A.length; idx++) {
            switch (state) {
                case 0:
                    if ((A[idx] >= 0 && A[idx] <= 0) || (A[idx] >= 3 && A[idx] <= 2147483647) || false) {
                        state = 4;
                        break;
                    }
                    if ((A[idx] >= 2 && A[idx] <= 2) || false) {
                        state = 0;
                        break;
                    }
                    if ((A[idx] >= 1 && A[idx] <= 1) || false) {
                        state = 3;
                        break;
                    }
                case 2:
                    if ((A[idx] >= 3 && A[idx] <= 3) || false) {
                        state = 0;
                        break;
                    }
                    if ((A[idx] >= 1 && A[idx] <= 1) || false) {
                        state = 3;
                        break;
                    }
                    if ((A[idx] >= 0 && A[idx] <= 0) || (A[idx] >= 4 && A[idx] <= 2147483647) || false) {
                        state = 4;
                        break;
                    }
                    if ((A[idx] >= 2 && A[idx] <= 2) || false) {
                        state = 2;
                        break;
                    }
                case 3:
                    if ((A[idx] >= 0 && A[idx] <= 0) || (A[idx] >= 3 && A[idx] <= 2147483647) || false) {
                        state = 4;
                        break;
                    }
                    if ((A[idx] >= 1 && A[idx] <= 1) || false) {
                        state = 2;
                        break;
                    }
                    if ((A[idx] >= 2 && A[idx] <= 2) || false) {
                        state = 3;
                        break;
                    }
                case 4:
                    if ((A[idx] >= 0 && A[idx] <= 2147483647) || false) {
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
    
    public static void main(String[] args) {

        boolean res = parse(new int[]{0, 0, 0, 0, 0, 0});

    }

}

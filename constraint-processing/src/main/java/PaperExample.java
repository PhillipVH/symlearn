public class PaperExample {

    public static boolean parse(int[] A) {
        int state = 0;
        for (int idx = 0; idx < A.length; idx++) {
            switch (state) {
                case 0:
                    if ((A[idx] >= 0 && A[idx] <= 50) || (A[idx] >= 101 && A[idx] <= 2147483647) || false) {
                        state = 0;
                        break;
                    }
                    if ((A[idx] >= 51 && A[idx] <= 100) || false) {
                        state = 1;
                        break;
                    }
                case 1:
                    if ((A[idx] >= 21 && A[idx] <= 2147483647) || false) {
                        state = 3;
                        break;
                    }
                    if ((A[idx] >= 0 && A[idx] <= 20) || false) {
                        state = 2;
                        break;
                    }
                case 2:
                    if ((A[idx] >= 21 && A[idx] <= 2147483647) || false) {
                        state = 3;
                        break;
                    }
                    if ((A[idx] >= 0 && A[idx] <= 20) || false) {
                        state = 0;
                        break;
                    }
                case 3:
                    if ((A[idx] >= 0 && A[idx] <= 2147483647) || false) {
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

    public static void main(String[] args) {
        boolean result = parse(new int[]{51, 51, 51});
        System.out.println(result);
    }
}


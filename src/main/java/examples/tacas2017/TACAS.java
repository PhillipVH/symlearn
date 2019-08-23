package examples.tacas2017;

import za.ac.sun.cs.coastal.Symbolic;

public class TACAS {
    public static boolean parse(int[] A) {
        int state = 0;
        for (int idx = 0; idx < A.length; idx++) {
            switch (state) {
                case 0:
                    if ((A[idx] >= 0 && A[idx] <= 20) || false) {
                        state = 1;
                        break;
                    }
                    if ((A[idx] >= 21 && A[idx] <= 2147483647) || false) {
                        state = 0;
                        break;
                    }
                case 1:
                    if ((A[idx] >= 25 && A[idx] <= 30) || false) {
                        state = 2;
                        break;
                    }
                    if ((A[idx] >= 0 && A[idx] <= 24) || (A[idx] >= 31 && A[idx] <= 98) || (A[idx] >= 100 && A[idx] <= 2147483647) || false) {
                        state = 1;
                        break;
                    }
                    if ((A[idx] >= 99 && A[idx] <= 99) || false) {
                        state = 3;
                        break;
                    }
                case 2:
                    if ((A[idx] >= 11 && A[idx] <= 2147483647) || false) {
                        state = 2;
                        break;
                    }
                    if ((A[idx] >= 0 && A[idx] <= 10) || false) {
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
            Symbolic.mark(1);
            return true;

        } else {
            Symbolic.mark(0);
            return false;
        }

    }

    public static void main(String[] args) {
        System.out.println(parse(new int[]{1, 2, 3}));
    }
}

package examples.tacas2017;

import za.ac.sun.cs.coastal.Symbolic;

public final class Regex {
    public static void main(String[] args) {
        boolean result = parse(new char[]{'d'});
        System.out.println(result);
    }

    public static boolean parse(char[] A) {
        int state = 0;
        for (int idx = 0; idx < A.length; idx++) {
            char current = A[idx];
            if (state == 0) {
                if ((current == (char) 98)) {
                    state = 1;
                    continue;
                }
                if ((current >= (char) 0 && current <= (char) 96) || (current >= (char) 99 && current <= (char) 65535)) {
                    state = 3;
                    continue;
                }
                if ((current == (char) 97)) {
                    state = 2;
                    continue;
                }
            }
            if (state == 1) {
                if ((current >= (char) 0 && current <= (char) 65535)) {
                    state = 3;
                    continue;
                }
            }
            if (state == 2) {
                if ((current >= (char) 0 && current <= (char) 65535)) {
                    state = 3;
                    continue;
                }
            }
            if (state == 3) {
                if ((current >= (char) 0 && current <= (char) 65535)) {
                    state = 3;
                    continue;
                }
            }
        }
        if ((
                state == 1) || (state == 2) || false) {
            Symbolic.mark(1);
            return true;
        } else {
            Symbolic.mark(0);
            return false;
        }
    }
}

public final class Parser {
	public static void main(String[] args) {
		boolean result = parse(new char[]{'a'});
	}
	public static boolean parse(char[] A) {
		int state = 0;
		for (int idx = 0; idx < A.length; idx++) {
			char current = A[idx];
			if (state == 0) {
				if ((current >= (char)0 && current <= (char)96) || (current >= (char)98 && current <= (char)65535)) {
					state = 2;
					continue;
				}
				if ((current == (char)97)) {
					state = 1;
					continue;
				}
			}
			if (state == 1) {
				if ((current >= (char)0 && current <= (char)65535)) {
					state = 2;
					continue;
				}
			}
			if (state == 2) {
				if ((current >= (char)0 && current <= (char)65535)) {
					state = 2;
					continue;
				}
			}
		}
		if ((state == 1) || false) { 
			return true;
		} else {
			return false;
		}
	}
}

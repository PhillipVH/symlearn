package examples.parsing.csv;

public class SimpleMax {
	public static void main(String[] args) {
		int result = max(1, 2);
	}
	

	public static int max(int x, int y) {
		if (x > y) {
			return x;
		} else if (x < y){
			return y;
		} else {
			return x;
		}
	}
}

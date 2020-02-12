import java.util.Arrays;
import java.util.Scanner;

public class Quick {

  {{{target-fn}}}

	public static void main(String[] args) {
    // boolean result = parse(args[0].toCharArray());
      Scanner scanner = new Scanner(System.in);
      String input = scanner.nextLine();
      boolean result = parse(input.toCharArray());
    if (result) {
        System.out.println("+");
    } else {
        System.out.println("-");
    }
	}
}

import java.util.Arrays;
import java.util.Stack;

/**
 * User: Karl Rege
 */


public class Calculator {
	private static Stack<Double> numbers_stack = new Stack<>();

	public static void expr()	throws Exception {
		term();
		while (Scanner.la == Token.PLUS 
				|| Scanner.la == Token.MINUS) {
			Scanner.scan();
			int op = Scanner.token.kind;
			term();
			switch (op) {
				case(Token.PLUS):
					numbers_stack.push(numbers_stack.pop() + numbers_stack.pop());
					break;
				case(Token.MINUS):
					numbers_stack.push(- numbers_stack.pop() + numbers_stack.pop());
					break;

				default:
					numbers_stack.push(-1.0);
					System.out.println("ERROR IN ADDITION/SUBTRACTION");
			}
		}
	}

	public static void term()	throws Exception {
		factor();
		while (Scanner.la == Token.TIMES || Scanner.la == Token.SLASH) {
			Scanner.scan();
			int op = Scanner.token.kind;
			factor();
			switch (op) {
				case(Token.TIMES):
					numbers_stack.push(numbers_stack.pop() * numbers_stack.pop());
					break;
				case(Token.SLASH):
					//double division = numbers_stack.pop() / numbers_stack.pop();
					//System.out.println("Test dividend = " + numbers_stack.peek());
					//System.out.println("Test divisor = " + numbers_stack.peek());
					double first = numbers_stack.pop();
					double second = numbers_stack.pop();
					numbers_stack.push(second / first);
					break;

				default:
					numbers_stack.push(-1.0);
					System.out.println("ERROR IN MULTIPLICATION/DIVISION");
			}
		}
	}

	public static void factor() throws Exception {
		if (Scanner.la == Token.LBRACK) {
			Scanner.scan();
			expr();
			Scanner.check(Token.RBRACK);
		} else if (Scanner.la == Token.NUMBER || Scanner.la == Token.IDENT) {
			//System.out.println("Scanner.toke.val= " + Scanner.laToken.val);
			//numbers_stack.push(Scanner.laToken.val);
			Scanner.scan();
			if (Scanner.token.str.equals("PI")) numbers_stack.push(Math.PI);
			else if (Scanner.token.str.equals("E")) numbers_stack.push(Math.E);
			else numbers_stack.push(Scanner.token.val);
		}
	}
	
	public static double start(String expr) throws Exception {
       	Scanner.init(expr);
       	Scanner.scan();
		System.out.println("test");
       	expr();
		for (double number:numbers_stack) {
			System.out.println(number);
		}
       	return numbers_stack.pop();
	}

   public static void main(String[] args) throws Exception {
   	    System.out.println("result="+start("84/2.0"));
   }
      
}

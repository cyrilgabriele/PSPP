import de.inetsoftware.jwebassembly.JWebAssembly;
import de.inetsoftware.jwebassembly.module.*;

public class Calculator implements Emitter {
    //private static Stack<Double> numbers_stack = new Stack<>();

    @Override
    public void emit() {
        try {
            expr();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void expr() throws Exception {
        term();
        while (Scanner.la == Token.PLUS
                || Scanner.la == Token.MINUS) {
            Scanner.scan();
            int op = Scanner.token.kind;
            term();
            switch (op) {
                case(Token.PLUS):
                    //numbers_stack.push(numbers_stack.pop() + numbers_stack.pop());
                    JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.add, ValueType.f64, 0));
                    break;
                case(Token.MINUS):
                    //numbers_stack.push(- numbers_stack.pop() + numbers_stack.pop());
                    JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.sub, ValueType.f64, 0));
                    break;

                default:
                    //numbers_stack.push(-1.0);
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
                    //numbers_stack.push(numbers_stack.pop() * numbers_stack.pop());
                    JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.mul, ValueType.f64, 0));
                    break;
                case(Token.SLASH):
                    //double division = numbers_stack.pop() / numbers_stack.pop();
                    //System.out.println("Test dividend = " + numbers_stack.peek());
                    //System.out.println("Test divisor = " + numbers_stack.peek());
                    //double first = numbers_stack.pop();
                    //double second = numbers_stack.pop();
                    //numbers_stack.push(second / first);
                    JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.div, ValueType.f64, 0));
                    break;

                default:
                    //numbers_stack.push(-1.0);
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
            if (Scanner.token.str.equals("PI")) JWebAssembly.il.add(new WasmConstInstruction(Math.PI, 0));
            else if (Scanner.token.str.equals("E")) JWebAssembly.il.add(new WasmConstInstruction(Math.E, 0));
            else JWebAssembly.il.add(new WasmConstInstruction(Scanner.token.val, 0));
        }
    }

    /*
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
    */


    public static void main(String[] args) throws Exception {
        Scanner.init("4.2 + 3.2*2");
        Scanner.scan();
        System.out.println("Started the main-function");
        JWebAssembly.emitCode(ICalculator.class, new Calculator());
    }
}
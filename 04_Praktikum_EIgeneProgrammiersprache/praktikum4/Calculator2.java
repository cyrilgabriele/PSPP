import de.inetsoftware.jwebassembly.JWebAssembly;
import de.inetsoftware.jwebassembly.module.*;

public class Calculator2 implements Emitter {

    @Override
    public void emit() {
        try {
            //expr();
            programm();
            //JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.RETURN, null, 0));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void expr() throws Exception {
        term();
        while (Scanner.la == Token.PLUS || Scanner.la == Token.MINUS) {
            Scanner.scan();
            int op = Scanner.token.kind;
            term();
            switch (op) {
                case(Token.PLUS):
                    JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.add, ValueType.f64, 0));
                    break;

                case(Token.MINUS):
                    JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.sub, ValueType.f64, 0));
                    break;

                default:
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
                    JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.mul, ValueType.f64, 0));
                    break;

                case(Token.SLASH):
                    JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.div, ValueType.f64, 0));
                    break;

                default:
                    System.out.println("ERROR IN MULTIPLICATION/DIVISION");
            }
        }
    }

    public static void factor() throws Exception {
        /*
        //=> HERE IS SOMEWHERE A BUG!!!!
        if (Scanner.la == Token.LBRACK) {
            Scanner.scan();
            expr();
            Scanner.check(Token.RBRACK);
        } else if (Scanner.la == Token.NUMBER || Scanner.la == Token.IDENT) {
            Scanner.scan();
            if (Scanner.token.str.equals("PI")) JWebAssembly.il.add(new WasmConstInstruction(Math.PI, 0));
            else if (Scanner.token.str.equals("E")) JWebAssembly.il.add(new WasmConstInstruction(Math.E, 0));
            else new WasmLoadStoreInstruction(true, JWebAssembly.local(ValueType.f64, Scanner.token.str), 0);
            //in line (70) before si something wrong...
        }


         */

        if (Scanner.la == Token.LBRACK) {
            Scanner.scan();
            expr();
            Scanner.check(Token.RBRACK);
        } else if (Scanner.la == Token.NUMBER) {
            Scanner.scan();
            JWebAssembly.il.add(new WasmConstInstruction(Scanner.token.val, 0));
        } else if (Scanner.la == Token.IDENT) {
            Scanner.scan();
            switch (Scanner.token.str) {
                case "PI":
                    JWebAssembly.il.add(new WasmConstInstruction(Math.PI, 0));
                    break;
                case "E":
                    JWebAssembly.il.add(new WasmConstInstruction(Math.E, 0));
                    break;
                default:
                    JWebAssembly.il.add(new WasmLoadStoreInstruction(true, JWebAssembly.local(ValueType.f64, Scanner.token.str), 0));
                    break;
            }
        } else {
            Scanner.error("illegal Symbol");
        }

    }

    public static void programm() {
        statementSequence();
        JWebAssembly.il.add(new WasmLoadStoreInstruction(true, JWebAssembly.local(ValueType.f64, "value"), 0));
        JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.RETURN, null, 0));
    }

    public static void statementSequence() {
        statement();
        while ((Scanner.la != Token.RBRACK) && Scanner.la != Token.EOF) {
            statement();
        }
    }

    public static void returnStatement() {
        try {
            Scanner.check(Token.RETURN);
            JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.RETURN, null, 0));
        } catch (Exception e) {
            System.out.print(e.getMessage());
        }
    }

    public static void statement() {
        if(Scanner.la == Token.IF) {
            ifStatement();
        }
        if(Scanner.la == Token.WHILE) {
           whileStatement();
        }
        assignment();
        returnStatement();
        block();
    }

    public static void assignment() {
        /*
        Scanner.check(Scanner.token.kind);
        if (Scanner.la == Token.EQUAL) {
            Scanner.scan();
        } else if (Scanner.la == Token.IDENT)
        */
        try {
            Scanner.check(Token.IDENT); //checks if Scaner.la == Token.IDENT
            int indexOfVariable = JWebAssembly.local(ValueType.f64, Scanner.token.str);
            Scanner.check(Token.EQUAL);
            expr();
            Scanner.check(Token.SCOLON);
            JWebAssembly.il.add(new WasmLoadStoreInstruction(false, indexOfVariable, 0));
            JWebAssembly.il.add(new WasmLoadStoreInstruction(true, indexOfVariable, 0));
        } catch (Exception e) {
            System.out.print(e.getMessage());
        }
    }

    public static void block(){
        try {
            Scanner.check(Token.LCBRACK);
            statementSequence();
            Scanner.check(Token.RBRACK);
        } catch (Exception e) {
            System.out.print(e.getMessage());
        }
    }


    public static void ifStatement(){
        try {
            Scanner.check(Token.IF);
            Scanner.scan();
            condition();
        } catch (Exception e) {
            System.out.print(e.getMessage());
        }
    }

    public static void whileStatement() {
        try {
            Scanner.check(Token.WHILE);
            Scanner.scan();
            condition();
            statement();
        } catch (Exception e) {
            System.out.print(e.getMessage());
        }
    }

    public static void condition(){
        try {
            Scanner.check(Token.LBRACK);
        } catch (Exception e) {
            System.out.print(e.getMessage());
        }

    }

    public static void main(String[] args) throws Exception {
        Scanner.init("4.2 + 3.2*2");
        Scanner.scan();
        System.out.println("Started the main-function");
        JWebAssembly.emitCode(ICalculator.class, new Calculator2());
    }
}
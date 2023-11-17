import de.inetsoftware.jwebassembly.JWebAssembly;
import de.inetsoftware.jwebassembly.module.*;

import java.util.HashMap;
import java.util.Map;


interface IProgram {
    double main(double arg);
}

public class Program implements Emitter {

    @Override
    public void emit() {
        try {
            //expr();
            program();
            //JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.RETURN, null, 0));
            JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.UNREACHABLE, null, 0));
        } catch (Exception e) {
            throw new RuntimeException(e);
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

    public static void term() throws Exception {
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

    public static void program() throws Exception {
        statementSequence();
        Scanner.check(Token.EOF);
        //JWebAssembly.il.add(new WasmLoadStoreInstruction(true, JWebAssembly.local(ValueType.f64, "value"), 0));
        //JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.RETURN, null, 0));
    }

    public static void statementSequence() {
        statement();
        while (Scanner.la == Token.RETURN ||  Scanner.la == Token.IF || Scanner.la == Token.WHILE
                || Scanner.la == Token.LCBRACK || Scanner.la == Token.IDENT ) {
            statement();
        }
    }

    public static void returnStatement() {
        try {
            Scanner.check(Token.RETURN);
            expr();
            Scanner.check(Token.SCOLON);
            JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.RETURN, null, 0));
        } catch (Exception e) {
            System.out.print(e.getMessage());
        }
    }

    public static void statement() {
        /*
        if(Scanner.la == Token.IF) {
            ifStatement();
        }
        if(Scanner.la == Token.WHILE) {
           whileStatement();
        }
        if(Scanner.la == Token.RETURN) {
            returnStatement();
        }
        if(Scanner.la == Token.LCBRACK) {
            block();
        }
        if(Scanner.la == Token.IDENT) {
            assignment();
        }
        */
        try{
            switch (Scanner.la) {
                case Token.RETURN:
                    returnStatement();
                    break;
                case Token.IF:
                    ifStatement();
                    break;
                case Token.WHILE:
                    whileStatement();
                    break;
                case Token.LCBRACK:
                    block();
                    break;
                case Token.IDENT:
                    assignment();
                    break;
                default:
                    Scanner.error("Error in Statement");
            }
        } catch (Exception e) {
            System.out.print(e.getMessage());
        }


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
            //Scanner.scan();
            condition();
            JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.IF, null, 0)); //überprüft, ob condition != 0 ist
            statement();
            if(Scanner.la == Token.ELSE) {
                Scanner.check(Token.ELSE);
                JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.ELSE, null, 0));
                statement();
            }
            JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.END, null, 0));
        } catch (Exception e) {
            System.out.print(e.getMessage());
        }
    }

    public static void whileStatement() {
        try {
            Scanner.check(Token.WHILE);
            JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.BLOCK, null, 0));
            JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.LOOP, null, 0));
            condition();
            JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.eqz, ValueType.i32, 0));
            JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.BR_IF, 1, 0));
            statement();
            JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.BR, 0, 0));
            JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.END, null, 0));
            JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.END, null, 0));
        } catch (Exception e) {
            System.out.print(e.getMessage());
        }
    }

    public static void condition(){

        try {
            boolean negated_statement = false;
            Scanner.check(Token.LBRACK);
            if(Scanner.la == Token.NOT) {
                negated_statement = true;
                Scanner.scan();
            }
            //Scanner.check(Token.NOT); //TODO fix here the semantic of [!] the brackets!
            expr();
            JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.nearest, ValueType.f64, 0));
            JWebAssembly.il.add(new WasmConvertInstruction(ValueTypeConvertion.d2i, 0));
            if(negated_statement) {
                JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.eqz, ValueType.i32, 0));
            }
            Scanner.check(Token.RBRACK);
        } catch (Exception e) {
            System.out.print(e.getMessage());
        }

    }
}
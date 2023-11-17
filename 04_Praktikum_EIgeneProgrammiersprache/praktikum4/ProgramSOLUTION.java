import java.util.*;
import de.inetsoftware.jwebassembly.*;
import de.inetsoftware.jwebassembly.module.*;

interface IProgram {
    double main(double arg);
}

public class Program implements Emitter {

    static void assignment() throws Exception {
        Scanner.check(Token.IDENT);
        int variableIndex = JWebAssembly.local(ValueType.f64, Scanner.token.str);
        Scanner.check(Token.EQUAL);
        expr();
        JWebAssembly.il.add(new WasmLoadStoreInstruction(false, variableIndex, 0));
        Scanner.check(Token.SCOLON);
    }

    static void block() throws Exception {
        Scanner.check(Token.LCBRACK);
        statementSequence();
        Scanner.check(Token.RCBRACK);
    }

    static void ifStatement() throws Exception {
        Scanner.check(Token.IF);
        condition();
        JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.IF, null, 0));
        statement();
        if (Scanner.la == Token.ELSE) {
            Scanner.check(Token.ELSE);
            JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.ELSE, null, 0));
            statement();
        }
        JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.END, null, 0));
    }

    static void returnStatement() throws Exception {
        Scanner.check(Token.RETURN);
        expr();
        Scanner.check(Token.SCOLON);
        JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.RETURN, null, 0));
    }

    static void whileStatement() throws Exception {
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
    }

    static void condition() throws Exception {
        Scanner.check(Token.LBRACK);
        boolean negated = false;
        if (Scanner.la == Token.NOT) {
            negated = true;
            Scanner.scan();
        }
        expr();
        JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.nearest, ValueType.f64, 0));
        JWebAssembly.il.add(new WasmConvertInstruction(ValueTypeConvertion.d2i, 0));
        if (negated) {
            JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.eqz, ValueType.i32, 0));
        }
        Scanner.check(Token.RBRACK);
    }

    static void statement() throws Exception {
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
    }

    static void statementSequence() throws Exception {
        statement();
        while (Scanner.la == Token.RETURN ||  Scanner.la == Token.IF || Scanner.la == Token.WHILE 
                        || Scanner.la == Token.LCBRACK || Scanner.la == Token.IDENT ) {
            statement();
        }
    }

    static void program() throws Exception {
        statementSequence();
        Scanner.check(Token.EOF);
    }

    static void expr() throws Exception {
        term();
        while (Scanner.la == Token.PLUS || Scanner.la == Token.MINUS) {
            Scanner.scan();
            int op = Scanner.token.kind;
            term();
            if (op == Token.PLUS) {
                JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.add, ValueType.f64, 0));
            } else {
                JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.sub, ValueType.f64, 0));
            }
        }
    }

    static void term() throws Exception {
        factor();
        while (Scanner.la == Token.TIMES || Scanner.la == Token.SLASH) {
            Scanner.scan();
            int op = Scanner.token.kind;
            factor();
            if (op == Token.TIMES) {
                JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.mul, ValueType.f64, 0));
            } else {
                JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.div, ValueType.f64, 0));
            }
        }
    }
    
    static Map<String, Double> constMap = new HashMap<String, Double>() {
        {
            put("PI", Math.PI);
            put("E", Math.E);
        }
    };

    static void factor() throws Exception {
        if (Scanner.la == Token.LBRACK) {
            Scanner.scan();
            expr();
            Scanner.check(Token.RBRACK);
        } else if (Scanner.la == Token.NUMBER) {
            Scanner.scan();
            JWebAssembly.il.add(new WasmConstInstruction(Scanner.token.val, 0));
        } else if (Scanner.la == Token.IDENT) {
            Scanner.scan();
            String varString = Scanner.token.str;
            Double constValue = constMap.get(varString);
            if (constValue != null) {
                JWebAssembly.il.add(new WasmConstInstruction(constValue, 0));
            } else {
                JWebAssembly.il.add(new WasmLoadStoreInstruction(true, JWebAssembly.local(ValueType.f64, varString), 0));
            }       
        }
    }

    @Override
    public void emit()  {
        try {
            program();
            JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.UNREACHABLE, null, 0));
        } catch (Exception e) {
            throw new RuntimeException (e);
        }
    }
}


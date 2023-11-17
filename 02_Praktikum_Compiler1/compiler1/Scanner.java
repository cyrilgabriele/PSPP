public class Scanner {
    public static int pos;
    static String input;
    static char ch;
    static final char EOF = '\u0080';
    static int line;
    static int col;

    private static void readName(Token t) {
        /*
        t.kind = Token.IDENT;
        t.str = "";
        boolean isAddingChar = true;
        while(isAddingChar) {
            if (ch == EOF || ch == ' ') {
                isAddingChar = false;
                //nextCh();
                break;
            }
            //System.out.println("Current char= "+ ch);
            t.str += ch;
            nextCh();
        }
        switch (t.str) {
            case("PI"):
                t.val = 3.141592653589793;
                //t.str = "";
                nextCh();
                break;
            case("E"):
                t.val = 2.718281828459;
                //t.str = "";
                nextCh();
                break;
            default:
                t.val = -1;
                System.out.println("Value of t= " + t.str);
                System.out.println("ERROR IN PI / E");
        }
        */
        t.kind = Token.IDENT;
        t.str = "";
        if (ch == 'P') {
            t.str += ch;
            nextCh();
            if (ch == 'I') {
                t.kind = Token.IDENT;
                t.str += ch;
                t.val = 3.141592653589793;
                nextCh();
            } else {
                t.str = "";
            }
        } else if (ch == 'E') {
            t.kind = Token.IDENT;
            t.str += ch;
            t.val = 2.718281828459045;
            nextCh();
        }
    }

    private static void readNumber(Token t) {
        t.kind = Token.NUMBER;
        t.str = "";
        int state = 0;
        for (; ; ) {
            switch (state) {
                case 0:
                    if ((ch >= '0' && ch <= '9') || ch == '.') {
                        t.str += ch;
                        nextCh();
                    } else state = 1;
                    break;
                case 1:
                    t.val = Double.parseDouble(t.str);
                    return;
            }
        }
    }

    private static void nextCh() {
        if (pos < input.length()) {
            ch = input.charAt(pos++);
            if (ch == '\n') {
                line++;
                col++;
            }
        } else
            ch = EOF;
    }

    public static void init(String s) {
        input = s;
        pos = 0;
        nextCh();
    }

    public static Token next() {
        while (ch <= ' ') nextCh(); // skip blanks, tabs, eols
        Token t = new Token();
        t.pos = pos;
        t.line = line;
        switch (ch) {
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
                readNumber(t);
                break;
            case 'A':
            case 'B':
            case 'C':
            case 'D':
            case 'E':
            case 'F':
            case 'G':
            case 'H':
            case 'I':
            case 'J':
            case 'K':
            case 'L':
            case 'M':
            case 'N':
            case 'O':
            case 'P':
            case 'Q':
            case 'R':
            case 'S':
            case 'T':
            case 'U':
            case 'V':
            case 'W':
            case 'X':
            case 'Y':
            case 'Z':
            case 'a':
            case 'b':
            case 'c':
            case 'd':
            case 'e':
            case 'f':
            case 'g':
            case 'h':
            case 'i':
            case 'j':
            case 'k':
            case 'l':
            case 'm':
            case 'n':
            case 'o':
            case 'p':
            case 'q':
            case 'r':
            case 's':
            case 't':
            case 'u':
            case 'v':
            case 'w':
            case 'x':
            case 'y':
            case 'z':
            case '$':
                readName(t);
                break;
            case '+':
                t.kind = Token.PLUS;
                nextCh();
                break;
            case '-':
                t.kind = Token.MINUS;
                nextCh();
                break;
            case '*':
                t.kind = Token.TIMES;
                nextCh();
                break;
            case '/':
                t.kind = Token.SLASH;
                nextCh();
                break;
            case '(':
                t.kind = Token.LBRACK;
                nextCh();
                break;
            case ')':
                t.kind = Token.RBRACK;
                nextCh();
                break;
            case EOF:
                t.kind = Token.EOF;
                break;
            default:
                t.kind = Token.NONE;
                nextCh();
                break;
        }
        return t;
    }


    public static Token token;  // zuletzt erkanntes Token
    public static int la; // kind von lookahead token
    public static Token laToken;  // lookahead token

    // lookahead Methoden
    public static void scan() {
        token = laToken;
        laToken = Scanner.next();
        la = laToken.kind;
    }

    public static void check(int expected) throws Exception {
        if (la == expected) scan();  // erkannt, daher weiterlesen
        else error(Token.name(expected) + " expected");
    }

    public static void error(String msg) throws Exception {
        throw new Exception(msg + " at " + Scanner.laToken.pos);
    }

    /* Test */
    public static void main(String[] args) {
        // init("42 + 45 + (32)*78+45-56");
        init("PI E");
        Token t = next();
        while (t.kind != Token.EOF) {
            System.out.println("<" + Token.name(t.kind) + ":" + t.val + ">");
            t = next();
        }
        System.out.println();

    }

}

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

import static org.junit.Assert.*;
import org.junit.Test;
import java.io.*;

/**
 *
 * @author Karl Rege
 */
public class PS2_1_test {
    String fileToTest = "Scanner.java";

    @Test
    public void checkFileUpload() throws Exception {
        File f = new File(fileToTest);
        assertTrue("File uploaded "+fileToTest,f.exists());
    }

    private void testToken(String source, int... tokens) {
        Scanner.init(source);
        for (int t : tokens) {
            Token s = Scanner.next();
            assertEquals(source, t, s.kind);
        }
    }

    private void testValue(String source, double... vals) {
        Scanner.init(source);
        final double EPS = 1E-10;
        for (double t : vals) {
            Token s = Scanner.next();
            assertEquals(source, t, s.val, EPS);
        }
    }

    @Test
    public void testTokens() {
        testToken("42.0", Token.NUMBER, Token.EOF);
        testValue("42.0", 42.0);
        testToken("123.4", Token.NUMBER, Token.EOF);
        testValue("123.4", 123.4);
        testToken("42.0 123.4", Token.NUMBER, Token.NUMBER, Token.EOF);
        testValue("42.0 123.4", 42.0, 123.4);
    }

}

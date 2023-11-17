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
public class PS2_3_test {
    
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

    private void testStr(String source, String... strs) {
        Scanner.init(source);
        for (String t : strs) {
            Token s = Scanner.next();
            assertEquals(source, t, s.str);
        }
    }

    @Test
    public void testPi() {
        testToken("PI", Token.IDENT, Token.EOF);
        testStr("PI", "PI");
    }
    
    @Test
    public void testE() {
        testToken("E", Token.IDENT, Token.EOF);
        testStr("E", "E");
    }
    
    @Test
    public void testPiAndE() {
        testToken("PI E", Token.IDENT, Token.IDENT, Token.EOF);
        testStr("PI E", "PI", "E");
    }

}

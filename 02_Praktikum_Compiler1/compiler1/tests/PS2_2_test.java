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
public class PS2_2_test {
    String fileToTest = "Calculator.java";

    @Test
    public void checkFileUpload() throws Exception {
        File f = new File(fileToTest);
        assertTrue("File uploaded "+fileToTest,f.exists());
    }

    private void testResult(String source, double val) throws Exception {
        final double EPS = 1E-10;
        double s = Calculator.start(source);
        assertEquals(source, val, s, EPS);
    }

    @Test
    public void testSingleValue() throws Exception {
        testResult("42.0", 42.0);
        testResult("123.4", 123.4);
    }    

    @Test
    public void testAddSub() throws Exception {
        testResult("20+22.0", 42.0);
        testResult("64-22.0", 42.0);
    }
    
    @Test
    public void testMulDiv() throws Exception {
        testResult("2*21.0", 42.0);
        testResult("84/2.0", 42.0);
    }
    
    @Test
    public void testMixed() throws Exception {
        testResult("2 *(10 + 11.0)", 42.0);
        testResult("20 + 2 * 11.0", 42.0);
    }

}

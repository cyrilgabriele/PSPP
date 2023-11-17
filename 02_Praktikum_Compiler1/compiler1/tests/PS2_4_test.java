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
public class PS2_4_test {
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
    public void testPi() throws Exception {
        testResult("PI", Math.PI);
    }
    
    @Test
    public void testE() throws Exception {
        testResult("E", Math.E);
    }
    
    @Test
    public void testPiAndE() throws Exception {
        testResult("PI+E", Math.PI+Math.E);
        testResult("PI/E", Math.PI/Math.E);
        testResult("4*PI/E", 4*Math.PI/Math.E);
    }

}

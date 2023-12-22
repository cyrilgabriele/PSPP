package ch.zhaw.montecarlo;
import ch.zhaw.random.HighQualityRandom;

public class MonteCarlo {
    public static void main(String[] args) {
        HighQualityRandom randomGenerator = new HighQualityRandom();
        int totalPoints = 1000000;
        int insideCircle = 0;

        for (int i = 0; i < totalPoints; i++) {
            double x = randomGenerator.nextDouble();
            double y = randomGenerator.nextDouble();

            if (x * x + y * y < 1) {
                insideCircle++;
            }
        }

        double piApproximation = 4.0 * insideCircle / totalPoints;
        System.out.println("Approximation of Pi: " + piApproximation);
    }
}

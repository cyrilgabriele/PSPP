"Exercise 3: MC Pi calculation"
|x y randomClass pointsInsideCircle numberOfPoints distanceSquared|

randomClass := Random new.
pointsInsideCircle := 0.
numberOfPoints := 1000.

"Transcript open.
Transcript show: randomClass next; cr."

1 to: numberOfPoints do: [ :i |
	x := randomClass next.
	y := randomClass next.
	
	distanceSquared := (x squared) + (y squared).
	(distanceSquared <= 1.0)
		ifTrue: [pointsInsideCircle := pointsInsideCircle + 1].
	].

"Pi approimation in quadrant"
approxPi := (pointsInsideCircle / numberOfPoints) * 4.0.

Transcript open.
Transcript show: approxPi; cr.
	
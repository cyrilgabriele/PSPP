|canvasMorph rand x y distanceSquared scaleFactor|
canvasMorph := CanvasMorph new.
scaleFactor := 100.
canvasMorph 
	openInWindowLabeled: 'MC Pi approximation';
	drawBlock: [:c |
		rand := Random new. 
        1 to: 10000 do: [:i |
            x := rand next * scaleFactor.
            y := rand next * scaleFactor.
            distanceSquared := (x / scaleFactor) squared + (y / scaleFactor) squared.
            (distanceSquared <= 1.0)
                ifTrue: [
                    c line: (x - 1)@y to: (x + 1)@y color: Color red.
                    c line: x@(y - 1) to: x@(y + 1) color: Color red.
                ]
                ifFalse: [
                    c line: (x - 1)@y to: (x + 1)@y color: Color blue.
                    c line: x@(y - 1) to: x@(y + 1) color: Color blue.
                ].
        ]
    ].

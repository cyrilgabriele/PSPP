|stringToTranskript hello world result1 result2|. "declaration of the variables"

"Here a we concatenate a String which is later printed on the transcript"
hello := 'Hello '.
world := 'World'.
"Here we create some arithmetik calculations, later printed with using cascaded message calls"
result1 := 2 + 3 squared.
result2 := 2 + 2 * 10. "<= here we would expect 22 (due to arithmetic presidences, BUT we get 40 bc Smalltalk simply executes from left to right!)"

stringToTranskript := hello, world. 
Transcript open.
Transcript 
	show: stringToTranskript; cr;
	show: result1; cr;
	show: result2.
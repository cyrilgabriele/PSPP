|stringToTranskript hello world result1 result2 testClass answer|. "declaration of the variables"
"Exercise 1"
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
	show: result2; cr.
	
"Exercise 2 => MyClass needs to be defined in the workspace, aswell as the method theanswer"
testClass := MyClass new.
answer := testClass theanswer. 
Transcript show: answer; cr.
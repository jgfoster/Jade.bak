"Filed out from Dolphin Smalltalk 7"!

SmalltalkSyntaxTutorial subclass: #GemStoneSyntaxTutorial
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GemStoneSyntaxTutorial guid: (GUID fromString: '{3b693f04-b02c-4d7b-9f42-671a30534155}')!
GemStoneSyntaxTutorial comment: ''!
!GemStoneSyntaxTutorial categoriesForClass!Kernel-Objects! !
!GemStoneSyntaxTutorial methodsFor!

basicTypesArray
	^ Lesson
title: 'Basic types: Array' 
contents: 
'"Literal arrays are created at parse time:"

#(1 2 3).

#( 1 2 3 #(4 5 6)) size.

#(1 2 4) isEmpty.

#(1 2 3) first.

#(''hello'' ''GemStone'') at: 2.

ProfStef next.'
!

basicTypesCharacters
	^ Lesson
title: 'Basic types: Characters' 
contents: 
'"A Character can be instantiated using $ operator:"

$A.

$A class.

$B codePoint.

Character cr codePoint.

Character space codePoint.

"Any Unicode character can be instantiated using its code point 
(here we have a hex number for the gem stone character)"

(Character codePoint: 16r1F48E) asString encodeAsUTF8.

ProfStef next.'
!

basicTypesNumbers
	^ Lesson
title: 'Basic types: Numbers' 
contents: 
'"You now know how to execute Smalltalk code. 

Now let''s talk about basic objects.

1, 2, 100, 2/3 ... are Numbers, and respond to many messages evaluating 
mathematical expressions. Display these:"

2.

"Notice that fractions are kept as fractions and not reduced to floats"

(1/3).

(1/3) + (4/5).

(18/5) rounded.

"Also Smalltalk can represent VERY large integers"

200 factorial.

1 class.

200 factorial class.

1 negated.

1 negated negated.

(1 + 3) odd.

ProfStef next.'
!

basicTypesString
	^ Lesson
title: 'Basic types: Strings' 
contents: 
'"A String is a collection of characters. Use single quotes to create a String object. 
Display these expressions:"

''ProfStef''.

''ProfStef'' size.

''abc'' asUppercase.

''Hello World'' reverse. 

"You can access each character using at: message"

''ProfStef'' at: 1.

"String concatenation uses the comma operator:"

''ProfStef'', '' is cool''.

ProfStef next.'
!

basicTypesSymbol
	^ Lesson
title: 'Basic types: Symbols' 
contents: 
'"A Symbol is a special String which is guaranteed to be globally unique. The formal way to
write a Symbol in code is to preceed the string with the # character, such as #''ProfStef'' or
#''This is a Symbol''. For convenience a shortcut is provided if the string starts with a letter or 
underscore and includes only letters, digits, and underscores. So we can also say #ProfStef.

There is one and only one Symbol #ProfStef. There may be several ''ProfStef'' String objects.

(As you try the following code, understand that == returns true if the two objects are the 
SAME)"

''ProfStef'' asSymbol.

#ProfStef asString.

(2 printString) == (2 printString).

(2 printString) asSymbol == (2 printString) asSymbol.


(<DICT> at: #ProfStef) next.'
!

blocks
	^ Lesson
title: 'Blocks' 
contents: 
'"Cascade is cool !! Let''s talk about blocks.

Blocks are anonymous methods that can be stored into variables and executed on demand.

Blocks are delimited by square brackets: []"

[Transcript show: 3 + 4; cr].

"does not open a Transcript because the block is not executed.

Here is a block that adds 2 to its argument (its argument is named x):"

[:x | x + 2].

"We can execute a block by sending it value messages."

[:x | x + 2] value: 5.

[Transcript show: 3 + 4; cr] value.

[:x | x + 2] value: 10.

[:x :y| x + y] value: 3 value: 5.

[ProfStef next] value.'
!

blocksAssignation
	^ Lesson
title: 'Block assignation' 
contents: 
'"Like any other object, blocks can be assigned to a variable
and sent messages later. 

Note that | b | is the declaration of a variable named ''b'' 
and that '':='' assigns a value to a variable.

Select the three lines then Display It:"

| b |
b := [:x | x + 2].
b value: 12.

ProfStef next.'
!

conditionals
	^ Lesson
title: 'Conditionals' 
contents: 
'"Conditionals are just messages sent to Boolean objects"

1 < 2
  ifTrue: [100]
  ifFalse: [42].

"Here the message is ifTrue:ifFalse:

With the Transcript visible, try this:"

3 > 10 
	ifTrue: [Transcript nextPutAll: ''maybe there''''s a bug ....''; cr]
	ifFalse: [Transcript nextPutAll: ''Everything''''s cool: 3 is less than 10''; cr].

3 = 3 ifTrue: [ProfStef next].'.
!

debugger
	^ Lesson
title: 'Debugger' 
contents: '"The Debugger may be the most famous tool of Smalltalk environments. 

The following code will open the debugger. Either hit Go or step through the code 
to the next lesson."

self halt. ProfStef next.'
!

displaying 
	^ Lesson
title: 'Evaluating vs Displaying: Displaying' 
contents: 
'"Now you''re an Evaluate It master!! Let''s talk about displaying results.
It''s an Evaluate It which displays the result next to the expression you''ve selected.
For example, select the text below (or click anywhere on the line) and do one of the following:
    (1) Select the ''Display'' command from the Jade menu;
    (2) Select the ''DIsplay'' command from the context (right-click) menu;
    (3) Press <Ctrl>+<D>; or,
    (4) Click the hourglass button with a little screen in the toolbar (first from left)."

1 + 2.

"Try the following expressions:"

DateAndTime now.

"The result is selected, so you can erase it using the backspace key. Try it!!"

Date today weekDayName.

Time now.

ProfStef next.'
!

evaluatingVsDisplaying
	^Lesson title: 'Evaluating vs Displaying: Evaluating'
		contents: '"Cool !! (I like to say Cooool :) ).
You''ve just executed a Smalltalk expression.
More precisely, you sent the message ''next'' to ProfStef class (it''s me !!).

Note you can run this tutorial again by evaluating: ''ProfStef go''. 
''ProfStef previous'' returns to the previous lesson.

Try to evaluate this expression:"

Transcript show: ''Hello world!!''; cr.

"To see the result select the Transcript tab from the Transcript window, 
then go to the next lesson:"

ProfStef next.'!

gemStoneEnvironment
	^Lesson title: 'GemStone Smalltalk Environment'
		contents: '"Every Smalltalk system is full of objects.
There are strings, numbers, dates, and much more.
You can interact with objects in a much more direct way than is possible 
with other programming languages.

With Jade, you can inspect any object by using various Inspect It commands 
(menus, button, or keyboard shortcut):"

DateAndTime now.

"This shows that a DateAndTime object consists of two internal objects; 
a number of seconds since some base time and a time (January 1, 2001),
and an offset (in seconds) from GMT."

"Code in GemStone Smalltalk is typically written in a System Browser. To open
a System Browser, select the Browse Classes command from the Jade menu."

ProfStef next.'!

inspecting 
	^ Lesson
title: 'Evaluating vs Displaying: Inspecting' 
contents: 
'"Displaying a result gives a simple string representation. Sometimes you have a 
complex object that you want to inspect in more detail. To do so, select the text 
below (or click anywhere on the line) and do one of the following:
    (1) Select the ''Inspect'' command from the Jade menu;
    (2) Select the ''Inspect'' command from the context (right-click) menu;
    (3) Press <Ctrl>+<Q>; or,
    (4) Click the hourglass button with a little magnifying glass in the toolbar (third from left)."

1 + 2.

"Try the following expressions:"

DateAndTime now.

Date today weekDayName.

Time now.

ProfStef next.'
!

instantiation
	^Lesson title: 'Instantiation'
		contents: 
'"Objects are instances of their class. Usually, we send the message #new 
to a class for creating an instance of this class.

For example, let''s create an instance of the class OrderedCollection"

OrderedCollection new
	add: ''Some text'';
	add: 3;
	yourself.

"See the collection we''ve created? An OrderedCollection is like an Array but items 
can be added or removed from it. In most Smalltalks, the size of an Array is fixed."

"If we use a variable to keep track of this object, we''ll be able to do stuff with it."

"The following code must be ran all at one, as the ''aCollection'' variable will 
cease to exist once the execution finishes:"

| aCollection |
aCollection := OrderedCollection new
	add: ''Some text'';
	add: 3;
	yourself.
Transcript show: aCollection; cr.
aCollection remove: 3.
Transcript show: aCollection; cr.
aCollection add: ''Some more text!!''.
Transcript show: aCollection; cr.

	
ProfStef next'!

iterators
	^ Lesson
title: 'Iterators' 
contents: 
'"The message do: is sent to a collection of objects (Array, Dictionary, String, etc), 
evaluating the block for each element.

Here we want to print all the numbers on the Transcript (a console)"

#(11 38 3 -2 10) do: [:each |
     Transcript show: each; cr].

"Some other really nice iterators"

#(11 38 3 -2 10) collect: [:each | each negated].

#(11 38 3 -2 10) collect: [:each | each odd].

#(11 38 3 -2 10) select: [:each | each odd].

#(11 38 3 -2 10) select: [:each | each > 10].

#(11 38 3 -2 10) reject: [:each | each > 10].

#(11 38 3 -2 10) 
     do: [:each | Transcript show: each; nextPut: Character space].
Transcript cr.


(<DICT> 
	select: [:eachGlobal | eachGlobal isClass and: [eachGlobal name = #ProfStef]]) 
	do: [:eachProfStef | eachProfStef next].'
!

loops
	^ Lesson
title: 'Loops' 
contents: 
'"Loops are high-level collection iterators, implemented as regular methods."

"Basic loops:
  to:do:
  to:by:do"

1 to: 10 do:
  [:i | Transcript show: i; cr ].

0 to: 30 by: 3 do: [:i | Transcript show: i; cr].

10 to: 0 by: -2 do: 
    [:i | Transcript show: i; cr].

1 to: 1 do: [:i | ProfStef next].'
!

mathematicalPrecedence
	^ Lesson
title: 'Mathematical precedence'
contents: 
'"Traditional precedence rules from mathematics do not follow in Smalltalk."

2 * 10 + 2.

"Here the message * is sent to 2, which answers 20, then 20 receive the message +

Remember that all messages always follow a simple left-to-right precedence rule, 
WITHOUT EXCEPTIONS."

2 + 2 * 10.

2 + (2 * 10).

8 - 5 / 2.

(8 - 5) / 2.

8 - (5 / 2).

ProfStef next.'
!

messageSyntaxBinary
	^ Lesson
title: 'Message syntax: Binary messages' 
contents: 
'"Binary messages have the following form:
    anObject + anotherObject"

3 * 2.

Date today year = 2019.

false | false.

false | true.

true & true.

true & false.

''key'' -> ''value''.

10 <= 12.

''ab'', ''cd''.

ProfStef next.'
!

messageSyntaxCascade
	^ Lesson
title: 'Message syntax: Cascade' 
contents: 
'"; is the cascade operator. It''s useful to send message to the SAME receiver.
To see this, select the Transcript tab on the Transcript window, then:"

Transcript nextPutAll: ''hello ''.
Transcript nextPutAll: ''Smalltalk''.
Transcript cr.

"is equivalent to:"

Transcript 
	   nextPutAll: ''hello '';
	   nextPutAll: ''Smalltalk'' ;
	   cr.

"To add a single quote character in a string, place two single quote characters:"

Transcript nextPutAll: ''What''''s the deal!!?''; cr.

"Try to go to the next lesson by writing a cascade of two ''next'' messages:"

ProfStef '.
!

messageSyntaxExecutionOrder
	^ Lesson
title: 'Message syntax: Execution order' 
contents: 
'"Unary messages are executed first, then binary messages and finally keyword messages:
    Unary > Binary > Keywords"

2.5 + 3.8 rounded.

3 max: 2 + 2.
  
(1 -> 2) class.

1 -> 2 key: 100.

(1 -> 2 key: 100) class.

"Between messages of similar precedence, expressions are executed from left to right"

-12345 negated printString reverse.

ProfStef next.'
!

reflection
	^ Lesson
title: 'Reflection' 
contents: 
'"You can inspect and change the system at runtime.

Take a look at the source code of the method #and: of the class Boolean:"

(Integer compiledMethodAt: #''bitInvert'') sourceString.

"Or all the methods that send that message (you may want to inspect this):"

ClassOrganizer new allReferencesTo: #''bitInvert''.

"Here''s all the methods I implement:"

ProfStef class selectors.

"Let''s create a new method to go to the next lesson:"

ProfStef class compileMethod: ''goToNextLesson self next'' dictionaries: #() category: ''ProfStef''

"Wow!! I can''t wait to use my new method!!"

ProfStef goToNextLesson.'
!

reflectionContinued
	^ Lesson
title: 'Reflection continued' 
contents: 
'"So cool, isn''t it ?  Before going further, let''s remove this method:"

ProfStef class compiledMethodAt: #goToNextLesson.

ProfStef class removeSelector: #goToNextLesson.

"So now this should fail - try it and hit Terminate in the box that appears."

ProfStef class compiledMethodAt: #goToNextLesson.


"Then move forward:"

ProfStef perform: #next'
!

tableOfContents
	^#(#welcome #evaluatingVsDisplaying #displaying #inspecting #basicTypesNumbers #basicTypesCharacters #basicTypesString #basicTypesSymbol #basicTypesArray #messageSyntaxUnary #messageSyntaxBinary #messageSyntaxKeyword #messageSyntaxExecutionOrder #messageSyntaxExecutionOrderParentheses #mathematicalPrecedence #messageSyntaxCascade #messageSyntaxCascadeShouldNotBeHere #blocks #blocksAssignation #conditionals #loops #iterators #instantiation #reflection #reflectionContinued #gemStoneEnvironment #debugger #theEnd)!

theEnd
	^ Lesson
title: 'The End !!' 
contents: 
'"This tutorial is done. Enjoy programming Smalltalk with GemStone. 

You can run this tutorial again by evaluating: "

ProfStef go.

"See you soon !!"
'
!

welcome
	^ Lesson
title: 'Welcome' 
contents: 
'"Hello!! I''m Professor Stef. 

You must want me to help you learn GemStone Smalltalk.

This window is a ''workspace'' and is a place where you can evaluate code 
and expressions. The first few lessons explore executing code and displaying 
or inspecting the result of executing code.

So let''s go to the first lesson by executing the last line in this workspace. To do 
so, select the line (or click anywhere on the line) and do one of the following:
    (1) Select the ''Execute'' command from the Jade menu;
    (2) Select the ''Execute'' command from the context (right-click) menu;
    (3) Press <Ctrl>+<E>; or,
    (4) Click the hourglass button in the toolbar (second from left)."

ProfStef goto: 2.
ProfStef next.'
! !
!GemStoneSyntaxTutorial categoriesFor: #basicTypesArray!pages!public! !
!GemStoneSyntaxTutorial categoriesFor: #basicTypesCharacters!pages!public! !
!GemStoneSyntaxTutorial categoriesFor: #basicTypesNumbers!pages!public! !
!GemStoneSyntaxTutorial categoriesFor: #basicTypesString!pages!public! !
!GemStoneSyntaxTutorial categoriesFor: #basicTypesSymbol!pages!public! !
!GemStoneSyntaxTutorial categoriesFor: #blocks!pages!public! !
!GemStoneSyntaxTutorial categoriesFor: #blocksAssignation!pages!public! !
!GemStoneSyntaxTutorial categoriesFor: #conditionals!pages!public! !
!GemStoneSyntaxTutorial categoriesFor: #debugger!pages!public! !
!GemStoneSyntaxTutorial categoriesFor: #displaying!pages!public! !
!GemStoneSyntaxTutorial categoriesFor: #evaluatingVsDisplaying!pages!public! !
!GemStoneSyntaxTutorial categoriesFor: #gemStoneEnvironment!pages!public! !
!GemStoneSyntaxTutorial categoriesFor: #inspecting!pages!public! !
!GemStoneSyntaxTutorial categoriesFor: #instantiation!pages!public! !
!GemStoneSyntaxTutorial categoriesFor: #iterators!pages!public! !
!GemStoneSyntaxTutorial categoriesFor: #loops!pages!public! !
!GemStoneSyntaxTutorial categoriesFor: #mathematicalPrecedence!pages!public! !
!GemStoneSyntaxTutorial categoriesFor: #messageSyntaxBinary!pages!public! !
!GemStoneSyntaxTutorial categoriesFor: #messageSyntaxCascade!pages!public! !
!GemStoneSyntaxTutorial categoriesFor: #messageSyntaxExecutionOrder!pages!public! !
!GemStoneSyntaxTutorial categoriesFor: #reflection!pages!public! !
!GemStoneSyntaxTutorial categoriesFor: #reflectionContinued!pages!public! !
!GemStoneSyntaxTutorial categoriesFor: #tableOfContents!contents!public! !
!GemStoneSyntaxTutorial categoriesFor: #theEnd!pages!public! !
!GemStoneSyntaxTutorial categoriesFor: #welcome!pages!public! !


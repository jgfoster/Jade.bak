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

"You can print all 256 characters of the ASCII extended set:"

Character byteCharacterSet.

"Any Unicode character can be instantiated using its hex code point"

Character codePoint: 1F42C.

ProfStef next.'
!

displaying 
	^ Lesson
title: 'Evaluating vs Displaying: Displaying' 
contents: 
'"Now you''re an Evaluate It master !! Let''s talk about displaying results.
It''s an Evaluate It which displays the result next to the expression you''ve selected.
For example, select the text below (or click anywhere on the line) and do one of the following:
    (1) Select the ''Display'' command from the Jade menu;
    (2) Select the ''DIsplay'' command from the context (right-click) menu;
    (3) Press <Ctrl>+<D>; or,
    (4) Click the hourglass button with a little screen in the toolbar (first from left)."

1 + 2.

"Try the following expressions:"

DateAndTime now.

"The result is selected, so you can erase it using the backspace key. Try it !!"

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

Transcript show: ''hello world!!''; cr.

"To see the result select the Transcript tab from the Transcript window, then go to the next lesson:"

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

tableOfContents
	^#(#welcome #evaluatingVsDisplaying #displaying #inspecting #basicTypesNumbers #basicTypesCharacters #basicTypesString #basicTypesSymbol #basicTypesArray #messageSyntaxUnary #messageSyntaxBinary #messageSyntaxKeyword #messageSyntaxExecutionOrder #messageSyntaxExecutionOrderParentheses #mathematicalPrecedence #messageSyntaxCascade #messageSyntaxCascadeShouldNotBeHere #blocks #blocksAssignation #conditionals #loops #iterators #instantiation #reflection #reflectionContinued #dolphinEnvironment #debugger #theEnd)!

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

ProfStef next.'
! !
!GemStoneSyntaxTutorial categoriesFor: #basicTypesCharacters!pages!public! !
!GemStoneSyntaxTutorial categoriesFor: #displaying!pages!public! !
!GemStoneSyntaxTutorial categoriesFor: #evaluatingVsDisplaying!pages!public! !
!GemStoneSyntaxTutorial categoriesFor: #inspecting!pages!public! !
!GemStoneSyntaxTutorial categoriesFor: #tableOfContents!contents!public! !
!GemStoneSyntaxTutorial categoriesFor: #welcome!pages!public! !


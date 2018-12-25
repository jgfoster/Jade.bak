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

tableOfContents
	^#(#welcome #evaluatingVsDisplaying #displaying #basicTypesNumbers #basicTypesCharacters #basicTypesString #basicTypesSymbol #basicTypesArray #messageSyntaxUnary #messageSyntaxBinary #messageSyntaxKeyword #messageSyntaxExecutionOrder #messageSyntaxExecutionOrderParentheses #mathematicalPrecedence #messageSyntaxCascade #messageSyntaxCascadeShouldNotBeHere #blocks #blocksAssignation #conditionals #loops #iterators #instantiation #reflection #reflectionContinued #dolphinEnvironment #debugger #theEnd)!

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
!GemStoneSyntaxTutorial categoriesFor: #tableOfContents!contents!public! !
!GemStoneSyntaxTutorial categoriesFor: #welcome!pages!public! !


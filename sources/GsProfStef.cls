"Filed out from Dolphin Smalltalk 7"!

JadeWorkspace subclass: #GsProfStef
	instanceVariableNames: 'tutorial index'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsProfStef guid: (GUID fromString: '{494f1fe2-2494-4841-961a-12238584c390}')!
GsProfStef comment: ''!
!GsProfStef categoriesForClass!Unclassified! !
!GsProfStef methodsFor!

initialize

	super initialize.
	tutorial := GemStoneSyntaxTutorial new.
	index := 1.
!

lesson

	^tutorial lessonAt: index
!

onViewOpened

	super onViewOpened.
	self updateLesson.
!

updateCaption

	self caption: self lesson title , ' (' , index printString , '/' , tutorial size printString , ')'.
!

updateLesson

	codePane text: self lesson contents.
! !
!GsProfStef categoriesFor: #initialize!public! !
!GsProfStef categoriesFor: #lesson!public! !
!GsProfStef categoriesFor: #onViewOpened!public! !
!GsProfStef categoriesFor: #updateCaption!private!updating! !
!GsProfStef categoriesFor: #updateLesson!public! !

!GsProfStef class methodsFor!

first
	"Move to the first lesson. We post to the input queue to avoid issues
	if the user has chosen to display a result - we don't want this
	inserted in our new contents."

	[self default first] postToInputQueue!

go
	self first.
!

icon
	"Answers an Icon that can be used to represent this class."

	^##(TextTileIcon text: $\x1F393
		color: (Color
				r: 221
				g: 84
				b: 140))
		textcolor: Color white;
		yourself!

next
	"Move to the next lesson. We post to the input queue to avoid issues
	if the user has chosen to display a result - we don't want this
	inserted in our new contents."

	[self default next] postToInputQueue!

previous
	"Move to the previous lesson. We post to the input queue to avoid issues
	if the user has chosen to display a result - we don't want this
	inserted in our new contents."

	[self default previous] postToInputQueue! !
!GsProfStef class categoriesFor: #first!navigation!public! !
!GsProfStef class categoriesFor: #go!navigation!public! !
!GsProfStef class categoriesFor: #icon!constants!public! !
!GsProfStef class categoriesFor: #next!navigation!public! !
!GsProfStef class categoriesFor: #previous!navigation!public! !


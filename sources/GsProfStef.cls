"Filed out from Dolphin Smalltalk 7"!

JadeWorkspace subclass: #GsProfStef
	instanceVariableNames: 'dictionaryName index tutorial'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'current'!
GsProfStef guid: (GUID fromString: '{494f1fe2-2494-4841-961a-12238584c390}')!
GsProfStef comment: ''!
!GsProfStef categoriesForClass!Unclassified! !
!GsProfStef methodsFor!

executeSelectionOrLine

	[
		^super executeSelectionOrLine
	] on: GsClientForwarderSend do: [:ex | 
		ex halt.
	].!

first
	index := 1.
	self updateLesson.
!

initialize

	super initialize.
	tutorial := GemStoneSyntaxTutorial new.
	index := 1.
!

lesson

	^tutorial lessonAt: index
!

next
	index < tutorial size ifTrue: [index := index + 1].
	self updateLesson.
!

onViewDestroyed

	self class clearCurrent.
	super onViewDestroyed.
!

onViewOpened

	super onViewOpened.
	self updateLesson.
	dictionaryName := gciSession serverPerform: #'addProfStef'.
	(gciSession indexOfClientForwarder: self class) == 3 	"This registers the object if it isn't present"
		ifFalse: [self error: 'GsProfStef should be the third client forwarder!!'].
!

previous
	1 < index ifTrue: [index := index - 1].
	self updateLesson.
!

updateCaption

	self caption: self lesson title , ' (' , index printString , '/' , tutorial size printString , ')'.
!

updateLesson

	codePane isModified: false.
	codePane text: self lesson contents.
	self updateCaption.
! !
!GsProfStef categoriesFor: #executeSelectionOrLine!public! !
!GsProfStef categoriesFor: #first!public! !
!GsProfStef categoriesFor: #initialize!public! !
!GsProfStef categoriesFor: #lesson!public! !
!GsProfStef categoriesFor: #next!public! !
!GsProfStef categoriesFor: #onViewDestroyed!public! !
!GsProfStef categoriesFor: #onViewOpened!public! !
!GsProfStef categoriesFor: #previous!public! !
!GsProfStef categoriesFor: #updateCaption!private!updating! !
!GsProfStef categoriesFor: #updateLesson!public! !

!GsProfStef class methodsFor!

clearCurrent

	current := nil.
!

current
	^current!

first
	"Move to the first lesson. We post to the input queue to avoid issues
	if the user has chosen to display a result - we don't want this
	inserted in our new contents."

	[self current first] postToInputQueue.
	^self current gciSession oopIllegal!

go
	self first.
	^true
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

	[self current next] postToInputQueue.
	^self current gciSession oopIllegal!

previous
	"Move to the previous lesson. We post to the input queue to avoid issues
	if the user has chosen to display a result - we don't want this
	inserted in our new contents."

	[self current previous] postToInputQueue.
	^self current gciSession oopIllegal!

showOnSession: aGciSession

	current ifNil: [
		current := super showOnSession: aGciSession.
	] ifNotNil: [
		current gciSession == aGciSession ifFalse: [
			MessageBox warning: 'Can have only one ProfStef open at a time!!'.
		] ifTrue: [
			current view activate.
			current first.
		].
	].! !
!GsProfStef class categoriesFor: #clearCurrent!public! !
!GsProfStef class categoriesFor: #current!public! !
!GsProfStef class categoriesFor: #first!navigation!public! !
!GsProfStef class categoriesFor: #go!navigation!public! !
!GsProfStef class categoriesFor: #icon!constants!public! !
!GsProfStef class categoriesFor: #next!navigation!public! !
!GsProfStef class categoriesFor: #previous!navigation!public! !
!GsProfStef class categoriesFor: #showOnSession:!public! !


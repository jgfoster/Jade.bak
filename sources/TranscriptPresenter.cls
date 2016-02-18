"Filed out from Dolphin Smalltalk 7"!

CodeSourcePresenter subclass: #TranscriptPresenter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TranscriptPresenter guid: (GUID fromString: '{A7807134-2577-4A89-87CD-7070EBF8CD2B}')!
TranscriptPresenter comment: ''!
!TranscriptPresenter categoriesForClass!Unclassified! !
!TranscriptPresenter methodsFor!

cr

	^self nextPutAll: Character lf asString.
!

edit: aBlock

	| transcriptView |
	transcriptView := documentPresenter view.
	[
		transcriptView isReadOnly: false.
		aBlock value.
	] ensure: [
		transcriptView isReadOnly: true.
	].!

editCut

	self edit: [super editCut].
!

editDelete

	self edit: [super editDelete].
!

nextPut: aCharacter

	^self nextPutAll: aCharacter asString.
!

nextPutAll: anObject

	| string transcriptView |
	string := (anObject isKindOf: String)
		ifTrue: [anObject]
		ifFalse: [anObject printString].
	transcriptView := documentPresenter view.
	self edit: [
		transcriptView
			goto: transcriptView textLength + 1;
			insertText: string at: transcriptView caretPosition;
			goto: transcriptView caretPosition + string size;
			yourself.
	].
	"marker to signal not to replace the top-of-stack"
	^gciSession oopIllegal.
!

onViewOpened

	super onViewOpened.
	(gciSession indexOfClientForwarder: self) = 2 	"This registers the object if it isn't present"
		ifFalse: [self error: 'Transcript should be the second client forwarder!!'].
	documentPresenter view 
		backcolor: Color face3d;
		isReadOnly: true;
		yourself.
!

show: aString

	^self nextPutAll: aString.
!

tab

	^self nextPutAll: Character tab asString.
! !
!TranscriptPresenter categoriesFor: #cr!public! !
!TranscriptPresenter categoriesFor: #edit:!public! !
!TranscriptPresenter categoriesFor: #editCut!public! !
!TranscriptPresenter categoriesFor: #editDelete!public! !
!TranscriptPresenter categoriesFor: #nextPut:!public! !
!TranscriptPresenter categoriesFor: #nextPutAll:!public! !
!TranscriptPresenter categoriesFor: #onViewOpened!public! !
!TranscriptPresenter categoriesFor: #show:!public! !
!TranscriptPresenter categoriesFor: #tab!public! !


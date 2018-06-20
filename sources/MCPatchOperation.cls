"Filed out from Dolphin Smalltalk 7"!

GsObject subclass: #MCPatchOperation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCPatchOperation guid: (GUID fromString: '{656d8071-9137-4a7a-9aba-d0273ee9d33c}')!
MCPatchOperation comment: ''!
!MCPatchOperation categoriesForClass!Unclassified! !
!MCPatchOperation methodsFor!

<= aPatchOperation

	^self sortString <= aPatchOperation sortString.
!

className

	self subclassResponsibility.
!

hasEquivalentText

	^false.
!

initialize: aStream session: aGciSession

	| string |
	gciSession := aGciSession.
	string := aStream upTo: Character tab.
	oopType := gciSession oopTypeWithOop: string asNumber.
	self initialize: aStream.
!

modificationMemo

	^''.
!

modificationMemoMarkers

	^#().
!

modificationText

	^''.
!

obsoletionMemo

	^''.
!

obsoletionMemoMarkers

	^#().
!

obsoletionText

	^''.
!

selector

	self subclassResponsibility.
!

sortString

	^self className , '>>' , self selector.
!

typeString

	^self class name copyFrom: 3 to: self class name size.
! !
!MCPatchOperation categoriesFor: #<=!public! !
!MCPatchOperation categoriesFor: #className!public! !
!MCPatchOperation categoriesFor: #hasEquivalentText!public! !
!MCPatchOperation categoriesFor: #initialize:session:!public! !
!MCPatchOperation categoriesFor: #modificationMemo!public! !
!MCPatchOperation categoriesFor: #modificationMemoMarkers!public! !
!MCPatchOperation categoriesFor: #modificationText!public! !
!MCPatchOperation categoriesFor: #obsoletionMemo!public! !
!MCPatchOperation categoriesFor: #obsoletionMemoMarkers!public! !
!MCPatchOperation categoriesFor: #obsoletionText!public! !
!MCPatchOperation categoriesFor: #selector!public! !
!MCPatchOperation categoriesFor: #sortString!public! !
!MCPatchOperation categoriesFor: #typeString!public! !

!MCPatchOperation class methodsFor!

fromStream: aStream session: aGciSession

	| char class |
	char := aStream next.
	aStream next.
	class := 
		char = $A ifTrue: [MCAddition] ifFalse: [
		char = $M ifTrue: [MCModification] ifFalse: [
		char = $R ifTrue: [MCRemoval] ifFalse: [
		self error: 'Invalid MCPatchOperation']]].
	^class new
		initialize: aStream
		session:  aGciSession.
! !
!MCPatchOperation class categoriesFor: #fromStream:session:!public! !


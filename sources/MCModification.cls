"Filed out from Dolphin Smalltalk 7"!

MCPatchOperation subclass: #MCModification
	instanceVariableNames: 'obsoletion obsoletionMemo obsoletionMemoMarkers obsoletionLines modification modificationMemo modificationMemoMarkers modificationLines'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCModification guid: (GUID fromString: '{29f0bdf4-c441-4f8e-bba1-3613f678d6ae}')!
MCModification comment: ''!
!MCModification categoriesForClass!Unclassified! !
!MCModification methodsFor!

bestMatchAfter: anInteger

	| list start altList |
	list := OrderedCollection new.
	anInteger to: obsoletionLines size do: [:i | 
		anInteger to: modificationLines size do: [:j | 
			| k |
			k := self matchCountFromObsoletionLine: i modificationLine: j.
			0 < k ifTrue: [list add: (Array with: i with: j with: k)].
		].
	].
	list isEmpty ifTrue: [^Array with: obsoletionLines size + 1 with: modificationLines size + 1 with: 0].
	start := list first first.
	list do: [:each | 
		start := (start min: (each at: 1)) min: (each at: 2).
	].
	list := list select: [:each | (each at: 1) = start or: [(each at: 2) = start]].
	1 = list size ifTrue: [^list first].
	altList := list select: [:each | (each at: 1) = (each at: 2)].
	1 = altList size ifTrue: [^altList first].
	^list first.
!

className 

	^obsoletion className.
!

computeDifferences

	| start stream |
	obsoletionMemo notNil ifTrue: [^self].
	obsoletionLines := (obsoletion displayMemo subStrings: Character lf) asOrderedCollection.
	obsoletionMemoMarkers := OrderedCollection new.
	modificationLines := (modification displayMemo subStrings: Character lf) asOrderedCollection.
	modificationMemoMarkers := OrderedCollection new.
	start := 1.
	[
		| match |
		match := self matchCountFromObsoletionLine: start modificationLine: start.
		start := start + match.
		start <= (obsoletionLines size max: modificationLines size).
	] whileTrue: [
		| end lastChangedLine firstMatchedLine |
		end := self bestMatchAfter: start.
		lastChangedLine := ((end at: 1) min: (end at: 2)) - 1.
		start to: lastChangedLine do: [:i | 
			obsoletionMemoMarkers add: #'changed' -> i.
			modificationMemoMarkers add: #'changed' -> i.
		].
		firstMatchedLine := (end at: 1) max: (end at: 2).
		(end at: 1) to: firstMatchedLine - 1 do: [:i | 
			obsoletionLines add: '' afterIndex: i - 1.
			modificationMemoMarkers add: #'added' -> i.
		].
		(end at: 2) to: firstMatchedLine - 1 do: [:i | 
			modificationLines add: '' afterIndex: i - 1.
			obsoletionMemoMarkers add: #'removed' -> i.
		].
		start := firstMatchedLine.
	].
	stream := WriteStream on: String new.
	obsoletionLines do: [:each | stream nextPutAll: each; nextPut: Character lf].
	obsoletionMemo := stream contents.
	obsoletionMemoMarkers := obsoletionMemoMarkers asArray.
	stream := WriteStream on: String new.
	modificationLines do: [:each | stream nextPutAll: each; nextPut: Character lf].
	modificationMemo := stream contents.
	modificationMemoMarkers := modificationMemoMarkers asArray.
!

detailsString

	^modification detailsString.
!

firstDifferenceAfter: anInteger

	| end |
	end := obsoletionLines size min: modificationLines size.
	anInteger to: end do: [:i | 
		(obsoletionLines at: i) ~= (modificationLines at: i) ifTrue: [^i].
	].
	^end.
!

firstMatchAfter: anInteger

	| x y |
	x := OrderedCollection new.
	y := OrderedCollection new.
	anInteger to: obsoletionLines size do: [:i | 
		anInteger to: modificationLines size do: [:j | 
			| k |
			k := self matchCountFromObsoletionLine: i modificationLine: j.
			0 < k ifTrue: [x add: (Array with: i with: j with: k)].
		].
	].
	MessageBox notify: 'Sorry, we have not yet implemented this feature!!'.
	SessionManager current pause.
!

hasEquivalentText

	| crlf lf left right |
	crlf := Character cr asString , Character lf asString.
	lf := Character lf asString.
	left := obsoletion displayMemo copyReplaceAll: crlf with: lf.
	right := modification displayMemo copyReplaceAll: crlf with: lf.
	left := left trimBlanks.
	right := right trimBlanks.
	^left = right.
!

initialize: aStream

	obsoletion := MCDefinition fromStream: aStream.
	modification := MCDefinition fromStream: aStream.
!

matchCountFromObsoletionLine: obsInteger modificationLine: modInteger

	| obsOffset modOffset |
	obsOffset := obsInteger.
	modOffset := modInteger.
	[
		obsoletionLines size < obsOffset ifTrue: [^obsOffset - obsInteger].
		modificationLines size < modOffset ifTrue: [^obsOffset - obsInteger].
		(obsoletionLines at: obsOffset) = (modificationLines at: modOffset).
	] whileTrue: [
		obsOffset := obsOffset + 1.
		modOffset := modOffset + 1.
	].
	^obsOffset - obsInteger.
!

modificationMemo

	self computeDifferences.
	^modificationMemo.
!

modificationMemoMarkers

	self computeDifferences.
	^modificationMemoMarkers.

!

modificationSource

	^modification source!

modificationText

	^modification displayText.
!

obsoletionMemo

	self computeDifferences.
	^obsoletionMemo.
!

obsoletionMemoMarkers

	self computeDifferences.
	^obsoletionMemoMarkers.
!

obsoletionSource

	^obsoletion source!

obsoletionText

	^obsoletion displayText.
!

selector

	^modification selector.
! !
!MCModification categoriesFor: #bestMatchAfter:!differences!public! !
!MCModification categoriesFor: #className!public! !
!MCModification categoriesFor: #computeDifferences!differences!public! !
!MCModification categoriesFor: #detailsString!public! !
!MCModification categoriesFor: #firstDifferenceAfter:!differences!public! !
!MCModification categoriesFor: #firstMatchAfter:!differences!public! !
!MCModification categoriesFor: #hasEquivalentText!public! !
!MCModification categoriesFor: #initialize:!public! !
!MCModification categoriesFor: #matchCountFromObsoletionLine:modificationLine:!differences!public! !
!MCModification categoriesFor: #modificationMemo!public! !
!MCModification categoriesFor: #modificationMemoMarkers!differences!public! !
!MCModification categoriesFor: #modificationSource!public! !
!MCModification categoriesFor: #modificationText!public! !
!MCModification categoriesFor: #obsoletionMemo!public! !
!MCModification categoriesFor: #obsoletionMemoMarkers!differences!public! !
!MCModification categoriesFor: #obsoletionSource!public! !
!MCModification categoriesFor: #obsoletionText!public! !
!MCModification categoriesFor: #selector!public! !


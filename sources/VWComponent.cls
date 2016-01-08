"Filed out from Dolphin Smalltalk 7"!

Object subclass: #VWComponent
	instanceVariableNames: 'element name environment category package isPrivate sharedVariables'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
VWComponent guid: (GUID fromString: '{408B2BD9-180C-481F-970B-1EA93BA60CDD}')!
VWComponent comment: ''!
!VWComponent categoriesForClass!Unclassified! !
!VWComponent methodsFor!

addSharedVar: nameString initializer: codeString

	sharedVariables 
		at: nameString 
		put: codeString.
!

category

	^category.
!

environment

	^environment.
!

initialize: anElement

	element := anElement.
	self 
		readCommon;
		readOther;
		yourself.
	element := nil.
!

initializerFor: anArray

	| stream period |
	stream := WriteStream on: String new.
	period := ''.
	anArray do: [:each | 
		stream 
			nextPutAll: period;
			nextPutAll: each;
			yourself.
		period := '.'.
	].
	stream
		nextPutAll: ' := ';
		nextPutAll: (sharedVariables at: anArray last);
		nextPut: $.;
		yourself.
	^stream contents.
!

name

	^name.
!

package

	^package.
!

printOn: aStream

	super printOn: aStream.
	aStream nextPutAll: ': ' , name.
!

readAttributes

	| attributes |
	attributes := element entityAt: #attributes.
	attributes entitiesDo: [:key :value | 
		key = #'package' ifTrue: [
			package := value elements first string.
		] ifFalse: [
			MessageBox notify: 'Sorry, we are not yet prepared to handle ' , key printString , ' (we only support the #package attribute)!!'.
			Keyboard default isShiftDown ifTrue: [self halt].
		].
	].
!

readCommon

	name 		:= (element entityAt: #name		) elements first string.
	environment 	:= (element entityAt: #environment 	) elements first string.
	category 		:= (element entityAt: #category 	) elements first string.
	isPrivate 		:= (element entityAt: #private		) elements first string = 'true'.
	sharedVariables := Dictionary new.
	self readAttributes.
! !
!VWComponent categoriesFor: #addSharedVar:initializer:!public! !
!VWComponent categoriesFor: #category!public! !
!VWComponent categoriesFor: #environment!public! !
!VWComponent categoriesFor: #initialize:!public! !
!VWComponent categoriesFor: #initializerFor:!public! !
!VWComponent categoriesFor: #name!public! !
!VWComponent categoriesFor: #package!public! !
!VWComponent categoriesFor: #printOn:!public! !
!VWComponent categoriesFor: #readAttributes!public! !
!VWComponent categoriesFor: #readCommon!public! !

!VWComponent class methodsFor!

from: anElement

	^self new
		initialize: anElement;
		yourself.
! !
!VWComponent class categoriesFor: #from:!public! !


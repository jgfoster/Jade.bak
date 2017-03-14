"Filed out from Dolphin Smalltalk 7"!

Object subclass: #MCVersionName
	instanceVariableNames: 'name isLoaded isModified'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCVersionName guid: (GUID fromString: '{f4afbc1c-2c99-4741-8fba-5c571f07ca36}')!
MCVersionName comment: ''!
!MCVersionName categoriesForClass!Unclassified! !
!MCVersionName methodsFor!

<= anMCVersionName

	^self name <= anMCVersionName name.
!

initialize

	name := ''.
	isLoaded := false.
	isModified := false.
!

isLoaded
	^isLoaded!

isLoaded: anObject
	isLoaded := anObject!

isModified
	^isModified!

isModified: anObject
	isModified := anObject!

name
	^name!

name: anObject
	name := anObject!

printOn: aStream

	| string |
	string := (name endsWith: '.mcz')
		ifTrue: [name copyFrom: 1 to: name size - 4] 
		ifFalse: [name].
	aStream nextPutAll: string.
! !
!MCVersionName categoriesFor: #<=!public! !
!MCVersionName categoriesFor: #initialize!public! !
!MCVersionName categoriesFor: #isLoaded!accessing!public! !
!MCVersionName categoriesFor: #isLoaded:!accessing!public! !
!MCVersionName categoriesFor: #isModified!accessing!public! !
!MCVersionName categoriesFor: #isModified:!accessing!public! !
!MCVersionName categoriesFor: #name!accessing!public! !
!MCVersionName categoriesFor: #name:!accessing!public! !
!MCVersionName categoriesFor: #printOn:!public! !

!MCVersionName class methodsFor!

new

	^super new
		initialize;
		yourself.
! !
!MCVersionName class categoriesFor: #new!public! !


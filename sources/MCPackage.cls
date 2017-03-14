"Filed out from Dolphin Smalltalk 7"!

GsObject subclass: #MCPackage
	instanceVariableNames: 'repository versionNames loaded'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCPackage guid: (GUID fromString: '{c1da7939-7500-412e-b95b-88c59221ceaf}')!
MCPackage comment: ''!
!MCPackage categoriesForClass!Unclassified! !
!MCPackage methodsFor!

infoForVersion: aString

	^repository versionInfoForPackageNamed: self name version: aString.
!

isLoaded

	^loaded notNil.
!

isModified

	^loaded notNil and: [loaded value].
!

loaded: anObject

	loaded := anObject.
!

loadedEditionName

	^loaded notNil 
		ifTrue: [loaded key]
		ifFalse: [''].
!

name: aString 
	name := aString!

repository
	^repository!

repository: anMCRepository

	repository := anMCRepository.
	gciSession := anMCRepository gciSession.
!

versionNames
	^versionNames!

versionNames: aList

	| list |
	list := aList asArray.
	list := list collect: [:each1 | 
		| pieces |
		pieces := each1 subStrings: $..
		pieces = #('current') ifTrue: [
			0 -> 'current'
		] ifFalse: [
			((pieces at: pieces size - 1) collect: [:each2 | each2 isDigit ifTrue: [each2] ifFalse: [$0]]) asNumber -> each1.
		].
	].
	list := list asSortedCollection.	"at first it seemed that the server provided them in order but that seems to not be consistent"
	list := list collect: [:each | MCVersionName new name: each value].
	versionNames := list.! !
!MCPackage categoriesFor: #infoForVersion:!public! !
!MCPackage categoriesFor: #isLoaded!public! !
!MCPackage categoriesFor: #isModified!public! !
!MCPackage categoriesFor: #loaded:!public! !
!MCPackage categoriesFor: #loadedEditionName!public! !
!MCPackage categoriesFor: #name:!public! !
!MCPackage categoriesFor: #repository!accessing!public! !
!MCPackage categoriesFor: #repository:!accessing!public! !
!MCPackage categoriesFor: #versionNames!accessing!public! !
!MCPackage categoriesFor: #versionNames:!accessing!public! !


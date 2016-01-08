"Filed out from Dolphin Smalltalk 7"!

GsObject subclass: #GsMethod
	instanceVariableNames: 'gsBehavior category source instVarReferences package isDirty symbolDictionaryName'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsMethod guid: (GUID fromString: '{B1E7FC81-607C-4011-8820-B81AAA95A565}')!
GsMethod comment: ''!
!GsMethod categoriesForClass!Unclassified! !
!GsMethod methodsFor!

<= aGsMethod

	^self printString <= aGsMethod printString.
!

= aGsMethod

	^self class == aGsMethod class and: [
		self gsBehavior = aGsMethod gsBehavior and: [
		self name = aGsMethod name]].
!

category
	^category!

category: anObject
	category := anObject!

changeToCategory: aString

	| string |
	string := '| gsBehavior |
	gsBehavior := ' , gsBehavior , '.
	(gsBehavior categoryNames includes: #' , aString printString , ') ifFalse: [gsBehavior addCategory: ' , aString printString , '].
	gsBehavior
		moveMethod: #' , name printString , ' 
		toCategory: ' , aString printString.
	gciSession executeString: string.
	self category: aString.
	self isDirty: true.
!

changeToPackage: aPackage

	package removeMethod: self.
	aPackage addMethod: self.
!

codeForObject

	^'(' , gsBehavior codeForObject , ' compiledMethodAt: #' , name , ')'.
!

fileOutTo: aStream

	aStream 
		nextPutAll: 'category: ';
		nextPutAll: category printString; lf;
		nextPutAll: gsBehavior methodPrefix;
		nextPutAll: gsBehavior thisClass name; lf;
		nextPutAll: self source replaceCrLfWithLf trimBlanks; lf;
		nextPutAll: '%'; lf.
!

gsBehavior

	^gsBehavior!

gsBehavior: aGsBehavior

	gsBehavior := aGsBehavior.
!

hash

	^self name hash.
!

initialize: xmlElement

	self error: 'use #initializeXML:'.
!

initializeXML: xmlElement

	super initializeXML: xmlElement.
	name := (xmlElement getElementsByTagName: 'selector') first text.
	category := xmlElement getAttribute: 'category'.
	instVarReferences := (xmlElement getElementsByTagName: 'instVar') collect: [:each | each text].
	isDirty := false.
!

instVarReferences
	^instVarReferences!

instVarReferences: anObject
	instVarReferences := anObject!

isDirty
	^isDirty == true.
!

isDirty: anObject
	isDirty := anObject!

package
	^package!

package: anObject
	package := anObject!

printOn: aStream

	gsBehavior printOn: aStream.
	aStream
		nextPutAll: '>>';
		nextPutAll: name.
!

remove

	| string |
	string := gsBehavior codeForObject , ' removeSelector: ' , name printString , ' ifAbsent: []'.
	gciSession executeString: string.
	gsBehavior removeMethod: self.
	package removeMethod: self.
!

source

	| gsCode result |
	source isNil ifTrue: [
		gsCode := self codeForObject , ' sourceString'.
		(result := gciSession executeString: gsCode) isNil ifTrue: [^self].
		source := result.
	].
	^source.
!

source: anObject
	source := anObject!

symbolDictionaryName

	^symbolDictionaryName.
!

symbolDictionaryName: aString

	symbolDictionaryName := aString.
! !
!GsMethod categoriesFor: #<=!public! !
!GsMethod categoriesFor: #=!public! !
!GsMethod categoriesFor: #category!accessing!public! !
!GsMethod categoriesFor: #category:!accessing!public! !
!GsMethod categoriesFor: #changeToCategory:!public! !
!GsMethod categoriesFor: #changeToPackage:!public! !
!GsMethod categoriesFor: #codeForObject!public! !
!GsMethod categoriesFor: #fileOutTo:!public! !
!GsMethod categoriesFor: #gsBehavior!public! !
!GsMethod categoriesFor: #gsBehavior:!public! !
!GsMethod categoriesFor: #hash!public! !
!GsMethod categoriesFor: #initialize:!private! !
!GsMethod categoriesFor: #initializeXML:!private! !
!GsMethod categoriesFor: #instVarReferences!accessing!public! !
!GsMethod categoriesFor: #instVarReferences:!accessing!public! !
!GsMethod categoriesFor: #isDirty!accessing!public! !
!GsMethod categoriesFor: #isDirty:!accessing!public! !
!GsMethod categoriesFor: #package!accessing!public! !
!GsMethod categoriesFor: #package:!accessing!public! !
!GsMethod categoriesFor: #printOn:!public! !
!GsMethod categoriesFor: #remove!public! !
!GsMethod categoriesFor: #source!accessing!public! !
!GsMethod categoriesFor: #source:!accessing!public! !
!GsMethod categoriesFor: #symbolDictionaryName!public! !
!GsMethod categoriesFor: #symbolDictionaryName:!public! !

!GsMethod class methodsFor!

codeToGetMethod 

^':gsClass :aSelector :aStream |
	| compiledMethod |
	compiledMethod := gsClass compiledMethodAt: aSelector.
	aStream 
		nextPut: Character cr; nextPut: Character lf; tab;
		nextPutAll: ''<method oop='';
		nextPutAll: compiledMethod asOop printString printString;
		nextPutAll: '' category='';
		nextPutAll: (gsClass categoryOfSelector: aSelector) asString printString;
		nextPutAll: '' >''.
	compiledMethod instVarsAccessed asSortedCollection do: [:each |
		aStream
			nextPutAll: ''<instVar>'';
			nextPutAll: each;
			nextPutAll: ''</instVar>''.
	].
	aStream
		nextPutAll: ''<selector'';
		nextPutAll: '' ><'';
		nextPutAll: ''!!['';
		nextPutAll: ''CDATA'';
		nextPutAll: ''['';
		nextPutAll: aSelector;
		nextPutAll: '']'';
		nextPutAll: '']'';
		nextPutAll: ''></selector></method>''.
'.
! !
!GsMethod class categoriesFor: #codeToGetMethod!public! !


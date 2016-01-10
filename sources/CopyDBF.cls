"Filed out from Dolphin Smalltalk 7"!

GsShellCommand subclass: #CopyDBF
	instanceVariableNames: 'sourceNRS destinationNRS'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CopyDBF guid: (GUID fromString: '{99DF6986-C176-4009-9E4E-2EDEBF810C98}')!
CopyDBF comment: ''!
!CopyDBF categoriesForClass!Unclassified! !
!CopyDBF methodsFor!

addArgumentStringTo: aStream 

	super addArgumentStringTo: aStream.
	sourceNRS notNil ifTrue: [
		aStream space; nextPutAll: '"' , sourceNRS printString , '"'.
	].
	destinationNRS notNil ifTrue: [
		aStream space; nextPutAll: '"' , destinationNRS printString , '"'.
	].
!

commandName

	^'copydbf'.
!

destinationNRS: aGsNRS

	(aGsNRS isKindOf: GsNRS) ifFalse: [self error: 'Invalid parameter type'].
	destinationNRS := aGsNRS!

information
	"Information only. When this option si present without <destinationNRS>, 
	information about <sourceNRS> is printed without performing a file copy.
	This option is ignored if <destinationNRS> is present."

	arguments add: '-i'.
	^self result.
!

sourceNRS: aGsNRS

	(aGsNRS isKindOf: GsNRS) ifFalse: [self error: 'Invalid parameter type'].
	sourceNRS := aGsNRS! !
!CopyDBF categoriesFor: #addArgumentStringTo:!public! !
!CopyDBF categoriesFor: #commandName!public! !
!CopyDBF categoriesFor: #destinationNRS:!accessing!public! !
!CopyDBF categoriesFor: #information!public! !
!CopyDBF categoriesFor: #sourceNRS:!accessing!public! !

!CopyDBF class methodsFor!

exeName

	^'copydbf.exe'.
!

sample1

	^CopyDBF new
		sourceNRS: 		(GsNRS body: 'c:\gemstone\614\data\extent0.dbf');
		destinationNRS: 	(GsNRS body: 'c:\gemstone\614\data\myExtent0.dbf');
		result.
!

sample2
	^(CopyDBF new)
		sourceNRS: ((GsNRS new)
					node: 'localhost';
					body: 'c:\gemstone\614\data\extent0.dbf';
					yourself);
		information! !
!CopyDBF class categoriesFor: #exeName!public! !
!CopyDBF class categoriesFor: #sample1!public! !
!CopyDBF class categoriesFor: #sample2!public! !


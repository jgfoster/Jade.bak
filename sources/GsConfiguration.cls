"Filed out from Dolphin Smalltalk 7"!

Object subclass: #GsConfiguration
	instanceVariableNames: 'path dictionary'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsConfiguration guid: (GUID fromString: '{CD6CC3A9-D9FA-4F8E-A4AE-200573A63366}')!
GsConfiguration comment: ''!
!GsConfiguration categoriesForClass!Unclassified! !
!GsConfiguration methodsFor!

dbfExtentNames: aString

	dictionary 
		at: 'DBF_EXTENT_NAMES'
		put: aString.

!

defaultPath

	^GsEnvironment gemstone , '/data/myConfig.conf'.
!

extentNames: aString

	dictionary 
		at: 'DBF_EXTENT_NAMES'
		put: aString.

!

initialize

	path := self defaultPath.
	dictionary := Dictionary new
		at: 'STN_TRAN_FULL_LOGGING'			put: 'FALSE';
		at: 'STN_TRAN_LOG_DIRECTORIES'		put: '$GEMSTONE/data/, $GEMSTONE/data/';
		at: 'STN_TRAN_LOG_SIZES'					put: '10, 10';
		yourself.
	self
		dbfExtentNames: '$GEMSTONE/data/myExtent0.dbf';
		yourself.
!

path
	^path!

path: anObject
	path := anObject!

stnTranLogPrefix: aString

	dictionary 
		at: 'STN_TRAN_LOG_PREFIX'
		put: aString.
!

write

	| stream |
	stream := GsFile openWriteOnServer: path.
	[
		dictionary keys asSortedCollection do: [:each | 
			stream
				nextPutAll: each;
				nextPutAll: ' = ';
				nextPutAll: (dictionary at: each);
				nextPutAll: ';';
				lf.
		].
	] ensure: [
		stream close.
	].
! !
!GsConfiguration categoriesFor: #dbfExtentNames:!public! !
!GsConfiguration categoriesFor: #defaultPath!public! !
!GsConfiguration categoriesFor: #extentNames:!public! !
!GsConfiguration categoriesFor: #initialize!public! !
!GsConfiguration categoriesFor: #path!accessing!public! !
!GsConfiguration categoriesFor: #path:!accessing!public! !
!GsConfiguration categoriesFor: #stnTranLogPrefix:!public! !
!GsConfiguration categoriesFor: #write!public! !

!GsConfiguration class methodsFor!

emptyExtentPath

	^GsEnvironment gemstone , '\bin\extent0.dbf'.
!

new

	^super new
		initialize;
		yourself.
! !
!GsConfiguration class categoriesFor: #emptyExtentPath!public! !
!GsConfiguration class categoriesFor: #new!public! !


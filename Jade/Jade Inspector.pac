| package |
package := Package name: 'Jade Inspector'.
package paxVersion: 1;
	basicComment: ''.

package basicPackageVersion: '0.015'.


package classNames
	add: #JadeInspector;
	yourself.

package methodNames
	add: #JadeServer -> #inspect:;
	add: #JadeServer -> #inspectDictionary:on:;
	add: #JadeServer -> #inspectNamedInstanceVariablesOf:on:;
	add: #JadeServer -> #printStringTo500:;
	add: #JadeServer64bit3x -> #inspect:;
	add: #JadeServer64bit3x -> #inspectNamedInstanceVariablesOf:on:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\Object Arts\Dolphin\MVP\Models\List\Dolphin List Models';
	add: '..\Object Arts\Dolphin\MVP\Presenters\List\Dolphin List Presenter';
	add: '..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: 'GemStone Session';
	add: 'Jade UI Base';
	yourself).

package!

"Class Definitions"!

JadeTextDocument subclass: #JadeInspector
	instanceVariableNames: 'object instVarListPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!JadeServer methodsFor!

inspect: anObject

	| stream string |
	(stream := WriteStream on: String new)
		nextPutAll: anObject class name; tab;
		yourself.
	(self oopOf: anObject) printOn: stream.
	stream lf.
	(anObject isKindOf: Dictionary superclass) ifTrue: [^self inspectDictionary: anObject on: stream].
	self inspectNamedInstanceVariablesOf: anObject on: stream.
	anObject class format > 0 ifTrue: [
		1 to: (anObject _basicSize min: 100) do: [:i | 
			i printOn: stream.
			stream tab.
			(self oopOf: (anObject _at: i)) printOn: stream.
			stream lf.
		].
	].
	(string := anObject printString) size > 5000 ifTrue: [string := (string copyFrom: 1 to: 5000) , '...'].
	^stream 
		nextPutAll: string; 
		contents.
!

inspectDictionary: aDictionary on: aStream

	| keys keyDict |
	keys := aDictionary keys.
	keyDict := Dictionary new.
	keys do: [:each | 
		| key |
		key := each printString , '~' , (self oopOf: each) printString.
		key := key collect: [:char | char codePoint < 32 ifTrue: [$?] ifFalse: [char]].
		keyDict
			at: key
			put: each.
	].
	keys size printOn: aStream.
	aStream lf.
	keyDict keys asSortedCollection do: [:each | 
		| index keyString key value valueString |
		index := each findLast: [:char | char = $~].
		keyString := each copyFrom: 1 to: index - 1.
		key := keyDict at: each.
		value := aDictionary at: key. 
		valueString := value printString.
		valueString := valueString copyFrom: 1 to: (valueString size min: 10).
		valueString := valueString collect: [:char | char codePoint < 32 ifTrue: [$?] ifFalse: [char]].
		aStream nextPutAll: keyString , '->' , valueString; tab.
		(self oopOf: value) printOn: aStream.
		aStream lf.
	].
	^aStream 
		lf; 
		contents.
!

inspectNamedInstanceVariablesOf: anObject on: aStream

	| list size |
	list := anObject class allInstVarNames.
	size := list size.
	anObject class format > 0 ifTrue: [
		size := size + (anObject _basicSize min: 100).
	].
	size printOn: aStream.
	aStream lf.
	1 to: list size do: [:i | 
		aStream nextPutAll: (list at: i); tab.
		(self oopOf: (anObject instVarAt: i)) printOn: aStream.
		aStream lf.
	].
!

printStringTo500: anObject

	| string |
	(string := anObject printString) size > 500 ifTrue: [string := (string copyFrom: 1 to: 500) , '...'].
	^string.
! !
!JadeServer categoriesFor: #inspect:!Inspector!public! !
!JadeServer categoriesFor: #inspectDictionary:on:!Inspector!public! !
!JadeServer categoriesFor: #inspectNamedInstanceVariablesOf:on:!Inspector!public! !
!JadeServer categoriesFor: #printStringTo500:!Inspector!public! !

!JadeServer64bit3x methodsFor!

inspect: anObject

	| stream list dynamic string size |
	(stream := WriteStream on: String new)
		nextPutAll: anObject class name; tab;
		yourself.
	(self oopOf: anObject) printOn: stream.
	stream lf.
	(anObject isKindOf: Dictionary superclass) ifTrue: [^self inspectDictionary: anObject on: stream].
	list := anObject class allInstVarNames.
	dynamic := anObject dynamicInstanceVariables.
	size := list size + dynamic size.
	anObject class format > 0 ifTrue: [
		size := size + (anObject _basicSize min: 100).
	].
	size printOn: stream.
	stream lf.
	1 to: list size do: [:i | 
		stream nextPutAll: (list at: i); tab.
		(self oopOf: (anObject instVarAt: i)) printOn: stream.
		stream lf.
	].
	1 to: dynamic size do: [:i | 
		stream nextPutAll: (dynamic at: i); tab.
		(self oopOf: (anObject dynamicInstVarAt: (dynamic at: i))) printOn: stream.
		stream lf.
	].
	anObject class format > 0 ifTrue: [
		1 to: (anObject _basicSize min: 100) do: [:i | 
			i printOn: stream.
			stream tab.
			(self oopOf: (anObject _basicAt: i)) printOn: stream.
			stream lf.
		].
	].
	(string := anObject printString) size > 5000 ifTrue: [string := (string copyFrom: 1 to: 5000) , '...'].
	^stream 
		nextPutAll: string; 
		contents.
!

inspectNamedInstanceVariablesOf: anObject on: aStream

	| list dynamic size |
	list := anObject class allInstVarNames.
	dynamic := anObject dynamicInstanceVariables.
	size := list size + dynamic size.
	anObject class format > 0 ifTrue: [
		size := size + (anObject _basicSize min: 100).
	].
	size printOn: aStream.
	aStream lf.
	1 to: list size do: [:i | 
		aStream nextPutAll: (list at: i); tab.
		(self oopOf: (anObject instVarAt: i)) printOn: aStream.
		aStream lf.
	].
	1 to: dynamic size do: [:i | 
		aStream nextPutAll: (dynamic at: i); tab.
		(self oopOf: (anObject dynamicInstVarAt: (dynamic at: i))) printOn: aStream.
		aStream lf.
	].
! !
!JadeServer64bit3x categoriesFor: #inspect:!Inspector!public!Transcript! !
!JadeServer64bit3x categoriesFor: #inspectNamedInstanceVariablesOf:on:!Inspector!public!Transcript! !

"End of package definition"!

"Source Globals"!

"Classes"!

JadeInspector guid: (GUID fromString: '{7CADE1F1-0352-4FF0-A34D-888A4DE0CD14}')!
JadeInspector comment: ''!
!JadeInspector categoriesForClass!Unclassified! !
!JadeInspector methodsFor!

contextObject

	^object.
!

createComponents

	super createComponents.
	instVarListPresenter := self add: ListPresenter new name: 'instVarList'.
!

createSchematicWiring

	super createSchematicWiring.
	instVarListPresenter when: #selectionChanged send: #selectedInstVar to: self.
	instVarListPresenter when: #actionPerformed send: #inspectInstVar to: self.
!

displayObject

	| result stream className oop size instVarNames |
	result := gciSession 
		serverPerform: #'inspect:' 
		with: object.
	(result isKindOf: String) ifFalse: [self error: 'Result of #inspect: on ' , object printString , ' should be a String'].
	stream := ReadStream on: result.
	className := stream upTo: Character tab.
	self setLastGsShape: className.
	oop := stream nextLine.
	size := stream nextLine asNumber.
	instVarNames := OrderedCollection with: '-.asOop' -> oop.
	size timesRepeat: [
		| name |
		name := '-' , (stream upTo: Character tab).
		oop := stream upTo: Character lf.
		instVarNames add: (name -> (gciSession oopTypeWithOop: oop asNumber)).
	].
	instVarNames addFirst: 'self' -> stream upToEnd.
	instVarListPresenter 
		list: instVarNames;
		selection: instVarNames first;
		yourself.
	self view caption: className.!

inspectInstVar

	JadeInspector showOn: gciSession -> instVarListPresenter selection value.
!

isModified

	^false.
!

on: anAssociation	"gciSession -> anObject (often a GsObject, but could be an Integer, String, etc."

	object := anAssociation value.
	super on: anAssociation key.
!

onViewOpened

	super onViewOpened.
	super model: object.

	(object isKindOf: String) ifTrue: [
		documentPresenter value: object.
		codePane setDocumentPresenterWith: (self registry getClass: 'String').
		self view caption: 'String'.
		^self.
	].
	(object isKindOf: ByteArray) ifTrue: [
		documentPresenter value: object printString.
		codePane setDocumentPresenterWith: (self registry getClass: 'ByteArray').
		self view caption: object class name.
		^self.
	].
	(object isKindOf: Integer) ifTrue: [
		documentPresenter value: object printString.
		codePane setDocumentPresenterWith: (self registry getClass: object class name).
		self view caption: 'Integer'.
		^self.
	].
	object isNil ifTrue: [
		documentPresenter value: object printString.
		codePane setDocumentPresenterWith: (self registry getClass: 'UndefinedObject').
		self view caption: 'UndefinedObject'.
		^self.
	].
	(object isKindOf: Boolean) ifTrue: [
		documentPresenter value: object printString.
		codePane setDocumentPresenterWith: (self registry getClass: object class name).
		self view caption: 'Boolean'.
		^self.
	]. 
	(object isKindOf: Character) ifTrue: [
		documentPresenter value: object printString.
		codePane setDocumentPresenterWith: (self registry getClass: object class name).
		self view caption: 'Character'.
		^self.
	]. 
	(object isKindOf: Float) ifTrue: [
		documentPresenter value: object printString.
		codePane setDocumentPresenterWith: (self registry getClass: object class name).
		self view caption: 'Float'.
		^self.
	].
	self displayObject.
!

selectedInstVar

	| string |
	instVarListPresenter selection key = 'self' ifTrue: [
		string := instVarListPresenter selection value.
		documentPresenter value: string.
		^self.
	].
	instVarListPresenter selection key = '-.asOop' ifTrue: [
		documentPresenter value: instVarListPresenter selection value.
		^self.
	].
	string := gciSession
		serverPerform: #'printStringTo500:' 
		with: instVarListPresenter selection value.
	documentPresenter value: string.
!

setLastGsShape: className

	codePane setDocumentPresenterWith: (self registry getClass: className)! !
!JadeInspector categoriesFor: #contextObject!public! !
!JadeInspector categoriesFor: #createComponents!accessing!public! !
!JadeInspector categoriesFor: #createSchematicWiring!accessing!public! !
!JadeInspector categoriesFor: #displayObject!accessing!public! !
!JadeInspector categoriesFor: #inspectInstVar!accessing!public! !
!JadeInspector categoriesFor: #isModified!public! !
!JadeInspector categoriesFor: #on:!public! !
!JadeInspector categoriesFor: #onViewOpened!accessing!public! !
!JadeInspector categoriesFor: #selectedInstVar!accessing!public! !
!JadeInspector categoriesFor: #setLastGsShape:!accessing!public! !

!JadeInspector class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  801 601 551 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 0 32 234 256 98 4 410 8 ##(Smalltalk.ReferenceView)  98 14 0 416 98 2 8 1140850688 131073 656 0 482 8 4278190080 0 7 0 0 0 656 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.JadeCodePresenter)  8 #resource_Default_view 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 393 1 530 377 485 656 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 196 0 0 0 0 0 0 0 128 1 0 0 242 0 0 0] 608 530 193 193 0 27 8 'codePane' 410 8 ##(Smalltalk.ListBox)  98 17 0 416 98 2 8 1144062209 1025 1072 590662 2 ##(Smalltalk.ListModel)  202 208 608 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  482 8 4278190080 0 7 0 0 0 1072 0 8 4294902521 459270 ##(Smalltalk.Message)  8 #key 98 0 608 32 834 202 208 98 2 898 928 98 2 530 1 1 530 375 485 1072 898 8 #horizontalExtent: 98 1 1 1072 994 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 187 0 0 0 242 0 0 0] 98 0 1040 0 27 8 'instVarList' 0 461638 4 ##(Smalltalk.MenuBar)  0 16 98 4 265030 4 ##(Smalltalk.Menu)  0 16 98 7 984134 2 ##(Smalltalk.CommandMenuItem)  1 1180998 4 ##(Smalltalk.CommandDescription)  8 #fileNew 8 '&New Workspace' 9373 1 0 0 0 1666 1 1698 8 #fileOpen 8 '&Open Workspace...' 9375 1 0 0 0 1666 1 1698 8 #fileSave 8 '&Save' 9383 1 0 0 0 1666 1 1698 8 #fileSaveAs 8 'Save &As...' 1 1 0 0 0 1666 1 1698 8 #fileRevert 8 '&Revert' 1025 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem)  4097 1666 1 1698 8 #exit 8 'E&xit Jade' 17639 1 0 0 0 8 '&File' 0 1 0 0 20835 0 0 1618 0 16 98 15 1666 1 1698 8 #undo 8 '&Undo' 9397 1 0 0 0 1666 1 1698 8 #redo 8 'R&edo' 9395 1 0 0 0 2018 4097 1666 1 1698 8 #editCut 8 'Cu&t' 9393 1 0 0 0 1666 1 1698 8 #editCopy 8 '&Copy' 9351 1 0 0 0 1666 1 1698 8 #editPaste 8 '&Paste' 9389 1 0 0 0 1666 1 1698 8 #editSelectAll 8 'Select &All' 9347 1 0 0 0 1666 1 1698 8 #editDelete 8 '&Delete' 1629 1 0 0 0 2018 4097 1666 1 1698 8 #editFind 8 '&Find...' 9357 1 0 0 0 1666 1 1698 8 #editFindNext 8 'Find &Next' 9359 1 0 0 0 1666 1 1698 8 #editReplace 8 '&Replace...' 9361 1 0 0 0 2018 4097 1666 1 1698 8 #addQuotesToSelection 8 'Add &Quotes' 1 1 0 0 0 1666 1 1698 8 #removeQuotesFromSelection 8 'Re&move Quotes' 1 1 0 0 0 8 '&Edit' 0 1 0 0 20861 0 0 1618 0 16 98 9 1666 1 1698 8 #abortTransaction 8 '&Abort Transaction' 1 1 0 0 0 1666 1 1698 8 #commitTransaction 8 '&Commit Transaction' 1 1 0 0 0 2018 4097 1666 1 1698 8 #jadeInspect 8 '&Inspect' 9379 1 0 0 0 1666 1 1698 8 #jadeDisplay 8 '&Display' 9353 1 0 0 0 1666 1 1698 8 #jadeExecute 8 '&Execute' 9355 1 0 0 0 1666 1 1698 8 #fileIn 8 'Fi&le In' 1 1 0 0 0 2018 4097 1666 1 1698 8 #jadeBrowseClasses 8 '&Browse Classes' 9349 1 0 0 0 8 '&Jade' 0 1 0 0 20877 0 0 1618 0 16 98 1 1666 1 1698 8 #aboutJade 8 '&About Jade' 1 1 0 0 0 8 '&Help' 0 1 0 0 20881 0 0 8 '' 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 834 202 208 98 3 898 928 98 2 530 2879 21 530 801 601 416 898 8 #text: 98 1 8 'Jade Object Inspector' 416 898 8 #updateMenuBar 608 416 994 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 47 7 0 0 54 1 0 0] 98 3 1072 410 8 ##(Smalltalk.Splitter)  98 12 0 416 98 2 8 1140850688 1 3904 0 482 8 4278190080 0 519 0 0 0 3904 834 202 208 98 1 898 928 98 2 530 375 1 530 19 485 3904 994 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 187 0 0 0 0 0 0 0 196 0 0 0 242 0 0 0] 98 0 1040 0 27 656 1040 0 27 )! !
!JadeInspector class categoriesFor: #resource_Default_view!public!resources-views! !

"Binary Globals"!


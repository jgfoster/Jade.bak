"Filed out from Dolphin Smalltalk 7"!

JadeTextDocument subclass: #JadeCodeBrowser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeCodeBrowser guid: (GUID fromString: '{9FC19CAC-6913-4452-814E-19335EA63B9D}')!
JadeCodeBrowser comment: ''!
!JadeCodeBrowser categoriesForClass!Unclassified! !
!JadeCodeBrowser methodsFor!

browseMethodImplementors

	self browseMethodImplementorsOf: self currentMethodSelector.
!

browseMethodImplementorsOf

	| selector gsCode result list |
	(selector := Prompter prompt: 'Enter selector:') isNil ifTrue: [^self].
	selector := selector reject: [:each | each = Character space].
	(selector includes: $*) ifFalse: [
		self browseMethodImplementorsOf: selector.
		^self.
	].
	list := (selector subStrings: $*) asOrderedCollection collect: [:each | each asUppercase].
	list size - 1 to: 1 do: [:i | list add: $* afterIndex: i].
	selector last = $* ifTrue: [list addLast: $*].
	gsCode := '| stream list classOrganizer |
stream := WriteStream on: String new.
list := ((AllUsers userWithId: #SymbolUser ifAbsent: [AllUsers userWithId: #DataCurator]) resolveSymbol: #AllSymbols) value select: [:each |each asUppercase matchPattern: ' , list asArray printString , '].
classOrganizer := ClassOrganizer new.
list := list select: [:each | (classOrganizer implementorsOf: each) notEmpty].
list := list asSortedCollection.
list do: [:each | stream nextPutAll: each; nextPut: Character lf; yourself].
stream contents'.
	(result := gciSession executeString: gsCode) isNil ifTrue: [^self].
	list := result subStrings: Character lf.
	(selector := ChoicePrompter choices: list) isNil ifTrue: [^self].
	self browseMethodImplementorsOf: selector.
!

browseMethodImplementorsOf: aString

	self 
		browseMethodsWhere: 'implementorsOf: #' , aString printString
		searchFor: nil
		caption: 'implementing #' , aString printString.
!

browseMethodSenders

	self browseMethodSendersOf: self currentMethodSelector.
!

browseMethodSendersOf

	| selector gsCode result list |
	(selector := Prompter prompt: 'Enter selector:') isNil ifTrue: [^self].
	selector := selector reject: [:each | each = Character space].
	(selector includes: $*) ifFalse: [
		self browseMethodSendersOf: selector.
		^self.
	].
	list := (selector subStrings: $*) asOrderedCollection collect: [:each | each asUppercase].
	list size - 1 to: 1 do: [:i | list add: $* afterIndex: i].
	selector last = $* ifTrue: [list addLast: $*].
	gsCode := '| stream list classOrganizer |
stream := WriteStream on: String new.
list := AllSymbols select: [:each |each asUppercase matchPattern: ' , list asArray printString , '].
classOrganizer := ClassOrganizer new.
list := list select: [:each | (classOrganizer sendersOf: each) notEmpty].
list := list asSortedCollection.
list do: [:each | stream nextPutAll: each; nextPut: Character lf; yourself].
stream contents'.
	(result := gciSession executeString: gsCode) isNil ifTrue: [^self].
	list := result subStrings: Character lf.
	(selector := ChoicePrompter choices: list) isNil ifTrue: [^self].
	self browseMethodSendersOf: selector.
!

browseMethodSendersOf: aString

	self 
		browseMethodsWhere: 'sendersOf: #' , aString printString
		searchFor: aString
		caption: 'sending #' , aString printString.
!

browseMethodsWhere: codeString searchFor: searchString caption: captionString

	| gsCode result list title |
	gsCode := '| stream list |
stream := WriteStream on: String new.
list := ClassOrganizer new ' , codeString , '.
((list isKindOf: Array) and: [list size = 2 and: [list first isKindOf: Collection]]) ifTrue: [
	list := list first.
].
1 to: list size do: [:i | | gsMethod gsClass dictName |
	gsMethod := list at: i.
	gsClass := gsMethod inClass.
	dictName := (System myUserProfile symbolResolutionOf: gsClass thisClass name) subStrings at: 2.
	stream 
		nextPutAll: dictName; tab;
		nextPutAll: gsClass name; tab;
		nextPutAll: gsMethod selector; tab;
		nextPut: Character lf;
		yourself.
].
stream contents'.
	(result := gciSession executeString: gsCode) isNil ifTrue: [^self].
	list := result subStrings: Character lf.
	list := list collect: [:each | each subStrings: Character tab].
	list := list collect: [:each | 
		GsMethod new
			symbolDictionaryName: (each at: 1);
			gsBehavior: (each at: 2);
			name: (each at: 3);
			yourself.
	].
	title := captionString notNil
		ifTrue: [captionString]
		ifFalse: ['Jade Method Browser on ' , (searchString notNil
			ifTrue: [searchString]
			ifFalse: [codeString])].
	(JadeMethodList showOn: gciSession)
		methodList: list;
		captionString: title;
		searchFor: searchString;
		yourself.

!

browseMethodsWithSubstring

	| string |
	(string := Prompter prompt: 'Enter substring:') isNil ifTrue: [^self].
	self 
		browseMethodsWhere: 'substringSearch: ' , string printString
		searchFor: string
		caption: 'containing ' , string printString.
!

browseReferencesTo: anObject

	self 
		browseMethodsWhere: 'referencesToObject: ' , anObject codeForObject
		searchFor: anObject name
		caption: 'referencing ' , anObject name.
!

codePresenterIsMethod

	^true!

currentMethodSelector

	self subclassResponsibility.
!

methodSelector

	| list stream |
	list := documentPresenter value subStrings.
	list first last = $: ifFalse: [^list first].
	stream := WriteStream on: String new.
	list do: [:each | 
		each last = $:
			ifTrue: [stream nextPutAll: each]
			ifFalse: [^stream contents].
	].
	MessageBox notify: 'Sorry, we are not yet prepared to handle this feature!!'.
	Keyboard default isShiftDown ifTrue: [self halt].
! !
!JadeCodeBrowser categoriesFor: #browseMethodImplementors!browse!public! !
!JadeCodeBrowser categoriesFor: #browseMethodImplementorsOf!browse!public! !
!JadeCodeBrowser categoriesFor: #browseMethodImplementorsOf:!browse!public! !
!JadeCodeBrowser categoriesFor: #browseMethodSenders!browse!public! !
!JadeCodeBrowser categoriesFor: #browseMethodSendersOf!browse!public! !
!JadeCodeBrowser categoriesFor: #browseMethodSendersOf:!browse!public! !
!JadeCodeBrowser categoriesFor: #browseMethodsWhere:searchFor:caption:!browse!public! !
!JadeCodeBrowser categoriesFor: #browseMethodsWithSubstring!browse!public! !
!JadeCodeBrowser categoriesFor: #browseReferencesTo:!browse!public! !
!JadeCodeBrowser categoriesFor: #codePresenterIsMethod!public! !
!JadeCodeBrowser categoriesFor: #currentMethodSelector!browse!public! !
!JadeCodeBrowser categoriesFor: #methodSelector!public! !


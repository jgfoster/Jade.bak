"Filed out from Dolphin Smalltalk 7"!

JadeShell subclass: #JadeMethodHistoryBrowser
	instanceVariableNames: 'versionListPresenter editorPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeMethodHistoryBrowser guid: (GUID fromString: '{1a2befc8-8219-4ae2-bd65-318e08dac9bf}')!
JadeMethodHistoryBrowser comment: ''!
!JadeMethodHistoryBrowser categoriesForClass!Unclassified! !
!JadeMethodHistoryBrowser methodsFor!

createComponents

	super createComponents.
	versionListPresenter 	:= self add: ListPresenter	new name: 'versionList'.
	editorPresenter		:= self add: TextPresenter	new name: 'editor'.

!

createSchematicWiring

	super createSchematicWiring.
	versionListPresenter when: #'selectionChanged' send: #'fillMethodSource' to: self.
!

fillMethodSource

	editorPresenter value: (versionListPresenter selection at: 3) asUtf8String.
!

setContents: aReadStream

	| versions | 
	versions := OrderedCollection new.
	[
		aReadStream atEnd not.
	] whileTrue: [
		| category timeStamp writeStream |
		category := aReadStream upTo: Character tab.
		timeStamp := aReadStream upTo: Character tab.
		writeStream := WriteStream on: String new.
		writeStream nextPutAll: aReadStream nextLine; lf.
		[
			aReadStream peekFor: $%.
		] whileFalse: [
			writeStream nextPutAll: aReadStream nextLine; lf.
		].
		aReadStream nextLine.
		versions add: (Array with: category with: timeStamp with: writeStream contents).
	].
	versionListPresenter 
		list: versions;
		selection: versions first;
		yourself.
!

shellName

	^'Jade Method History Browser'.
! !
!JadeMethodHistoryBrowser categoriesFor: #createComponents!private! !
!JadeMethodHistoryBrowser categoriesFor: #createSchematicWiring!private! !
!JadeMethodHistoryBrowser categoriesFor: #fillMethodSource!private! !
!JadeMethodHistoryBrowser categoriesFor: #setContents:!private! !
!JadeMethodHistoryBrowser categoriesFor: #shellName!private! !

!JadeMethodHistoryBrowser class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 1201 801 551 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() true 170 192 34 4 410 ##(Smalltalk.ScintillaView) 34 50 nil 416 34 2 8 1445007428 1025 640 721990 2 ##(Smalltalk.ValueHolder) nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil 466 8 4278190080 nil 7 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 514 193 193 nil 640 nil 504331129 852486 ##(Smalltalk.NullConverter) nil nil 11 nil 170 192 34 4 #normal 1182790 1 ##(Smalltalk.ScintillaTextStyle) 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 962 77 786694 ##(Smalltalk.IndexedColor) 33554457 1010 33554471 1 nil nil nil nil #callTip nil nil nil 34 256 976 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 992 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1377542 ##(Smalltalk.SmalltalkMethodStyler) 1 nil nil false 138 144 592 170 192 34 2 #default 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 1 nil nil nil 640 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 592 nil 170 176 592 9215 nil nil nil nil 1010 33554447 nil nil 65 nil nil 170 192 34 6 #literalBytes 8 '[]' #literalArray 8 '()' #specialCharacter 8 '()[]<>' 8 '' 3 170 192 34 2 #container 170 192 34 4 #normal 976 #callTip 992 nil nil nil nil 1 nil 170 192 34 6 1 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 1 640 65025 3 false 1 nil nil nil nil nil nil 3 1426 3 640 33423361 5 false 3 nil nil nil nil nil nil 5 1426 5 640 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Warning 962 1031 196934 1 ##(Smalltalk.RGB) 60239 1538 31326207 1 nil nil nil nil #Warning nil nil nil #Error 962 1029 1538 335 1538 31317503 1 nil nil nil nil #Error nil nil nil #Notification 962 1027 nil 1538 31185883 1 nil nil nil nil #Notification nil nil nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 9 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 1 367 514 1169 357 640 1730 #textLimit: 8 #(1073741823) 640 1730 #modificationEventMask: 8 #(4607) 640 1730 #caretPeriod: 8 #(530) 640 1730 #sciSetEOLMode: 8 #(2) 640 1730 #margins: 34 1 34 3 985158 2 ##(Smalltalk.ScintillaMargin) 1 640 1 3 nil nil nil nil 1986 3 640 33 nil 3 67108863 nil nil 1986 5 640 1 nil 3 -67108863 nil nil 640 1730 #tabWidth: 8 #(4) 640 1730 #maxCompletionListHeight: 8 #(9) 640 1730 #sciSetCodePage: 8 #(65001) 640 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 183 0 0 0 72 2 0 0 105 1 0 0] 8 #() 514 193 193 nil 29 8 'editor' 410 ##(Smalltalk.ListView) 34 30 nil 416 34 2 8 1409355853 1025 2240 590662 2 ##(Smalltalk.ListModel) 138 144 592 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 466 8 4278190080 nil 7 nil nil nil 2240 nil 8 1795483760 459270 ##(Smalltalk.Message) #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 2 920646 5 ##(Smalltalk.ListViewColumn) 8 'Category' 501 #left 2434 #displayString 2464 ##(Smalltalk.SortedCollection) 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 2624 7 257 nil nil 2240 nil 1 nil nil 2546 8 'Timestamp' 501 #left 2434 #displayString 8 #() 2434 #<= 2768 2610 0 nil 2642 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 2800 7 257 nil nil 2240 nil 1 nil nil #report 592 nil 131169 nil 8 #(nil) 1666 138 144 34 2 1730 #createAt:extent: 34 2 514 1 1 514 1169 357 2240 1730 #text: 34 1 8 'Category' 2240 2146 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 178 0 0 0] 8 #() 2208 nil 27 8 'versionList' nil nil nil nil nil 1 nil nil nil nil 1 nil nil 1666 138 144 34 2 1730 #createAt:extent: 34 2 514 2879 21 514 1201 801 416 1730 #updateMenuBar 592 416 2146 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 247 7 0 0 154 1 0 0] 34 3 2240 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 3296 nil 466 8 4278190080 nil 519 nil nil nil 3296 1666 138 144 34 1 1730 #createAt:extent: 34 2 514 1 357 514 1169 11 3296 2146 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 178 0 0 0 72 2 0 0 183 0 0 0] 8 #() 2208 nil 27 640 2208 nil 27 )! !
!JadeMethodHistoryBrowser class categoriesFor: #resource_Default_view!public!resources-views! !


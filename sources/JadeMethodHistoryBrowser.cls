"Filed out from Dolphin Smalltalk 7"!

JadeShell subclass: #JadeMethodHistoryBrowser
	instanceVariableNames: 'versionListPresenter editorPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeMethodHistoryBrowser guid: (GUID fromString: '{1A2BEFC8-8219-4AE2-BD65-318E08DAC9BF}')!
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

	editorPresenter value: (versionListPresenter selection at: 3).
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

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy) 8 ##(Smalltalk.ShellView) 98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 1201 801 551 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout) 234 240 98 0 16 234 256 98 4 410 8 ##(Smalltalk.ScintillaView) 98 50 0 416 98 2 8 1445007428 1025 656 721990 2 ##(Smalltalk.ValueHolder) 0 32 1310726 ##(Smalltalk.EqualitySearchPolicy) 0 482 8 4278190080 0 7 0 0 0 656 0 8 4294902941 852486 ##(Smalltalk.NullConverter) 0 0 11 0 234 256 98 4 8 #callTip 1182790 1 ##(Smalltalk.ScintillaTextStyle) 77 786694 ##(Smalltalk.IndexedColor) 33554457 962 33554471 1 0 0 0 0 912 0 0 0 8 #normal 930 1 0 0 1 0 0 0 0 1008 0 0 0 98 256 1024 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 944 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1377542 ##(Smalltalk.SmalltalkMethodStyler) 1 0 0 32 202 208 608 234 256 98 2 8 #default 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 1 0 0 0 656 8 #circle 0 0 0 0 0 0 202 8 ##(Smalltalk.IdentitySet) 608 0 234 240 608 9215 0 0 0 0 962 33554447 0 0 65 0 0 234 256 98 6 8 #literalBytes 8 '[]' 8 #literalArray 8 '()' 8 #specialCharacter 8 '()[]<>' 8 '' 3 234 256 98 2 8 #container 234 256 98 4 912 944 1008 1024 0 0 0 0 1 0 234 256 98 6 1 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 1 656 65025 3 32 1 0 0 0 0 0 0 3 1522 3 656 33423361 5 32 3 0 0 0 0 0 0 5 1522 5 656 511 1 32 5 0 0 0 0 0 0 0 0 234 256 98 6 8 #Notification 930 1027 0 196934 1 ##(Smalltalk.RGB) 31185883 1 0 0 0 0 1616 0 0 0 8 #Error 930 1029 1650 335 1650 31317503 1 0 0 0 0 1680 0 0 0 8 #Warning 930 1031 1650 60239 1650 31326207 1 0 0 0 0 1744 0 0 0 0 983302 ##(Smalltalk.MessageSequence) 202 208 98 5 721670 ##(Smalltalk.MessageSend) 8 #createAt:extent: 98 2 530 1 367 530 1169 357 656 1874 8 #modificationEventMask: 98 1 9215 656 1874 8 #caretPeriod: 98 1 1061 656 1874 8 #margins: 98 1 98 3 984902 1 ##(Smalltalk.ScintillaMargin) 1 656 1 3 32 1 15 2130 3 656 33 1 16 67108863 15 2130 5 656 1 1 16 -67108863 15 656 1874 8 #tabWidth: 98 1 9 656 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 183 0 0 0 72 2 0 0 105 1 0 0] 98 0 530 193 193 0 29 8 'editor' 410 8 ##(Smalltalk.ListView) 98 30 0 416 98 2 8 1409355853 1025 2336 590662 2 ##(Smalltalk.ListModel) 202 208 608 0 1310726 ##(Smalltalk.IdentitySearchPolicy) 482 8 4278190080 0 7 0 0 0 2336 0 8 4294902815 459270 ##(Smalltalk.Message) 8 #displayString 98 0 8 ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) 0 0 0 0 0 0 202 208 98 2 920646 5 ##(Smalltalk.ListViewColumn) 8 'Category' 501 8 #left 2546 2576 2592 8 ##(Smalltalk.SortedCollection) 787814 3 ##(Smalltalk.BlockClosure) 0 0 1180966 ##(Smalltalk.CompiledExpression) 1 83886081 8 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 2800 7 257 0 0 2336 0 1 0 0 2690 8 'Timestamp' 501 2736 2546 2576 98 0 2546 8 #<= 2960 2786 0 0 2818 1 83886081 2848 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 3008 7 257 0 0 2336 0 1 0 0 8 #report 608 0 131169 0 98 1 0 1810 202 208 98 2 1874 1904 98 2 530 1 1 530 1169 357 2336 1874 8 #text: 98 1 8 'Category' 2336 2242 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 178 0 0 0] 98 0 2304 0 27 8 'versionList' 0 0 0 0 0 1 0 0 0 0 1 0 0 1810 202 208 98 2 1874 1904 98 2 530 2559 21 530 1201 801 416 1874 8 #updateMenuBar 608 416 2242 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 10 0 0 0 87 7 0 0 154 1 0 0] 98 3 2336 410 8 ##(Smalltalk.Splitter) 98 12 0 416 98 2 8 1140850688 1 3552 0 482 8 4278190080 0 519 0 0 0 3552 1810 202 208 98 1 1874 1904 98 2 530 1 357 530 1169 11 3552 2242 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 178 0 0 0 72 2 0 0 183 0 0 0] 98 0 2304 0 27 656 2304 0 27 )! !
!JadeMethodHistoryBrowser class categoriesFor: #resource_Default_view!public!resources-views! !


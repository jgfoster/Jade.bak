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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 0 0 8 #(13565952 65536) 416 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 1201 801 551 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() true 170 192 34 4 410 ##(Smalltalk.ScintillaView) 34 50 0 416 34 2 8 1445007428 1025 640 721990 2 ##(Smalltalk.ValueHolder) 0 false 1310726 ##(Smalltalk.EqualitySearchPolicy) 0 466 8 4278190080 0 7 0 263174 ##(Smalltalk.Font) 0 true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 514 193 193 0 640 0 8 4294905729 852486 ##(Smalltalk.NullConverter) 0 0 11 0 170 192 34 4 #normal 1182790 1 ##(Smalltalk.ScintillaTextStyle) 1 0 0 1 0 0 0 0 #normal 0 0 0 #callTip 978 77 786694 ##(Smalltalk.IndexedColor) 33554457 1026 33554471 1 0 0 0 0 #callTip 0 0 0 34 256 992 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1008 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1377542 ##(Smalltalk.SmalltalkMethodStyler) 1 0 0 false 138 144 592 170 192 34 2 #default 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 1 0 0 0 640 #circle 0 0 0 0 0 0 138 ##(Smalltalk.IdentitySet) 592 0 170 176 592 9215 0 0 0 0 1026 33554447 0 0 65 0 0 170 192 34 6 #literalBytes 8 '[]' #specialCharacter 8 '()[]<>' #literalArray 8 '()' 8 '' 3 170 192 34 2 #container 170 192 34 4 #normal 992 #callTip 1008 0 0 0 0 1 0 170 192 34 6 1 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 1 640 65025 3 false 1 0 0 0 0 0 0 3 1442 3 640 33423361 5 false 3 0 0 0 0 0 0 5 1442 5 640 511 1 false 5 0 0 0 0 0 0 0 0 170 192 34 6 #Notification 978 1027 0 196934 1 ##(Smalltalk.RGB) 31185883 1 0 0 0 0 #Notification 0 0 0 #Warning 978 1031 1554 60239 1554 31326207 1 0 0 0 0 #Warning 0 0 0 #Error 978 1029 1554 335 1554 31317503 1 0 0 0 0 #Error 0 0 0 0 983302 ##(Smalltalk.MessageSequence) 138 144 34 5 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 1 367 514 1169 357 640 1746 #modificationEventMask: 8 #(4607) 640 1746 #caretPeriod: 8 #(530) 640 1746 #margins: 34 1 34 3 985158 2 ##(Smalltalk.ScintillaMargin) 1 640 1 3 0 0 0 0 1938 3 640 33 0 3 67108863 0 0 1938 5 640 1 0 3 -67108863 0 0 640 1746 #tabWidth: 8 #(4) 640 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 183 0 0 0 72 2 0 0 105 1 0 0] 8 #() 514 193 193 0 29 8 'editor' 410 ##(Smalltalk.ListView) 34 30 0 416 34 2 8 1409355853 1025 2128 590662 2 ##(Smalltalk.ListModel) 138 144 592 0 1310726 ##(Smalltalk.IdentitySearchPolicy) 466 8 4278190080 0 7 0 0 0 2128 0 8 4294904383 459270 ##(Smalltalk.Message) #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) 0 0 0 0 0 0 138 144 34 2 920646 5 ##(Smalltalk.ListViewColumn) 8 'Category' 501 #left 2322 #displayString 2352 ##(Smalltalk.SortedCollection) 787814 3 ##(Smalltalk.BlockClosure) 0 0 1180966 ##(Smalltalk.CompiledExpression) 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 2512 7 257 0 0 2128 0 1 0 0 2434 8 'Timestamp' 501 #left 2322 #displayString 8 #() 2322 #<= 2656 2498 0 0 2530 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 2688 7 257 0 0 2128 0 1 0 0 #report 592 0 131169 0 8 #(nil) 1682 138 144 34 2 1746 #createAt:extent: 34 2 514 1 1 514 1169 357 2128 1746 #text: 34 1 8 'Category' 2128 2034 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 178 0 0 0] 8 #() 2096 0 27 8 'versionList' 0 0 0 0 0 1 0 0 0 0 1 0 0 1682 138 144 34 2 1746 #createAt:extent: 34 2 514 5119 21 514 1201 801 416 1746 #updateMenuBar 592 416 2034 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 87 12 0 0 154 1 0 0] 34 3 2128 410 ##(Smalltalk.Splitter) 34 12 0 416 34 2 8 1140850688 1 3184 0 466 8 4278190080 0 519 0 0 0 3184 1682 138 144 34 1 1746 #createAt:extent: 34 2 514 1 357 514 1169 11 3184 2034 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 178 0 0 0 72 2 0 0 183 0 0 0] 8 #() 2096 0 27 640 2096 0 27 )! !
!JadeMethodHistoryBrowser class categoriesFor: #resource_Default_view!public!resources-views! !


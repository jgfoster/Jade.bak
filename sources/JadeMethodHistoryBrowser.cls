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

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  1201 801 551 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 0 16 234 256 98 4 410 8 ##(Smalltalk.ListView)  98 30 0 416 98 2 8 1409355853 1025 656 590662 2 ##(Smalltalk.ListModel)  202 208 608 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  482 8 4278190080 0 7 0 0 0 656 0 8 4294903981 459270 ##(Smalltalk.Message)  8 #displayString 98 0 8 ##(Smalltalk.IconicListAbstract)  1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 0 202 208 98 2 920646 5 ##(Smalltalk.ListViewColumn)  8 'Category' 501 8 #left 866 896 912 8 ##(Smalltalk.SortedCollection)  787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  1 83886081 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 1120 7 257 0 0 656 0 1 0 0 1010 8 'Timestamp' 501 1056 866 896 608 866 8 #<= 608 1106 0 0 1138 1 83886081 1168 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 1312 7 257 0 0 656 0 1 0 0 8 #report 608 0 131169 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 1 1 530 1169 353 656 1474 8 #text: 98 1 8 'Category' 656 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 176 0 0 0] 98 0 530 193 193 0 27 8 'versionList' 410 8 ##(Smalltalk.ScintillaView)  98 46 0 416 98 2 8 1445007428 1025 1728 721990 2 ##(Smalltalk.ValueHolder)  0 32 1310726 ##(Smalltalk.EqualitySearchPolicy)  0 482 8 4278190080 0 7 0 0 0 1728 0 8 4294904083 852486 ##(Smalltalk.NullConverter)  0 0 11 0 234 256 98 2 8 #normal 1182726 ##(Smalltalk.ScintillaTextStyle)  1 0 0 1 0 0 0 0 1984 0 0 0 98 40 2016 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1245510 1 ##(Smalltalk.NullScintillaStyler)  1984 234 256 98 2 8 #default 1639942 ##(Smalltalk.ScintillaMarkerDefinition)  1 1 786694 ##(Smalltalk.IndexedColor)  33554433 2162 33554471 1728 8 #circle 202 208 608 0 63 9215 0 0 0 0 2162 33554447 0 0 0 0 0 0 8 '' 3 234 256 98 2 8 #container 1952 0 0 0 0 1 0 234 256 98 6 1 1509190 1 ##(Smalltalk.ScintillaIndicatorStyle)  1 1728 65025 3 32 1 0 3 2354 3 1728 33423361 5 32 3 0 5 2354 5 1728 511 1 32 5 0 1410 202 208 98 8 1474 1504 98 2 530 1 371 530 1169 355 1728 1474 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 1728 1474 8 #isTextModified: 98 1 32 1728 1474 8 #modificationEventMask: 98 1 9215 1728 1474 8 #margins: 98 1 98 3 984582 ##(Smalltalk.ScintillaMargin)  1 1728 1 3 32 1 2770 3 1728 33 1 16 67108863 2770 5 1728 1 1 16 -67108863 1728 1474 8 #indentationGuides: 98 1 0 1728 1474 8 #tabIndents: 98 1 16 1728 1474 8 #tabWidth: 98 1 9 1728 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 185 0 0 0 72 2 0 0 106 1 0 0] 98 0 1696 0 27 8 'editor' 0 0 0 0 0 1 0 0 0 0 1 0 0 1410 202 208 98 2 1474 1504 98 2 530 2879 21 530 1201 801 416 1474 8 #updateMenuBar 608 416 1634 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 247 7 0 0 154 1 0 0] 98 3 656 410 8 ##(Smalltalk.Splitter)  98 12 0 416 98 2 8 1140850688 1 3232 0 482 8 4278190080 0 519 0 0 0 3232 1410 202 208 98 1 1474 1504 98 2 530 1 353 530 1169 19 3232 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 176 0 0 0 72 2 0 0 185 0 0 0] 98 0 1696 0 27 1728 1696 0 27 )! !
!JadeMethodHistoryBrowser class categoriesFor: #resource_Default_view!public!resources-views! !


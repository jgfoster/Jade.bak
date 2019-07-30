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

compareSelectedVersions
	| diffPresenter |

	(versionListPresenter selections size = 2) ifFalse: [^MessageBox notify: 'Select two versions to compare' ].

	diffPresenter := DifferencesPresenter show.
	diffPresenter 
		textStyles: SmalltalkWorkspace textStyles;
		styler: SmalltalkMethodStyler new.

	diffPresenter topShell caption: (versionListPresenter selections first at: 2), ' -- ', (versionListPresenter selections second at: 2).

	diffPresenter compare: (versionListPresenter selections first at: 3) with: (versionListPresenter selections second at: 3)!

createComponents

	super createComponents.
	versionListPresenter 	:= self add: ListPresenter	new name: 'versionList'.
	editorPresenter		:= self add: MethodSourcePresenter	new name: 'editor'.

!

createSchematicWiring

	super createSchematicWiring.
	versionListPresenter when: #'selectionChanged' send: #'fillMethodSource' to: self.
!

fillMethodSource

	(versionListPresenter selections size = 1) ifFalse: [^self].

	editorPresenter model: (versionListPresenter selection at: 3) asUtf8String asValue.
!

onViewOpened

	super onViewOpened.

	editorPresenter view textStyles: SmalltalkWorkspace textStyles.
	editorPresenter view styler: SmalltalkMethodStyler new.!

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
!JadeMethodHistoryBrowser categoriesFor: #compareSelectedVersions!private! !
!JadeMethodHistoryBrowser categoriesFor: #createComponents!private! !
!JadeMethodHistoryBrowser categoriesFor: #createSchematicWiring!private! !
!JadeMethodHistoryBrowser categoriesFor: #fillMethodSource!private! !
!JadeMethodHistoryBrowser categoriesFor: #onViewOpened!public! !
!JadeMethodHistoryBrowser categoriesFor: #setContents:!private! !
!JadeMethodHistoryBrowser categoriesFor: #shellName!private! !

!JadeMethodHistoryBrowser class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 786694 ##(Smalltalk.ColorDefault) 8 4278190080 328198 ##(Smalltalk.Point) 1201 801 549 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() true 170 192 34 4 410 ##(Smalltalk.ReferenceView) 34 14 nil 416 34 2 8 1140850688 131073 640 nil 480 nil 7 nil nil nil 640 1180230 1 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.MethodWorkspace) #resource_Default_view nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 1 367 514 1169 357 640 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 183 0 0 0 72 2 0 0 105 1 0 0] 592 514 193 193 nil 27 8 'editor' 410 ##(Smalltalk.ListView) 34 30 nil 416 34 2 8 1409355849 1025 960 590662 2 ##(Smalltalk.ListModel) 138 144 592 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 480 nil 7 265030 4 ##(Smalltalk.Menu) nil true 34 1 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #compareSelectedVersions 8 'Compare Selected Versions' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 960 nil 8 1937040751 459270 ##(Smalltalk.Message) #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 2 920646 5 ##(Smalltalk.ListViewColumn) 8 'Category' 501 #left 1266 #displayString 1296 ##(Smalltalk.SortedCollection) 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 1456 7 257 nil nil 960 nil 1 nil nil 1378 8 'Timestamp' 501 #left 1266 #displayString 8 #() 1266 #<= 1600 1442 0 nil 1474 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 1632 7 257 nil nil 960 nil 1 nil nil #report 592 nil 131169 nil 34 4 nil nil 514 1 1 nil 738 138 144 34 3 802 #createAt:extent: 34 2 514 1 1 514 1169 357 960 802 #contextMenu: 34 1 1120 960 802 #text: 34 1 8 'Category' 960 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 178 0 0 0] 8 #() 928 nil 27 8 'versionList' nil nil nil nil nil 1 nil nil nil nil 1 nil nil 738 138 144 34 2 802 #createAt:extent: 34 2 514 3059 21 514 1201 801 416 802 #updateMenuBar 592 416 882 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 249 5 0 0 10 0 0 0 81 8 0 0 154 1 0 0] 34 3 960 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 2176 nil 480 nil 519 nil nil nil 1510470 1 ##(Smalltalk.DraggableViewInteractor) 2176 nil 1 #left nil nil nil 1728 514 9 9 nil 1728 nil 738 138 144 34 1 802 #createAt:extent: 34 2 514 1 357 514 1169 11 2176 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 178 0 0 0 72 2 0 0 183 0 0 0] 8 #() 928 nil 27 640 928 nil 27 )! !
!JadeMethodHistoryBrowser class categoriesFor: #resource_Default_view!public!resources-views! !


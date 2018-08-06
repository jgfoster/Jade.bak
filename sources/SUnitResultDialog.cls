"Filed out from Dolphin Smalltalk 7"!

JadeValueDialog subclass: #SUnitResultDialog
	instanceVariableNames: 'listPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SUnitResultDialog guid: (GUID fromString: '{0fef434a-dcf0-49c0-8d00-a179396abf7b}')!
SUnitResultDialog comment: ''!
!SUnitResultDialog categoriesForClass!Unclassified! !
!SUnitResultDialog methodsFor!

apply

	self model: listPresenter selection.
	super apply.
!

createComponents

	super createComponents.
	listPresenter := self add: ListPresenter new name: 'list'.
!

createSchematicWiring

	super createSchematicWiring.
	listPresenter when: #'actionPerformed' send: #'ok' to: self.
!

onViewOpened

	super onViewOpened.
	self caption: (self model value at: 2).
	listPresenter list: (self model value at: 3).
!

openWorkspaceWithList

	| stream |
	stream := WriteStream on: String new.
	listPresenter list do: [:each | 
		stream nextPutAll: each; nextPut: $.; cr.
	].
	(JadeWorkspace showOnSession: (self model value at: 1)) showText: stream contents.
!

queryCommand: aCommandQuery

	(#(#'ok' ) includes: aCommandQuery commandSymbol) ifTrue: [
		aCommandQuery isEnabled: listPresenter hasSelection.
		^true.
	].
	^super queryCommand: aCommandQuery.
! !
!SUnitResultDialog categoriesFor: #apply!public! !
!SUnitResultDialog categoriesFor: #createComponents!public! !
!SUnitResultDialog categoriesFor: #createSchematicWiring!public! !
!SUnitResultDialog categoriesFor: #onViewOpened!public! !
!SUnitResultDialog categoriesFor: #openWorkspaceWithList!public! !
!SUnitResultDialog categoriesFor: #queryCommand:!public! !

!SUnitResultDialog class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.DialogView)  98 30 0 0 98 2 26214401 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 167 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 8 410 8 ##(Smalltalk.PushButton)  98 20 0 416 98 2 8 1140924416 1 592 0 0 0 7 0 0 0 592 0 8 4294903629 1180998 4 ##(Smalltalk.CommandDescription)  8 #openWorkspaceWithList 8 'Open Workspace with List' 1 1 0 0 32 0 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  25 661 882 361 51 592 818 8 #text: 98 1 8 'Open Workspace with List' 592 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 12 0 0 0 74 1 0 0 192 0 0 0 99 1 0 0] 98 0 882 193 193 0 29 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 5 1106 8 #fixedViewLeft 361 1106 8 #fixedParentBottom -43 1106 8 #fixedViewTop 51 410 8 ##(Smalltalk.ListView)  98 30 0 416 98 2 8 1409372237 1025 1248 590662 2 ##(Smalltalk.ListModel)  202 208 98 0 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  482 8 4278190080 0 7 0 0 0 1248 0 8 4294904243 459270 ##(Smalltalk.Message)  8 #displayString 98 0 8 ##(Smalltalk.IconicListAbstract)  1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 0 202 208 98 1 920646 5 ##(Smalltalk.ListViewColumn)  8 'Column 1' 871 8 #left 1474 1504 1520 8 ##(Smalltalk.SortedCollection)  0 0 1248 0 3 0 0 8 #report 1376 0 131169 0 0 754 202 208 98 2 818 848 98 2 882 21 21 882 879 625 1248 818 944 98 1 8 'Column 1' 1248 994 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 10 0 0 0 193 1 0 0 66 1 0 0] 98 0 1056 0 27 1074 1120 1 1106 8 #fixedParentRight 1 1106 8 #fixedParentTop 1 1184 -59 410 608 98 20 0 416 98 2 8 1140924416 1 2016 0 0 0 7 0 0 0 2016 0 8 4294903629 690 8 #cancel 8 'Cancel' 1025 1 0 0 32 0 0 0 754 202 208 98 2 818 848 98 2 882 759 661 882 141 51 2016 818 944 98 1 8 'Cancel' 2016 994 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 123 1 0 0 74 1 0 0 193 1 0 0 99 1 0 0] 98 0 1056 0 29 1074 1952 -139 1152 141 1184 -43 1216 51 410 608 98 20 0 416 98 2 8 1140924416 1 2368 0 0 0 7 0 0 0 2368 0 8 4294903629 690 8 #ok 8 'Run in Debugger' 1 1 0 0 16 0 0 0 754 202 208 98 2 818 848 98 2 882 499 661 882 251 51 2368 818 944 98 1 8 'Run in Debugger' 2368 994 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 249 0 0 0 74 1 0 0 118 1 0 0 99 1 0 0] 98 0 1056 0 29 1074 1952 -399 1152 251 1184 -43 1216 51 234 256 98 2 1248 8 'list' 590342 ##(Smalltalk.Rectangle)  882 21 21 882 21 21 0 0 0 0 10373 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 0 8 1988726999 754 202 208 98 3 818 848 98 2 882 6239 21 882 931 781 416 818 944 98 1 8 'SUnit Results' 416 818 8 #updateMenuBar 1376 416 994 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 47 12 0 0 10 0 0 0 0 14 0 0 144 1 0 0] 98 4 1248 592 2016 2368 1056 0 27 )! !
!SUnitResultDialog class categoriesFor: #resource_Default_view!public!resources-views! !


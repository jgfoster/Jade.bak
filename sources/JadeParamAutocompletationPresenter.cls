"Filed out from Dolphin Smalltalk 7"!

JadePresenterA subclass: #JadeParamAutocompletationPresenter
	instanceVariableNames: 'methodNamePresenter paramPresenter paramsPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeParamAutocompletationPresenter guid: (GUID fromString: '{C2C838B3-59B2-4713-B655-86630F98210D}')!
JadeParamAutocompletationPresenter comment: ''!
!JadeParamAutocompletationPresenter categoriesForClass!Unclassified! !
!JadeParamAutocompletationPresenter methodsFor!

addParam

	methodNamePresenter value isEmpty ifTrue: [^self].
	paramPresenter value isEmpty ifTrue: [^self].

	self registry registerParamFor: methodNamePresenter value param: paramPresenter value.

	self onViewOpened.!

createComponents

	super createComponents.

	methodNamePresenter := self add: TextPresenter new name: 'methodName'.
	paramPresenter := self add: TextPresenter new name: 'param'.
	paramsPresenter := self add: ListPresenter new name: 'params'.!

onViewOpened

	super onViewOpened.

	paramsPresenter model: (ListModel on: self registry paramsToDisplay).

!

removeParam

	paramsPresenter hasSelection ifFalse: [^self].

	self registry unregisterParam: (paramsPresenter selection subStrings: '>>') first.

	self onViewOpened.! !
!JadeParamAutocompletationPresenter categoriesFor: #addParam!public! !
!JadeParamAutocompletationPresenter categoriesFor: #createComponents!public! !
!JadeParamAutocompletationPresenter categoriesFor: #onViewOpened!public! !
!JadeParamAutocompletationPresenter categoriesFor: #removeParam!public! !

!JadeParamAutocompletationPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ContainerView)  98 15 0 0 98 2 8 1409286144 131073 416 0 721158 ##(Smalltalk.SystemColor)  31 0 517 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 0 16 234 256 576 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  2731 21 738 951 791 416 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 23 5 0 0 10 0 0 0 242 6 0 0 149 1 0 0] 98 2 410 432 98 15 0 416 98 2 8 1140850688 131073 848 0 0 0 5 0 0 0 848 0 234 256 98 4 410 8 ##(Smalltalk.TextEdit)  98 16 0 848 98 2 8 1140916352 1025 944 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 5 0 0 0 944 0 8 4294903883 852486 ##(Smalltalk.NullConverter)  0 0 1 610 202 208 98 3 674 704 98 2 738 371 161 738 481 51 944 674 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 944 674 8 #isTextModified: 98 1 32 944 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 185 0 0 0 80 0 0 0 169 1 0 0 105 0 0 0] 98 0 738 193 193 0 27 8 'param' 410 960 98 16 0 848 98 2 8 1140916352 1025 1440 0 1026 8 4278190080 0 5 0 0 0 1440 0 8 4294903883 1090 0 0 1 610 202 208 98 3 674 704 98 2 738 371 71 738 481 51 1440 674 1248 98 1 1282 3 1 3 1440 674 1328 98 1 32 1440 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 185 0 0 0 35 0 0 0 169 1 0 0 60 0 0 0] 98 0 1408 0 27 8 'methodName' 0 610 202 208 98 1 674 704 98 2 738 1 1 738 951 395 848 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 219 1 0 0 197 0 0 0] 98 6 410 8 ##(Smalltalk.StaticText)  98 16 0 848 98 2 8 1140850944 1 1984 0 0 0 5 0 0 0 1984 0 8 4294903639 1090 0 0 0 610 202 208 98 2 674 704 98 2 738 71 81 738 211 41 1984 674 8 #text: 98 1 8 'Method Name' 1984 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 35 0 0 0 40 0 0 0 140 0 0 0 60 0 0 0] 98 0 1408 0 27 410 2000 98 16 0 848 98 2 8 1140850944 1 2320 0 0 0 5 0 0 0 2320 0 8 4294903639 1090 0 0 0 610 202 208 98 2 674 704 98 2 738 71 161 738 241 39 2320 674 2224 98 1 8 'Param String' 2320 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 35 0 0 0 80 0 0 0 155 0 0 0 99 0 0 0] 98 0 1408 0 27 1440 944 410 8 ##(Smalltalk.PushButton)  98 20 0 848 98 2 8 1140924416 1 2624 0 0 0 5 0 0 0 2624 0 8 4294903669 1180998 4 ##(Smalltalk.CommandDescription)  8 #addParam 8 'Add' 1 1 0 0 32 0 0 0 610 202 208 98 3 674 704 98 2 738 711 231 738 141 51 2624 674 8 #isEnabled: 98 1 32 2624 674 2224 98 1 8 'Add' 2624 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 99 1 0 0 115 0 0 0 169 1 0 0 140 0 0 0] 98 0 1408 0 29 410 8 ##(Smalltalk.GroupBox)  98 14 0 848 98 2 8 1140850695 65 3040 0 1026 8 4278190080 0 5 0 0 0 3040 0 8 4294903669 610 202 208 98 2 674 704 98 2 738 41 21 738 841 271 3040 674 2224 98 1 8 'Create New Param' 3040 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 20 0 0 0 10 0 0 0 184 1 0 0 145 0 0 0] 98 0 1408 0 27 1408 0 27 410 432 98 15 0 416 98 2 8 1140850688 131073 3376 0 0 0 5 0 0 0 3376 530 234 240 576 32 234 256 98 2 410 8 ##(Smalltalk.ListView)  98 30 0 3376 98 2 8 1409355853 1025 3504 590662 2 ##(Smalltalk.ListModel)  202 208 576 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  1026 8 4278190080 0 5 265030 4 ##(Smalltalk.Menu)  0 16 98 1 984134 2 ##(Smalltalk.CommandMenuItem)  1 2722 8 #removeParam 8 'Remove Param' 1 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 3504 0 8 4294903635 459270 ##(Smalltalk.Message)  8 #displayString 98 0 8 ##(Smalltalk.IconicListAbstract)  1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 0 202 208 98 2 920646 5 ##(Smalltalk.ListViewColumn)  8 'Method Name' 501 8 #left 787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  4 1 3856 8 'doIt' 8 '[:each | (each subStrings: ''>>'') first]' 8 #[32 105 17 29 177 160 106] 8 '>>' 8 #subStrings: 8 #first 4080 7 257 0 8 ##(Smalltalk.SortedCollection)  0 0 3504 0 1 0 0 4002 8 'Parameters' 441 4048 4066 0 0 4098 4 1 3856 8 'doIt' 8 '[:each | (each subStrings: ''>>'') last]' 8 #[32 105 17 29 177 160 106] 8 '>>' 4192 8 #last 4272 7 257 0 3858 8 #<= 98 0 0 0 3504 0 1 0 0 8 #report 576 0 131169 0 0 610 202 208 98 3 674 704 98 2 738 1 1 738 951 397 3504 674 8 #contextMenu: 98 1 3712 3504 674 2224 98 1 8 'Method Name' 3504 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 219 1 0 0 198 0 0 0] 98 0 1408 0 27 8 'params' 0 610 202 208 98 1 674 704 98 2 738 1 395 738 951 397 3376 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 197 0 0 0 219 1 0 0 139 1 0 0] 98 1 3504 1408 0 27 1408 0 27 )! !
!JadeParamAutocompletationPresenter class categoriesFor: #resource_Default_view!public!resources-views! !


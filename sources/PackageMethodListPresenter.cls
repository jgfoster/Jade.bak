"Filed out from Dolphin Smalltalk 7"!

MethodListPresenter subclass: #PackageMethodListPresenter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
PackageMethodListPresenter guid: (GUID fromString: '{91876023-8beb-4d08-afbc-aacbcf92b295}')!
PackageMethodListPresenter comment: ''!
!PackageMethodListPresenter categoriesForClass!Unclassified! !
!PackageMethodListPresenter class methodsFor!

resource_MultiClass_view
	"Answer the literal data from which the 'MultiClass view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_MultiClass_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() false 170 192 34 2 410 ##(Smalltalk.ListView) 34 30 nil 416 34 2 8 1140920905 1025 576 590662 2 ##(Smalltalk.ListModel) 138 144 528 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 13 265030 4 ##(Smalltalk.Menu) nil true 34 10 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #removeFromList 8 'Remove from &List' 1 1 nil nil nil 818 1 850 #browseClass 8 'Browse Class' 1 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 818 1 850 #browseImplementors 8 'Browse &Implementors' 1 1 nil nil nil 818 1 850 #browseImplementorsOf 8 'Browse Implementors of ...' 1 1 nil nil nil 818 1 850 #browseSenders 8 'Browse &Senders' 1 1 nil nil nil 818 1 850 #browseSendersOf 8 'Browse Senders of ...' 1 1 nil nil nil 818 1 850 #browseMethodsContaining 8 'Browse Methods Containing ...' 1 1 nil nil nil 946 4097 818 1 850 #runTests 8 'Run &Test(s)' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 576 nil 8 4294905043 459270 ##(Smalltalk.Message) #displayString 8 #() nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 4 920646 5 ##(Smalltalk.ListViewColumn) 8 'Package' 301 #left 1314 #displayString 8 #() 1314 #<= 1488 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 3 1 1520 8 'doIt' 8 '[:each | each gsClass dictionary]' 8 #[31 105 226 0 159 106] #gsClass #dictionary 1536 7 257 nil nil 576 nil 1 nil nil 1426 8 'Class' 301 #left 1314 #displayString 8 #() 1314 #<= 1680 1522 0 nil 1554 3 1 1520 8 'doIt' 8 '[:each | each gsClass name]' 8 #[31 105 226 0 159 106] #gsClass #name 1712 7 257 nil nil 576 nil 3 nil nil 1426 8 'Protocol' 301 #left 1314 #displayString 8 #() 1314 #<= 1840 1522 0 nil 1554 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each category]' 8 #[30 105 226 0 106] #category 1872 7 257 nil nil 576 nil 3 nil nil 1426 8 'Method' 301 #left 1314 #displayString 1344 ##(Smalltalk.SortedCollection) 1522 0 nil 1554 2 1 1520 8 'doIt' 8 '[:each | each name]' 8 #[30 105 226 0 106] #name 2000 7 257 nil nil 576 nil 3 nil nil #report 528 nil 131169 nil 34 4 nil nil 328198 ##(Smalltalk.Point) 1 1 nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 3 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 2098 1 1 2098 1201 401 576 2194 #contextMenu: 34 1 784 576 2194 #text: 34 1 8 'Package' 576 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 88 2 0 0 200 0 0 0] 8 #() 2098 193 193 nil 27 8 'methodList' nil 2130 138 144 34 1 2194 #createAt:extent: 34 2 2098 3839 21 2098 1201 401 416 2354 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 215 9 0 0 210 0 0 0] 34 1 576 2416 nil 27 )! !
!PackageMethodListPresenter class categoriesFor: #resource_MultiClass_view!public!resources-views! !


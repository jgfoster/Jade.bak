"Filed out from Dolphin Smalltalk 7"!

MethodListPresenter subclass: #PackageMethodListPresenter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
PackageMethodListPresenter guid: (GUID fromString: '{91876023-8beb-4d08-afbc-aacbcf92b295}')!
PackageMethodListPresenter comment: ''!
!PackageMethodListPresenter categoriesForClass!Unclassified! !
!PackageMethodListPresenter methodsFor!

removeMethods
	| service   |
	(MessageBox confirm: 'Really remove the selected methods?') ifFalse:[^self].
	methodListPresenter selections do: 
			[:methodDef |
				service := RowanMethodService new.
				service selector: methodDef name.
				service className: methodDef gsClass name.
				service removeSelector].
	methodListPresenter selections copy
		do: [:each | methodListPresenter model removeAtIndex: (methodListPresenter model list indexOf: each)]! !
!PackageMethodListPresenter categoriesFor: #removeMethods!menus!public! !

!PackageMethodListPresenter class methodsFor!

defaultModel
	^RowanMethodService new!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 34 2 410 ##(Smalltalk.ListView) 34 30 nil 416 34 2 8 1140920649 1025 544 590662 2 ##(Smalltalk.ListModel) 138 144 8 #() nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 13 265030 4 ##(Smalltalk.Menu) nil true 34 11 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #removeFromList 8 'Remove from &List' 1 1 nil nil nil 802 1 834 #removeMethods 8 'Remove Selected Methods ...' 1 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 802 1 834 #browseClass 8 'Browse Class' 1 1 nil nil nil 802 1 834 #browseImplementors 8 'Browse &Implementors' 1 1 nil nil nil 802 1 834 #browseImplementorsOf 8 'Browse Implementors of ...' 1 1 nil nil nil 802 1 834 #browseSenders 8 'Browse &Senders' 1 1 nil nil nil 802 1 834 #browseSendersOf 8 'Browse Senders of ...' 1 1 nil nil nil 802 1 834 #browseMethodsContaining 8 'Browse Methods Containing ...' 1 1 nil nil nil 930 4097 802 1 834 #runTests 8 'Run &Test(s)' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 544 nil 8 4294905919 459270 ##(Smalltalk.Message) #displayString 8 #() nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 5 920646 5 ##(Smalltalk.ListViewColumn) 8 'Project' 201 #left 1346 #displayString 8 #() 1346 #<= 1520 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each projectName ]' 8 #[30 105 226 0 106] #projectName 1568 7 257 nil nil 544 nil 1 nil nil 1458 8 'Package' 251 #left 1346 #displayString 8 #() 1346 #<= 1712 1554 0 nil 1586 2 1 1552 8 'doIt' 8 '[:each | each packageName ]' 8 #[30 105 226 0 106] #packageName 1744 7 257 nil nil 544 nil 1 nil nil 1458 8 'Class' 315 #left 1346 #displayString 8 #() 1346 #<= 1872 1554 0 nil 1586 3 1 1552 8 'doIt' 8 '[:each | each gsClass name ]' 8 #[31 105 226 0 159 106] #gsClass #name 1904 7 257 nil nil 544 nil 3 nil nil 1458 8 'Protocol' 315 #left 1346 #displayString 8 #() 1346 #<= 2032 1554 0 nil 1586 2 1 1552 8 'doIt' 8 '[:each | each category]' 8 #[30 105 226 0 106] #category 2064 7 257 nil nil 544 nil 3 nil nil 1458 8 'Method' 315 #left 1346 #displayString 1376 ##(Smalltalk.SortedCollection) 1554 0 nil 1586 2 1 1552 8 'doIt' 8 '[:each | each name]' 8 #[30 105 226 0 106] #name 2192 7 257 nil nil 544 nil 3 nil nil #report 656 nil 131169 328198 ##(Smalltalk.Point) 129 129 34 4 nil nil 2274 1 1 nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 3 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 2274 1 1 2274 1401 401 544 2402 #contextMenu: 34 1 768 544 2402 #text: 34 1 8 'Project' 544 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 188 2 0 0 200 0 0 0] 8 #() 2274 193 193 nil 27 3 false 170 192 34 2 544 8 'methodList' nil 2338 138 144 34 1 2402 #createAt:extent: 34 2 2274 3839 21 2274 1401 401 416 2562 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 59 10 0 0 210 0 0 0] 34 1 544 2624 nil 27 )! !
!PackageMethodListPresenter class categoriesFor: #defaultModel!private! !
!PackageMethodListPresenter class categoriesFor: #resource_Default_view!public!resources-views! !


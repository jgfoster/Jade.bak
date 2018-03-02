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

defaultModel

	^RowanMethodDefinitionService new!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 34 2 410 ##(Smalltalk.ListView) 34 30 nil 416 34 2 8 1140921161 1025 544 590662 2 ##(Smalltalk.ListModel) 138 144 8 #() nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 13 265030 4 ##(Smalltalk.Menu) nil true 34 10 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #removeFromList 8 'Remove from &List' 1 1 nil nil nil 802 1 834 #browseClass 8 'Browse Class' 1 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 802 1 834 #browseImplementors 8 'Browse &Implementors' 1 1 nil nil nil 802 1 834 #browseImplementorsOf 8 'Browse Implementors of ...' 1 1 nil nil nil 802 1 834 #browseSenders 8 'Browse &Senders' 1 1 nil nil nil 802 1 834 #browseSendersOf 8 'Browse Senders of ...' 1 1 nil nil nil 802 1 834 #browseMethodsContaining 8 'Browse Methods Containing ...' 1 1 nil nil nil 930 4097 802 1 834 #runTests 8 'Run &Test(s)' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 544 nil 8 4294905145 459270 ##(Smalltalk.Message) #displayString 8 #() nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 4 920646 5 ##(Smalltalk.ListViewColumn) 8 'Package' 251 #left 1298 #displayString 8 #() 1298 #<= 1472 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 2 1 1504 8 'doIt' 8 '[:each | ''package'' ]' 8 #[30 105 29 106] 8 'package' 1520 7 257 nil nil 544 nil 1 nil nil 1410 8 'Class' 315 #left 1298 #displayString 8 #() 1298 #<= 1680 1506 0 nil 1538 3 1 1504 8 'doIt' 8 '[:each | each gsClass name ]' 8 #[31 105 226 0 159 106] #gsClass #name 1712 7 257 nil nil 544 nil 3 nil nil 1410 8 'Protocol' 315 #left 1298 #displayString 8 #() 1298 #<= 1840 1506 0 nil 1538 2 1 1504 8 'doIt' 8 '[:each | each category]' 8 #[30 105 226 0 106] #category 1872 7 257 nil nil 544 nil 3 nil nil 1410 8 'Method' 315 #left 1298 #displayString 1328 ##(Smalltalk.SortedCollection) 1506 0 nil 1538 2 1 1504 8 'doIt' 8 '[:each | each name]' 8 #[30 105 226 0 106] #name 2000 7 257 nil nil 544 nil 3 nil nil #report 656 nil 131169 328198 ##(Smalltalk.Point) 129 129 34 4 nil nil 2082 1 1 nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 3 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 2082 1 1 2082 1201 401 544 2210 #contextMenu: 34 1 768 544 2210 #text: 34 1 8 'Package' 544 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 88 2 0 0 200 0 0 0] 8 #() 2082 193 193 nil 27 3 false 170 192 34 2 544 8 'methodList' nil 2146 138 144 34 1 2210 #createAt:extent: 34 2 2082 3839 21 2082 1201 401 416 2370 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 215 9 0 0 210 0 0 0] 34 1 544 2432 nil 27 )! !
!PackageMethodListPresenter class categoriesFor: #defaultModel!private! !
!PackageMethodListPresenter class categoriesFor: #resource_Default_view!public!resources-views! !


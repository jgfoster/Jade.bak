"Filed out from Dolphin Smalltalk 7"!

JadePresenterA subclass: #JadeParamAutocompletationPresenter
	instanceVariableNames: 'methodNamePresenter paramPresenter paramsPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeParamAutocompletationPresenter guid: (GUID fromString: '{c2c838b3-59b2-4713-b655-86630f98210d}')!
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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil 721158 ##(Smalltalk.SystemColor) 31 nil 517 nil nil nil 416 788230 ##(Smalltalk.BorderLayout) 1 1 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 544 nil nil nil 5 nil nil nil 544 nil 170 192 34 4 410 ##(Smalltalk.TextEdit) 34 16 nil 544 34 2 8 1140916352 1025 640 nil 786694 ##(Smalltalk.ColorDefault) 8 4278190080 nil 5 nil nil nil 640 nil 8 1921358507 852486 ##(Smalltalk.NullConverter) nil nil 1 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 371 161 914 481 51 640 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 185 0 0 0 80 0 0 0 169 1 0 0 105 0 0 0] 8 #() 914 193 193 nil 27 8 'param' 410 ##(Smalltalk.TextEdit) 34 16 nil 544 34 2 8 1140916352 1025 1056 nil 720 nil 5 nil nil nil 1056 nil 8 1921358507 770 nil nil 1 802 138 144 34 1 866 #createAt:extent: 34 2 914 371 71 914 481 51 1056 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 185 0 0 0 35 0 0 0 169 1 0 0 60 0 0 0] 8 #() 1024 nil 27 8 'methodName' nil 802 138 144 34 1 866 #createAt:extent: 34 2 914 1 1 914 951 311 544 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 219 1 0 0 155 0 0 0] 34 6 410 ##(Smalltalk.StaticText) 34 16 nil 544 34 2 8 1140850944 1 1488 nil nil nil 5 nil nil nil 1488 nil 8 1921133906 770 nil nil nil 802 138 144 34 2 866 #createAt:extent: 34 2 914 71 81 914 211 41 1488 866 #text: 34 1 8 'Method Name' 1488 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 35 0 0 0 40 0 0 0 140 0 0 0 60 0 0 0] 8 #() 1024 nil 27 410 ##(Smalltalk.StaticText) 34 16 nil 544 34 2 8 1140850944 1 1792 nil nil nil 5 nil nil nil 1792 nil 8 1921133906 770 nil nil nil 802 138 144 34 2 866 #createAt:extent: 34 2 914 71 161 914 241 39 1792 866 #text: 34 1 8 'Param String' 1792 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 35 0 0 0 80 0 0 0 155 0 0 0 99 0 0 0] 8 #() 1024 nil 27 1056 640 410 ##(Smalltalk.PushButton) 34 20 nil 544 34 2 8 1140924416 1 2096 nil nil nil 5 nil nil nil 2096 nil 8 1921221004 1180998 4 ##(Smalltalk.CommandDescription) #addParam 8 'Add' 1 1 nil nil false nil nil nil 802 138 144 34 3 866 #createAt:extent: 34 2 914 711 231 914 141 51 2096 866 #isEnabled: 8 #(false) 2096 866 #text: 34 1 8 'Add' 2096 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 99 1 0 0 115 0 0 0 169 1 0 0 140 0 0 0] 8 #() 1024 nil 29 410 ##(Smalltalk.GroupBox) 34 14 nil 544 34 2 8 1140850695 65 2464 nil 720 nil 5 nil nil nil 2464 nil 8 1921221004 802 138 144 34 2 866 #createAt:extent: 34 2 914 41 21 914 841 271 2464 866 #text: 34 1 8 'Create New Param' 2464 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 20 0 0 0 10 0 0 0 184 1 0 0 145 0 0 0] 8 #() 1024 nil 27 1024 nil 27 nil nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 2752 nil nil nil 5 nil nil nil 2752 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() false 170 192 34 2 410 ##(Smalltalk.ListView) 34 30 nil 2752 34 2 8 1409355853 1025 2912 590662 2 ##(Smalltalk.ListModel) 138 144 2864 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 720 nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 1 984134 2 ##(Smalltalk.CommandMenuItem) 1 2178 #removeParam 8 'Remove Param' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 2912 nil 8 1921377647 459270 ##(Smalltalk.Message) #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 2 920646 5 ##(Smalltalk.ListViewColumn) 8 'Method Name' 501 #left 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 4 1 3200 8 'doIt' 8 '[:each | (each subStrings: ''>>'') first]' 8 #[32 105 17 29 177 160 106] 8 '>>' #subStrings: #first 3376 7 257 nil ##(Smalltalk.SortedCollection) nil nil 2912 nil 1 nil nil 3314 8 'Parameters' 441 #left 3362 0 nil 3394 4 1 3200 8 'doIt' 8 '[:each | (each subStrings: ''>>'') last]' 8 #[32 105 17 29 177 160 106] 8 '>>' #subStrings: #last 3520 7 257 nil 3202 #<= 8 #() nil nil 2912 nil 1 nil nil #report 2864 nil 131169 nil 8 #(nil) 802 138 144 34 3 866 #createAt:extent: 34 2 914 1 1 914 951 481 2912 866 #contextMenu: 34 1 3072 2912 866 #text: 34 1 8 'Method Name' 2912 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 219 1 0 0 240 0 0 0] 8 #() 1024 nil 27 8 'params' nil 802 138 144 34 1 866 #createAt:extent: 34 2 914 1 311 914 951 481 2752 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 155 0 0 0 219 1 0 0 139 1 0 0] 34 1 2912 1024 nil 27 170 192 2864 nil 802 138 144 34 1 866 #createAt:extent: 34 2 914 2719 21 914 951 791 416 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 79 5 0 0 10 0 0 0 42 7 0 0 149 1 0 0] 34 2 544 2752 1024 nil 27 )! !
!JadeParamAutocompletationPresenter class categoriesFor: #resource_Default_view!public!resources-views! !


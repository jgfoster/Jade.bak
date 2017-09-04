"Filed out from Dolphin Smalltalk 7"!

JadeValueDialog subclass: #JadeMigrateClassDialog
	instanceVariableNames: 'copyMethodsPresenter recompileSubclassesPresenter migrateInstancesPresenter removeFromClassHistoryPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeMigrateClassDialog guid: (GUID fromString: '{f275bc44-a982-4ec0-a934-3d9c87a9296d}')!
JadeMigrateClassDialog comment: ''!
!JadeMigrateClassDialog categoriesForClass!Unclassified! !
!JadeMigrateClassDialog methodsFor!

createComponents

	super createComponents.
	copyMethodsPresenter 				:= self add: BooleanPresenter	new name: 'copyMethods'.
	recompileSubclassesPresenter 			:= self add: BooleanPresenter	new name: 'recompileSubclasses'.
	migrateInstancesPresenter 			:= self add: BooleanPresenter	new name: 'migrateInstances'.
	removeFromClassHistoryPresenter	:= self add: BooleanPresenter	new name: 'removeFromClassHistory'.
!

ok

	self model value: (Dictionary new
		at: #copyMethods 						put: copyMethodsPresenter value;
		at: #recompileSubclasses			put: recompileSubclassesPresenter value;
		at: #migrateInstances 				put: migrateInstancesPresenter value;
		at: #removeFromClassHistory	put: removeFromClassHistoryPresenter value;
		yourself).
	super ok.
!

onViewOpened

	super onViewOpened.
	copyMethodsPresenter 				value: true.
	recompileSubclassesPresenter 			value: true.
	migrateInstancesPresenter 			value: false.
	removeFromClassHistoryPresenter value: false.
! !
!JadeMigrateClassDialog categoriesFor: #createComponents!public! !
!JadeMigrateClassDialog categoriesFor: #ok!public! !
!JadeMigrateClassDialog categoriesFor: #onViewOpened!public! !

!JadeMigrateClassDialog class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.DialogView) 34 30 nil nil 8 #(13107200 65536) 416 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 167 nil nil nil 416 788230 ##(Smalltalk.BorderLayout) 1 1 nil nil nil nil nil 170 192 34 8 410 ##(Smalltalk.CheckBox) 34 16 nil 416 34 2 8 1140924419 1 576 721990 2 ##(Smalltalk.ValueHolder) nil false 1114118 ##(Smalltalk.NeverSearchPolicy) true 466 496 nil 7 nil nil nil 576 nil 8 4294902195 852486 ##(Smalltalk.NullConverter) nil nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 15 149 882 521 61 576 834 #text: 34 1 8 'Copy methods from old class' 576 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 7 0 0 0 74 0 0 0 11 1 0 0 104 0 0 0] 8 #() 882 193 193 nil 27 8 'copyMethods' 410 ##(Smalltalk.CheckBox) 34 16 nil 416 34 2 8 1140924419 1 1072 642 nil false 688 true 466 496 nil 7 nil nil nil 1072 nil 8 4294902195 738 nil nil nil 770 138 144 34 2 834 #createAt:extent: 34 2 882 15 199 882 521 61 1072 834 #text: 34 1 8 'Recompile all subclasses' 1072 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 7 0 0 0 99 0 0 0 11 1 0 0 129 0 0 0] 8 #() 1040 nil 27 8 'recompileSubclasses' 410 ##(Smalltalk.CheckBox) 34 16 nil 416 34 2 8 1140924419 1 1424 642 nil false 688 true 466 496 nil 7 nil nil nil 1424 nil 8 4294902195 738 nil nil nil 770 138 144 34 2 834 #createAt:extent: 34 2 882 15 299 882 531 61 1424 834 #text: 34 1 8 'Remove old classes from history' 1424 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 7 0 0 0 149 0 0 0 16 1 0 0 179 0 0 0] 8 #() 1040 nil 27 8 'removeFromClassHistory' 410 ##(Smalltalk.CheckBox) 34 16 nil 416 34 2 8 1140924419 1 1776 642 nil false 688 true 466 496 nil 7 nil nil nil 1776 nil 8 4294902195 738 nil nil nil 770 138 144 34 2 834 #createAt:extent: 34 2 882 15 249 882 521 61 1776 834 #text: 34 1 8 'Migrate all instances (requires Commit)' 1776 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 7 0 0 0 124 0 0 0 11 1 0 0 154 0 0 0] 8 #() 1040 nil 27 8 'migrateInstances' 590342 ##(Smalltalk.Rectangle) 882 21 21 882 21 21 nil nil nil nil 37777 nil nil nil nil 1 nil nil 590598 ##(Smalltalk.Semaphore) nil nil 1 nil 8 2008110560 770 138 144 34 3 834 #createAt:extent: 34 2 882 2879 21 882 571 521 416 834 #text: 34 1 8 'ClassHistory' 416 834 #updateMenuBar 8 #() 416 978 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 188 6 0 0 14 1 0 0] 34 7 410 ##(Smalltalk.StaticText) 34 16 nil 416 34 2 8 1140850944 1 2480 nil nil nil 7 nil nil nil 2480 nil 8 4294902311 738 nil nil nil 770 138 144 34 2 834 #createAt:extent: 34 2 882 11 11 882 481 131 2480 834 #text: 34 1 8 'This new class replaces a pre-existing class with the same name. What additional actions should be taken?' 2480 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 5 0 0 0 245 0 0 0 70 0 0 0] 8 #() 1040 nil 27 576 1072 1776 1424 410 ##(Smalltalk.PushButton) 34 20 nil 416 34 2 8 1140924416 1 2784 nil nil nil 7 nil nil nil 2784 nil 8 4294902195 1180998 4 ##(Smalltalk.CommandDescription) #ok 8 'Proceed' 1 1 nil nil true nil nil nil 770 138 144 34 3 834 #createAt:extent: 34 2 882 241 381 882 141 51 2784 834 #isEnabled: 8 #(false) 2784 834 #text: 34 1 8 'Proceed' 2784 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 120 0 0 0 190 0 0 0 190 0 0 0 215 0 0 0] 8 #() 1040 nil 29 410 ##(Smalltalk.PushButton) 34 20 nil 416 34 2 8 1140924416 1 3152 nil nil nil 7 nil nil nil 3152 nil 8 4294902195 2866 #cancel 8 'Revert' 1 1 nil nil false nil nil nil 770 138 144 34 2 834 #createAt:extent: 34 2 882 391 381 882 141 51 3152 834 #text: 34 1 8 'Revert' 3152 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 190 0 0 0 9 1 0 0 215 0 0 0] 8 #() 1040 nil 29 1040 nil 27 )! !
!JadeMigrateClassDialog class categoriesFor: #resource_Default_view!public!resources-views! !


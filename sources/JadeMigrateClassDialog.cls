"Filed out from Dolphin Smalltalk 7"!

JadeValueDialog subclass: #JadeMigrateClassDialog
	instanceVariableNames: 'copyMethodsPresenter recompileSubclassesPresenter migrateInstancesPresenter removeFromClassHistoryPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeMigrateClassDialog guid: (GUID fromString: '{F275BC44-A982-4EC0-A934-3D9C87A9296D}')!
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

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.DialogView)  98 30 0 0 98 2 26214401 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 167 0 0 0 416 788230 ##(Smalltalk.BorderLayout)  1 1 0 0 0 0 0 234 256 98 8 410 8 ##(Smalltalk.CheckBox)  98 16 0 416 98 2 8 1140924419 1 592 721990 2 ##(Smalltalk.ValueHolder)  0 32 1114118 ##(Smalltalk.NeverSearchPolicy)  16 482 512 0 7 0 0 0 592 0 8 4294903163 852486 ##(Smalltalk.NullConverter)  0 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  11 251 930 491 61 592 866 8 #text: 98 1 8 'Migrate all instances (requires Commit)' 592 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 125 0 0 0 250 0 0 0 155 0 0 0] 98 0 930 193 193 0 27 8 'migrateInstances' 410 608 98 16 0 416 98 2 8 1140924419 1 1136 674 0 32 720 16 482 512 0 7 0 0 0 1136 0 8 4294903163 770 0 0 0 802 202 208 98 2 866 896 98 2 930 11 301 930 511 61 1136 866 992 98 1 8 'Remove old classes from history' 1136 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 150 0 0 0 4 1 0 0 180 0 0 0] 98 0 1104 0 27 8 'removeFromClassHistory' 410 608 98 16 0 416 98 2 8 1140924419 1 1488 674 0 32 720 16 482 512 0 7 0 0 0 1488 0 8 4294903163 770 0 0 0 802 202 208 98 2 866 896 98 2 930 11 151 930 451 61 1488 866 992 98 1 8 'Copy methods from old class' 1488 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 75 0 0 0 230 0 0 0 105 0 0 0] 98 0 1104 0 27 8 'copyMethods' 410 608 98 16 0 416 98 2 8 1140924419 1 1840 674 0 32 720 16 482 512 0 7 0 0 0 1840 0 8 4294903163 770 0 0 0 802 202 208 98 2 866 896 98 2 930 11 201 930 441 61 1840 866 992 98 1 8 'Recompile all subclasses' 1840 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 100 0 0 0 225 0 0 0 130 0 0 0] 98 0 1104 0 27 8 'recompileSubclasses' 590342 ##(Smalltalk.Rectangle)  930 21 21 930 21 21 0 0 0 0 10627 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 0 8 1976922596 802 202 208 98 3 866 896 98 2 930 5119 21 930 541 591 416 866 992 98 1 8 'ClassHistory' 416 866 8 #updateMenuBar 98 0 416 1042 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 13 11 0 0 49 1 0 0] 98 7 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 2560 0 0 0 7 0 0 0 2560 0 8 4294903337 770 0 0 0 802 202 208 98 2 866 896 98 2 930 11 11 930 481 131 2560 866 992 98 1 8 'This new class replaces a pre-existing class with the same name. What additional actions should be taken?' 2560 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 5 0 0 0 245 0 0 0 70 0 0 0] 98 0 1104 0 27 1488 1840 592 1136 410 8 ##(Smalltalk.PushButton)  98 20 0 416 98 2 8 1140924416 1 2880 0 0 0 7 0 0 0 2880 0 8 4294903163 1180998 4 ##(Smalltalk.CommandDescription)  8 #ok 8 'Proceed' 1 1 0 0 16 0 0 0 802 202 208 98 3 866 896 98 2 930 211 461 930 141 51 2880 866 8 #isEnabled: 98 1 32 2880 866 992 98 1 8 'Proceed' 2880 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 105 0 0 0 230 0 0 0 175 0 0 0 255 0 0 0] 98 0 1104 0 29 410 2896 98 20 0 416 98 2 8 1140924416 1 3296 0 0 0 7 0 0 0 3296 0 8 4294903163 2978 8 #cancel 8 'Revert' 1 1 0 0 32 0 0 0 802 202 208 98 3 866 896 98 2 930 371 461 930 141 51 3296 866 3168 98 1 32 3296 866 992 98 1 8 'Revert' 3296 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 185 0 0 0 230 0 0 0 255 0 0 0 255 0 0 0] 98 0 1104 0 29 1104 0 27 )! !
!JadeMigrateClassDialog class categoriesFor: #resource_Default_view!public!resources-views! !


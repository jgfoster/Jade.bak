"Filed out from Dolphin Smalltalk 7"!

JadePresenterA subclass: #JadePreferencesPresenter
	instanceVariableNames: 'autoCommitPresenter autoMigratePresenter codeFontPresenter defaultFontPresenter nativeCodePresenter unimplementedPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadePreferencesPresenter guid: (GUID fromString: '{2f8f4283-5acc-469c-add8-8e3e65dc5c17}')!
JadePreferencesPresenter comment: ''!
!JadePreferencesPresenter categoriesForClass!Unclassified! !
!JadePreferencesPresenter methodsFor!

autoCommitChanged

	self halt.!

autoMigrateChanged

	self halt.!

createComponents

	super createComponents.
	autoCommitPresenter 	:= self add: BooleanPresenter 	new name: 'autoCommit'.
	autoMigratePresenter 	:= self add: BooleanPresenter 	new name: 'autoMigrate'.
	codeFontPresenter 		:= self add: FontPresenter 		new name: 'codeFont'.
	defaultFontPresenter 	:= self add: FontPresenter 		new name: 'defaultFont'.
	nativeCodePresenter 	:= self add: BooleanPresenter 	new name: 'nativeCode'.
	unimplementedPresenter	:= self add: BooleanPresenter	new name: 'unimplemented'.
!

createSchematicWiring

	super createSchematicWiring.
	autoCommitPresenter 	when: #valueChanged send: #autoCommitChanged 	to: self.
	autoMigratePresenter 	when: #valueChanged send: #autoMigrateChanged 	to: self.
	nativeCodePresenter 	when: #valueChanged send: #nativeCodeChanged 	to: self.
	unimplementedPresenter	when: #valueChanged send: #unimplementedChanged	to: self.
!

nativeCodeChanged

	self halt.!

onViewOpened

	super onViewOpened.
	(autoCommitPresenter 	setValue: true; view) refreshContents.
	(autoMigratePresenter 	setValue: true; view) refreshContents.
	(nativeCodePresenter 	setValue: true; view) refreshContents.
	(unimplementedPresenter	setValue: (gciSession serverPerform: #'showUnimplementedMessages'); view) refreshContents.

	codeFontPresenter		value: CodeSourcePresenter codeFont.
	defaultFontPresenter 	value: View desktop font.
!

queryCommand: aCommandQuery

	(#(#'setCodeFont' #'setDefaultFont') includes: aCommandQuery command) ifTrue: [aCommandQuery beEnabled; receiver: self. ^true].
	^super queryCommand: aCommandQuery!

setCodeFont

	| font |
	font := CodeSourcePresenter codeFont 
		ifNil: [FontDialog showModal]
		ifNotNil: [:value | FontDialog showModalOn: value].
	font notNil ifTrue: [
		CodeSourcePresenter codeFont: font.
		codeFontPresenter	value: font.
	].
!

setDefaultFont

	| font |
	font := FontDialog showModalOn: View desktop font.
	font notNil ifTrue: [
		View desktop font: font.
		defaultFontPresenter value: font.
	].
!

unimplementedChanged

	gciSession
		serverPerform: #'showUnimplementedMessages:' 
		with: unimplementedPresenter value! !
!JadePreferencesPresenter categoriesFor: #autoCommitChanged!public! !
!JadePreferencesPresenter categoriesFor: #autoMigrateChanged!public! !
!JadePreferencesPresenter categoriesFor: #createComponents!public! !
!JadePreferencesPresenter categoriesFor: #createSchematicWiring!public! !
!JadePreferencesPresenter categoriesFor: #nativeCodeChanged!public! !
!JadePreferencesPresenter categoriesFor: #onViewOpened!public! !
!JadePreferencesPresenter categoriesFor: #queryCommand:!public! !
!JadePreferencesPresenter categoriesFor: #setCodeFont!public! !
!JadePreferencesPresenter categoriesFor: #setDefaultFont!public! !
!JadePreferencesPresenter categoriesFor: #unimplementedChanged!public! !

!JadePreferencesPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil 328198 ##(Smalltalk.Point) 1201 801 5 nil nil nil 416 nil 170 192 34 16 410 ##(Smalltalk.CheckBox) 34 16 nil 416 34 2 8 1409363203 1 544 721990 2 ##(Smalltalk.ValueHolder) nil nil 1114118 ##(Smalltalk.NeverSearchPolicy) false nil nil 5 nil nil nil 544 nil 8 1568838720 852486 ##(Smalltalk.NullConverter) nil nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 482 11 1 482 227 43 544 786 #text: 34 1 8 'Auto-commit' 544 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 0 0 0 0 118 0 0 0 21 0 0 0] 8 #() 482 193 193 nil 27 8 'autoCommit' 410 ##(Smalltalk.FontView) 34 17 nil 416 34 2 8 1409286400 1 1008 610 nil nil 656 nil nil nil 5 nil nil nil 1008 nil 8 1568836256 690 nil nil nil nil 722 138 144 34 2 786 #createAt:extent: 34 2 482 631 51 482 571 51 1008 786 #text: 34 1 8 'Default' 1008 914 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 59 1 0 0 25 0 0 0 88 2 0 0 50 0 0 0] 8 #() 976 nil 27 8 'codeFont' 410 ##(Smalltalk.FontView) 34 17 nil 416 34 2 8 1409286400 1 1344 610 nil nil 656 nil nil nil 5 nil nil nil 1344 nil 8 1568836256 690 nil nil nil nil 722 138 144 34 2 786 #createAt:extent: 34 2 482 631 1 482 571 51 1344 786 #text: 34 1 8 'Default' 1344 914 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 59 1 0 0 0 0 0 0 88 2 0 0 25 0 0 0] 8 #() 976 nil 27 8 'defaultFont' 410 ##(Smalltalk.CheckBox) 34 16 nil 416 34 2 8 1409363203 1 1680 610 nil nil 656 false nil nil 5 nil nil nil 1680 nil 8 1568838720 690 nil nil nil 722 138 144 34 2 786 #createAt:extent: 34 2 482 11 121 482 421 41 1680 786 #text: 34 1 8 'Show unimplemented messages' 1680 914 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 60 0 0 0 215 0 0 0 80 0 0 0] 8 #() 976 nil 27 8 'unimplemented' 410 ##(Smalltalk.PushButton) 34 20 nil 416 34 2 8 1140924416 1 2016 nil nil nil 5 nil nil nil 2016 nil 8 1568838720 1180998 4 ##(Smalltalk.CommandDescription) #setDefaultFont 8 'Default Font' 1 1 nil nil false nil nil nil 722 138 144 34 2 786 #createAt:extent: 34 2 482 431 1 482 201 51 2016 786 #text: 34 1 8 'Default Font' 2016 914 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 215 0 0 0 0 0 0 0 59 1 0 0 25 0 0 0] 8 #() 976 nil 29 8 'setDefaultFont' 410 ##(Smalltalk.CheckBox) 34 16 nil 416 34 2 8 1409363203 1 2368 610 nil nil 656 false nil nil 5 nil nil nil 2368 nil 8 1568838720 690 nil nil nil 722 138 144 34 2 786 #createAt:extent: 34 2 482 11 81 482 227 43 2368 786 #text: 34 1 8 'Native code' 2368 914 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 40 0 0 0 118 0 0 0 61 0 0 0] 8 #() 976 nil 27 8 'nativeCode' 410 ##(Smalltalk.CheckBox) 34 16 nil 416 34 2 8 1409363203 1 2704 610 nil nil 656 false nil nil 5 nil nil nil 2704 nil 8 1568838720 690 nil nil nil 722 138 144 34 2 786 #createAt:extent: 34 2 482 11 41 482 227 43 2704 786 #text: 34 1 8 'Auto-migrate' 2704 914 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 20 0 0 0 118 0 0 0 41 0 0 0] 8 #() 976 nil 27 8 'autoMigrate' 410 ##(Smalltalk.PushButton) 34 20 nil 416 34 2 8 1140924416 1 3040 nil nil nil 5 nil nil nil 3040 nil 8 1568838720 2098 #setCodeFont 8 'Code Font' 1 1 nil nil false nil nil nil 722 138 144 34 2 786 #createAt:extent: 34 2 482 431 51 482 201 51 3040 786 #text: 34 1 8 'Code Font' 3040 914 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 215 0 0 0 25 0 0 0 59 1 0 0 50 0 0 0] 8 #() 976 nil 29 8 'setCodeFont' nil 722 138 144 34 1 786 #createAt:extent: 34 2 482 5119 21 482 1201 801 416 914 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 87 12 0 0 154 1 0 0] 34 8 544 2704 2368 1680 2016 1344 3040 1008 976 nil 27 )! !
!JadePreferencesPresenter class categoriesFor: #resource_Default_view!public!resources-views! !


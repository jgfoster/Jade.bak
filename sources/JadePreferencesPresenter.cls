"Filed out from Dolphin Smalltalk 7"!

JadePresenterA subclass: #JadePreferencesPresenter
	instanceVariableNames: 'autoCommitPresenter autoMigratePresenter codeFontPresenter debugPresenter defaultFontPresenter nativeCodePresenter unimplementedPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadePreferencesPresenter guid: (GUID fromString: '{2f8f4283-5acc-469c-add8-8e3e65dc5c17}')!
JadePreferencesPresenter comment: ''!
!JadePreferencesPresenter categoriesForClass!Unclassified! !
!JadePreferencesPresenter methodsFor!

autoCommitChanged

	self gciSession isAutoCommit: autoCommitPresenter value.
!

autoMigrateChanged

	self gciSession isAutoMigrate: autoMigratePresenter value.
!

createComponents

	super createComponents.
	autoCommitPresenter 	:= self add: BooleanPresenter 	new name: 'autoCommit'.
	autoMigratePresenter 	:= self add: BooleanPresenter 	new name: 'autoMigrate'.
	codeFontPresenter 		:= self add: FontPresenter 		new name: 'codeFont'.
	debugPresenter			:= self add: BooleanPresenter	new name: 'debug'.
	defaultFontPresenter 		:= self add: FontPresenter 		new name: 'defaultFont'.
	nativeCodePresenter 		:= self add: BooleanPresenter 	new name: 'nativeCode'.
	unimplementedPresenter	:= self add: BooleanPresenter	new name: 'unimplemented'.
!

createSchematicWiring

	super createSchematicWiring.
	autoCommitPresenter 	when: #valueChanged send: #autoCommitChanged 	to: self.
	autoMigratePresenter 	when: #valueChanged send: #autoMigrateChanged 	to: self.
	debugPresenter			when: #valueChanged send: #debugChanged		to: self.
	nativeCodePresenter 		when: #valueChanged send: #nativeCodeChanged 	to: self.
	unimplementedPresenter	when: #valueChanged send: #unimplementedChanged	to: self.
	self parentPresenter view	when: #viewActivated	 send: #update				to: self.!

debugChanged

	GciSession debug: debugPresenter model value.
!

nativeCodeChanged

	gciSession isNativeCode: nativeCodePresenter value.
!

onViewActivated

	| x |
	super onViewOpened.
	codeFontPresenter	value: CodeSourcePresenter codeFont.
	defaultFontPresenter	value: View desktop font.
	self gciSession isAutoCommit
		ifNil: [autoCommitPresenter view disable]
		ifNotNil: [:value | (autoCommitPresenter setValue: value; view) refreshContents].
	self gciSession isAutoMigrate
		ifNil: [autoMigratePresenter view disable] 
		ifNotNil: [:value | (autoMigratePresenter setValue: value; view) refreshContents].
	self gciSession isNativeCode
		ifNil: [nativeCodePresenter view disable] 
		ifNotNil: [:value | (nativeCodePresenter setValue: value; view) refreshContents].
	self gciSession showUnimplementedMessages
		ifNil: [unimplementedPresenter view disable] 
		ifNotNil: [:value | (unimplementedPresenter setValue: value; view) refreshContents].
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
		self class codeFont: font.
		codeFontPresenter value: font.
	].
!

setDefaultFont

	| font |
	font := FontDialog showModalOn: View desktop font.
	font notNil ifTrue: [
		self class defaultFont: font.
		defaultFontPresenter value: font.
	].
!

unimplementedChanged

	self gciSession isShowUnimplementedMessages: unimplementedPresenter value.!

update

	codeFontPresenter	value: CodeSourcePresenter codeFont.
	defaultFontPresenter	value: View desktop font.
	debugPresenter		value: GciSession debugFlag.
	self gciSession updatePreferences.
	self gciSession isAutoCommit
		ifNil: [autoCommitPresenter view disable]
		ifNotNil: [:value | (autoCommitPresenter setValue: value; view) refreshContents].
	self gciSession isAutoMigrate
		ifNil: [autoMigratePresenter view disable] 
		ifNotNil: [:value | (autoMigratePresenter setValue: value; view) refreshContents].
	self gciSession isNativeCode
		ifNil: [nativeCodePresenter view disable] 
		ifNotNil: [:value | (nativeCodePresenter setValue: value; view) refreshContents].
	self gciSession isShowUnimplementedMessages
		ifNil: [unimplementedPresenter view disable] 
		ifNotNil: [:value | (unimplementedPresenter setValue: value; view) refreshContents].
! !
!JadePreferencesPresenter categoriesFor: #autoCommitChanged!public! !
!JadePreferencesPresenter categoriesFor: #autoMigrateChanged!public! !
!JadePreferencesPresenter categoriesFor: #createComponents!public! !
!JadePreferencesPresenter categoriesFor: #createSchematicWiring!public! !
!JadePreferencesPresenter categoriesFor: #debugChanged!public! !
!JadePreferencesPresenter categoriesFor: #nativeCodeChanged!public! !
!JadePreferencesPresenter categoriesFor: #onViewActivated!public! !
!JadePreferencesPresenter categoriesFor: #queryCommand:!public! !
!JadePreferencesPresenter categoriesFor: #setCodeFont!public! !
!JadePreferencesPresenter categoriesFor: #setDefaultFont!public! !
!JadePreferencesPresenter categoriesFor: #unimplementedChanged!public! !
!JadePreferencesPresenter categoriesFor: #update!public! !

!JadePreferencesPresenter class methodsFor!

codeFont

	^CodeSourcePresenter codeFont
!

codeFont: aFont

	CodeSourcePresenter codeFont: aFont.
!

defaultFont

	^View desktop font
!

defaultFont: aFont

	View desktop font: aFont.
!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil 328198 ##(Smalltalk.Point) 1201 801 5 nil nil nil 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 22 410 ##(Smalltalk.PushButton) 34 20 nil 416 34 2 8 1140924416 1 576 nil nil nil 5 nil nil nil 576 nil 8 1631818816 1180998 4 ##(Smalltalk.CommandDescription) #setCodeFont 8 'Code Font' 1 1 nil nil false nil nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 482 21 101 482 201 51 576 770 #text: 34 1 8 'Code Font' 576 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 50 0 0 0 110 0 0 0 75 0 0 0] 8 #() 482 193 193 nil 29 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 21 1010 #fixedViewLeft 201 1010 #fixedParentTop 101 1010 #fixedViewTop 51 410 ##(Smalltalk.CheckBox) 34 16 nil 416 34 2 8 1409363203 1 1088 721990 2 ##(Smalltalk.ValueHolder) nil nil 1114118 ##(Smalltalk.NeverSearchPolicy) false nil nil 5 nil nil nil 1088 nil 8 1631818816 852486 ##(Smalltalk.NullConverter) nil nil nil 706 138 144 34 2 770 #createAt:extent: 34 2 482 521 51 482 227 43 1088 770 #text: 34 1 8 'Debug' 1088 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 1 0 0 25 0 0 0 117 1 0 0 46 0 0 0] 8 #() 960 nil 27 978 1024 521 1040 227 1056 51 1072 43 410 ##(Smalltalk.FontView) 34 17 nil 416 34 2 8 1409286400 1 1488 1154 nil nil 1200 nil nil nil 5 nil nil nil 1488 nil 8 1631816352 1234 nil nil nil nil 706 138 144 34 2 770 #createAt:extent: 34 2 482 221 51 482 301 51 1488 770 #text: 34 1 8 'Default' 1488 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 110 0 0 0 25 0 0 0 4 1 0 0 50 0 0 0] 8 #() 960 nil 27 978 1010 #fixedPreviousRight 1 1040 301 1056 51 1072 51 410 ##(Smalltalk.GroupBox) 34 14 nil 416 34 2 8 1140850695 65 1840 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 5 nil nil nil 1840 nil 8 1631818816 706 138 144 34 2 770 #createAt:extent: 34 2 482 11 171 482 1181 221 1840 770 #text: 34 1 8 'Current Session' 1840 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 85 0 0 0 83 2 0 0 195 0 0 0] 8 #() 960 nil 27 978 1024 11 1010 #fixedParentRight -9 1056 171 1072 221 410 ##(Smalltalk.CheckBox) 34 16 nil 416 34 2 8 1409363203 1 2208 1154 nil nil 1200 false nil nil 5 nil nil nil 2208 nil 8 1631818816 1234 nil nil nil 706 138 144 34 2 770 #createAt:extent: 34 2 482 21 291 482 227 43 2208 770 #text: 34 1 8 'Native code' 2208 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 145 0 0 0 123 0 0 0 166 0 0 0] 8 #() 960 nil 27 978 1024 21 1040 227 1056 291 1072 43 410 ##(Smalltalk.FontView) 34 17 nil 416 34 2 8 1409286400 1 2544 1154 nil nil 1200 nil nil nil 5 nil nil nil 2544 nil 8 1631816352 1234 nil nil nil nil 706 138 144 34 2 770 #createAt:extent: 34 2 482 221 101 482 301 51 2544 770 #text: 34 1 8 'Default' 2544 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 110 0 0 0 50 0 0 0 4 1 0 0 75 0 0 0] 8 #() 960 nil 27 978 1824 1 1040 301 1056 101 1072 51 410 ##(Smalltalk.PushButton) 34 20 nil 416 34 2 8 1140924416 1 2880 nil nil nil 7 nil nil nil 2880 nil 8 1631818816 658 #setDefaultFont 8 'Default Font' 1 1 nil nil false nil nil nil 706 138 144 34 2 770 #createAt:extent: 34 2 482 21 51 482 201 51 2880 770 #text: 34 1 8 'Default Font' 2880 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 25 0 0 0 110 0 0 0 50 0 0 0] 8 #() 960 nil 29 978 1024 21 1040 201 1056 51 1072 51 410 ##(Smalltalk.GroupBox) 34 14 nil 416 34 2 8 1140850695 65 3216 nil 1906 1936 nil 7 nil nil nil 3216 nil 8 1631818816 706 138 144 34 2 770 #createAt:extent: 34 2 482 11 11 482 1181 151 3216 770 #text: 34 1 8 'All Sessions' 3216 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 5 0 0 0 83 2 0 0 80 0 0 0] 8 #() 960 nil 27 978 1024 11 2192 -9 1056 11 1072 151 410 ##(Smalltalk.CheckBox) 34 16 nil 416 34 2 8 1409363203 1 3536 1154 nil nil 1200 false nil nil 5 nil nil nil 3536 nil 8 1631818816 1234 nil nil nil 706 138 144 34 2 770 #createAt:extent: 34 2 482 21 251 482 227 43 3536 770 #text: 34 1 8 'Auto-migrate' 3536 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 125 0 0 0 123 0 0 0 146 0 0 0] 8 #() 960 nil 27 978 1024 21 1040 227 1056 251 1072 43 410 ##(Smalltalk.CheckBox) 34 16 nil 416 34 2 8 1409363203 1 3872 1154 nil nil 1200 false nil nil 5 nil nil nil 3872 nil 8 1631818816 1234 nil nil nil 706 138 144 34 2 770 #createAt:extent: 34 2 482 21 331 482 421 41 3872 770 #text: 34 1 8 'Show unimplemented messages' 3872 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 165 0 0 0 220 0 0 0 185 0 0 0] 8 #() 960 nil 27 978 1024 21 1040 421 1056 331 1072 41 410 ##(Smalltalk.CheckBox) 34 16 nil 416 34 2 8 1409363203 1 4208 1154 nil nil 1200 false nil nil 5 nil nil nil 4208 nil 8 1631818816 1234 nil nil nil 706 138 144 34 2 770 #createAt:extent: 34 2 482 21 211 482 227 43 4208 770 #text: 34 1 8 'Auto-commit' 4208 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 105 0 0 0 123 0 0 0 126 0 0 0] 8 #() 960 nil 27 978 1024 21 1040 227 1056 211 1072 43 170 192 34 18 2880 8 'setDefaultFont' 2544 8 'codeFont' 2208 8 'nativeCode' 1488 8 'defaultFont' 4208 8 'autoCommit' 576 8 'setCodeFont' 3872 8 'unimplemented' 1088 8 'debug' 3536 8 'autoMigrate' nil 706 138 144 34 1 770 #createAt:extent: 34 2 482 5119 21 482 1201 801 416 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 87 12 0 0 154 1 0 0] 34 11 3216 2880 1488 576 2544 1840 4208 3536 2208 3872 1088 960 nil 27 )! !
!JadePreferencesPresenter class categoriesFor: #codeFont!public! !
!JadePreferencesPresenter class categoriesFor: #codeFont:!public! !
!JadePreferencesPresenter class categoriesFor: #defaultFont!public! !
!JadePreferencesPresenter class categoriesFor: #defaultFont:!public! !
!JadePreferencesPresenter class categoriesFor: #resource_Default_view!public!resources-views! !


"Filed out from Dolphin Smalltalk 7"!

Dialog subclass: #MCHttpRepositoryInfoDialog
	instanceVariableNames: 'locationPresenter userPresenter passwordPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCHttpRepositoryInfoDialog guid: (GUID fromString: '{B2A7D8A5-B9A7-4DB2-BD09-28B3289D28F8}')!
MCHttpRepositoryInfoDialog comment: ''!
!MCHttpRepositoryInfoDialog categoriesForClass!Unclassified! !
!MCHttpRepositoryInfoDialog methodsFor!

createComponents

	super createComponents.
	locationPresenter 	:= self add: TextPresenter new name: 'location'.
	userPresenter		:= self add: TextPresenter new name: 'user'.
	passwordPresenter	:= self add: TextPresenter new name: 'password'.
!

createSchematicWiring

	super createSchematicWiring.
	locationPresenter 	when: #'valueChanged' send: #'locationChanged' 		to: self.
	userPresenter 		when: #'valueChanged' send: #'userChanged' 		to: self.
	passwordPresenter	when: #'valueChanged' send: #'passwordChanged' 	to: self.
!

locationChanged

	model location: locationPresenter value.
!

model: anMCHttpRepositoryInfo

	super model: anMCHttpRepositoryInfo.
	model
		aspectValue: #'location';
		aspectValue: #'user';
		aspectValue: #'password';
		yourself.
!

onViewOpened

	super onViewOpened.
	locationPresenter 	value: model location.
	userPresenter 		value: model user.
	passwordPresenter 	value: model password.
!

passwordChanged

	model password: passwordPresenter value.
!

userChanged

	model user: userPresenter value.
! !
!MCHttpRepositoryInfoDialog categoriesFor: #createComponents!private! !
!MCHttpRepositoryInfoDialog categoriesFor: #createSchematicWiring!private! !
!MCHttpRepositoryInfoDialog categoriesFor: #locationChanged!private! !
!MCHttpRepositoryInfoDialog categoriesFor: #model:!private! !
!MCHttpRepositoryInfoDialog categoriesFor: #onViewOpened!private! !
!MCHttpRepositoryInfoDialog categoriesFor: #passwordChanged!private! !
!MCHttpRepositoryInfoDialog categoriesFor: #userChanged!private! !

!MCHttpRepositoryInfoDialog class methodsFor!

icon

	^Icon fromFile: 'icons\GS32x32.ico'.
!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.DialogView)  98 30 0 0 98 2 26214401 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 167 0 0 0 416 788230 ##(Smalltalk.BorderLayout)  1 1 0 410 8 ##(Smalltalk.ReferenceView)  98 14 0 416 98 2 8 1140850688 131073 560 0 0 0 7 0 0 0 560 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.Presenter)  8 #resource_OK_Cancel_button_block 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  21 107 834 729 71 560 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 53 0 0 0 118 1 0 0 88 0 0 0] 98 0 834 193 193 0 27 0 0 0 234 256 98 6 410 8 ##(Smalltalk.TextEdit)  98 16 0 416 98 2 8 1140916352 1025 992 0 482 8 4278190080 0 7 0 0 0 992 0 8 4294902749 852486 ##(Smalltalk.NullConverter)  0 0 1 706 202 208 98 3 770 800 98 2 834 141 11 834 621 41 992 770 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 992 770 8 #isTextModified: 98 1 32 992 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 70 0 0 0 5 0 0 0 124 1 0 0 25 0 0 0] 98 0 944 0 27 8 'location' 410 1008 98 16 0 416 98 2 8 1140916352 1025 1456 0 482 1088 0 7 0 0 0 1456 0 8 4294902749 1122 0 0 1 706 202 208 98 3 770 800 98 2 834 141 61 834 241 39 1456 770 1280 98 1 1314 3 1 3 1456 770 1360 98 1 32 1456 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 70 0 0 0 30 0 0 0 190 0 0 0 49 0 0 0] 98 0 944 0 27 8 'user' 410 1008 98 16 0 416 98 2 8 1140916384 1025 1824 0 482 8 4278190080 0 7 0 0 0 1824 0 8 4294902749 1122 0 0 1 706 202 208 98 3 770 800 98 2 834 521 61 834 241 39 1824 770 1280 98 1 1314 3 1 3 1824 770 1360 98 1 32 1824 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 1 0 0 30 0 0 0 124 1 0 0 49 0 0 0] 98 0 944 0 27 8 'password' 590342 ##(Smalltalk.Rectangle)  834 21 21 834 21 21 0 0 0 0 9249 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 0 8 2118378815 706 202 208 98 3 770 800 98 2 834 2879 21 834 781 261 416 770 8 #text: 98 1 8 'Monticello HTTP Repository' 416 770 8 #updateMenuBar 928 416 882 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 37 7 0 0 140 0 0 0] 98 7 560 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 2576 0 0 0 7 0 0 0 2576 0 8 4294902779 1122 0 0 0 706 202 208 98 2 770 800 98 2 834 11 11 834 131 41 2576 770 2448 98 1 8 'Location:' 2576 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 5 0 0 0 70 0 0 0 25 0 0 0] 98 0 944 0 27 410 2592 98 16 0 416 98 2 8 1140850944 1 2896 0 0 0 7 0 0 0 2896 0 8 4294902779 1122 0 0 0 706 202 208 98 2 770 800 98 2 834 11 61 834 111 41 2896 770 2448 98 1 8 'User:' 2896 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 30 0 0 0 60 0 0 0 50 0 0 0] 98 0 944 0 27 410 2592 98 16 0 416 98 2 8 1140850944 1 3200 0 0 0 7 0 0 0 3200 0 8 4294902779 1122 0 0 0 706 202 208 98 2 770 800 98 2 834 391 61 834 131 41 3200 770 2448 98 1 8 'Password:' 3200 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 30 0 0 0 4 1 0 0 50 0 0 0] 98 0 944 0 27 992 1456 1824 944 0 27 )! !
!MCHttpRepositoryInfoDialog class categoriesFor: #icon!public! !
!MCHttpRepositoryInfoDialog class categoriesFor: #resource_Default_view!public!resources-views! !


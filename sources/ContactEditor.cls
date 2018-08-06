"Filed out from Dolphin Smalltalk 7"!

ContactsShell subclass: #ContactEditor
	instanceVariableNames: 'contact namePresenter titlePresenter phonePresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ContactEditor guid: (GUID fromString: '{8f931be6-a07f-4b11-94dc-52527d28596d}')!
ContactEditor comment: ''!
!ContactEditor categoriesForClass!Unclassified! !
!ContactEditor methodsFor!

attemptLock: aSymbol

	contact
		attemptLock: aSymbol 
		session: model.
!

cancel

	self view close.
!

contact: aContact

	contact := aContact.

!

createComponents

	super createComponents.
	namePresenter 	:= self add: TextPresenter new name: 'name'.
	titlePresenter 	:= self add: TextPresenter new name: 'title'.
	phonePresenter := self add: TextPresenter new name: 'phone'.
!

onViewOpened

	self caption: 'Contact Editor for ' , contact name.
	super onViewOpened.
	self updateFields.
!

readLock

	self attemptLock: #read.
!

save

	contact
		name: 			namePresenter value;
		title:				titlePresenter value;
		phone: 			phonePresenter value;
		saveUsing:	self model;
		yourself.
	self view close.
!

updateFields

	namePresenter 	value: contact name.
	titlePresenter 	value: contact title.
	phonePresenter value: contact phone.
	!

writeLock

	self attemptLock: #write.
! !
!ContactEditor categoriesFor: #attemptLock:!public! !
!ContactEditor categoriesFor: #cancel!public! !
!ContactEditor categoriesFor: #contact:!accessing!public! !
!ContactEditor categoriesFor: #createComponents!public! !
!ContactEditor categoriesFor: #onViewOpened!public! !
!ContactEditor categoriesFor: #readLock!public! !
!ContactEditor categoriesFor: #save!public! !
!ContactEditor categoriesFor: #updateFields!public! !
!ContactEditor categoriesFor: #writeLock!public! !

!ContactEditor class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  621 261 551 0 0 0 416 0 234 256 98 6 410 8 ##(Smalltalk.TextEdit)  98 16 0 416 98 2 8 1140916352 1025 592 0 482 8 4278190080 0 7 0 0 0 592 0 8 4294902817 852486 ##(Smalltalk.NullConverter)  0 0 1 983302 ##(Smalltalk.MessageSequence)  202 208 98 3 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 153 81 530 431 41 592 818 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 592 818 8 #isTextModified: 98 1 32 592 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 76 0 0 0 40 0 0 0 35 1 0 0 60 0 0 0] 98 0 530 193 193 0 27 8 'phone' 410 608 98 16 0 416 98 2 8 1140916352 1025 1136 0 482 688 0 7 0 0 0 1136 0 8 4294902817 722 0 0 1 754 202 208 98 3 818 848 98 2 530 153 41 530 431 41 1136 818 928 98 1 962 3 1 3 1136 818 1008 98 1 32 1136 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 76 0 0 0 20 0 0 0 35 1 0 0 40 0 0 0] 98 0 1104 0 27 8 'title' 410 608 98 16 0 416 98 2 8 1140916352 1025 1504 0 482 688 0 7 0 0 0 1504 0 8 4294902817 722 0 0 1 754 202 208 98 3 818 848 98 2 530 153 1 530 431 41 1504 818 928 98 1 962 3 1 3 1504 818 1008 98 1 32 1504 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 76 0 0 0 0 0 0 0 35 1 0 0 20 0 0 0] 98 0 1104 0 27 8 'name' 0 0 0 0 0 1 0 0 0 0 1 0 0 754 202 208 98 3 818 848 98 2 530 2879 21 530 621 261 416 818 8 #text: 98 1 8 'Contact Editor' 416 818 8 #updateMenuBar 98 0 416 1042 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 213 6 0 0 140 0 0 0] 98 10 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 2144 0 0 0 7 0 0 0 2144 0 8 4294902811 722 0 0 0 754 202 208 98 2 818 848 98 2 530 1 1 530 151 39 2144 818 2000 98 1 8 ' Name:' 2144 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 75 0 0 0 19 0 0 0] 98 0 1104 0 27 410 2160 98 16 0 416 98 2 8 1140850944 1 2464 0 0 0 7 0 0 0 2464 0 8 4294902811 722 0 0 0 754 202 208 98 2 818 848 98 2 530 1 41 530 151 39 2464 818 2000 98 1 8 ' Title:' 2464 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 75 0 0 0 39 0 0 0] 98 0 1104 0 27 410 2160 98 16 0 416 98 2 8 1140850944 1 2768 0 0 0 7 0 0 0 2768 0 8 4294902811 722 0 0 0 754 202 208 98 2 818 848 98 2 530 1 81 530 151 39 2768 818 2000 98 1 8 ' Phone:' 2768 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 40 0 0 0 75 0 0 0 59 0 0 0] 98 0 1104 0 27 1504 1136 592 410 8 ##(Smalltalk.PushButton)  98 20 0 416 98 2 8 1140924416 1 3072 0 0 0 7 0 0 0 3072 0 8 4294902801 1180998 4 ##(Smalltalk.CommandDescription)  8 #readLock 8 'Read Lock' 1 1 0 0 32 0 0 0 754 202 208 98 3 818 848 98 2 530 1 131 530 141 51 3072 818 8 #isEnabled: 98 1 32 3072 818 2000 98 1 8 'Read Lock' 3072 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 65 0 0 0 70 0 0 0 90 0 0 0] 98 0 1104 0 29 410 3088 98 20 0 416 98 2 8 1140924416 1 3488 0 0 0 7 0 0 0 3488 0 8 4294902801 3170 8 #writeLock 8 'Write Lock' 1 1 0 0 32 0 0 0 754 202 208 98 3 818 848 98 2 530 141 131 530 141 51 3488 818 3360 98 1 32 3488 818 2000 98 1 8 'Write Lock' 3488 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 70 0 0 0 65 0 0 0 140 0 0 0 90 0 0 0] 98 0 1104 0 29 410 3088 98 20 0 416 98 2 8 1140924416 1 3856 0 0 0 7 0 0 0 3856 0 8 4294902801 3170 8 #cancel 8 '&Cancel' 1 1 0 0 32 0 0 0 754 202 208 98 3 818 848 98 2 530 301 131 530 141 51 3856 818 3360 98 1 32 3856 818 2000 98 1 8 '&Cancel' 3856 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 150 0 0 0 65 0 0 0 220 0 0 0 90 0 0 0] 98 0 1104 0 29 410 3088 98 20 0 416 98 2 8 1140924416 1 4224 0 0 0 7 0 0 0 4224 0 8 4294902801 3170 8 #save 8 'Save' 1 1 0 0 32 0 0 0 754 202 208 98 3 818 848 98 2 530 441 131 530 141 51 4224 818 3360 98 1 32 4224 818 2000 98 1 8 'Save' 4224 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 220 0 0 0 65 0 0 0 34 1 0 0 90 0 0 0] 98 0 1104 0 29 1104 0 27 )!

showOn: aGciSession 
	^self showOn: Contact new inSession: aGciSession!

showOn: aContact inSession: aGciSession

	^(self on: aGciSession)
		contact: aContact;
		createView: self defaultView; 
		showShell;
		yourself.
! !
!ContactEditor class categoriesFor: #resource_Default_view!public!resources-views! !
!ContactEditor class categoriesFor: #showOn:!public! !
!ContactEditor class categoriesFor: #showOn:inSession:!public! !


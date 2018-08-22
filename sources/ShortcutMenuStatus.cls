"Filed out from Dolphin Smalltalk 7"!

Notification subclass: #ShortcutMenuStatus
	instanceVariableNames: ''
	classVariableNames: 'Status'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ShortcutMenuStatus guid: (GUID fromString: '{141b492e-356c-4349-ac57-28d52e5c564b}')!
ShortcutMenuStatus comment: ''!
!ShortcutMenuStatus categoriesForClass!Kernel-Exception Handling! !
!ShortcutMenuStatus methodsFor!

defaultAction

	Status := tag.
! !
!ShortcutMenuStatus categoriesFor: #defaultAction!public! !

!ShortcutMenuStatus class methodsFor!

isClosed

	^Status = #'stop'!

isOpen

	^Status = #'start'!

reset

	self signalWith: nil.
!

start

	self signalWith: #'start'.
!

status

	^Status
!

stop

	self signalWith: #'stop'.
! !
!ShortcutMenuStatus class categoriesFor: #isClosed!public! !
!ShortcutMenuStatus class categoriesFor: #isOpen!public! !
!ShortcutMenuStatus class categoriesFor: #reset!public! !
!ShortcutMenuStatus class categoriesFor: #start!public! !
!ShortcutMenuStatus class categoriesFor: #status!public! !
!ShortcutMenuStatus class categoriesFor: #stop!public! !


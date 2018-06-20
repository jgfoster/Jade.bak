"Filed out from Dolphin Smalltalk 7"!

JadeShell subclass: #JadeMaximizedCodeShell
	instanceVariableNames: 'maximizedPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeMaximizedCodeShell guid: (GUID fromString: '{6b5d835d-2dd4-4808-a3d9-0231f940b201}')!
JadeMaximizedCodeShell comment: ''!
!JadeMaximizedCodeShell categoriesForClass!Unclassified! !
!JadeMaximizedCodeShell methodsFor!

createComponents

	super createComponents.

	maximizedPresenter := self add: JadeMaximizedCodePresenter new name: 'maximizedPresenter'.!

editSave

	maximizedPresenter parentMethodPresenter parentPresenter editSave!

model: aModel

	super model: aModel.

	maximizedPresenter model: aModel.!

onViewOpened

	super onViewOpened.

	view showMaximized!

parentMethodPresenter: aJadeAutoTextPresenter

	maximizedPresenter ifNil: [^self].

	maximizedPresenter parentMethodPresenter: aJadeAutoTextPresenter!

queryCommand: aCommandQuery 
	"Private - Enter details about a potential command for the receiver into the <CommandQuery>."

	| cmd |

	cmd := aCommandQuery commandSymbol.
	cmd == #editSave 
		ifTrue: 
			[aCommandQuery isEnabled: true; commandDescription acceleratorKey: 4691.
			^true].
	cmd == #saveAndClose 
		ifTrue: 
			[aCommandQuery isEnabled: true; commandDescription acceleratorKey: 4676.
			^true].!

saveAndClose

	self editSave.

	view destroy.!

updateCaption

	self caption: view caption! !
!JadeMaximizedCodeShell categoriesFor: #createComponents!public! !
!JadeMaximizedCodeShell categoriesFor: #editSave!public! !
!JadeMaximizedCodeShell categoriesFor: #model:!public! !
!JadeMaximizedCodeShell categoriesFor: #onViewOpened!public! !
!JadeMaximizedCodeShell categoriesFor: #parentMethodPresenter:!public! !
!JadeMaximizedCodeShell categoriesFor: #queryCommand:!private! !
!JadeMaximizedCodeShell categoriesFor: #saveAndClose!public! !
!JadeMaximizedCodeShell categoriesFor: #updateCaption!public! !

!JadeMaximizedCodeShell class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 721158 ##(Smalltalk.SystemColor)  31 0 39 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 0 32 234 256 98 2 410 8 ##(Smalltalk.ReferenceView)  98 14 0 416 98 2 8 1140850688 131073 608 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 7 0 0 0 608 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.JadeMaximizedCodePresenter)  8 #resource_Default_view 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  1 1 930 1749 1133 608 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 106 3 0 0 54 2 0 0] 560 930 193 193 0 27 8 'maximizedPresenter' 0 0 0 0 0 1 0 0 0 0 1 0 0 802 202 208 98 3 866 896 98 2 930 2719 21 930 1781 1211 416 866 8 #text: 98 1 8 'Maximized Code Presenter' 416 866 8 #updateMenuBar 560 416 978 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 17 5 0 0 10 0 0 0 139 8 0 0 103 2 0 0] 98 1 608 1024 0 27 )! !
!JadeMaximizedCodeShell class categoriesFor: #resource_Default_view!public!resources-views! !


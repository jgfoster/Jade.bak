"Filed out from Dolphin Smalltalk 7"!

JadePresenter subclass: #JadeMaximizedCodePresenter
	instanceVariableNames: 'parentMethodPresenter codePresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeMaximizedCodePresenter guid: (GUID fromString: '{11c3dfda-e53e-4247-bc39-a79ffa963e3d}')!
JadeMaximizedCodePresenter comment: ''!
!JadeMaximizedCodePresenter categoriesForClass!Unclassified! !
!JadeMaximizedCodePresenter methodsFor!

codePresenter
	^codePresenter!

codePresenter: anObject
	codePresenter := anObject!

createComponents

	super createComponents.

	codePresenter := self  add: JadeAutoTextPresenter3 new name: 'code'.!

doesNotUnderstand: aMessage
	"The receiver has not understood aMessage. Most likely this is because it was intended for
	the parentMethodPresenter of the receiver."

	^aMessage forwardTo: parentMethodPresenter!

hasClassSideSelected
	^false!

model: aModel

	super model: aModel.

	codePresenter model: aModel.!

onViewOpened

	super onViewOpened.

	codePresenter view backcolor: (RGB red: 253 green: 222 blue: 181).

	codePresenter view zoomLevel: 2.!

parentMethodPresenter
	^parentMethodPresenter!

parentMethodPresenter: anObject
	parentMethodPresenter := anObject! !
!JadeMaximizedCodePresenter categoriesFor: #codePresenter!accessing!private! !
!JadeMaximizedCodePresenter categoriesFor: #codePresenter:!accessing!private! !
!JadeMaximizedCodePresenter categoriesFor: #createComponents!public! !
!JadeMaximizedCodePresenter categoriesFor: #doesNotUnderstand:!public! !
!JadeMaximizedCodePresenter categoriesFor: #hasClassSideSelected!accessing!public! !
!JadeMaximizedCodePresenter categoriesFor: #model:!public! !
!JadeMaximizedCodePresenter categoriesFor: #onViewOpened!public! !
!JadeMaximizedCodePresenter categoriesFor: #parentMethodPresenter!accessing!private! !
!JadeMaximizedCodePresenter categoriesFor: #parentMethodPresenter:!accessing!private! !

!JadeMaximizedCodePresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ContainerView)  98 15 0 0 98 2 8 1409286144 131073 416 0 0 0 5 0 0 0 416 788230 ##(Smalltalk.BorderLayout)  1 1 410 8 ##(Smalltalk.Toolbar)  98 25 0 416 98 2 8 1409288972 131137 528 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 517 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 328198 ##(Smalltalk.Point)  193 193 0 528 610 640 8 4294903266 234 256 98 0 234 256 98 4 29509 1115910 ##(Smalltalk.ToolbarIconButton)  29509 0 528 1 1180998 4 ##(Smalltalk.CommandDescription)  8 #saveAndClose 8 'Save and Close' 9353 1 263494 3 ##(Smalltalk.Icon)  0 16 1572870 ##(Smalltalk.ImageRelativeFileLocator)  8 'SystemBrowserShell.ico' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy)  8 'dolphindr006.dll' 0 395334 3 ##(Smalltalk.Bitmap)  0 16 0 0 0 0 3 738 33 33 1 29507 1246982 ##(Smalltalk.ToolbarSystemButton)  29507 0 528 1 898 8 #editSave 8 'Save' 9383 1 0 1 17 98 2 1152 880 234 240 98 4 1 1 1104 31 0 1 0 738 33 33 738 45 45 0 656198 1 ##(Smalltalk.FlowLayout)  1 1 1 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 738 1 1 738 1441 51 528 1394 8 #updateSize 816 528 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 208 2 0 0 25 0 0 0] 98 0 738 193 193 0 27 0 0 0 410 8 ##(Smalltalk.ReferenceView)  98 14 0 416 98 2 8 1140850688 131073 1600 0 610 8 4278190080 0 5 0 0 0 1600 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.TextPresenter)  8 #resource_Scintilla_view 0 1330 202 208 98 1 1394 1424 98 2 738 1 51 738 1441 931 1600 1522 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 208 2 0 0 234 1 0 0] 816 1584 0 27 234 256 98 2 1600 8 'code' 0 1330 202 208 98 1 1394 1424 98 2 738 2719 21 738 1441 981 416 1522 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 17 5 0 0 10 0 0 0 225 7 0 0 244 1 0 0] 98 2 528 1600 1584 0 27 )! !
!JadeMaximizedCodePresenter class categoriesFor: #resource_Default_view!public!resources-views! !


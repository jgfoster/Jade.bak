"Filed out from Dolphin Smalltalk 7"!

JadeShell subclass: #JadePreferencesShell2
	instanceVariableNames: 'preferencesPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadePreferencesShell2 guid: (GUID fromString: '{a193b613-cc5f-442d-88b2-85f73b3ce67e}')!
JadePreferencesShell2 comment: ''!
!JadePreferencesShell2 categoriesForClass!Unclassified! !
!JadePreferencesShell2 methodsFor!

createComponents
	"Create the presenters contained by the receiver"


	super createComponents.
	preferencesPresenter := self add: TwinLevelInspector new name: 'preferences'!

model: aListModelOfObjects

	super model: aListModelOfObjects.
	preferencesPresenter model: aListModelOfObjects!

onViewOpened
	super onViewOpened.

	JadePreferenceObject gciSession: gciSession!

shellName

	^'Jade Preferences'.! !
!JadePreferencesShell2 categoriesFor: #createComponents!public! !
!JadePreferencesShell2 categoriesFor: #model:!public! !
!JadePreferencesShell2 categoriesFor: #onViewOpened!public! !
!JadePreferencesShell2 categoriesFor: #shellName!public! !

!JadePreferencesShell2 class methodsFor!

defaultModel

	^OrderedCollection new
		add: JadeAllSessionsPreference;
		add: JadeCurrentSessionPreference;
		asArray;
		yourself!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 786694 ##(Smalltalk.ColorDefault) 8 4278190080 nil 549 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() false 170 192 34 2 410 ##(Smalltalk.ReferenceView) 34 14 nil 416 34 2 8 1140850688 131073 608 nil 480 nil 7 nil nil nil 608 1180230 1 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.TwinLevelInspector) #resource_Default_view nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 1 1 818 1169 723 608 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 105 1 0 0] 560 818 193 193 nil 27 8 'preferences' nil nil nil nil nil 1 nil nil nil nil 1 nil nil 706 138 144 34 2 770 #createAt:extent: 34 2 818 3059 21 818 1201 801 416 770 #updateMenuBar 560 416 866 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 249 5 0 0 10 0 0 0 81 8 0 0 154 1 0 0] 34 1 608 912 nil 27 )! !
!JadePreferencesShell2 class categoriesFor: #defaultModel!public! !
!JadePreferencesShell2 class categoriesFor: #resource_Default_view!public!resources-views! !


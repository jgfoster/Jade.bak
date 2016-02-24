"Filed out from Dolphin Smalltalk 7"!

JadePresenterA subclass: #JadeAutocompletationConfigurationPresenter
	instanceVariableNames: 'isEnabledPresenter logEnabledPresenter filterObjectPresenter filterPrimitivePresenter hideClassVarPresenter hideInstClassVarPresenter hidePoolDictionariesPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeAutocompletationConfigurationPresenter guid: (GUID fromString: '{6841F57C-9954-4A74-8098-0BE96F0070ED}')!
JadeAutocompletationConfigurationPresenter comment: ''!
!JadeAutocompletationConfigurationPresenter categoriesForClass!Unclassified! !
!JadeAutocompletationConfigurationPresenter methodsFor!

createComponents

	super createComponents.

	isEnabledPresenter := self add: BooleanPresenter new name: 'isEnabled'.
	filterObjectPresenter := self add: BooleanPresenter new name: 'filterObject'. 
	filterPrimitivePresenter := self add: BooleanPresenter new name: 'filterPrimitive'.
	hideClassVarPresenter := self add: BooleanPresenter new name: 'hideClassVar'.
	hideInstClassVarPresenter := self add: BooleanPresenter new name: 'hideInstClassVar'.
	hidePoolDictionariesPresenter := self add: BooleanPresenter new name: 'hidePool'.
	logEnabledPresenter := self add: BooleanPresenter new name: 'logEnabled'.!

model: aJadeAutocompletationConfiguration

	super model: aJadeAutocompletationConfiguration.

	isEnabledPresenter model: (aJadeAutocompletationConfiguration aspectValue: #isEnabled).
	filterObjectPresenter model: (aJadeAutocompletationConfiguration aspectValue: #filterObjectMethods).
	filterPrimitivePresenter model: (aJadeAutocompletationConfiguration aspectValue: #filterPrimitiveMethods).

	hideClassVarPresenter model: (aJadeAutocompletationConfiguration aspectValue: #hideClassVars).
	hideInstClassVarPresenter model: (aJadeAutocompletationConfiguration aspectValue: #hideInstClassVars).
	hidePoolDictionariesPresenter model: (aJadeAutocompletationConfiguration aspectValue: #hidePoolDictionaries).

	logEnabledPresenter model: (aJadeAutocompletationConfiguration aspectValue: #logEnabled).


	! !
!JadeAutocompletationConfigurationPresenter categoriesFor: #createComponents!public! !
!JadeAutocompletationConfigurationPresenter categoriesFor: #model:!public! !

!JadeAutocompletationConfigurationPresenter class methodsFor!

defaultModel

	^JadeAutocompletationConfiguration default!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ContainerView)  98 15 0 0 98 2 8 1409286144 131073 416 0 721158 ##(Smalltalk.SystemColor)  31 0 5 0 0 0 416 0 234 256 98 14 410 8 ##(Smalltalk.CheckBox)  98 16 0 416 98 2 8 1409363203 1 560 721990 2 ##(Smalltalk.ValueHolder)  0 0 1114118 ##(Smalltalk.NeverSearchPolicy)  32 0 0 5 0 0 0 560 0 8 4294903817 852486 ##(Smalltalk.NullConverter)  0 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  31 291 882 411 41 560 818 8 #text: 98 1 8 'Hide class variables' 560 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 145 0 0 0 220 0 0 0 165 0 0 0] 98 0 882 193 193 0 27 8 'hideClassVar' 410 576 98 16 0 416 98 2 8 1409363203 1 1088 642 0 0 688 32 0 0 5 0 0 0 1088 0 8 4294903817 722 0 0 0 754 202 208 98 2 818 848 98 2 882 31 211 882 411 41 1088 818 944 98 1 8 'Filter Methods starting with "_"' 1088 994 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 105 0 0 0 220 0 0 0 125 0 0 0] 98 0 1056 0 27 8 'filterPrimitive' 410 576 98 16 0 416 98 2 8 1409363203 1 1424 642 0 0 688 32 0 0 5 0 0 0 1424 0 8 4294903817 722 0 0 0 754 202 208 98 2 818 848 98 2 882 31 441 882 411 41 1424 818 944 98 1 8 'Hide pool dictionaries' 1424 994 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 220 0 0 0 220 0 0 0 240 0 0 0] 98 0 1056 0 27 8 'hidePoolDictionaries' 410 576 98 16 0 416 98 2 8 1409363203 1 1760 642 0 0 688 32 0 0 5 0 0 0 1760 0 8 4294903817 722 0 0 0 754 202 208 98 2 818 848 98 2 882 31 371 882 411 41 1760 818 944 98 1 8 'Hide instance class variables' 1760 994 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 185 0 0 0 220 0 0 0 205 0 0 0] 98 0 1056 0 27 8 'hideInstClassVar' 410 576 98 16 0 416 98 2 8 1409363203 1 2096 642 0 0 688 32 0 0 5 0 0 0 2096 0 8 4294903817 722 0 0 0 754 202 208 98 2 818 848 98 2 882 31 51 882 411 41 2096 818 944 98 1 8 'Autocompletation is enabled' 2096 994 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 25 0 0 0 220 0 0 0 45 0 0 0] 98 0 1056 0 27 8 'isEnabled' 410 576 98 16 0 416 98 2 8 1409363203 1 2432 642 0 0 688 32 0 0 5 0 0 0 2432 0 8 4294903817 722 0 0 0 754 202 208 98 2 818 848 98 2 882 31 131 882 411 41 2432 818 944 98 1 8 'Filter Object Methods' 2432 994 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 65 0 0 0 220 0 0 0 85 0 0 0] 98 0 1056 0 27 8 'filterObject' 410 576 98 16 0 416 98 2 8 1409363203 1 2768 642 0 0 688 32 0 0 5 0 0 0 2768 0 8 4294903817 722 0 0 0 754 202 208 98 2 818 848 98 2 882 31 521 882 381 41 2768 818 944 98 1 8 'Log is enabled' 2768 994 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 4 1 0 0 205 0 0 0 24 1 0 0] 98 0 1056 0 27 8 'logEnabled' 0 754 202 208 98 1 818 848 98 2 882 2731 21 882 511 611 416 994 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 23 5 0 0 10 0 0 0 22 6 0 0 59 1 0 0] 98 7 2096 2432 1088 560 1760 1424 2768 1056 0 27 )! !
!JadeAutocompletationConfigurationPresenter class categoriesFor: #defaultModel!public! !
!JadeAutocompletationConfigurationPresenter class categoriesFor: #resource_Default_view!public!resources-views! !


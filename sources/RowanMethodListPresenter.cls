"Filed out from Dolphin Smalltalk 7"!

JadeMethodListPresenter subclass: #RowanMethodListPresenter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanMethodListPresenter guid: (GUID fromString: '{eae7647f-1178-4328-8c70-76e118a2c731}')!
RowanMethodListPresenter comment: ''!
!RowanMethodListPresenter categoriesForClass!Unclassified! !
!RowanMethodListPresenter methodsFor!

browseMethodsFromString: aString

	| list |
	list := RowanMethodDefinition
		listFromString: aString
		session: self gciSession.
	methodListPresenter list: list asSortedCollection.
	list notEmpty ifTrue: [
		methodListPresenter selectFirstMethod.
	].
!

browseMethodsFromString: aString methodName: methodName
	| list |

	list := RowanMethodDefinition
		listFromString: aString
		session: self gciSession.
	methodListPresenter list: list asSortedCollection.
	list notEmpty ifTrue: [
		methodListPresenter selectFirstMethod.
	].!

browseMethodsFromString: aString methodName: methodName type: browseType
	| list |
	list := RowanMethodDefinition
		listFromString: aString
		session: self gciSession.
	methodListPresenter list: list asSortedCollection.
	list notEmpty ifTrue: [
		methodListPresenter selectFirstMethod.
	].!

createComponents
	methodListPresenter := self add: PackageMethodListPresenter new name: 'methodList'.
	methodSourcePresenter := (self add: RowanMethodSourcePresenter new name: 'methodSource')
				menuTitle: '&Method Source';
				yourself!

list: aCollection
	methodListPresenter list: aCollection
!

methodSourcePresenter

	^methodSourcePresenter!

selectedPackage

	^methodListPresenter selectedMethod packageName!

updateSaveMethodOop: anInteger

	super updateSaveMethodOop: anInteger.
	methodListPresenter primaryPresenter selection!

updateSource
	| gsClass rowanMethodDef gsMethod | 
	(gsClass := self selectedClass)
		ifNotNil: 
			[(rowanMethodDef := methodListPresenter selectedMethod)
				ifNotNil: 
					[rowanMethodDef gsClass ~= gsClass ifTrue: [self error: 'Method not from selected class!!'].
					gsMethod := GsMethod fromOopType: rowanMethodDef oopType session: rowanMethodDef gciSession]].
	gsMethod
		ifNotNil: 
			[methodSourcePresenter gsMethod: gsMethod.
			rowanMethodDef source: gsMethod source.
			self doSearch].
	methodListPresenter selectedMethod ifNil: [^self].
	methodSourcePresenter documentPresenter
		lastGsShape: (self registry getClass: methodListPresenter selectedMethod gsClass name)! !
!RowanMethodListPresenter categoriesFor: #browseMethodsFromString:!public! !
!RowanMethodListPresenter categoriesFor: #browseMethodsFromString:methodName:!public! !
!RowanMethodListPresenter categoriesFor: #browseMethodsFromString:methodName:type:!public! !
!RowanMethodListPresenter categoriesFor: #createComponents!public! !
!RowanMethodListPresenter categoriesFor: #list:!public! !
!RowanMethodListPresenter categoriesFor: #methodSourcePresenter!public! !
!RowanMethodListPresenter categoriesFor: #selectedPackage!public! !
!RowanMethodListPresenter categoriesFor: #updateSaveMethodOop:!public!updating! !
!RowanMethodListPresenter categoriesFor: #updateSource!public!updating! !

!RowanMethodListPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() true 170 192 34 6 410 ##(Smalltalk.ReferenceView) 34 14 nil 416 34 2 8 1140850688 131073 576 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 5 nil nil nil 576 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.RowanMethodSourcePresenter) #resource_Default_view nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 1 415 834 1461 407 576 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 207 0 0 0 218 2 0 0 154 1 0 0] 528 834 193 193 nil 27 8 'methodSource' 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 960 nil 642 8 4278190080 nil 517 nil nil nil 960 722 138 144 34 1 786 #createAt:extent: 34 2 834 1 405 834 1461 11 960 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 202 0 0 0 218 2 0 0 207 0 0 0] 8 #() 928 nil 27 8 'Splitter' 410 ##(Smalltalk.ReferenceView) 34 14 nil 416 34 2 8 1140916224 131073 1232 nil 642 8 4278190080 nil 5 nil nil nil 1232 690 ##(Smalltalk.PackageMethodListPresenter) #resource_Default_view nil 722 138 144 34 1 786 #createAt:extent: 34 2 834 1 1 834 1461 405 1232 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 218 2 0 0 202 0 0 0] 528 928 nil 27 8 'methodList' nil 722 138 144 34 1 786 #createAt:extent: 34 2 834 3839 21 834 1461 821 416 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 89 10 0 0 164 1 0 0] 34 3 1232 960 576 928 nil 27 )! !
!RowanMethodListPresenter class categoriesFor: #resource_Default_view!public!resources-views! !


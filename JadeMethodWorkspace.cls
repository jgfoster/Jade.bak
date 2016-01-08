"Filed out from Dolphin Smalltalk 7"!

MethodWorkspace subclass: #JadeMethodWorkspace
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeMethodWorkspace guid: (GUID fromString: '{5EE96257-340D-413A-AD5C-5804F49CB94F}')!
JadeMethodWorkspace comment: ''!
!JadeMethodWorkspace categoriesForClass!Unclassified! !
!JadeMethodWorkspace methodsFor!

buildParseTree
	| methodClass |
	"methodClass := self parseContext ifNil: [ProtoObject]."
	self clearErrors.
"	self selectedMethod 
		ifNotNil: 
			[:method | 
			method isExpression ifTrue: [^SmalltalkParser parseExpression: method getSource in: methodClass]]."
	^[SmalltalkParser parseMethod: self text in: nil] on: Compiler notificationClass
		do: 
			[:err | 
			compilationErrors addLast: err.
			err resume]!

value
	^self text!

value: aString 
	^self text: aString! !
!JadeMethodWorkspace categoriesFor: #buildParseTree!helpers!private! !
!JadeMethodWorkspace categoriesFor: #value!public! !
!JadeMethodWorkspace categoriesFor: #value:!public! !


"Filed out from Dolphin Smalltalk 7"!

JadeStyler subclass: #JadeExpressionStyler
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeExpressionStyler guid: (GUID fromString: '{055C9EA2-7ED5-4798-A27E-E6A87B195F64}')!
JadeExpressionStyler comment: 'JadeExpressionStyler is a <JadeStyler> specialised for colouring Smalltalk expressions (as opposed to methods) in workspaces.'!
!JadeExpressionStyler categoriesForClass!Unclassified! !
!JadeExpressionStyler methodsFor!

initialBinaryStyle: aStBinarySelectorToken 
	aStBinarySelectorToken value = #'||' 
		ifTrue: 
			[self applyStyle: #tempOpenBar toNext: 1.
			aStBinarySelectorToken start: aStBinarySelectorToken start + 1.
			^#tempCloseBar].
	aStBinarySelectorToken value = #| ifTrue: [^#tempOpenBar].
	^#binaryMessage!

initialKeywordStyle
	^#keywordMessage!

initialUnaryStyle
	^#identifier! !
!JadeExpressionStyler categoriesFor: #initialBinaryStyle:!constants!private! !
!JadeExpressionStyler categoriesFor: #initialKeywordStyle!constants!private! !
!JadeExpressionStyler categoriesFor: #initialUnaryStyle!constants!private! !


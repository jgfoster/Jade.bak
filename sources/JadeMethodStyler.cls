"Filed out from Dolphin Smalltalk 7"!

JadeStyler subclass: #JadeMethodStyler
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeMethodStyler guid: (GUID fromString: '{446D7673-3AE1-40EB-8D06-81127700D496}')!
JadeMethodStyler comment: 'JadeMethodStyler is a <JadeStyler> specialised to colour method definitions. It differs in recognising the method signature, which is not itself a valid Smalltalk expression.'!
!JadeMethodStyler categoriesForClass!Unclassified! !
!JadeMethodStyler methodsFor!

initialBinaryStyle: aStBinarySelectorToken 
	^#binarySelector!

initialKeywordStyle
	^#keywordSelector!

initialLiteralStyle: aStLiteralToken 
	| style |
	style := aStLiteralToken value literalTextStyle.
	^style == #literalPseudo ifTrue: [#unarySelector] ifFalse: [style]!

initialUnaryStyle
	^#unarySelector! !
!JadeMethodStyler categoriesFor: #initialBinaryStyle:!constants!private! !
!JadeMethodStyler categoriesFor: #initialKeywordStyle!constants!private! !
!JadeMethodStyler categoriesFor: #initialLiteralStyle:!helpers!private! !
!JadeMethodStyler categoriesFor: #initialUnaryStyle!constants!private! !


"Filed out from Dolphin Smalltalk 7"!

RBFormatter subclass: #JGFormatter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JGFormatter guid: (GUID fromString: '{BB6283CA-3084-47F9-97DD-1B30DDD12D3A}')!
JGFormatter comment: ''!
!JGFormatter categoriesForClass!System-Compiler-Formatters! !
!JGFormatter methodsFor!

acceptBlockNode: aBlockNode startBlockString: startBlockString endBlockString: endBlockString 
	
	| seqNode multiline formattedBody formatter |
	seqNode := aBlockNode body.
	formatter := self copy
		lineStart: 0;
		yourself.
	formattedBody := formatter format: seqNode.
	multiline := self lineLength + formattedBody size > self maxLineSize or: [formatter isMultiLine].
	codeStream nextPutAll: startBlockString.
	aBlockNode arguments do: [:each | 
			codeStream nextPut: $:.
			self visitNode: each.
			self space].
	aBlockNode arguments notEmpty ifTrue: [codeStream nextPutAll: '| '].
	multiline ifTrue: [self newLine].
	codeStream nextPutAll: formattedBody.
	multiline ifTrue: [codeStream nextPut: $.. self newLine].
	codeStream nextPutAll: endBlockString.
!

acceptMessageNode: aMessageNode 

	| newFormatter code |
	newFormatter := self copy.
	code := newFormatter format: aMessageNode receiver.
	codeStream nextPutAll: code.
	self space.
	newFormatter isMultiLine ifTrue: [lineStart := codeStream position - newFormatter lastLineLength].
	"self indent: (newFormatter isMultiLine ifTrue: [2] ifFalse: [1])
		around: [self formatMessage: aMessageNode cascade: false]."
	self 
		formatMessage: aMessageNode
		cascade: false.
!

formatMethodBodyFor: aMethodNode 

	super formatMethodBodyFor: aMethodNode.
	codeStream cr.
!

formatMethodCommentFor: aNode indentBefore: aBoolean 

	| comments comment |
	comments := aNode source 
				ifNil: [#()]
				ifNotNil: [aNode methodComments].
	comments isEmpty ifTrue: [
			aBoolean ifTrue: [codeStream cr].
			^self.
			].
	comments 
		do: [:each | 
			comment := aNode source 
						copyFrom: each first
						to: each last.
			aBoolean ifTrue: [
					((comment at: 2) == Character cr and: [(comment at: comment size - 1) == Character lf]) 
						ifTrue: [codeStream cr]
						ifFalse: [self newLine].
					].
			codeStream nextPutAll: comment.
			]
		separatedBy: [
			codeStream cr.
			aBoolean ifFalse: [self newLine].
			].
	codeStream cr.
!

formatStatementsFor: aSequenceNode 
	| statements |
	statements := aSequenceNode statements.
	statements isEmpty ifTrue: [^self].
	1 to: statements size - 1
		do: 
			[:i | 
			self visitNode: (statements at: i).
			codeStream nextPut: $..
			self formatStatementCommentFor: (statements at: i).
			self newLine].
	self visitNode: statements last.
	"1 < statements size ifTrue: [codeStream nextPut: $.]."
	aSequenceNode parent 
		ifNotNil: [:parent | (parent isMethod and: [self class periodsAtEndOfMethod]) ifTrue: [codeStream nextPut: $.]].
	self formatStatementCommentFor: statements last.!

indent: anInteger around: aBlock 

	| x |
	x := indent.
	indent := indent + anInteger.
	^aBlock ensure: [indent := indent - anInteger]!

indentWhile: aBlock 
	
	| x |
	x := indent.
	self 
		indent: 1
		around: aBlock.
!

needsParenthesisFor: aNode 
	
	| parent |
	aNode isValue ifFalse: [^false].
	parent := aNode parent.
	parent isNil ifTrue: [^false].
	aNode precedence < parent precedence ifTrue: [^false].
	aNode isAssignment & parent isAssignment ifTrue: [^false].
	aNode isAssignment | aNode isCascade ifTrue: [^true].
	aNode precedence == 0 ifTrue: [^false].
	aNode isMessage ifFalse: [^true].
	aNode precedence = parent precedence ifFalse: [^true].
	aNode isUnary ifTrue: [^false].
	aNode isKeyword ifTrue: [^true].
	parent receiver == aNode ifFalse: [^true].
	^self 
		precedenceOf: parent selector
		greaterThan: aNode selector.
! !
!JGFormatter categoriesFor: #acceptBlockNode:startBlockString:endBlockString:!public! !
!JGFormatter categoriesFor: #acceptMessageNode:!public! !
!JGFormatter categoriesFor: #formatMethodBodyFor:!public! !
!JGFormatter categoriesFor: #formatMethodCommentFor:indentBefore:!public! !
!JGFormatter categoriesFor: #formatStatementsFor:!public! !
!JGFormatter categoriesFor: #indent:around:!public! !
!JGFormatter categoriesFor: #indentWhile:!public! !
!JGFormatter categoriesFor: #needsParenthesisFor:!public! !

!JGFormatter class methodsFor!

initialize
"
	self initialize.
"
	SmalltalkSystem current formatterClass: JGFormatter.
	self maximumArgumentsPerLine: 1.
	self periodsAtEndOfMethod: true.
	self selectorsToStartOnNewLine: #().
!

test
	
	(1 to: 2) do: [:each | 
			each size.
			each + 1
			].
	2 + 3.
! !
!JGFormatter class categoriesFor: #initialize!public! !
!JGFormatter class categoriesFor: #test!public! !


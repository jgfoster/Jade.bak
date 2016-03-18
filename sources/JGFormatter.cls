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

acceptAssignmentNode: anAssignmentNode 
	
	self 
		indent: 0
		around: [
			self visitNode: anAssignmentNode variable.
			codeStream nextPutAll: ' := '.
			self visitNode: anAssignmentNode value].
!

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
	codeStream
		nextPutAll: formattedBody;
		nextPutAll: endBlockString.
!

formatMethodBodyFor: aMethodNode 
	
	super formatMethodBodyFor: aMethodNode.
	codeStream cr.
!

formatMethodCommentFor: aNode indentBefore: aBoolean 
	
	| comments comment newLineAtEnd |
	comments := aNode source 
		ifNil: [#()]
		ifNotNil: [aNode methodComments].
	comments isEmpty ifTrue: 
			[aBoolean ifTrue: [self newLine].
			^self].
	newLineAtEnd := true.
	comments 
		do: 
			[:each | 
			comment := aNode source 
				copyFrom: each first
				to: each last.
			aBoolean ifTrue: 
					[((comment at: 2) == Character cr and: [(comment at: comment size - 1) == Character lf]) 
						ifTrue: 
							[codeStream cr.
							newLineAtEnd := false]
						ifFalse: [self newLine]].
			codeStream nextPutAll: comment]
		separatedBy: 
			[codeStream cr.
			aBoolean ifFalse: [self newLine]].
	newLineAtEnd ifTrue: 
			[aBoolean 
				ifTrue: [codeStream cr]
				ifFalse: [self newLine]].
!

needsParenthesisFor: aNode 
	
	| parent |
	aNode isValue ifFalse: [^false].
	parent := aNode parent.
	parent isNil ifTrue: [^false].
	"(aNode isMessage and: [parent isMessage and: [parent receiver == aNode]]) 
		ifTrue: 
			[| grandparent |
			grandparent := parent parent.
			(grandparent notNil and: [grandparent isCascade]) ifTrue: [^true]]."
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
!JGFormatter categoriesFor: #acceptAssignmentNode:!public! !
!JGFormatter categoriesFor: #acceptBlockNode:startBlockString:endBlockString:!public! !
!JGFormatter categoriesFor: #formatMethodBodyFor:!public! !
!JGFormatter categoriesFor: #formatMethodCommentFor:indentBefore:!public! !
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
			each + 1].
	2 + 3.
! !
!JGFormatter class categoriesFor: #initialize!public! !
!JGFormatter class categoriesFor: #test!public! !


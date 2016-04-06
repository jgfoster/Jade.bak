"Filed out from Dolphin Smalltalk 7"!

TestCase subclass: #JGFormatterTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JGFormatterTest guid: (GUID fromString: '{596AFE05-96A1-4FD0-98C4-83157DF69411}')!
JGFormatterTest comment: ''!
!JGFormatterTest categoriesForClass!SUnit! !
!JGFormatterTest methodsFor!

runExpressionTest: aString

	| before after tree |
	before := aString withCRs.
	tree := RBParser  parseExpression: before.
	after := RBFormatter1  format: tree.
	self  assert: before = after.
!

runMethodTest: aString

	| before after tree |
	before := aString withCRs.
	tree := RBParser  parseMethod: before.
	after := RBFormatter1  format: tree.
	self  assert: before = after.
!

testAssignments

	self
		runExpressionTest: 'x := 3';
		runExpressionTest: 'x := y := z';
		runExpressionTest: '(x := y) size';
		yourself.
!

testBeginningNewLines

	self runMethodTest: 'test\	| x |\	^self size.'!

testBinaryMessage

	self
		runExpressionTest: 'x + y';
		runExpressionTest: 'x + (y + z)';
		runExpressionTest: 'self foo + self bar';
		runExpressionTest: 'x + y + z';
		runExpressionTest: '(x == y) -> (a ++ b)';
		runExpressionTest: '(foo , bar) baz';
		yourself.
!

testBlock

	self 
		runExpressionTest: '[]';
		runExpressionTest: '[x := 3 + 4]';
		runExpressionTest: '[x foo.\b := y + z]';
		runExpressionTest: '[| x y |\x := y]';
		runExpressionTest: '[:x :y | x := y]';
		runExpressionTest: '[:x :y | \x := y.\y := z]';
		yourself.!

testBlockArguments

	self
		runExpressionTest: 'self at: 1 put: [self foo]';
		"runExpressionTest: 'self\	at: 1\	put: [\		self foo: 2.\		z := q\	]';"
		yourself.!

testCascadeKeywordMessages

	self
		runExpressionTest: 'self\	foo;\	at: 3 put: 4;\	baz;\	yourself';
		runExpressionTest: 'self\	bar;\	with: 1\		with: 2\		with: 3';
		yourself.
!

testCascadeMessages

	self
		runExpressionTest: 'self\	foo;\	bar;\	baz;\	yourself';
		runExpressionTest: 'self\	at: 1;\	bar;\	+ 3';
		runExpressionTest: '(self at: 1)\	bar;\	foo';
		runExpressionTest: 'self\	at: 1 bar;\	baz';
		yourself.
!

testIndent

	self runMethodTest: 'test\	^self size.'!

testKeywordMessage

	self
		runExpressionTest: 'self at: x';
		runExpressionTest: 'self at: x put: y';
		runExpressionTest: 'self\	at: x + y\	put: z\	with: self foo';
		runExpressionTest: 'self at: (self at: foo)';
		runExpressionTest: '(self at: foo) at: foo';
		runExpressionTest: '(self at: foo) + z';
		runExpressionTest: '(self at: foo) size';
		yourself.
!

testLiterals

	self
		runExpressionTest: '3';
		runExpressionTest: '#foo';
		runExpressionTest: '#(1 #b ''c'' #[12] #())';
		runExpressionTest: '3.0s + 3.0 = 6.0';
		runExpressionTest: '$ ';
		yourself.
!

testMethod

	self
		runMethodTest: 'test\	^self size.';
		runMethodTest: '+ anInteger\	^self + anInteger.';
		runMethodTest: 'at: anInteger\	^5.';
		runMethodTest: 'at: anInteger\	| x |\	^x + 1.';
		runMethodTest: 'foo\	<primitive: 1>\	^self error.';
		runMethodTest: 'foo: x bar: y baz: z\	^self.';
		yourself.
!

testMethodComment

	self
		runMethodTest: 'test\	"Test Comment"\\	^self size.';
		runMethodTest: 'test\	self foo.	"Comment 1"\	self bar.	"Comment 2"';
		yourself.
!

testPeriodTerminators

	self
		runExpressionTest: 'self yourself.\^self at: 1 put: [self foo].';
		runExpressionTest: 'self foo.\self\	at: 1\	put:\		[self foo: 2.\		z := q].';
		yourself.
!

testReturn

	self  runExpressionTest: '^5'.
!

testSequence

	self
		runExpressionTest: 'self foo.\self at: 1.\x := 3 + 4.';
		runExpressionTest: '| x y |\x := y + 3.\^x.';
		yourself.
!

testUnaryMessage

	self
		runExpressionTest: 'self halt';
		runExpressionTest: 'self halt halt';
		yourself.
! !
!JGFormatterTest categoriesFor: #runExpressionTest:!public! !
!JGFormatterTest categoriesFor: #runMethodTest:!public! !
!JGFormatterTest categoriesFor: #testAssignments!public! !
!JGFormatterTest categoriesFor: #testBeginningNewLines!public! !
!JGFormatterTest categoriesFor: #testBinaryMessage!public! !
!JGFormatterTest categoriesFor: #testBlock!public! !
!JGFormatterTest categoriesFor: #testBlockArguments!public! !
!JGFormatterTest categoriesFor: #testCascadeKeywordMessages!public! !
!JGFormatterTest categoriesFor: #testCascadeMessages!public! !
!JGFormatterTest categoriesFor: #testIndent!public! !
!JGFormatterTest categoriesFor: #testKeywordMessage!public! !
!JGFormatterTest categoriesFor: #testLiterals!public! !
!JGFormatterTest categoriesFor: #testMethod!public! !
!JGFormatterTest categoriesFor: #testMethodComment!public! !
!JGFormatterTest categoriesFor: #testPeriodTerminators!public! !
!JGFormatterTest categoriesFor: #testReturn!public! !
!JGFormatterTest categoriesFor: #testSequence!public! !
!JGFormatterTest categoriesFor: #testUnaryMessage!public! !


| package |
package := Package name: 'Jade Dolphin Developer'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #JadeMethodWorkspace;
	yourself.

package methodNames
	add: #JadeSystemBrowserPresenter -> #newMethodPresenter;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: 'Jade System Browser';
	add: '..\Object Arts\Dolphin\System\Compiler\Smalltalk Parser';
	yourself).

package!

"Class Definitions"!

MethodWorkspace subclass: #JadeMethodWorkspace
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!JadeSystemBrowserPresenter methodsFor!

newMethodPresenter

	^JadeMethodWorkspace new.
! !
!JadeSystemBrowserPresenter categoriesFor: #newMethodPresenter!overrides!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

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

"Binary Globals"!


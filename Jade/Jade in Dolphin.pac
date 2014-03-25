| package |
package := Package name: 'Jade in Dolphin'.
package paxVersion: 1;
	basicComment: ''.

package basicPackageVersion: '0.006'.

package imageStripperBytes: (ByteArray fromBase64String: 'IVNUQiAzIEYPEQAEAAAASmFkZUltYWdlU3RyaXBwZXIAAAAAUgAAAA8AAABKYWRlIGluIERvbHBo
aW5SAAAAJAAAAEphbWVzXEdlbVN0b25lXEphZGVcUnVudGltZVxKYWRlLmV4ZZoAAABSAAAABwAA
AEphZGUgVUlSAAAAEgAAAEphZGVTZXNzaW9uTWFuYWdlcu+/JQAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAA').
package basicScriptAt: #postinstall put: 'JadeLoginShell addToDevelopmentTools.

OopType32Array template.
OopType64Array template.
OopType64 template.
OopType32 template.
GciErrSType32 template.
GciErrSType64 template.
'.
package basicScriptAt: #preuninstall put: 'JadeLoginShell removeFromDevelopmentTools.'.

package classNames
	add: #JadeMethodWorkspace;
	yourself.

package methodNames
	add: #JadeSystemBrowserPresenter -> #newMethodPresenter;
	add: 'JadeLoginShell class' -> #addToDevelopmentTools;
	add: 'JadeLoginShell class' -> #removeFromDevelopmentTools;
	add: 'JadeLoginShell class' -> #toolsFolderIcon;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: 'Jade Deployment';
	add: 'Jade Login';
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

!JadeLoginShell class methodsFor!

addToDevelopmentTools
"
	JadeLoginShell addToDevelopmentTools.
"
	Smalltalk developmentSystem addAdditionalToolsFolderIcon: self toolsFolderIcon.
!

removeFromDevelopmentTools
"
	JadeLoginShell removeFromDevelopmentTools.
"
	Smalltalk developmentSystem removeSystemFolderIcon: self toolsFolderIcon.
!

toolsFolderIcon

	^SmalltalkSystemIcon 
		icon: self icon
		description: 'Jade Login'
		openBlock: [:folder :item | self show].
! !
!JadeLoginShell class categoriesFor: #addToDevelopmentTools!Development Image!public! !
!JadeLoginShell class categoriesFor: #removeFromDevelopmentTools!Development Image!public! !
!JadeLoginShell class categoriesFor: #toolsFolderIcon!Development Image!public! !

!JadeSystemBrowserPresenter methodsFor!

newMethodPresenter

	^1 = 1
		ifTrue: [super newMethodPresenter]
		ifFalse: [JadeMethodWorkspace new].
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

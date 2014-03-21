| package |
package := Package name: 'Jade in Dolphin CE'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #ScintillaIndicatorStyle;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\Object Arts\Dolphin\MVP\Views\Scintilla\Dolphin Scintilla View';
	yourself).

package!

"Class Definitions"!

ScintillaIndicatorDefinition subclass: #ScintillaIndicatorStyle
	instanceVariableNames: 'under name _reserved2'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

ScintillaIndicatorStyle guid: (GUID fromString: '{9615FB38-CC8F-4C76-BFB7-3DF5C88265C2}')!
ScintillaIndicatorStyle comment: ''!
!ScintillaIndicatorStyle categoriesForClass!Unclassified! !
!ScintillaIndicatorStyle methodsFor!

id
	(id isNil and: [name isInteger]) ifTrue: [id := name].
	^id!

initialize
	style := INDIC_PLAIN.
	forecolor := 0.
	under := false!

isUnderText
	^under!

isUnderText: aBoolean 
	under := aBoolean asBoolean.
	self updateViewAttribute: #isUnderText!

printableAttributes
	^(super printableAttributes)
		remove: #style;
		add: #styleName;
		yourself! !
!ScintillaIndicatorStyle categoriesFor: #id!private! !
!ScintillaIndicatorStyle categoriesFor: #initialize!private! !
!ScintillaIndicatorStyle categoriesFor: #isUnderText!public! !
!ScintillaIndicatorStyle categoriesFor: #isUnderText:!public! !
!ScintillaIndicatorStyle categoriesFor: #printableAttributes!private! !

!ScintillaIndicatorStyle class methodsFor!

initialize
	"
		self initialize
	"

	StyleNames := #(#underline #squiggle #tt #hatch #strikeOut #hidden #box #roundBox).
	getMessages := (IdentityDictionary new)
				at: #forecolor: put: SCI_INDICGETFORE;
				at: #style: put: SCI_INDICGETSTYLE;
				at: #isUnderText: put: 2511;	"SCI_INDICGETUNDER;"
				shrink;
				isImmutable: true;
				yourself.
	attributes := (IdentityDictionary new)
				at: #forecolor put: SCI_INDICSETFORE;
				at: #style put: SCI_INDICSETSTYLE;
				at: #isUnderText put: 2510;		"SCI_INDICSETUNDER;"
				shrink;
				isImmutable: true;
				yourself!

new
	^(super new)
		initialize;
		yourself!

publishedAspectsOfInstances
	"Answer a <LookupTable> of the <Aspect>s published by instances of the receiver."

	^(super publishedAspectsOfInstances)
		add: (Aspect color: #forecolor);
		add: (Aspect choice: #styleName from: StyleNames);
		add: (Aspect boolean: #isUnderText);
		add: (Aspect name: #name);
		yourself!

stbConvertFrom: anSTBClassFormat 
	^
	[:vars | 
	| instance |
	instance := self new.
	vars keysAndValuesDo: [:eachKey :eachValue | instance instVarAt: eachKey put: eachValue].
	instance]!

stbVersion
	^1! !
!ScintillaIndicatorStyle class categoriesFor: #initialize!public! !
!ScintillaIndicatorStyle class categoriesFor: #new!public! !
!ScintillaIndicatorStyle class categoriesFor: #publishedAspectsOfInstances!public! !
!ScintillaIndicatorStyle class categoriesFor: #stbConvertFrom:!private! !
!ScintillaIndicatorStyle class categoriesFor: #stbVersion!public! !

"Binary Globals"!


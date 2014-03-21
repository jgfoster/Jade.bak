| package |
package := Package name: 'YAXO DOM'.
package paxVersion: 1;
	basicComment: 'Yaxo: Yet another XML Framework
Authors: Duane Maxwell, Andres Valloud, Michael Rueger
http://www.squeaklet.com/Yax/index.html

Ported from Squeak by Steve Waring
==========
This package contains the DOM Support

==========
Note: This package is not used by Spray and the port has had less testing than "YAXO Base"'.

package basicPackageVersion: '4'.


package classNames
	add: #XMLDocument;
	add: #XMLDOMParser;
	add: #XMLElement;
	add: #XMLException;
	add: #XMLMalformedException;
	add: #XMLNode;
	add: #XMLNodeWithEntities;
	add: #XMLParser;
	add: #XMLPI;
	add: #XMLStringNode;
	add: #XMLWriter;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Dolphin\Jade\Object Arts\Dolphin\Base\Dolphin';
	add: 'YAXO Base';
	yourself).

package!

"Class Definitions"!

Object subclass: #XMLNode
	instanceVariableNames: ''
	classVariableNames: 'CanonicalTable'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #XMLWriter
	instanceVariableNames: 'stream stack scanner canonical'
	classVariableNames: 'XMLTranslation'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Error subclass: #XMLException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
XMLException subclass: #XMLMalformedException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SAXHandler subclass: #XMLDOMParser
	instanceVariableNames: 'entity stack incremental'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
XMLNode subclass: #XMLNodeWithEntities
	instanceVariableNames: 'entities'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
XMLNode subclass: #XMLPI
	instanceVariableNames: 'target data'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
XMLNode subclass: #XMLStringNode
	instanceVariableNames: 'string'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
XMLNodeWithEntities subclass: #XMLDocument
	instanceVariableNames: 'dtd version encoding requiredMarkup'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
XMLNodeWithEntities subclass: #XMLElement
	instanceVariableNames: 'name contents attributes'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
XMLTokenizer subclass: #XMLParser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

XMLNode guid: (GUID fromString: '{F5E6786E-7892-452D-A2C8-37E4DC0D3E55}')!
XMLNode comment: ''!
!XMLNode categoriesForClass!XML-Parser! !
!XMLNode methodsFor!

addContent: contentString
	SAXMalformedException signal: 'Illegal string data.'!

contentsDo: aBlock!

firstTagNamed: aSymbol
	"Return the first encountered node with the specified tag. Pass the message on"

	| answer |
	#swYAXO.
	self contentsDo: 
			[:node | 
			(answer := node firstTagNamed: aSymbol) ifNotNil: 
					[:t | 
					"sw squeak expects monadic"

					^answer]].
	^nil!

firstTagNamed: aSymbol with: aBlock
	"Return the first encountered node with the specified tag that
	allows the block to evaluate to true. Pass the message on"

	| answer |
	#swYAXO.
	self contentsDo: 
			[:node | 
			(answer := node firstTagNamed: aSymbol with: aBlock) ifNotNil: 
					[:t | 
					"sw squeak expects monadic"

					^answer]].
	^nil!

isDocument
	#swYAXO.
	^false!

isProcessingInstruction
	^false!

isTag
	^false!

isText
	^false!

printOn: stream
	#swYAXO.	"sw: changed for debugging"
	^super printOn: stream
	"self printXMLOn: (XMLWriter on: stream)"!

printXMLOn: writer
	self subclassResponsibility!

tagsNamed: aSymbol childrenDo: aOneArgumentBlock
	"Evaluate aOneArgumentBlock for all children who match"

	self contentsDo: [:each | 
		each tagsNamed: aSymbol ifReceiverDo: aOneArgumentBlock]!

tagsNamed: aSymbol childrenDoAndRecurse: aOneArgumentBlock
	"Evaluate aOneArgumentBlock for all children who match and recurse"

	self contentsDo: [:each | 
		each tagsNamed: aSymbol ifReceiverDoAndRecurse: aOneArgumentBlock]!

tagsNamed: aSymbol contentsDo: aBlock
	"Evaluate aBlock for all of the contents of the receiver.
	The receiver has no tag, so pass the message on"

	self contentsDo: [:each | each tagsNamed: aSymbol contentsDo: aBlock]!

tagsNamed: aSymbol do: aOneArgumentBlock
	"Search for nodes with tag aSymbol. When encountered evaluate aOneArgumentBlock"

	self contentsDo: [:each | each tagsNamed: aSymbol do: aOneArgumentBlock]!

tagsNamed: aSymbol ifReceiverDo: aOneArgumentBlock
	"Handled only by XMLTagNode subclass"

!

tagsNamed: aSymbol ifReceiverDoAndRecurse: aOneArgumentBlock
	"Recurse all children"

	self contentsDo: [:each | each tagsNamed: aSymbol ifReceiverDoAndRecurse: aOneArgumentBlock]!

tagsNamed: aSymbol ifReceiverOrChildDo: aOneArgumentBlock
	"Recurse all children"

	self contentsDo: [:each | each tagsNamed: aSymbol ifReceiverDo: aOneArgumentBlock]! !
!XMLNode categoriesFor: #addContent:!accessing!public! !
!XMLNode categoriesFor: #contentsDo:!enumerating!public! !
!XMLNode categoriesFor: #firstTagNamed:!public!searching! !
!XMLNode categoriesFor: #firstTagNamed:with:!public!searching! !
!XMLNode categoriesFor: #isDocument!public!testing! !
!XMLNode categoriesFor: #isProcessingInstruction!public!testing! !
!XMLNode categoriesFor: #isTag!public!testing! !
!XMLNode categoriesFor: #isText!public!testing! !
!XMLNode categoriesFor: #printOn:!printing!public! !
!XMLNode categoriesFor: #printXMLOn:!printing!public! !
!XMLNode categoriesFor: #tagsNamed:childrenDo:!public!searching! !
!XMLNode categoriesFor: #tagsNamed:childrenDoAndRecurse:!public!searching! !
!XMLNode categoriesFor: #tagsNamed:contentsDo:!public!searching! !
!XMLNode categoriesFor: #tagsNamed:do:!public!searching! !
!XMLNode categoriesFor: #tagsNamed:ifReceiverDo:!public!searching! !
!XMLNode categoriesFor: #tagsNamed:ifReceiverDoAndRecurse:!public!searching! !
!XMLNode categoriesFor: #tagsNamed:ifReceiverOrChildDo:!public!searching! !

XMLWriter guid: (GUID fromString: '{D99A57A4-3582-4065-9544-4B3CFA0BA480}')!
XMLWriter comment: ''!
!XMLWriter categoriesForClass!XML-Parser! !
!XMLWriter methodsFor!

attribute: attributeName value: attributeValue
	self stream
		space;
		nextPutAll: attributeName.
	self
		eq;
		putAsXMLString: attributeValue.
	self stream flush!

canonical
	^canonical!

canonical: aBoolean
	canonical := aBoolean!

cdata: aString
	self startCData.
	self stream nextPutAll: aString.
	self endCData!

comment: aString
	self startComment.
	self stream nextPutAll: aString.
	self endComment!

endCData
	self stream nextPutAll: ']]>'!

endComment
	self stream nextPutAll: ' -->'!

endDecl: type
	self endTag!

endDeclaration
	self stream
		cr;
		nextPut: $].
	self endTag!

endEmptyTag: tagName
	self popTag: tagName.
	self stream nextPutAll: '/>'.
	self canonical
		ifFalse: [self stream space]!

endPI
	self stream nextPutAll: '?>'!

endTag
	self stream nextPut: $>.
	"self canonical
		ifFalse: [self stream space]"!

endTag: tagName
	self popTag: tagName.
	self stream
		nextPutAll: '</';
		nextPutAll: tagName.
	self endTag.
!

eq
	self stream nextPut: $=!

initialize
	stack := OrderedCollection new.
	canonical := false!

pcData: aString
	aString do: [:c |
		self stream nextPutAll: (XMLTranslation at: c ifAbsent: [String with: c])].!

pi: piTarget data: piData
	self startPI: piTarget.
	self stream nextPutAll: piData.
	self endPI!

popTag: tagName
	| stackTop |
	stackTop := self stack isEmpty
		ifTrue: ['<empty>']
		ifFalse: [self stack last].
	^stackTop = tagName
		ifTrue: [self stack removeLast]
		ifFalse: [self error: 'Closing tag "' , tagName , '" does not match "' , stackTop]!

pushTag: tagName
	self stack add: tagName!

putAsXMLString: aValue
	self stream nextPut: $".
	self pcData: aValue.
	self stream nextPut: $"!

stack
	^stack!

startCData
	self stream nextPutAll: '<!![CDATA['!

startComment
	self stream nextPutAll: '<-- '!

startDecl: type
	self stream
		nextPutAll: '<!!';
		nextPutAll: type asUppercase;
		space!

startDecl: type named: aString
	self stream
		nextPutAll: '<!!';
		nextPutAll: type asUppercase;
		space;
		nextPutAll: aString;
		space!

startDeclaration: dtdName
	self startDecl: 'DOCTYPE' named: dtdName.
	self stream
		nextPut: $[;
		cr!

startElement: elementName attributeList: attributeList
	self canonical
		ifFalse: [self stream cr].
	self startTag: elementName.
	attributeList keys asSortedCollection do: [:key |
		self attribute: key value: (attributeList at: key)]!

startPI: identifier
	self stream
		nextPutAll: '<?';
		nextPutAll: identifier;
		space!

startTag: tagName
	self stream
		nextPut: $<;
		nextPutAll: tagName.
	"self canonical
		ifFalse: [self stream space]."
	self pushTag: tagName!

stream
	^stream!

stream: aStream
	stream := aStream!

xmlDeclaration: versionString
	self canonical
		ifFalse: [
			self
				startPI: 'XML';
				attribute: 'version' value: versionString;
				endPI]! !
!XMLWriter categoriesFor: #attribute:value:!public!writing xml! !
!XMLWriter categoriesFor: #canonical!accessing!public! !
!XMLWriter categoriesFor: #canonical:!accessing!public! !
!XMLWriter categoriesFor: #cdata:!public!writing xml! !
!XMLWriter categoriesFor: #comment:!public!writing xml! !
!XMLWriter categoriesFor: #endCData!private!tags! !
!XMLWriter categoriesFor: #endComment!private!tags! !
!XMLWriter categoriesFor: #endDecl:!public!writing dtd! !
!XMLWriter categoriesFor: #endDeclaration!public!writing dtd! !
!XMLWriter categoriesFor: #endEmptyTag:!public!writing xml! !
!XMLWriter categoriesFor: #endPI!private!tags! !
!XMLWriter categoriesFor: #endTag!public!writing xml! !
!XMLWriter categoriesFor: #endTag:!public!writing xml! !
!XMLWriter categoriesFor: #eq!private!writing xml! !
!XMLWriter categoriesFor: #initialize!initialize!public! !
!XMLWriter categoriesFor: #pcData:!public!writing xml! !
!XMLWriter categoriesFor: #pi:data:!public!writing xml! !
!XMLWriter categoriesFor: #popTag:!private!tags! !
!XMLWriter categoriesFor: #pushTag:!private!tags! !
!XMLWriter categoriesFor: #putAsXMLString:!private!writing xml! !
!XMLWriter categoriesFor: #stack!accessing!private! !
!XMLWriter categoriesFor: #startCData!private!tags! !
!XMLWriter categoriesFor: #startComment!private!tags! !
!XMLWriter categoriesFor: #startDecl:!public!writing dtd! !
!XMLWriter categoriesFor: #startDecl:named:!public!writing dtd! !
!XMLWriter categoriesFor: #startDeclaration:!public!writing dtd! !
!XMLWriter categoriesFor: #startElement:attributeList:!public!writing xml! !
!XMLWriter categoriesFor: #startPI:!private!tags! !
!XMLWriter categoriesFor: #startTag:!public!writing xml! !
!XMLWriter categoriesFor: #stream!accessing!public! !
!XMLWriter categoriesFor: #stream:!accessing!public! !
!XMLWriter categoriesFor: #xmlDeclaration:!public!writing xml! !

!XMLWriter class methodsFor!

initialize
	"
	XMLWriter initialize
	"

	XMLTranslation := Dictionary new.
	XMLTranslation
		at: Character cr put: '&#13;';
		at: Character lf put: '&#10;';
		at: Character tab put: '&#9;';
		at: $& put: '&amp;';
		at: $< put: '&lt;';
		at: $> put: '&gt;';
		at: $" put: '&quot;'
	"		at: $' put: '&apos;'; "!

on: aStream
	^self basicNew initialize stream: aStream! !
!XMLWriter class categoriesFor: #initialize!class initialization!public! !
!XMLWriter class categoriesFor: #on:!instance creation!public! !

XMLException guid: (GUID fromString: '{C04E95D9-32A2-4004-ACAB-30ED65AC9A4E}')!
XMLException comment: ''!
!XMLException categoriesForClass!XML-Parser! !
XMLMalformedException guid: (GUID fromString: '{2C2CD5A1-C6B8-41F9-9D38-0F4875B92CE6}')!
XMLMalformedException comment: ''!
!XMLMalformedException categoriesForClass!XML-Parser! !
XMLDOMParser guid: (GUID fromString: '{9AA6DF2D-D670-4DA4-AFC3-D47D0003E980}')!
XMLDOMParser comment: ''!
!XMLDOMParser categoriesForClass!XML-Parser! !
!XMLDOMParser methodsFor!

characters: aString
	| newElement |
	newElement := XMLStringNode string: aString.
	self top addContent: newElement.
!

documentAttributes: attributeList
	self document version: (attributeList at: 'version' ifAbsent: [nil]).
	self document encoding: (attributeList at: 'encoding' ifAbsent: [nil]).
	self document requiredMarkup: (attributeList at: 'requiredMarkup' ifAbsent: [nil]).
!

endDocument
	| currentNode |
	#swYAXO.	"added wellformedness check"
	(currentNode := self pop) isDocument 
		ifFalse: [self driver errorExpected: 'document element must be complete'].
	super endDocument!

endElement: elementName
	| currentElement |
	#swYAXO.
	currentElement := self pop.
	"sw: added wellformedness check"
	currentElement isDocument ifTrue: [self driver malformedError: 'Invalid end tag'].
	"sw: added #asString"
	currentElement name asString = elementName 
		ifFalse: 
			[self driver 
				errorExpected: 'End tag "' , elementName , '" doesn''t match "' , currentElement name 
						, '".']!

incremental
	^incremental!

incremental: aBoolean
	incremental := aBoolean!

initialize
	super initialize.
	stack := OrderedCollection new.
	incremental := false!

nextEntity
	| currentTop |
	currentTop := self top.
	[self driver nextEntity isNil
		or: [self top ~~ currentTop]] whileTrue.
	^entity!

nextEntityStart
	[self driver nextEntity.
	self stack isEmpty] whileTrue.
	^entity!

pop
	| oldTop |
	oldTop := self stack removeLast.
	entity := oldTop.
	^oldTop!

processingInstruction: piName data: dataString
	| newElement |
	newElement := XMLPI target: piName data: dataString.
	self top addEntity: newElement!

push: anObject
	self stack add: anObject.
	entity := anObject
!

stack
	^stack!

startDocument
	self document: XMLDocument new.
	self push: self document !

startElement: elementName attributeList: attributeList
	| newElement |
	#swYAXO.
	newElement := XMLElement named: elementName attributes: attributeList.
	self incremental 
		ifFalse: 
			[self stack isEmpty 
				ifFalse: 
					["sw: added check for only one document element"

					| top |
					((top := self top) isDocument and: [top hasDocumentElement]) 
						ifTrue: [self driver errorExpected: 'only one document element'].
					top addEntity: newElement]].
	self push: newElement!

top
	^self stack isEmpty
		ifTrue: [nil]
		ifFalse: [self stack last]! !
!XMLDOMParser categoriesFor: #characters:!content!public! !
!XMLDOMParser categoriesFor: #documentAttributes:!content!public! !
!XMLDOMParser categoriesFor: #endDocument!content!public! !
!XMLDOMParser categoriesFor: #endElement:!content!public! !
!XMLDOMParser categoriesFor: #incremental!accessing!public! !
!XMLDOMParser categoriesFor: #incremental:!accessing!public! !
!XMLDOMParser categoriesFor: #initialize!initialize!public! !
!XMLDOMParser categoriesFor: #nextEntity!parsing!public! !
!XMLDOMParser categoriesFor: #nextEntityStart!parsing!public! !
!XMLDOMParser categoriesFor: #pop!accessing!private! !
!XMLDOMParser categoriesFor: #processingInstruction:data:!content!public! !
!XMLDOMParser categoriesFor: #push:!accessing!private! !
!XMLDOMParser categoriesFor: #stack!accessing!private! !
!XMLDOMParser categoriesFor: #startDocument!content!public! !
!XMLDOMParser categoriesFor: #startElement:attributeList:!content!public! !
!XMLDOMParser categoriesFor: #top!accessing!private! !

!XMLDOMParser class methodsFor!

addressBookXMLWithDTD
	"XMLDOMParser addressBookXMLWithDTD"
	^self parseDocumentFrom: XMLTokenizer addressBookXMLWithDTD readStream!

parseDocumentFrom: aStream
	#swYAXO.	"Note that unlike the super class method, this method answers a XMLDocument"
	^(super parseDocumentFrom: aStream) document! !
!XMLDOMParser class categoriesFor: #addressBookXMLWithDTD!examples!public! !
!XMLDOMParser class categoriesFor: #parseDocumentFrom:!instance creation!public! !

XMLNodeWithEntities guid: (GUID fromString: '{3A3489A9-1660-4041-AD01-203DC0006B3C}')!
XMLNodeWithEntities comment: ''!
!XMLNodeWithEntities categoriesForClass!XML-Parser! !
!XMLNodeWithEntities methodsFor!

addEntity: entity
	self addEntity: entity name value: entity!

addEntity: entityName value: entityValue
	self entities add: entityName->entityValue!

elements
	^self entities collect: [:each | each value]!

entities
	entities ifNil: [entities := OrderedCollection new].
	^entities!

entitiesDo: aBlock
	#swYAXO.
	entities ifNotNil: 
			[:t | 
			"sw squeak expects monadic"

			self entities do: [:each | aBlock value: each key value: each value]]!

entityAt: entityName
	^self entityAt: entityName ifAbsent: [nil]!

entityAt: entityName ifAbsent: aBlock
	^(entities detect: [:each | each key = entityName] ifNone: [^aBlock value]) value!

printXMLOn: writer
	self entitiesDo: [:eName :eValue | eValue printXMLOn: writer]!

topElement
	^self entities first value! !
!XMLNodeWithEntities categoriesFor: #addEntity:!accessing!public! !
!XMLNodeWithEntities categoriesFor: #addEntity:value:!accessing!public! !
!XMLNodeWithEntities categoriesFor: #elements!accessing!public! !
!XMLNodeWithEntities categoriesFor: #entities!accessing!public! !
!XMLNodeWithEntities categoriesFor: #entitiesDo:!enumerating!public! !
!XMLNodeWithEntities categoriesFor: #entityAt:!accessing!public! !
!XMLNodeWithEntities categoriesFor: #entityAt:ifAbsent:!accessing!public! !
!XMLNodeWithEntities categoriesFor: #printXMLOn:!printing!public! !
!XMLNodeWithEntities categoriesFor: #topElement!accessing!public! !

XMLPI guid: (GUID fromString: '{2A41E7A7-E44C-4134-9472-1E0AE3FA1C9C}')!
XMLPI comment: ''!
!XMLPI categoriesForClass!XML-Parser! !
!XMLPI methodsFor!

data
	^data!

data: aString
	data := aString!

isProcessingInstruction
	^true!

name
	#swYAXO.
	^target!

printXMLOn: writer
	writer pi: self target data: self data!

target
	^target!

target: aString
	target := aString! !
!XMLPI categoriesFor: #data!accessing!public! !
!XMLPI categoriesFor: #data:!accessing!public! !
!XMLPI categoriesFor: #isProcessingInstruction!public!testing! !
!XMLPI categoriesFor: #name!accessing!public! !
!XMLPI categoriesFor: #printXMLOn:!printing!public! !
!XMLPI categoriesFor: #target!accessing!public! !
!XMLPI categoriesFor: #target:!accessing!public! !

!XMLPI class methodsFor!

target: targetName data: aString
	^self new
		target: targetName;
		data: aString! !
!XMLPI class categoriesFor: #target:data:!instance creation!public! !

XMLStringNode guid: (GUID fromString: '{456E6E4A-5391-4DCB-8ECC-A504A9619B52}')!
XMLStringNode comment: ''!
!XMLStringNode categoriesForClass!XML-Parser! !
!XMLStringNode methodsFor!

characterData
	^self string!

isText
	^true!

printXMLOn: writer
	writer pcData: self string!

string
	^string ifNil: ['']!

string: aString
	string := aString! !
!XMLStringNode categoriesFor: #characterData!accessing!public! !
!XMLStringNode categoriesFor: #isText!public!testing! !
!XMLStringNode categoriesFor: #printXMLOn:!printing!public! !
!XMLStringNode categoriesFor: #string!accessing!public! !
!XMLStringNode categoriesFor: #string:!accessing!public! !

!XMLStringNode class methodsFor!

string: aString
	^self new string: aString! !
!XMLStringNode class categoriesFor: #string:!instance creation!public! !

XMLDocument guid: (GUID fromString: '{8C7AC663-973D-4B54-9AAE-E08E393B9616}')!
XMLDocument comment: ''!
!XMLDocument categoriesForClass!XML-Parser! !
!XMLDocument methodsFor!

dtd
	^dtd!

dtd: aDTD
	dtd := aDTD!

encoding	
	^encoding!

encoding: aString	
	encoding := aString!

hasDocumentElement
	#swYAXO.
	self entitiesDo: [ :k :v | v isTag ifTrue: [^true]].
	^false
	
!

isDocument
	#swYAXO.
	^true!

printCanonicalOn: aStream

	| writer |
	writer := XMLWriter on: aStream.
	writer canonical: true.
	self printXMLOn: writer!

printXMLOn: writer
	#swYAXO.
	version ifNotNil: 
			[:t | 
			"sw squeak expects monadic"

			writer xmlDeclaration: self version].
	super printXMLOn: writer!

requiredMarkup	
	^requiredMarkup!

requiredMarkup: aString	
	requiredMarkup := aString!

version	
	^version!

version: aString	
	version := aString! !
!XMLDocument categoriesFor: #dtd!accessing!public! !
!XMLDocument categoriesFor: #dtd:!accessing!public! !
!XMLDocument categoriesFor: #encoding!accessing!public! !
!XMLDocument categoriesFor: #encoding:!accessing!public! !
!XMLDocument categoriesFor: #hasDocumentElement!public!testing! !
!XMLDocument categoriesFor: #isDocument!public!testing! !
!XMLDocument categoriesFor: #printCanonicalOn:!printing!public! !
!XMLDocument categoriesFor: #printXMLOn:!printing!public! !
!XMLDocument categoriesFor: #requiredMarkup!accessing!public! !
!XMLDocument categoriesFor: #requiredMarkup:!accessing!public! !
!XMLDocument categoriesFor: #version!accessing!public! !
!XMLDocument categoriesFor: #version:!accessing!public! !

XMLElement guid: (GUID fromString: '{41122550-BC8F-4A1A-8913-AB228B16DC0F}')!
XMLElement comment: ''!
!XMLElement categoriesForClass!XML-Parser! !
!XMLElement methodsFor!

addContent: contentString
	self contents add: contentString!

attributeAt: attributeName
	^self attributeAt: attributeName ifAbsent: [nil]!

attributeAt: attributeName ifAbsent: aBlock
	^self attributes at: attributeName ifAbsent: [^aBlock value]!

attributeAt: attributeName put: attributeValue
	self attributes at: attributeName asSymbol put: attributeValue!

attributes
	^attributes!

characterData
	^self contentString!

contents
	contents ifNil: [contents := OrderedCollection new].
	^contents!

contentsDo: aBlock
	#swYAXO.
	contents ifNotNil: 
			[:t | 
			"sw squeak expects monadic"

			self contents do: [:each | aBlock value: each]]!

contentString
	^(self contents size == 1
		and: [self contents first isKindOf: XMLStringNode])
		ifTrue: [self contents first string]
		ifFalse: ['']!

contentStringAt: entityName
	^(self entityAt: entityName ifAbsent: [^'']) string!

elements
	^super elements , self contents!

firstTagNamed: aSymbol 
	"Return the first encountered node with the specified tag.
	If it is not the receiver, pass the message on"

	self tag == aSymbol ifTrue: [^self].
	^super firstTagNamed: aSymbol !

firstTagNamed: aSymbol with: aBlock
	"Return the first encountered node with the specified tag that allows
	the block to evaluate to true. Pass the message on"

	(self tag == aSymbol and: [aBlock value: self]) ifTrue: [^self].
	^super firstTagNamed: aSymbol with: aBlock.!

isEmpty
	^self entities isEmpty
		and: [self contents isEmpty]!

isTag
	^true!

name
	^name!

name: aString
	name := aString asSymbol!

printXMLOn: writer
	writer startElement: self name attributeList: self attributes.
	(writer canonical not
		and: [self isEmpty and: [self attributes isEmpty not]])
		ifTrue: [writer endEmptyTag: self name]
		ifFalse: [
			writer endTag.
			self contentsDo: [:content | content printXMLOn: writer].
			super printXMLOn: writer.
			writer endTag: self name]!

setAttributes: newAttributes
	attributes := newAttributes!

tag
	^name asSymbol!

tagsNamed: aSymbol contentsDo: aBlock
	"Evaluate aBlock for all of the contents of the receiver
	if the receiver tag equals aSymbol. Pass the message on"

	self tag == aSymbol ifTrue: [self contentsDo: aBlock].
	super tagsNamed: aSymbol contentsDo: aBlock!

tagsNamed: aSymbol do: aOneArgumentBlock
	"If the receiver tag equals aSymbol, evaluate aOneArgumentBlock
	with the receiver. Continue the search"

	self tag == aSymbol ifTrue: [aOneArgumentBlock value: self].
	super tagsNamed: aSymbol do: aOneArgumentBlock!

tagsNamed: aSymbol ifReceiverDo: aOneArgumentBlock
	"If the receiver tag equals aSymbol, evaluate aOneArgumentBlock with the receiver"

	self tag == aSymbol ifTrue: [aOneArgumentBlock value: self]
!

tagsNamed: aSymbol ifReceiverDoAndRecurse: aOneArgumentBlock
	"If the receiver tag equals aSymbol, evaluate aOneArgumentBlock
	with the receiver. Then recurse through all the children"

	self tag == aSymbol ifTrue: [aOneArgumentBlock value: self].
	super tagsNamed: aSymbol ifReceiverDoAndRecurse: aOneArgumentBlock!

tagsNamed: aSymbol ifReceiverOrChildDo: aOneArgumentBlock
	"If the receiver tag equals aSymbol, evaluate aOneArgumentBlock with the receiver.
	For each of the receivers children do the same. Do not go beyond direct children"

	self tag == aSymbol ifTrue: [aOneArgumentBlock value: self].
	super tagsNamed: aSymbol ifReceiverDo: aOneArgumentBlock!

valueFor: aSymbol 
	^self attributes at: aSymbol ifAbsent: ['']!

valueFor: aSymbol ifAbsent: aBlock 
	^self attributes at: aSymbol ifAbsent: aBlock! !
!XMLElement categoriesFor: #addContent:!initialize!public! !
!XMLElement categoriesFor: #attributeAt:!accessing!public! !
!XMLElement categoriesFor: #attributeAt:ifAbsent:!accessing!public! !
!XMLElement categoriesFor: #attributeAt:put:!accessing!public! !
!XMLElement categoriesFor: #attributes!accessing!public! !
!XMLElement categoriesFor: #characterData!accessing!public! !
!XMLElement categoriesFor: #contents!accessing!public! !
!XMLElement categoriesFor: #contentsDo:!enumerating!public! !
!XMLElement categoriesFor: #contentString!accessing!public! !
!XMLElement categoriesFor: #contentStringAt:!accessing!public! !
!XMLElement categoriesFor: #elements!accessing!public! !
!XMLElement categoriesFor: #firstTagNamed:!public!searching! !
!XMLElement categoriesFor: #firstTagNamed:with:!public!searching! !
!XMLElement categoriesFor: #isEmpty!public!testing! !
!XMLElement categoriesFor: #isTag!public!testing! !
!XMLElement categoriesFor: #name!accessing!public! !
!XMLElement categoriesFor: #name:!initialize!public! !
!XMLElement categoriesFor: #printXMLOn:!printing!public! !
!XMLElement categoriesFor: #setAttributes:!initialize!public! !
!XMLElement categoriesFor: #tag!accessing!public! !
!XMLElement categoriesFor: #tagsNamed:contentsDo:!public!searching! !
!XMLElement categoriesFor: #tagsNamed:do:!public!searching! !
!XMLElement categoriesFor: #tagsNamed:ifReceiverDo:!public!searching! !
!XMLElement categoriesFor: #tagsNamed:ifReceiverDoAndRecurse:!public!searching! !
!XMLElement categoriesFor: #tagsNamed:ifReceiverOrChildDo:!public!searching! !
!XMLElement categoriesFor: #valueFor:!accessing!public! !
!XMLElement categoriesFor: #valueFor:ifAbsent:!accessing!public! !

!XMLElement class methodsFor!

named: aString
	^self new name: aString!

named: aString attributes: attributeList
	^self new
		name: aString;
		setAttributes: attributeList! !
!XMLElement class categoriesFor: #named:!instance creation!public! !
!XMLElement class categoriesFor: #named:attributes:!instance creation!public! !

XMLParser guid: (GUID fromString: '{BE04633B-820F-4CFE-B0B1-9545388C6DE8}')!
XMLParser comment: ''!
!XMLParser categoriesForClass!XML-Parser! !
!XMLParser methodsFor!

attribute: aSymbol value: aString
	"This method is called for each attribute/value pair in a start tag"

	^self subclassResponsibility!

beginStartTag: aSymbol asPI: aBoolean
	"This method is called for at the beginning of a start tag.
	The asPI parameter defines whether or not the tag is a 'processing
	instruction' rather than a 'normal' tag."

	^self subclassResponsibility!

endStartTag: aSymbol
	"This method is called at the end of the start tag after all of the
	attributes have been processed"

	^self subclassResponsibility!

endTag: aSymbol
	"This method is called when the parser encounters either an
	end tag or the end of a unary tag"

	^self subclassResponsibility!

handleCData: aString
	self text: aString!

handleEndTag: aString
	self endTag: aString!

handlePCData: aString
	self text: aString!

handleStartTag: tagName attributes: attributes
	self beginStartTag: tagName asPI: false.
	attributes keysAndValuesDo: [:key :value |
		self attribute: key value: value].
	self endStartTag: tagName!

text: aString
	"This method is called for the blocks of text between tags.
	It preserves whitespace, but has all of the enclosed entities expanded"

	^self subclassResponsibility! !
!XMLParser categoriesFor: #attribute:value:!callbacks!public! !
!XMLParser categoriesFor: #beginStartTag:asPI:!callbacks!public! !
!XMLParser categoriesFor: #endStartTag:!callbacks!public! !
!XMLParser categoriesFor: #endTag:!callbacks!public! !
!XMLParser categoriesFor: #handleCData:!handling tokens!public! !
!XMLParser categoriesFor: #handleEndTag:!handling tokens!public! !
!XMLParser categoriesFor: #handlePCData:!handling tokens!public! !
!XMLParser categoriesFor: #handleStartTag:attributes:!handling tokens!public! !
!XMLParser categoriesFor: #text:!callbacks!public! !

"Binary Globals"!


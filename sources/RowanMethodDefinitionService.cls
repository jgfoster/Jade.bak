"Filed out from Dolphin Smalltalk 7"!

RowanDefinitionService subclass: #RowanMethodDefinitionService
	instanceVariableNames: 'source selector methodDefinitions category packageName className meta'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanMethodDefinitionService guid: (GUID fromString: '{f5550fdc-dbb4-4382-af82-88c561c0a9d1}')!
RowanMethodDefinitionService comment: ''!
!RowanMethodDefinitionService categoriesForClass!Kernel-Objects! !
!RowanMethodDefinitionService methodsFor!

clientDefinitionClass

	^RowanMethodDefinition!

compileMethod: methodString behavior: aBehavior user: user category: category
	| compileResult args |
	args := OrderedCollection
				with: methodString
				with: aBehavior
				with: user
				with: category.
	compileResult := transport serverPerform: #compileMethod:behavior:user:inCategory:
				withArguments: args asArray.
	^compileResult!

compileMethod: aBehavior user: user
	| compileResult args |
	args := Array
				with: source
				with: aBehavior
				with: user
				with: category.
	compileResult := transport serverPerform: #compileMethod:behavior:user:inCategory:
				withArguments: args.
	self rowanFixMe. "Handle comple errors"
	transport send: #addOrUpdateMethod to: self remoteService.
	^compileResult!

createSampleMethod
	transport send: #createSampleMethod to: remoteService!

implementorsOf: aString

	^methodDefinitions!

methodDefinitions
	^methodDefinitions!

methodDefinitions: anObject
	methodDefinitions := anObject.
	self trigger: #changed!

packageName
	^packageName!

packageName: anObject
	packageName := anObject!

remoteAccessors
	^#(#source #selector)!

remoteService
	| stream |
	stream := WriteStream new.
	stream
		nextPutAll: 'RowanMethodDefinitionService new ';
		nextPutAll: 'source: ';
		nextPutAll: source;
		cr;
		nextPutAll: ' selector: ';
		nextPutAll: selector;
		cr;
		nextPutAll: ' category: ';
		nextPutAll: category;
		cr;
		nextPutAll: ' className: ';
		nextPutAll: className;
		cr;
		nextPutAll: ' packageName: ';
		nextPutAll: packageName;
		cr; 
		nextPutAll: ' meta: ';  
		nextPutAll: meta printString; 
		cr. 
	^transport executeString: stream contents! !
!RowanMethodDefinitionService categoriesFor: #clientDefinitionClass!private! !
!RowanMethodDefinitionService categoriesFor: #compileMethod:behavior:user:category:!public!remote! !
!RowanMethodDefinitionService categoriesFor: #compileMethod:user:!public!remote! !
!RowanMethodDefinitionService categoriesFor: #createSampleMethod!examples!public! !
!RowanMethodDefinitionService categoriesFor: #implementorsOf:!method accessing!public! !
!RowanMethodDefinitionService categoriesFor: #methodDefinitions!accessing!private! !
!RowanMethodDefinitionService categoriesFor: #methodDefinitions:!accessing!private! !
!RowanMethodDefinitionService categoriesFor: #packageName!accessing!private! !
!RowanMethodDefinitionService categoriesFor: #packageName:!accessing!private! !
!RowanMethodDefinitionService categoriesFor: #remoteAccessors!private! !
!RowanMethodDefinitionService categoriesFor: #remoteService!public!remote! !


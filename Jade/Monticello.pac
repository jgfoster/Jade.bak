| package |
package := Package name: 'Monticello'.
package paxVersion: 1;
	basicComment: ''.

package basicPackageVersion: '0.096'.

package basicScriptAt: #postinstall put: '''Loaded: Monticello'' yourself.'.

package classNames
	add: #MCAddition;
	add: #MCClassDefinition;
	add: #MCDefinition;
	add: #MCDictionaryRepository;
	add: #MCDirectoryRepository;
	add: #MCFileBasedRepository;
	add: #MCFileTreeRepository;
	add: #MCGitHubRepository;
	add: #MCHttpRepository;
	add: #MCHttpRepositoryInfo;
	add: #MCHttpRepositoryInfoDialog;
	add: #MCMethodDefinition;
	add: #MCModification;
	add: #MCModificationTestCase;
	add: #MCOrganizationDefinition;
	add: #MCPackage;
	add: #MCPackageVersion;
	add: #MCPatch;
	add: #MCPatchBrowser;
	add: #MCPatchOperation;
	add: #MCRemoval;
	add: #MCRepository;
	add: #MCRepositoryBrowser;
	add: #MCServerDirectoryRepository;
	add: #MCVersionDialog;
	add: #MCVersionInfo;
	add: #MCVersionName;
	add: #MCWorkingCopy;
	yourself.

package methodNames
	add: #JadeServer -> #_describeMCAddition:on:;
	add: #JadeServer -> #_describeMCClassDefinition:on:;
	add: #JadeServer -> #_describeMCDefinition:on:;
	add: #JadeServer -> #_describeMCMethodDefinition:on:;
	add: #JadeServer -> #_describeMCModification:on:;
	add: #JadeServer -> #_describeMCOrganizationDefinition:on:;
	add: #JadeServer -> #_describeMCRemoval:on:;
	add: #JadeServer -> #_mcDescriptionOfPatch:baseName:alternateName:;
	add: #JadeServer -> #_mcTopazFrom:on:;
	add: #JadeServer -> #authorInitials:;
	add: #JadeServer -> #gsPackagePolicy;
	add: #JadeServer -> #gsPackagePolicyClass;
	add: #JadeServer -> #mcAddHttpRepository:;
	add: #JadeServer -> #mcAddPackage:;
	add: #JadeServer -> #mcAddRepository:toPackage:;
	add: #JadeServer -> #mcAllFileNamesIn:;
	add: #JadeServer -> #mcAllVersionInfoNamesIn:;
	add: #JadeServer -> #mcAllVersionNamesInDict:;
	add: #JadeServer -> #mcCategoryListFor:;
	add: #JadeServer -> #mcClassesInCategory:package:;
	add: #JadeServer -> #mcCreationTemplateFor:;
	add: #JadeServer -> #mcHttpRepository;
	add: #JadeServer -> #mcHttpRepository:user:password:;
	add: #JadeServer -> #mcHttpRepositoryClass;
	add: #JadeServer -> #mcInitials:;
	add: #JadeServer -> #mcInitialsA:;
	add: #JadeServer -> #mcLoadedVersionNames;
	add: #JadeServer -> #mcNewDirectoryRepository:;
	add: #JadeServer -> #mcNewFileTreeRepository:;
	add: #JadeServer -> #mcNewGitHubRepository:;
	add: #JadeServer -> #mcNewServerDirectoryRepository:;
	add: #JadeServer -> #mcPackageClass;
	add: #JadeServer -> #mcPatchFrom:to:inDictionaryRepository:;
	add: #JadeServer -> #mcPatchFrom:to:inFileBasedRepository:;
	add: #JadeServer -> #mcputDefinition:on:;
	add: #JadeServer -> #mcRemovePackage:;
	add: #JadeServer -> #mcRemoveRepository:;
	add: #JadeServer -> #mcRemoveRepository:toPackage:;
	add: #JadeServer -> #mcRepositoryFrom:;
	add: #JadeServer -> #mcRepositoryGroup;
	add: #JadeServer -> #mcRepositoryList;
	add: #JadeServer -> #mcStore:name:message:repository:;
	add: #JadeServer -> #mcTopazFrom:inDictionaryRepository:;
	add: #JadeServer -> #mcTopazFrom:inFileRepository:;
	add: #JadeServer -> #mcUniqueVersionNameFor:;
	add: #JadeServer -> #mcUserAndPasswordInHTTP:;
	add: #JadeServer -> #mcVersionInfoFrom:;
	add: #JadeServer -> #mcVersionInfoFromDictionaryPackageNamed:in:;
	add: #JadeServer -> #mcVersionInfoFromFileNamed:in:;
	add: #JadeServer -> #mcVersionLoad:fromDictionary:autoMigrate:;
	add: #JadeServer -> #mcVersionLoad:fromFile:autoMigrate:;
	add: #JadeServer -> #mcVersionMerge:from:autoMigrate:;
	add: #JadeServer -> #mcVersionNameAndMessageFrom:;
	add: #JadeServer -> #mcwcbWorkingCopies;
	add: #JadeServer -> #mcWorkingCopyClass;
	add: #JadeServer -> #mcWorkingCopyNamed:;
	add: #JadeServer -> #saveWorkingCopy:to:;
	add: #JadeServer32bit -> #mcInitialsA:;
	add: #JadeServer64bit -> #mcInitialsA:;
	add: #JadeServer64bit32 -> #gsPackagePolicy;
	add: #JadeTextDocument -> #jadeBrowseMonticello;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Boolean\Dolphin Boolean Presenter';
	add: '..\Object Arts\Dolphin\MVP\Views\Cards\Dolphin Card Containers';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Choice Prompter';
	add: '..\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\Object Arts\Dolphin\MVP\Dialogs\Common\Dolphin Common Dialogs';
	add: '..\Object Arts\Dolphin\MVP\Models\List\Dolphin List Models';
	add: '..\Object Arts\Dolphin\MVP\Presenters\List\Dolphin List Presenter';
	add: '..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Prompter';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Rich Text Presenter';
	add: '..\Object Arts\Dolphin\MVP\Views\Scintilla\Dolphin Scintilla View';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Text Presenter';
	add: '..\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	add: '..\Object Arts\Dolphin\MVP\Models\Value\Dolphin Value Models';
	add: 'GemStone Objects';
	add: 'GemStone Session';
	add: 'Jade Class Browser';
	add: 'Jade UI Base';
	add: '..\Camp Smalltalk\SUnit\SUnit';
	add: '..\Object Arts\Dolphin\ActiveX\Shell\Windows Shell';
	yourself).

package!

"Class Definitions"!

Object subclass: #MCDefinition
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #MCHttpRepositoryInfo
	instanceVariableNames: 'location user password'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #MCVersionName
	instanceVariableNames: 'name isLoaded isModified'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsObject subclass: #MCPackage
	instanceVariableNames: 'repository versionNames loaded'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsObject subclass: #MCPackageVersion
	instanceVariableNames: 'date time author id ancestors stepChildren message'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsObject subclass: #MCPatch
	instanceVariableNames: 'operations'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsObject subclass: #MCPatchOperation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsObject subclass: #MCRepository
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsObject subclass: #MCVersionInfo
	instanceVariableNames: 'date time author id ancestors stepChildren message'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsObject subclass: #MCWorkingCopy
	instanceVariableNames: 'isModified ancestors repositories'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCPatchOperation subclass: #MCAddition
	instanceVariableNames: 'definition'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCPatchOperation subclass: #MCModification
	instanceVariableNames: 'obsoletion obsoletionMemo obsoletionMemoMarkers obsoletionLines modification modificationMemo modificationMemoMarkers modificationLines'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCPatchOperation subclass: #MCRemoval
	instanceVariableNames: 'definition'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCRepository subclass: #MCDictionaryRepository
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCRepository subclass: #MCFileBasedRepository
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCFileBasedRepository subclass: #MCDirectoryRepository
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCFileBasedRepository subclass: #MCHttpRepository
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCDirectoryRepository subclass: #MCServerDirectoryRepository
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCServerDirectoryRepository subclass: #MCFileTreeRepository
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCFileTreeRepository subclass: #MCGitHubRepository
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCDefinition subclass: #MCClassDefinition
	instanceVariableNames: 'className definition'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCDefinition subclass: #MCMethodDefinition
	instanceVariableNames: 'classIsMeta source category selector className timeStamp'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCDefinition subclass: #MCOrganizationDefinition
	instanceVariableNames: 'categories'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Shell subclass: #MCPatchBrowser
	instanceVariableNames: 'includeIdenticalPresenter operationListPresenter leftTextPresenter leftMemoPresenter rightTextPresenter rightMemoPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Shell subclass: #MCRepositoryBrowser
	instanceVariableNames: 'repositoryListPresenter packageListPresenter versionListPresenter repositoryCreationTemplatePresenter versionNamePresenter versionDatePresenter versionTimePresenter versionAuthorPresenter versionIDPresenter versionAncestorsPresenter versionStepChildrenPresenter versionMessagePresenter loadedVersionNames'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Dialog subclass: #MCHttpRepositoryInfoDialog
	instanceVariableNames: 'locationPresenter userPresenter passwordPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Dialog subclass: #MCVersionDialog
	instanceVariableNames: 'namePresenter messagePresenter repositoryListPresenter httpUserPresenter httpPasswordPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TestCase subclass: #MCModificationTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!JadeServer methodsFor!

_describeMCAddition: anMCAddition on: aStream

	aStream 
		nextPut: $A; tab;
		nextPutAll: (self oopOf: anMCAddition) printString; tab;
		yourself.
	self 
		_describeMCDefinition: anMCAddition definition 
		on: aStream.
!

_describeMCClassDefinition: anMCClassDefinition on: aStream

	| string |
	string := anMCClassDefinition definitionString collect: [:char |
		char = Character lf
			ifTrue: [Character cr]
			ifFalse: [char].
	].
	aStream
		nextPut: $C; tab;
		nextPutAll: string; lf;
		yourself.
!

_describeMCDefinition: anMCDefinition on: aStream

	anMCDefinition isMethodDefinition ifTrue: [
		self 
			_describeMCMethodDefinition: anMCDefinition 
			on: aStream.
		^self.
	].
	anMCDefinition isOrganizationDefinition ifTrue: [
		self 
			_describeMCOrganizationDefinition: anMCDefinition 
			on: aStream.
		^self.
	].
	anMCDefinition isClassDefinition ifTrue: [
		self 
			_describeMCClassDefinition: anMCDefinition 
			on: aStream.
		^self.
	].
	self halt.
!

_describeMCMethodDefinition: anMCMethodDefinition on: aStream

	aStream
		nextPut: $M; tab;
		nextPutAll: anMCMethodDefinition timeStamp; tab;
		nextPutAll: anMCMethodDefinition className; tab;
		nextPutAll: anMCMethodDefinition classIsMeta printString; tab;
		nextPutAll: anMCMethodDefinition category; tab;
		nextPutAll: anMCMethodDefinition selector; tab;
		nextPutAll: anMCMethodDefinition source size printString; tab;
		nextPutAll: anMCMethodDefinition source; lf.
!

_describeMCModification: anMCModification on: aStream

	aStream nextPut: $M; tab;
		nextPutAll: (self oopOf: anMCModification) printString; tab;
		yourself.
	self 
		_describeMCDefinition: anMCModification obsoletion 
		on: aStream.
	self 
		_describeMCDefinition: anMCModification modification 
		on: aStream.
!

_describeMCOrganizationDefinition: anMCOrganizationDefinition on: aStream

	aStream
		nextPut: $O; tab;
		yourself.
	anMCOrganizationDefinition categories do: [:each | 
		aStream nextPutAll: each; tab.
	].
	aStream lf.
!

_describeMCRemoval: anMCRemoval on: aStream

	aStream nextPut: $R; tab;
		nextPutAll: (self oopOf: anMCRemoval) printString; tab;
		yourself.
	self 
		_describeMCDefinition: anMCRemoval definition 
		on: aStream.
!

_mcDescriptionOfPatch: aPatch baseName: aString1 alternateName: aString2

	| stream |
	stream := WriteStream on: String new.
	(self oopOf: aPatch) printOn: stream.
	stream 
		tab; nextPutAll: (aString1 isNil ifTrue: ['loaded'] ifFalse: [aString1]);
		nextPutAll: ' vs. ';
		nextPutAll: (aString2 isNil ifTrue: ['loaded'] ifFalse: [aString2]);
		lf.
	aPatch operations do: [:each | 
		each isAddition 		ifTrue: [self _describeMCAddition: 		each on: stream].
		each isModification 	ifTrue: [self _describeMCModification: 	each on: stream].
		each isRemoval 		ifTrue: [self _describeMCRemoval: 		each on: stream].
	].
	^stream contents.

!

_mcTopazFrom: aSnapshot on: aStream

	| classes dict parents methods queue |
	classes := aSnapshot definitions select: [:each | each isClassDefinition].
	dict := Dictionary new.
	classes do: [:each | 
		| parent myself |
		parent := dict 
			at: each superclassName 
			ifAbsentPut: [nil -> Set new].
		myself := dict
			at: each className
			ifAbsentPut: [nil -> Set new].
		myself key: each.
		parent value add: myself.
	].
	dict := dict reject: [:each | each key isNil].
	parents := dict keys.
	dict copy do: [:each | 
		(parents includes: each key superclassName) ifTrue: [
			dict removeKey: each key className.
		].
	].
	queue := (dict asSortedCollection: [:a :b | a key <= b key]) asOrderedCollection.
	[
		queue notEmpty.
	] whileTrue: [
		| assoc children def |
		assoc := queue removeFirst.
		children := (assoc value asSortedCollection: [:a :b | a key <= b key]) asOrderedCollection.
		queue := children , queue.
		def := assoc key.
		aStream 
			nextPutAll: '!! - ' , def className; lf;
			nextPutAll: '!! - ' , def commentStamp; lf;
			nextPutAll: 'run'; lf;
			nextPutAll: '(' , def superclassName; lf;
			tab; nextPutAll: 'subclass: ' , def className printString; lf;
			tab; nextPutAll: 'instVarNames: #(' , def instanceVariablesString , ')'; lf;
			tab; nextPutAll: 'classVars: #(' , def classVariablesString , ')'; lf;
			tab; nextPutAll: 'classInstVars: #(' , def classInstanceVariablesString , ')'; lf;
			tab; nextPutAll: 'poolDictionaries: #(' , def sharedPoolsString , ')'; lf;
			tab; nextPutAll: 'inDictionary: UserGlobals'; lf;
			tab; nextPutAll: 'instancesInvariant: false'; lf;
			tab; nextPutAll: 'isModifiable: false)'; lf;
			tab; nextPutAll: 'category: ' , def category printString , '.'; lf;
			nextPutAll: 'true.'; lf;
			nextPut: $%; lf;
			yourself.
	].
	methods := aSnapshot definitions select: [:each | each isMethodDefinition].
	methods := methods asSortedCollection.
	classes asSortedCollection do: [:eachClass | 
		| localMethods |
		localMethods := methods select: [:eachMethod | eachClass className = eachMethod className].
		methods removeAll: localMethods.
		aStream
			lf; nextPutAll: '!! - *** - ' , eachClass className; lf;
			nextPutAll: 'removeAllClassMethods ' , eachClass className; lf;
			nextPutAll: 'removeAllMethods ' , eachClass className; lf;
			yourself.
		localMethods do: [:eachMethod | 
			| source |
			source := eachMethod source copyReplaceAll: Character cr asString with: Character lf asString.
			aStream
				nextPutAll: 'category: ''' , eachMethod category , ''''; lf;
				nextPutAll: '!! - ' , eachMethod timeStamp; lf;
				nextPutAll: (eachMethod classIsMeta ifTrue: ['classMethod: '] ifFalse: ['method: ']) , eachMethod className; lf;
				nextPutAll: source; lf;
				nextPut: $%; lf;
				yourself.
		].
	].
	aStream lf; nextPutAll: '!! - *** - loose methods (where class is expected to be already defined)'; lf; lf.
	methods isEmpty ifTrue: [aStream nextPutAll: '!! - (none)'; lf; lf].

	methods do: [:eachMethod | 
		| source |
		source := eachMethod source copyReplaceAll: Character cr asString with: Character lf asString.
		aStream
			nextPutAll: 'category: ''' , eachMethod category , ''''; lf;
			nextPutAll: '!! - ' , eachMethod timeStamp; lf;
			nextPutAll: (eachMethod classIsMeta ifTrue: ['classMethod: '] ifFalse: ['method: ']) , eachMethod className; lf;
			nextPutAll: source; lf;
			nextPut: $%; lf;
			yourself.
	].

	aStream lf; nextPutAll: '!! - *** - class initialization'; lf.
	methods isEmpty ifTrue: [aStream nextPutAll: '!! - (none)'; lf; lf].
	classes do: [:each | 
		aStream nextPutAll: 'send ' , each className , ' initialize'; lf.
	].

!

authorInitials: aString

	| packagePolicy |
	(packagePolicy := self gsPackagePolicy) isNil ifTrue: [^self].
	packagePolicy authorInitials: aString.
!

gsPackagePolicy

	| class |
	class := self gsPackagePolicyClass.
	class isNil ifTrue: [^nil].
	^class current.
!

gsPackagePolicyClass

	^self objectNamed: #'GsPackagePolicy'.
!

mcAddHttpRepository: aString

	| list repositoryClass repository group |
	list := aString subStrings: (Character codePoint: 255).
	(repositoryClass := self mcHttpRepositoryClass) isNil ifTrue: [self error: 'MCHttpRepository not found!!'].
	repository := repositoryClass
		location: (list at: 1)
		user: (list at: 2)
		password: (list at: 3).
	(group := self mcRepositoryGroup) isNil ifTrue: [self error: 'MCRepositoryGroup not found!!'].
	group addRepository: repository.
!

mcAddPackage: aString

	self mcWorkingCopyClass forPackage: (self mcPackageClass named: aString).
!

mcAddRepository: aRepository toPackage: aMCWorkingCopy

	aMCWorkingCopy repositoryGroup addRepository: aRepository.
!

mcAllFileNamesIn: anMCRepository

	| stream |
	stream := WriteStream on: String new.
	anMCRepository allFileNames do: [:each | 
		stream nextPutAll: each; lf.
	].
	^stream contents.
!

mcAllVersionInfoNamesIn: anMCRepository

	| stream |
	stream := WriteStream on: String new.
	anMCRepository allVersionInfos do: [:each | 
		stream nextPutAll: each name; lf.
	].
	^stream contents.
!

mcAllVersionNamesInDict: anMCRepository

	| stream list |
	stream := WriteStream on: String new.
	list := anMCRepository dictionary values.
	list := list asSortedCollection: [:a :b | 
		a package name < b package name or: [
		a package name = b package name and: [
		a info date > b info date or: [
		a info date = b info date and: [
		a info time > b info time
	]]]]].
	stream := WriteStream on: String new.
	list do: [:each | 
		stream nextPutAll: each info name; lf.
	].
	^stream contents.
!

mcCategoryListFor: aMCWorkingCopy

	| categories stream |
	categories := Set new.
	aMCWorkingCopy package packageInfo classes do: [:each | 
		categories add: each _classCategory.
	].
	categories copy do: [:each | 
		1 to: each size do: [:i | 
			(each at: i) = $- ifTrue: [
				| string |
				string := each copyFrom: 1 to: i - 1.
				(categories includes: string) ifFalse: [
					categories add: string.
					self _addToPureExportSet: string.
				].
			].
		].
	].
	stream := WriteStream on: String new.
	categories asSortedCollection do: [:each | 
		(self oopOf: each) printOn: stream.
		stream tab; nextPutAll: each; lf.
	].
	^stream contents.
!

mcClassesInCategory: aString package: aMCWorkingCopy

	| visibleClasses allClasses stream queue |
	visibleClasses := aString isNil ifTrue: [
		aMCWorkingCopy packageInfo classes.
	] ifFalse: [
		aMCWorkingCopy packageInfo classes select: [:each | 
			each _classCategory notNil and: [
			each _classCategory = aString or: [
			aString notNil and: [each _classCategory matchPattern: (Array with: aString with: $*)]]]]
	].
	allClasses := visibleClasses asIdentitySet.
	queue := visibleClasses asOrderedCollection.
	[
		queue notEmpty.
	] whileTrue: [
		| parent |
		parent := queue removeFirst superclass.
		(parent notNil and: [(allClasses includes: parent) not]) ifTrue: [
			queue add: parent.
			allClasses add: parent.
		].
	].
	stream := WriteStream on: String new.
	allClasses do: [:each |
		self
			_addClass: each 
			toStream: stream 
			isVisible: (visibleClasses includes: each)
			fromDictionary: nil.
	].
	^stream contents.
!

mcCreationTemplateFor: anMCRepository

	^anMCRepository asCreationTemplate.
!

mcHttpRepository

	^self objectNamed: #'MCHttpRepository'.
!

mcHttpRepository: aRepository user: userString password: passwordString

	aRepository
		user: userString;
		password: passwordString;
		yourself.
!

mcHttpRepositoryClass

	^self objectNamed: #'MCHttpRepository'.
!

mcInitials: aString

	| mcPlatformSupport string |
	string := 'Jade-' , GsSession currentSession serialNumber printString , '-' , System myUserProfile userId.
	[
		self mcInitialsA: string.
	] whileFalse: [
		string := string copyFrom: 1 to: string size - 1.
	].
	mcPlatformSupport := self objectNamed: #'MCPlatformSupport'.
	mcPlatformSupport notNil ifTrue: [mcPlatformSupport setAuthorInitials: aString].
!

mcInitialsA: aString

	System _cacheName: aString.
!

mcLoadedVersionNames

	| mcWorkingCopyClass stream |
	(mcWorkingCopyClass := self mcWorkingCopyClass) isNil ifTrue: [^nil].
	stream := WriteStream on: String new.
	mcWorkingCopyClass allManagers do: [:each | 
		| packageOrVersion |
		packageOrVersion := each ancestors
			detect: [:ignored | true]
			ifNone: [each package].
		packageOrVersion := packageOrVersion notNil
			ifTrue: [packageOrVersion name]
			ifFalse: [''].
		stream
			nextPutAll: packageOrVersion; tab;
			nextPut: (each modified ifTrue: [$Y] ifFalse: [$N]); tab;
			nextPutAll: each package name;
			lf.
	].
	^stream contents.
!

mcNewDirectoryRepository: aString

	| mcRepositoryClass fileDirectoryClass repository |
	(mcRepositoryClass := self objectNamed: #'MCDirectoryRepository') isNil ifTrue: [self error: 'Monticello not available!!'].
	(fileDirectoryClass := self objectNamed: #'FileDirectory') isNil ifTrue: [self error: 'Monticello not available!!'].
	repository := mcRepositoryClass new directory: (fileDirectoryClass on: aString).
	self mcRepositoryGroup addRepository: repository.
!

mcNewFileTreeRepository: aString

	| mcRepositoryClass fileDirectoryClass repository |
	(mcRepositoryClass := self objectNamed: #'MCFileTreeRepository') isNil ifTrue: [self error: 'Monticello not available!!'].
	(fileDirectoryClass := self objectNamed: #'ServerFileDirectory') isNil ifTrue: [self error: 'Monticello not available!!'].
	repository := mcRepositoryClass new directory: (fileDirectoryClass on: aString).
	self mcRepositoryGroup addRepository: repository.
!

mcNewGitHubRepository: aString

	| mcRepositoryClass fileDirectoryClass repository |
	(mcRepositoryClass := self objectNamed: #'MCGitHubRepository') isNil ifTrue: [self error: 'Monticello not available!!'].
	(fileDirectoryClass := self objectNamed: #'ServerFileDirectory') isNil ifTrue: [self error: 'Monticello not available!!'].
	repository := mcRepositoryClass location: aString.
	self mcRepositoryGroup addRepository: repository.
!

mcNewServerDirectoryRepository: aString

	| mcDirectoryRepositoryClass fileDirectoryClass repository |
	(mcDirectoryRepositoryClass := self objectNamed: #'MCServerDirectoryRepository') isNil ifTrue: [self error: 'Monticello not available!!'].
	(fileDirectoryClass := self objectNamed: #'ServerFileDirectory') isNil ifTrue: [self error: 'Monticello not available!!'].
	repository := mcDirectoryRepositoryClass new directory: (fileDirectoryClass on: aString).
	self mcRepositoryGroup addRepository: repository.
!

mcPackageClass

	^self objectNamed: #'MCPackage'.
!

mcPatchFrom: aString1 to: aString2 inDictionaryRepository: aDictionaryRepository

	| index name leftSnapshot rightSnapshot patch |
	index := aString2 findLast: [:each | each = $-].
	name := aString2 copyFrom: 1 to: index - 1.
	(name includes: $.) ifTrue: [name := (name subStrings: $.) first].
	leftSnapshot := aString1 isNil ifTrue: [
		(self mcWorkingCopyClass allManagers detect: [:each | each package name = name]) package snapshot.
	] ifFalse: [
		(aDictionaryRepository versionFromVersionNamed: aString1) snapshot.
	].
	rightSnapshot := (aDictionaryRepository versionFromVersionNamed: aString2) snapshot.
	patch := rightSnapshot patchRelativeToBase: leftSnapshot.
	^self 
		_mcDescriptionOfPatch: patch
		baseName: aString1
		alternateName: aString2.
!

mcPatchFrom: aString1 to: aString2 inFileBasedRepository: aFileRepository

	| index name leftSnapshot rightSnapshot patch |
	index := aString2 findLast: [:each | each = $-].
	name := aString2 copyFrom: 1 to: index - 1.
	(name includes: $.) ifTrue: [name := (name subStrings: $.) first].
	leftSnapshot := aString1 isNil ifTrue: [
		(self mcWorkingCopyClass allManagers detect: [:each | each package name = name]) package snapshot.
	] ifFalse: [
		(aFileRepository versionFromFileNamed: aString1) snapshot.
	].
	rightSnapshot := (aFileRepository versionFromFileNamed: aString2) snapshot.
	patch := rightSnapshot patchRelativeToBase: leftSnapshot.
	^self 
		_mcDescriptionOfPatch: patch
		baseName: aString1
		alternateName: aString2.
!

mcputDefinition: aDefinition on: aStream

	| mcOrganizationDefinitionClass mcClassDefinitionClass mcMethodDefinitionClass |
	(mcOrganizationDefinitionClass := self objectNamed: 'MCOrganizationDefinition') isNil ifTrue: [^nil].
	(mcClassDefinitionClass := self objectNamed: 'MCClassDefinitionClass') isNil ifTrue: [^nil].
	(mcMethodDefinitionClass := self objectNamed: 'MCMethodDefinition') isNil ifTrue: [^nil].

	self _addToPureExportSet: aDefinition.
		aStream nextPutAll: (self oopOf: aDefinition) printString; tab;
			nextPutAll: aDefinition class name; tab.
			
		aDefinition class == mcOrganizationDefinitionClass ifTrue: [
			aDefinition categories do: [:eachCategory | 
				aStream nextPutAll: eachCategory; space]
		] ifFalse: [	aDefinition class == mcClassDefinitionClass ifTrue: [
			aStream
				nextPutAll: aDefinition className; tab;
				nextPutAll: aDefinition superclassName; tab;
				nextPutAll: aDefinition category; tab;
				nextPutAll: aDefinition type; tab;
				yourself.
		] ifFalse: [aDefinition class == mcMethodDefinitionClass ifTrue: [
			aStream
				nextPutAll: aDefinition classIsMeta printString; tab;
				nextPutAll: aDefinition category; tab;
				nextPutAll: aDefinition selector; tab;
				nextPutAll: aDefinition className; tab;
				nextPutAll: aDefinition timeStamp printString; tab]]].
	
	^aStream.
	!

mcRemovePackage: anMCWorkingCopy

	anMCWorkingCopy unregister.
!

mcRemoveRepository: aRepository

	| repositoryClass group |
	(repositoryClass := self mcHttpRepositoryClass) isNil ifTrue: [self error: 'MCHttpRepository not found!!'].
	(group := self mcRepositoryGroup) isNil ifTrue: [self error: 'MCRepositoryGroup not found!!'].
	group removeRepository: aRepository.
!

mcRemoveRepository: aRepository toPackage: aMCWorkingCopy

	aMCWorkingCopy repositoryGroup removeRepository: aRepository.
!

mcRepositoryFrom: aRepository

	| stream |
	stream := WriteStream on: String new.
	(self oopOf: aRepository) printOn: stream.
	stream 
		tab;
		nextPutAll: aRepository description;
		tab;
		nextPutAll: aRepository class name;
		tab.
	^stream contents.
!

mcRepositoryGroup

	| groupClass |
	(groupClass := self objectNamed: 'MCRepositoryGroup') isNil ifTrue: [^nil].
	^groupClass default.
!

mcRepositoryList

	| group stream |
	(group := self mcRepositoryGroup) isNil ifTrue: [^nil].
	stream := WriteStream on: String new.
	group repositories do: [:each | 
		stream nextPutAll: (self mcRepositoryFrom: each).
		stream lf.
	].
	^stream contents.
!

mcStore: aMCWorkingCopy name: nameString message: messageString repository: aRepository 

	| versionClass version |
	versionClass := self objectNamed: #'MCVersion'.
	version := (aMCWorkingCopy needsSaving not and: [aMCWorkingCopy currentVersionInfo name = nameString]) ifTrue: [
		versionClass
			package: aMCWorkingCopy package
			info: aMCWorkingCopy currentVersionInfo.
	] ifFalse: [
		aMCWorkingCopy
			newVersionWithName: nameString
			message: messageString.
	].
	aRepository storeVersion: version.
	(self objectNamed: #'MCCacheRepository') default storeVersion: version.
!

mcTopazFrom: aString inDictionaryRepository: aDictionaryRepository

	| snapshot stream |
	snapshot := (aDictionaryRepository versionFromVersionNamed: aString) snapshot.
	stream := (WriteStream on: String new)
		nextPutAll: '!! ' , aString , ' in ' , aDictionaryRepository printString; lf;
		yourself.
	self
		_mcTopazFrom: snapshot
		on: stream.
	^stream contents.

!

mcTopazFrom: aString inFileRepository: aFileRepository

	| snapshot stream |
	snapshot := (aFileRepository versionFromFileNamed: aString) snapshot.
	stream := (WriteStream on: String new)
		nextPutAll: '!! ' , aString , ' in ' , aFileRepository printString; lf;
		yourself.
	self
		_mcTopazFrom: snapshot
		on: stream.
	^stream contents.

!

mcUniqueVersionNameFor: anMCWorkingCopy

	^anMCWorkingCopy uniqueVersionName.
!

mcUserAndPasswordInHTTP: anMCHttpRepository

	^anMCHttpRepository user , Character tab asString , anMCHttpRepository password.
!

mcVersionInfoFrom: aVersionInfo

	| stream |
	stream := WriteStream on: String new.
	(self oopOf: aVersionInfo) printOn: stream.
	stream 
		lf; nextPutAll: aVersionInfo name; 
		lf; nextPutAll: aVersionInfo date yyyymmdd;
		lf.
	aVersionInfo time printOn: stream.
	stream 
		lf; nextPutAll: aVersionInfo author; 
		lf; nextPutAll: aVersionInfo id asString;
		lf.
	aVersionInfo ancestors do: [:each | 
		stream nextPutAll: each name; tab.
	].
	stream lf.
	aVersionInfo stepChildren do: [:each | 
		stream nextPutAll: each name; tab.
	].
	stream lf.
	stream nextPutAll: aVersionInfo message.
	^stream contents.
!

mcVersionInfoFromDictionaryPackageNamed: aString in: anMCDictionaryRepository

	| versionInfo |
	(versionInfo := anMCDictionaryRepository versionInfoFromVersionNamed: aString) isNil ifTrue: [^''].
	^self mcVersionInfoFrom: versionInfo.
!

mcVersionInfoFromFileNamed: aString in: anMCFileBasedRepository

	| versionInfo |
	(versionInfo := anMCFileBasedRepository versionInfoFromFileNamed: aString) isNil ifTrue: [^''].
	^self mcVersionInfoFrom: versionInfo.
!

mcVersionLoad: aString fromDictionary: anMCDictionaryRepository autoMigrate: aBoolean

	| version package workingCopy mcPlatformSupport autoMigrate |
	mcPlatformSupport := self objectNamed: #'MCPlatformSupport'.
	autoMigrate := mcPlatformSupport autoMigrate.
	mcPlatformSupport autoMigrate: aBoolean.
	version := anMCDictionaryRepository versionFromVersionNamed: aString.
	version load.
	package := version package.
	workingCopy := self mcWorkingCopyClass forPackage: package.
	workingCopy repositoryGroup addRepository: anMCDictionaryRepository.
	mcPlatformSupport autoMigrate: autoMigrate.
!

mcVersionLoad: aString fromFile: anMCFileBasedRepository autoMigrate: aBoolean

	| version package workingCopy mcPlatformSupport autoMigrate |
	mcPlatformSupport := self objectNamed: #'MCPlatformSupport'.
	autoMigrate := mcPlatformSupport autoMigrate.
	mcPlatformSupport autoMigrate: aBoolean.
	version := anMCFileBasedRepository loadVersionFromFileNamed: aString.
	version load.
	package := version package.
	workingCopy := self mcWorkingCopyClass forPackage: package.
	workingCopy repositoryGroup addRepository: anMCFileBasedRepository.
	mcPlatformSupport autoMigrate: autoMigrate.
!

mcVersionMerge: aString from: anMCFileBasedRepository autoMigrate: aBoolean

	| version mcPlatformSupport autoMigrate workingCopy |
	mcPlatformSupport := self objectNamed: #'MCPlatformSupport'.
	autoMigrate := mcPlatformSupport autoMigrate.
	mcPlatformSupport autoMigrate: aBoolean.
	version := anMCFileBasedRepository loadVersionFromFileNamed: aString.
	workingCopy := self mcWorkingCopyClass forPackage: version package.
	[
		[
			workingCopy merge: version.
		] on: (self objectNamed: #'MCNoChangesException') do: [:ex | 
			ex return.
		].
	] on: (self objectNamed: #'MCMergeResolutionRequest') do: [:ex | 
self halt.
		ex merger conflicts do: [:each | each chooseRemote].
		ex merger load.
		workingCopy 
			merged: version;
			modified: true;
			yourself.
		"ex resume: true."
		ex return.
	].

!

mcVersionNameAndMessageFrom: aMCWorkingCopy

	(aMCWorkingCopy needsSaving or: [aMCWorkingCopy ancestors isEmpty]) ifTrue: [
		^'<new>	<new>'.
	].
	^aMCWorkingCopy currentVersionInfo name , Character tab asString , aMCWorkingCopy currentVersionInfo message.
!

mcwcbWorkingCopies

	| mcWorkingCopyClass list stream |
	(mcWorkingCopyClass := self mcWorkingCopyClass) isNil ifTrue: [^nil].
	list := mcWorkingCopyClass allManagers.
	list := list asSortedCollection: [:a :b | a package name <= b package name].
	stream := WriteStream on: String new.
	list do: [:each |
		self saveWorkingCopy: each to: stream.
		stream lf].
	^stream contents!

mcWorkingCopyClass

	^self objectNamed: #'MCWorkingCopy'.
!

mcWorkingCopyNamed: aString

	| mcWorkingCopyClass workingCopy stream |
	(mcWorkingCopyClass := self mcWorkingCopyClass) isNil ifTrue: [^nil].
	workingCopy := mcWorkingCopyClass allManagers 
		detect: [:each | each package name = aString]
		ifNone: [^nil].
	stream := WriteStream on: String new.
	self 
		saveWorkingCopy: workingCopy 
		to: stream.
	^stream contents.
!

saveWorkingCopy: wc to: stream

	self _addToPureExportSet: wc.
	stream
		nextPutAll: (self oopOf: wc) printString; tab;
		nextPutAll: wc package name; tab;
		nextPutAll: wc modified printString; tab;
		nextPutAll: wc ancestors size printString; tab;
		yourself.
	wc ancestors do: [:ancestor |
		self _addToPureExportSet: ancestor.
		(self oopOf: ancestor) printOn: stream.
		stream tab.
	].
	stream nextPutAll: wc repositoryGroup repositories size printString; tab.
	wc repositoryGroup repositories do: [:repository |
		self _addToPureExportSet: repository.
		(self oopOf: repository) printOn: stream.
		stream tab.
	].
! !
!JadeServer categoriesFor: #_describeMCAddition:on:!Monticello!private! !
!JadeServer categoriesFor: #_describeMCClassDefinition:on:!Monticello!public! !
!JadeServer categoriesFor: #_describeMCDefinition:on:!Monticello!public! !
!JadeServer categoriesFor: #_describeMCMethodDefinition:on:!Monticello!private! !
!JadeServer categoriesFor: #_describeMCModification:on:!Monticello!private! !
!JadeServer categoriesFor: #_describeMCOrganizationDefinition:on:!Monticello!public! !
!JadeServer categoriesFor: #_describeMCRemoval:on:!Monticello!private! !
!JadeServer categoriesFor: #_mcDescriptionOfPatch:baseName:alternateName:!Monticello!private! !
!JadeServer categoriesFor: #_mcTopazFrom:on:!Monticello!private! !
!JadeServer categoriesFor: #authorInitials:!Monticello!public! !
!JadeServer categoriesFor: #gsPackagePolicy!public! !
!JadeServer categoriesFor: #gsPackagePolicyClass!public! !
!JadeServer categoriesFor: #mcAddHttpRepository:!Monticello!public! !
!JadeServer categoriesFor: #mcAddPackage:!Monticello!public! !
!JadeServer categoriesFor: #mcAddRepository:toPackage:!Monticello!public! !
!JadeServer categoriesFor: #mcAllFileNamesIn:!Monticello!public! !
!JadeServer categoriesFor: #mcAllVersionInfoNamesIn:!Monticello!public! !
!JadeServer categoriesFor: #mcAllVersionNamesInDict:!Monticello!public! !
!JadeServer categoriesFor: #mcCategoryListFor:!Monticello!public! !
!JadeServer categoriesFor: #mcClassesInCategory:package:!Monticello!public! !
!JadeServer categoriesFor: #mcCreationTemplateFor:!Monticello!public! !
!JadeServer categoriesFor: #mcHttpRepository!Monticello!public! !
!JadeServer categoriesFor: #mcHttpRepository:user:password:!Monticello!public! !
!JadeServer categoriesFor: #mcHttpRepositoryClass!Monticello!public! !
!JadeServer categoriesFor: #mcInitials:!Monticello!public! !
!JadeServer categoriesFor: #mcInitialsA:!Monticello!public! !
!JadeServer categoriesFor: #mcLoadedVersionNames!Monticello!public! !
!JadeServer categoriesFor: #mcNewDirectoryRepository:!Monticello!public! !
!JadeServer categoriesFor: #mcNewFileTreeRepository:!Monticello!public! !
!JadeServer categoriesFor: #mcNewGitHubRepository:!Monticello!public! !
!JadeServer categoriesFor: #mcNewServerDirectoryRepository:!Monticello!public! !
!JadeServer categoriesFor: #mcPackageClass!Monticello!public! !
!JadeServer categoriesFor: #mcPatchFrom:to:inDictionaryRepository:!Monticello!public! !
!JadeServer categoriesFor: #mcPatchFrom:to:inFileBasedRepository:!Monticello!public! !
!JadeServer categoriesFor: #mcputDefinition:on:!Monticello!public! !
!JadeServer categoriesFor: #mcRemovePackage:!Monticello!public! !
!JadeServer categoriesFor: #mcRemoveRepository:!Monticello!public! !
!JadeServer categoriesFor: #mcRemoveRepository:toPackage:!Monticello!public! !
!JadeServer categoriesFor: #mcRepositoryFrom:!Monticello!public! !
!JadeServer categoriesFor: #mcRepositoryGroup!Monticello!public! !
!JadeServer categoriesFor: #mcRepositoryList!Monticello!public! !
!JadeServer categoriesFor: #mcStore:name:message:repository:!Monticello!public! !
!JadeServer categoriesFor: #mcTopazFrom:inDictionaryRepository:!Monticello!public! !
!JadeServer categoriesFor: #mcTopazFrom:inFileRepository:!Monticello!public! !
!JadeServer categoriesFor: #mcUniqueVersionNameFor:!Monticello!public! !
!JadeServer categoriesFor: #mcUserAndPasswordInHTTP:!Monticello!public! !
!JadeServer categoriesFor: #mcVersionInfoFrom:!Monticello!public! !
!JadeServer categoriesFor: #mcVersionInfoFromDictionaryPackageNamed:in:!Monticello!public! !
!JadeServer categoriesFor: #mcVersionInfoFromFileNamed:in:!Monticello!public! !
!JadeServer categoriesFor: #mcVersionLoad:fromDictionary:autoMigrate:!Monticello!public! !
!JadeServer categoriesFor: #mcVersionLoad:fromFile:autoMigrate:!Monticello!public! !
!JadeServer categoriesFor: #mcVersionMerge:from:autoMigrate:!Monticello!public! !
!JadeServer categoriesFor: #mcVersionNameAndMessageFrom:!Monticello!public! !
!JadeServer categoriesFor: #mcwcbWorkingCopies!Monticello!public! !
!JadeServer categoriesFor: #mcWorkingCopyClass!Monticello!public! !
!JadeServer categoriesFor: #mcWorkingCopyNamed:!Monticello!public! !
!JadeServer categoriesFor: #saveWorkingCopy:to:!Monticello!public! !

!JadeServer32bit methodsFor!

mcInitialsA: aString

	(System class includesSelector: #'_cacheName:') ifFalse: [^true].
	Exception
		category: nil
		number: nil
		do: [:ex :cat :num :args | ^false].
	super mcInitialsA: aString.
	^true.
! !
!JadeServer32bit categoriesFor: #mcInitialsA:!Monticello!public! !

!JadeServer64bit methodsFor!

mcInitialsA: aString

	^[
		super mcInitialsA: aString.
		true.
	] on: Error do: [:ex | 
		ex return: false.
	].
! !
!JadeServer64bit categoriesFor: #mcInitialsA:!Monticello!public! !

!JadeServer64bit32 methodsFor!

gsPackagePolicy

	| class |
	class := self gsPackagePolicyClass.
	class isNil ifTrue: [^nil].
	class enabled ifFalse: [^nil].
	^class current
! !
!JadeServer64bit32 categoriesFor: #gsPackagePolicy!public! !

!JadeTextDocument methodsFor!

jadeBrowseMonticello

	gciSession hasServer ifTrue: [
		^MCRepositoryBrowser showOn: gciSession.
	].
	MessageBox
		warning: 'Server initialization failed at login.'
		caption: 'Unable to Open Browser'.
! !
!JadeTextDocument categoriesFor: #jadeBrowseMonticello!Jade!private! !

"End of package definition"!

"Source Globals"!

"Classes"!

MCDefinition guid: (GUID fromString: '{055475AC-74A4-4268-BBF9-805E3C53183B}')!
MCDefinition comment: ''!
!MCDefinition categoriesForClass!Unclassified! !
!MCDefinition methodsFor!

selector

	^''.
! !
!MCDefinition categoriesFor: #selector!public! !

!MCDefinition class methodsFor!

fromStream: aStream

	| char class |
	char := aStream next.
	aStream next.
	class := 
		char = $M ifTrue: [MCMethodDefinition] ifFalse: [
		char = $O ifTrue: [MCOrganizationDefinition] ifFalse: [
		char = $C ifTrue: [MCClassDefinition] ifFalse: [
		self error: 'Invalid MCDefinition']]].
	^class new
		initialize: aStream;
		yourself.
! !
!MCDefinition class categoriesFor: #fromStream:!public! !

MCHttpRepositoryInfo guid: (GUID fromString: '{9297CE93-5E15-48CE-8BE4-01AA17099F34}')!
MCHttpRepositoryInfo comment: ''!
!MCHttpRepositoryInfo categoriesForClass!Unclassified! !
!MCHttpRepositoryInfo methodsFor!

initialize

	location		:= 'http://seaside.gemtalksystems.com/ss/'.
	user			:= ''.
	password	:= ''.
!

location
	^location!

location: anObject
	location := anObject!

password
	^password!

password: anObject
	password := anObject!

user
	^user!

user: anObject
	user := anObject! !
!MCHttpRepositoryInfo categoriesFor: #initialize!private! !
!MCHttpRepositoryInfo categoriesFor: #location!accessing!private! !
!MCHttpRepositoryInfo categoriesFor: #location:!accessing!private! !
!MCHttpRepositoryInfo categoriesFor: #password!accessing!private! !
!MCHttpRepositoryInfo categoriesFor: #password:!accessing!private! !
!MCHttpRepositoryInfo categoriesFor: #user!accessing!private! !
!MCHttpRepositoryInfo categoriesFor: #user:!accessing!private! !

!MCHttpRepositoryInfo class methodsFor!

new

	^super new
		initialize;
		yourself.
! !
!MCHttpRepositoryInfo class categoriesFor: #new!public! !

MCVersionName guid: (GUID fromString: '{F4AFBC1C-2C99-4741-8FBA-5C571F07CA36}')!
MCVersionName comment: ''!
!MCVersionName categoriesForClass!Unclassified! !
!MCVersionName methodsFor!

<= anMCVersionName

	^self name <= anMCVersionName name.
!

initialize

	name := ''.
	isLoaded := false.
	isModified := false.
!

isLoaded
	^isLoaded!

isLoaded: anObject
	isLoaded := anObject!

isModified
	^isModified!

isModified: anObject
	isModified := anObject!

name
	^name!

name: anObject
	name := anObject!

printOn: aStream

	| string |
	string := (name endsWith: '.mcz')
		ifTrue: [name copyFrom: 1 to: name size - 4] 
		ifFalse: [name].
	aStream nextPutAll: string.
! !
!MCVersionName categoriesFor: #<=!public! !
!MCVersionName categoriesFor: #initialize!public! !
!MCVersionName categoriesFor: #isLoaded!accessing!public! !
!MCVersionName categoriesFor: #isLoaded:!accessing!public! !
!MCVersionName categoriesFor: #isModified!accessing!public! !
!MCVersionName categoriesFor: #isModified:!accessing!public! !
!MCVersionName categoriesFor: #name!accessing!public! !
!MCVersionName categoriesFor: #name:!accessing!public! !
!MCVersionName categoriesFor: #printOn:!public! !

!MCVersionName class methodsFor!

new

	^super new
		initialize;
		yourself.
! !
!MCVersionName class categoriesFor: #new!public! !

MCPackage guid: (GUID fromString: '{C1DA7939-7500-412E-B95B-88C59221CEAF}')!
MCPackage comment: ''!
!MCPackage categoriesForClass!Unclassified! !
!MCPackage methodsFor!

infoForVersion: aString

	^repository versionInfoForPackageNamed: self name version: aString.
!

isLoaded

	^loaded notNil.
!

isModified

	^loaded notNil and: [loaded value].
!

loaded: anObject

	loaded := anObject.
!

loadedEditionName

	^loaded notNil 
		ifTrue: [loaded key]
		ifFalse: [''].
!

name: aString 
	name := aString!

repository
	^repository!

repository: anMCRepository

	repository := anMCRepository.
	gciSession := anMCRepository gciSession.
!

versionNames
	^versionNames!

versionNames: aList

	| list |
	list := aList asArray.
	list := list collect: [:each | 
		| pieces |
		pieces := each subStrings: $..
		pieces = #('current') ifTrue: [
			0 -> 'current'
		] ifFalse: [
			((pieces at: pieces size - 1) collect: [:each | each isDigit ifTrue: [each] ifFalse: [$0]]) asNumber -> each.
		].
	].
	list := list collect: [:each | MCVersionName new name: each value].
	versionNames := list.! !
!MCPackage categoriesFor: #infoForVersion:!public! !
!MCPackage categoriesFor: #isLoaded!public! !
!MCPackage categoriesFor: #isModified!public! !
!MCPackage categoriesFor: #loaded:!public! !
!MCPackage categoriesFor: #loadedEditionName!public! !
!MCPackage categoriesFor: #name:!public! !
!MCPackage categoriesFor: #repository!accessing!public! !
!MCPackage categoriesFor: #repository:!accessing!public! !
!MCPackage categoriesFor: #versionNames!accessing!public! !
!MCPackage categoriesFor: #versionNames:!accessing!public! !

MCPackageVersion guid: (GUID fromString: '{75779A06-81A7-42DE-8521-65D983F02D77}')!
MCPackageVersion comment: ''!
!MCPackageVersion categoriesForClass!Unclassified! !
!MCPackageVersion methodsFor!

ancestors
	^ancestors!

author
	^author!

date
	^date!

id
	^id!

initialize: aList

	name 			:= name , '.mcz'.
	date 				:= aList at: 3.
	time 				:= aList at: 4.
	author 			:= aList at: 5.
	id 				:= aList at: 6.
	ancestors 		:= (aList at: 7) subStrings: Character tab.
	stepChildren 	:= (aList at: 8) subStrings: Character tab.
	message 		:= ''.
	(aList copyFrom: 9 to: aList size) do: [:each | 
		message notEmpty ifTrue: [message := message , '
'.].
		message := message , each.
	].
!

initialize: aString session: aGciSession

	| list |
	gciSession := aGciSession.
	list := aString subStrings: Character lf.
	oopType := gciSession oopTypeWithOop: (list at: 1) asNumber.
	name := list at: 2.
	self initialize: list.
!

message
	^message!

stepChildren
	^stepChildren!

time
	^time! !
!MCPackageVersion categoriesFor: #ancestors!accessing!public! !
!MCPackageVersion categoriesFor: #author!accessing!public! !
!MCPackageVersion categoriesFor: #date!accessing!public! !
!MCPackageVersion categoriesFor: #id!accessing!public! !
!MCPackageVersion categoriesFor: #initialize:!public! !
!MCPackageVersion categoriesFor: #initialize:session:!public! !
!MCPackageVersion categoriesFor: #message!accessing!public! !
!MCPackageVersion categoriesFor: #stepChildren!accessing!public! !
!MCPackageVersion categoriesFor: #time!accessing!public! !

MCPatch guid: (GUID fromString: '{F78F623F-4CC4-4E69-BC3D-F6320FC96009}')!
MCPatch comment: ''!
!MCPatch categoriesForClass!Unclassified! !
!MCPatch methodsFor!

initialize: aString session: aGciSession

	| stream |
	stream := ReadStream on: aString.
	super
		initialize: stream nextLine
		session: aGciSession.
	operations := OrderedCollection new.
	[
		stream atEnd not.
	] whileTrue: [
		operations add: (MCPatchOperation
			fromStream: stream
			session: aGciSession).
	].
!

operations
	^operations! !
!MCPatch categoriesFor: #initialize:session:!public! !
!MCPatch categoriesFor: #operations!public! !

MCPatchOperation guid: (GUID fromString: '{656D8071-9137-4A7A-9ABA-D0273EE9D33C}')!
MCPatchOperation comment: ''!
!MCPatchOperation categoriesForClass!Unclassified! !
!MCPatchOperation methodsFor!

<= aPatchOperation

	^self sortString <= aPatchOperation sortString.
!

className

	self subclassResponsibility.
!

hasEquivalentText

	^false.
!

initialize: aStream session: aGciSession

	| string |
	gciSession := aGciSession.
	string := aStream upTo: Character tab.
	oopType := gciSession oopTypeWithOop: string asNumber.
	self initialize: aStream.
!

modificationMemo

	^''.
!

modificationMemoMarkers

	^#().
!

modificationText

	^''.
!

obsoletionMemo

	^''.
!

obsoletionMemoMarkers

	^#().
!

obsoletionText

	^''.
!

selector

	self subclassResponsibility.
!

sortString

	^self className , '>>' , self selector.
!

typeString

	^self class name copyFrom: 3 to: self class name size.
! !
!MCPatchOperation categoriesFor: #<=!public! !
!MCPatchOperation categoriesFor: #className!public! !
!MCPatchOperation categoriesFor: #hasEquivalentText!public! !
!MCPatchOperation categoriesFor: #initialize:session:!public! !
!MCPatchOperation categoriesFor: #modificationMemo!public! !
!MCPatchOperation categoriesFor: #modificationMemoMarkers!public! !
!MCPatchOperation categoriesFor: #modificationText!public! !
!MCPatchOperation categoriesFor: #obsoletionMemo!public! !
!MCPatchOperation categoriesFor: #obsoletionMemoMarkers!public! !
!MCPatchOperation categoriesFor: #obsoletionText!public! !
!MCPatchOperation categoriesFor: #selector!public! !
!MCPatchOperation categoriesFor: #sortString!public! !
!MCPatchOperation categoriesFor: #typeString!public! !

!MCPatchOperation class methodsFor!

fromStream: aStream session: aGciSession

	| char class |
	char := aStream next.
	aStream next.
	class := 
		char = $A ifTrue: [MCAddition] ifFalse: [
		char = $M ifTrue: [MCModification] ifFalse: [
		char = $R ifTrue: [MCRemoval] ifFalse: [
		self error: 'Invalid MCPatchOperation']]].
	^class new
		initialize: aStream
		session:  aGciSession.
! !
!MCPatchOperation class categoriesFor: #fromStream:session:!public! !

MCRepository guid: (GUID fromString: '{AE19DCB0-74D7-4643-B3CB-541231353C0F}')!
MCRepository comment: ''!
!MCRepository categoriesForClass!Unclassified! !
!MCRepository methodsFor!

allVersionInfoNames

	self subclassResponsibility.
!

creationTemplate

	^gciSession
		serverPerform: #'mcCreationTemplateFor:' 
		with: self.
!

fullNameOfPackage: aString versionName: aString2 

	self subclassResponsibility.
!

initialize: aList

!

isDefault

	^false.
!

loadPackageNamed: package versionName: versionName

	self subclassResponsibility.
!

packageList
 
	| list dictionary |
	list := self allVersionInfoNames.
	list := list select: [:each | each endsWith: '.mcz'].
	dictionary := Dictionary new.
	list do: [:each | 
		| index packageName version versions |
		index := each lastIndexOf: $-.
		index = 0 ifTrue: [index := each size + 1].
		packageName := each copyFrom: 1 to: index - 1.
		version := each copyFrom: index + 1 to: each size.
		versions := dictionary
			at: packageName
			ifAbsentPut: [OrderedCollection new].
		versions addFirst: version.
	].
	list := OrderedCollection new.
	dictionary keysAndValuesDo: [:eachKey :eachValue | 
		list add: eachKey -> eachValue.
	].
	list := list asSet asSortedCollection.
	list := list collect: [:each | 
		MCPackage new
			name: each key;
			versionNames: each value;
			repository: self;
			yourself.
	].
	^list.
!

patchFrom: string1 to: string2 

	self subclassResponsibility.!

storeVersion: anMCWorkingCopy name: nameString message: messageString from: aMCVersionDialog

	gciSession
		withOopForString1: nameString 
		string2: messageString 
		do: [:oop1 :oop2 | 
			gciSession
				serverPerform: #'mcStore:name:message:repository:'
				with: anMCWorkingCopy
				with: oop1 
				with: oop2 
				with: self.
		].
!

topazFrom: aString 

	^Error notYetImplemented!

versionInfoForPackageNamed: aString

	| string version |
	gciSession
		withOopForString: aString
		do: [:oopType |
			string := gciSession
				serverPerform: #'mcVersionInfoFromFileNamed:in:' 
				with: oopType
				with: self.
	].
	version := MCPackageVersion
		fromString: string
		session: gciSession.
	^version.
!

versionInfoForPackageNamed: aString version: aString2 
	^Error notYetImplemented! !
!MCRepository categoriesFor: #allVersionInfoNames!public! !
!MCRepository categoriesFor: #creationTemplate!public! !
!MCRepository categoriesFor: #fullNameOfPackage:versionName:!public! !
!MCRepository categoriesFor: #initialize:!public! !
!MCRepository categoriesFor: #isDefault!public! !
!MCRepository categoriesFor: #loadPackageNamed:versionName:!public! !
!MCRepository categoriesFor: #packageList!public! !
!MCRepository categoriesFor: #patchFrom:to:!public! !
!MCRepository categoriesFor: #storeVersion:name:message:from:!public! !
!MCRepository categoriesFor: #topazFrom:!public! !
!MCRepository categoriesFor: #versionInfoForPackageNamed:!public! !
!MCRepository categoriesFor: #versionInfoForPackageNamed:version:!public! !

!MCRepository class methodsFor!

allIn: aGciSession

	| string |
	(string := aGciSession serverPerform: #'mcRepositoryList') isNil ifTrue: [^#()].
	^self
		listFromString: string 
		session: aGciSession.
!

fromString: aString session: aGciSession

	| list type class |
	list := aString subStrings: Character tab.
	type := list at: 3.
	type = 'MCDictionaryRepository' 			ifTrue: [class := MCDictionaryRepository].
	type = 'MCDirectoryRepository' 			ifTrue: [class := MCDirectoryRepository].
	type = 'MCFileTreeRepository'				ifTrue: [class := MCFileTreeRepository].
	type = 'MCGitHubRepository'				ifTrue: [class := MCGitHubRepository].
	type = 'MCHttpRepository' 					ifTrue: [class := MCHttpRepository].
	type = 'MCServerDirectoryRepository'	ifTrue: [class := MCServerDirectoryRepository].
	class isNil ifTrue: [
		MessageBox warning: 'Unrecognized repository type ' , type printString , ' will be ignored!!' caption: 'Monticello Repository'.
		^nil.
	].
	^class new 
		initialize: aString
		session:  aGciSession.
!

gciSession: aGciSession oop: anInteger 

	| oopType string |
	oopType := aGciSession oopTypeWithOop: anInteger.
	string := aGciSession
		serverPerform: #'mcRepositoryFrom:' 
		with: oopType.
	^self 
		fromString: string
		session: aGciSession.
!

listFromString: aString session: aGciSession

	^(super listFromString: aString session: aGciSession) reject: [:each | each isNil].
! !
!MCRepository class categoriesFor: #allIn:!public! !
!MCRepository class categoriesFor: #fromString:session:!public! !
!MCRepository class categoriesFor: #gciSession:oop:!public! !
!MCRepository class categoriesFor: #listFromString:session:!public! !

MCVersionInfo guid: (GUID fromString: '{202A46BC-0A0E-407F-A82D-5AD5B9DE8B87}')!
MCVersionInfo comment: ''!
!MCVersionInfo categoriesForClass!Unclassified! !
!MCVersionInfo methodsFor!

gciSession: aGciSession oop: anInteger 

	| string stream |
	gciSession := aGciSession.
	oopType := gciSession oopTypeWithOop: anInteger.
	string := gciSession
		serverPerform: #'mcVersionInfoFrom:' 
		with: oopType.
	stream := ReadStream on: string.
	stream nextLine. "OOP"
	name := stream nextLine.
	date := stream nextLine.
	time := stream nextLine.
	author := stream nextLine.
	id := stream nextLine.
	ancestors := stream nextLine subStrings: Character tab.
	stepChildren := stream nextLine subStrings: Character tab.
	message := stream upToEnd.
!

message
	^message!

timeStamp

	^date , ' ' , time.
! !
!MCVersionInfo categoriesFor: #gciSession:oop:!public! !
!MCVersionInfo categoriesFor: #message!public! !
!MCVersionInfo categoriesFor: #timeStamp!public! !

!MCVersionInfo class methodsFor!

gciSession: aGciSession oop: anInteger 

	^self new
		gciSession: aGciSession 
		oop: anInteger.
! !
!MCVersionInfo class categoriesFor: #gciSession:oop:!public! !

MCWorkingCopy guid: (GUID fromString: '{D388C1A7-ECC0-4FD6-B7CE-78433225BFA9}')!
MCWorkingCopy comment: ''!
!MCWorkingCopy categoriesForClass!Unclassified! !
!MCWorkingCopy methodsFor!

addRepository: aRepository

	gciSession
		serverPerform: #'mcAddRepository:toPackage:' 
		with: aRepository
		with: self.
	self repositories add: aRepository.
!

ancestors

	ancestors := ancestors collect: [:each | self mcVersionInfoFrom: each].
	^ancestors.
!

authorInitials: aString

	gciSession
		withOopForString: aString
		do: [:newOop |
			gciSession
				serverPerform: #'authorInitials:'
				with: newOop.
		].
!

categoryList

	| string list |
	string := gciSession 
		serverPerform: #'mcCategoryListFor:'
		with: self.
	list := GsString
		listFromString: string 
		session: gciSession.
	^list.
!

classesInCategory: aGsString 

	| string |
	string := gciSession 
		serverPerform: #mcClassesInCategory:package:
		with: aGsString
		with: self.
	^GsClass 
		listFromString: string 
		session: gciSession.
!

globals

	^#().
!

initialize: aList

	| stream |
	stream := ReadStream on: aList.
	stream next; next.
	isModified := stream next = 'true'.
	ancestors := Array new: stream next asNumber.
	1 to: ancestors size do: [:i | 
		ancestors 
			at: i
			put: stream next asNumber.
	].
	repositories := Array new: stream next asNumber.
	1 to: repositories size do: [:i | 
		repositories 
			at: i
			put: stream next asNumber.
	].
!

isModified
	^isModified!

mcRepositoryFrom: anObject

	(anObject isKindOf: MCRepository) ifTrue: [^anObject].
	^MCRepository
		gciSession: gciSession
		oop: anObject.
!

mcVersionInfoFrom: anObject

	(anObject isKindOf: MCVersionInfo) ifTrue: [^anObject].
	^MCVersionInfo
		gciSession: gciSession
		oop: anObject.
!

newVersionWithName: nameString message: messageString

	| string |
	string := gciSession
		withOopForString1: nameString 
		string2: messageString 
		do: [:oop1 :oop2 | 
			gciSession
				serverPerform: #'mcNewVersionWithNname:message:'
				with: self
				with: oop1 
				with: oop2 .
		].
	MessageBox notify: 'Sorry, we are not yet implemented this feature!!'.
	Keyboard default isShiftDown ifTrue: [self halt].
!

removeRepository: aRepository 

	gciSession
		serverPerform: #'mcRemoveRepository:toPackage:' 
		with: aRepository
		with: self.
	self repositories remove: aRepository.
!

repositories

	repositories := repositories asOrderedCollection collect: [:each | self mcRepositoryFrom: each].
	^repositories.
!

uniqueVersionName

	^gciSession
		serverPerform: #'mcUniqueVersionNameFor:'
		with: self.
!

versionNameAndMessage

	| string list |
	string := gciSession
		serverPerform: #'mcVersionNameAndMessageFrom:' 
		with: self.
	list := string subStrings: Character tab.
	^list first -> list last.
! !
!MCWorkingCopy categoriesFor: #addRepository:!public! !
!MCWorkingCopy categoriesFor: #ancestors!public! !
!MCWorkingCopy categoriesFor: #authorInitials:!public! !
!MCWorkingCopy categoriesFor: #categoryList!public! !
!MCWorkingCopy categoriesFor: #classesInCategory:!public! !
!MCWorkingCopy categoriesFor: #globals!public! !
!MCWorkingCopy categoriesFor: #initialize:!public! !
!MCWorkingCopy categoriesFor: #isModified!public! !
!MCWorkingCopy categoriesFor: #mcRepositoryFrom:!public! !
!MCWorkingCopy categoriesFor: #mcVersionInfoFrom:!public! !
!MCWorkingCopy categoriesFor: #newVersionWithName:message:!public! !
!MCWorkingCopy categoriesFor: #removeRepository:!public! !
!MCWorkingCopy categoriesFor: #repositories!public! !
!MCWorkingCopy categoriesFor: #uniqueVersionName!public! !
!MCWorkingCopy categoriesFor: #versionNameAndMessage!public! !

MCAddition guid: (GUID fromString: '{B78CB54F-D86C-4398-A734-2C5CFA934104}')!
MCAddition comment: ''!
!MCAddition categoriesForClass!Unclassified! !
!MCAddition methodsFor!

className

	^definition className.
!

detailsString

	^definition detailsString.
!

initialize: aStream

	definition := MCDefinition fromStream: aStream.
!

modificationMemo

	^definition displayMemo.
!

modificationText

	^definition displayText.
!

selector

	^definition selector.
! !
!MCAddition categoriesFor: #className!public! !
!MCAddition categoriesFor: #detailsString!public! !
!MCAddition categoriesFor: #initialize:!public! !
!MCAddition categoriesFor: #modificationMemo!public! !
!MCAddition categoriesFor: #modificationText!public! !
!MCAddition categoriesFor: #selector!public! !

MCModification guid: (GUID fromString: '{29F0BDF4-C441-4F8E-BBA1-3613F678D6AE}')!
MCModification comment: ''!
!MCModification categoriesForClass!Unclassified! !
!MCModification methodsFor!

bestMatchAfter: anInteger

	| list start altList |
	list := OrderedCollection new.
	anInteger to: obsoletionLines size do: [:i | 
		anInteger to: modificationLines size do: [:j | 
			| k |
			k := self matchCountFromObsoletionLine: i modificationLine: j.
			0 < k ifTrue: [list add: (Array with: i with: j with: k)].
		].
	].
	list isEmpty ifTrue: [^Array with: obsoletionLines size + 1 with: modificationLines size + 1 with: 0].
	start := list first first.
	list do: [:each | 
		start := (start min: (each at: 1)) min: (each at: 2).
	].
	list := list select: [:each | (each at: 1) = start or: [(each at: 2) = start]].
	1 = list size ifTrue: [^list first].
	altList := list select: [:each | (each at: 1) = (each at: 2)].
	1 = altList size ifTrue: [^altList first].
	^list first.
!

className 

	^obsoletion className.
!

computeDifferences

	| start stream |
	obsoletionMemo notNil ifTrue: [^self].
	obsoletionLines := (obsoletion displayMemo subStrings: Character lf) asOrderedCollection.
	obsoletionMemoMarkers := OrderedCollection new.
	modificationLines := (modification displayMemo subStrings: Character lf) asOrderedCollection.
	modificationMemoMarkers := OrderedCollection new.
	start := 1.
	[
		| match |
		match := self matchCountFromObsoletionLine: start modificationLine: start.
		start := start + match.
		start <= (obsoletionLines size max: modificationLines size).
	] whileTrue: [
		| end lastChangedLine firstMatchedLine |
		end := self bestMatchAfter: start.
		lastChangedLine := ((end at: 1) min: (end at: 2)) - 1.
		start to: lastChangedLine do: [:i | 
			obsoletionMemoMarkers add: #'changed' -> i.
			modificationMemoMarkers add: #'changed' -> i.
		].
		firstMatchedLine := (end at: 1) max: (end at: 2).
		(end at: 1) to: firstMatchedLine - 1 do: [:i | 
			obsoletionLines add: '' afterIndex: i - 1.
			modificationMemoMarkers add: #'added' -> i.
		].
		(end at: 2) to: firstMatchedLine - 1 do: [:i | 
			modificationLines add: '' afterIndex: i - 1.
			obsoletionMemoMarkers add: #'removed' -> i.
		].
		start := firstMatchedLine.
	].
	stream := WriteStream on: String new.
	obsoletionLines do: [:each | stream nextPutAll: each; nextPut: Character lf].
	obsoletionMemo := stream contents.
	obsoletionMemoMarkers := obsoletionMemoMarkers asArray.
	stream := WriteStream on: String new.
	modificationLines do: [:each | stream nextPutAll: each; nextPut: Character lf].
	modificationMemo := stream contents.
	modificationMemoMarkers := modificationMemoMarkers asArray.
!

detailsString

	^modification detailsString.
!

firstDifferenceAfter: anInteger

	| end |
	end := obsoletionLines size min: modificationLines size.
	anInteger to: end do: [:i | 
		(obsoletionLines at: i) ~= (modificationLines at: i) ifTrue: [^i].
	].
	^end.
!

firstMatchAfter: anInteger

	| x y |
	x := OrderedCollection new.
	y := OrderedCollection new.
	anInteger to: obsoletionLines size do: [:i | 
		anInteger to: modificationLines size do: [:j | 
			| k |
			k := self matchCountFromObsoletionLine: i modificationLine: j.
			0 < k ifTrue: [x add: (Array with: i with: j with: k)].
		].
	].
	MessageBox notify: 'Sorry, we are not yet implemented this feature!!'.
	Keyboard default isShiftDown ifTrue: [self halt].
!

hasEquivalentText

	| crlf lf left right |
	crlf := Character cr asString , Character lf asString.
	lf := Character lf asString.
	left := obsoletion displayMemo copyReplaceAll: crlf with: lf.
	right := modification displayMemo copyReplaceAll: crlf with: lf.
	^left = right.
!

initialize: aStream

	obsoletion := MCDefinition fromStream: aStream.
	modification := MCDefinition fromStream: aStream.
!

matchCountFromObsoletionLine: obsInteger modificationLine: modInteger

	| obsOffset modOffset |
	obsOffset := obsInteger.
	modOffset := modInteger.
	[
		obsoletionLines size < obsOffset ifTrue: [^obsOffset - obsInteger].
		modificationLines size < modOffset ifTrue: [^obsOffset - obsInteger].
		(obsoletionLines at: obsOffset) = (modificationLines at: modOffset).
	] whileTrue: [
		obsOffset := obsOffset + 1.
		modOffset := modOffset + 1.
	].
	^obsOffset - obsInteger.
!

modificationMemo

	self computeDifferences.
	^modificationMemo.
!

modificationMemoMarkers

	self computeDifferences.
	^modificationMemoMarkers.

!

modificationText

	^modification displayText.
!

obsoletionMemo

	self computeDifferences.
	^obsoletionMemo.
!

obsoletionMemoMarkers

	self computeDifferences.
	^obsoletionMemoMarkers.
!

obsoletionText

	^obsoletion displayText.
!

selector

	^modification selector.
! !
!MCModification categoriesFor: #bestMatchAfter:!differences!public! !
!MCModification categoriesFor: #className!public! !
!MCModification categoriesFor: #computeDifferences!differences!public! !
!MCModification categoriesFor: #detailsString!public! !
!MCModification categoriesFor: #firstDifferenceAfter:!differences!public! !
!MCModification categoriesFor: #firstMatchAfter:!differences!public! !
!MCModification categoriesFor: #hasEquivalentText!public! !
!MCModification categoriesFor: #initialize:!public! !
!MCModification categoriesFor: #matchCountFromObsoletionLine:modificationLine:!differences!public! !
!MCModification categoriesFor: #modificationMemo!public! !
!MCModification categoriesFor: #modificationMemoMarkers!differences!public! !
!MCModification categoriesFor: #modificationText!public! !
!MCModification categoriesFor: #obsoletionMemo!public! !
!MCModification categoriesFor: #obsoletionMemoMarkers!differences!public! !
!MCModification categoriesFor: #obsoletionText!public! !
!MCModification categoriesFor: #selector!public! !

MCRemoval guid: (GUID fromString: '{BA962C07-8639-4E40-89AA-6CE4EB10AB50}')!
MCRemoval comment: ''!
!MCRemoval categoriesForClass!Unclassified! !
!MCRemoval methodsFor!

className

	^definition className.
!

detailsString

	^definition detailsString.
!

initialize: aStream

	definition := MCDefinition fromStream: aStream.
!

obsoletionMemo

	^definition displayMemo.
!

obsoletionText

	^definition displayText.
!

selector

	^definition selector.
! !
!MCRemoval categoriesFor: #className!public! !
!MCRemoval categoriesFor: #detailsString!public! !
!MCRemoval categoriesFor: #initialize:!public! !
!MCRemoval categoriesFor: #obsoletionMemo!public! !
!MCRemoval categoriesFor: #obsoletionText!public! !
!MCRemoval categoriesFor: #selector!public! !

MCDictionaryRepository guid: (GUID fromString: '{E3F38D70-6EDE-4358-AC87-4309206F17DB}')!
MCDictionaryRepository comment: ''!
!MCDictionaryRepository categoriesForClass!Unclassified! !
!MCDictionaryRepository methodsFor!

allVersionInfoNames

	| string list |
	string := gciSession
		serverPerform: #'mcAllVersionNamesInDict:' 
		with: self.
	list := string subStrings: Character lf.
	list := list collect: [:each | each , '.mcz'].
	^list.
!

fullNameOfPackage: packageNameString versionName: versionNameString 

	^packageNameString , '-' , versionNameString.
!

isDefault

	^true.
!

loadPackageNamed: packageName versionName: versionName

	gciSession 
		withOopForString: packageName , '-' , (versionName copyFrom: 1 to: versionName size - 4)
		do: [:oopType |
			gciSession
				serverPerform: #'mcVersionLoad:fromDictionary:autoMigrate:' 
				with: oopType
				with: self
				with: true.
	].
!

patchFrom: string1 to: string2 

	| string |
	string := gciSession
		withOopForString1: (string1 ifNotNil: [:value | value copyFrom: 1 to: value size - 4])
		string2: (string2 ifNotNil: [:value | value copyFrom: 1 to: value size - 4])
		do: [:oop1 :oop2 | 
			gciSession
				serverPerform: #'mcPatchFrom:to:inDictionaryRepository:'
				with: oop1
				with: oop2
				with: self.
		].
	^MCPatch
		fromString: string
		session: gciSession.
!

topazFrom: aString 

	| string |
	string := aString.
	(string endsWith: '.mcz') ifTrue: [string := string copyFrom: 1 to: string size - 4].
	^gciSession
		withOopForString: string 
		do: [:oop | 
			gciSession
				serverPerform: #'mcTopazFrom:inDictionaryRepository:'
				with: oop
				with: self.
		].
!

updateVersionDialogTabIn: aMCVersionDialog

	aMCVersionDialog updateDictionaryTabWith: self.
!

versionInfoFor: aString 

	| string version |
	gciSession withOopForString: aString
		do: 
			[:oopType | 
			string := gciSession 
						serverPerform: #mcVersionInfoFromDictionaryPackageNamed:in:
						with: oopType
						with: self].
	string isEmpty ifTrue: [^nil].
	version := MCPackageVersion fromString: string session: gciSession.
	^version!

versionInfoForPackageNamed: aString version: aString2 

	| string version |
	string := aString , '-' , aString2.
	(string endsWith: '.mcz') ifTrue: [string := string copyFrom: 1 to: string size - 4].
	gciSession 
		withOopForString: string
		do: [:stringOopType | 
			string := gciSession 
				serverPerform: #mcVersionInfoFromDictionaryPackageNamed:in:
				with: stringOopType
				with: self.
		].
	string isEmpty ifTrue: [^nil].
	version := MCPackageVersion fromString: string session: gciSession.
	^version! !
!MCDictionaryRepository categoriesFor: #allVersionInfoNames!public! !
!MCDictionaryRepository categoriesFor: #fullNameOfPackage:versionName:!public! !
!MCDictionaryRepository categoriesFor: #isDefault!public! !
!MCDictionaryRepository categoriesFor: #loadPackageNamed:versionName:!public! !
!MCDictionaryRepository categoriesFor: #patchFrom:to:!public! !
!MCDictionaryRepository categoriesFor: #topazFrom:!public! !
!MCDictionaryRepository categoriesFor: #updateVersionDialogTabIn:!public! !
!MCDictionaryRepository categoriesFor: #versionInfoFor:!public! !
!MCDictionaryRepository categoriesFor: #versionInfoForPackageNamed:version:!public! !

MCFileBasedRepository guid: (GUID fromString: '{02AB65A3-7933-4FF3-A1C8-7960C4414821}')!
MCFileBasedRepository comment: ''!
!MCFileBasedRepository categoriesForClass!Unclassified! !
!MCFileBasedRepository methodsFor!

allVersionInfoNames

	| string list |
	string := gciSession
		serverPerform: #'mcAllFileNamesIn:' 
		with: self.
	list := string subStrings: Character lf.
	^list.
!

fullNameOfPackage: packageName versionName: versionName 

	^packageName , '-' , versionName.!

loadPackageNamed: packageName versionName: versionName

	gciSession 
		withOopForString: (self fullNameOfPackage: packageName versionName: versionName)
		do: [:anOopType |
			gciSession
				serverPerform: #'mcVersionLoad:fromFile:autoMigrate:' 
				with: anOopType
				with: self
				with: true.
	].
!

patchFrom: string1 to: string2 

	| string |
	string := gciSession
		withOopForString1: string1
		string2: string2
		do: [:oop1 :oop2 | 
			gciSession
				serverPerform: #'mcPatchFrom:to:inFileBasedRepository:'
				with: oop1
				with: oop2
				with: self.
		].
	^MCPatch
		fromString: string
		session: gciSession.
!

topazFrom: aString 

	^gciSession
		withOopForString: aString 
		do: [:oop | 
			gciSession
				serverPerform: #'mcTopazFrom:inFileRepository:'
				with: oop
				with: self.
		].
!

versionInfoForPackageNamed: packageName version: versionName 

	^self versionInfoForPackageNamed: packageName , '-' , versionName.
! !
!MCFileBasedRepository categoriesFor: #allVersionInfoNames!public! !
!MCFileBasedRepository categoriesFor: #fullNameOfPackage:versionName:!public! !
!MCFileBasedRepository categoriesFor: #loadPackageNamed:versionName:!public! !
!MCFileBasedRepository categoriesFor: #patchFrom:to:!public! !
!MCFileBasedRepository categoriesFor: #topazFrom:!public! !
!MCFileBasedRepository categoriesFor: #versionInfoForPackageNamed:version:!public! !

MCDirectoryRepository guid: (GUID fromString: '{8B1B9EE6-FA9A-4E55-93E0-873C1F44B600}')!
MCDirectoryRepository comment: ''!
!MCDirectoryRepository categoriesForClass!Unclassified! !
!MCDirectoryRepository methodsFor!

updateVersionDialogTabIn: aMCVersionDialog

	aMCVersionDialog updateDirectoryTabWith: self.
! !
!MCDirectoryRepository categoriesFor: #updateVersionDialogTabIn:!public! !

MCHttpRepository guid: (GUID fromString: '{140BABA7-58F1-4DC6-96F5-510365C3DF9C}')!
MCHttpRepository comment: ''!
!MCHttpRepository categoriesForClass!Unclassified! !
!MCHttpRepository methodsFor!

storeVersion: anMCWorkingCopy name: nameString message: messageString from: aMCVersionDialog

	self 
		user: aMCVersionDialog httpUser
		password: aMCVersionDialog httpPassword.
	gciSession
		withOopForString1: nameString 
		string2: messageString 
		do: [:oop1 :oop2 | 
			gciSession
				serverPerform: #'mcStore:name:message:repository:'
				with: anMCWorkingCopy
				with: oop1 
				with: oop2 
				with: self.
		].
!

updateVersionDialogTabIn: aMCVersionDialog

	aMCVersionDialog updateHttpTabWith: self.
!

user: userString password: passwordString

	gciSession
		withOopForString1: userString 
		string2: passwordString 
		do: [:oop1 :oop2 | 
			gciSession
				serverPerform: #'mcHttpRepository:user:password:'
				with: self
				with: oop1 
				with: oop2.
		].
!

userAndPassword

	^gciSession
		serverPerform: #'mcUserAndPasswordInHTTP:' 
		with: self.
! !
!MCHttpRepository categoriesFor: #storeVersion:name:message:from:!public! !
!MCHttpRepository categoriesFor: #updateVersionDialogTabIn:!public! !
!MCHttpRepository categoriesFor: #user:password:!public! !
!MCHttpRepository categoriesFor: #userAndPassword!public! !

MCServerDirectoryRepository guid: (GUID fromString: '{45FEE87B-B286-47B5-852A-E006BFB4FC16}')!
MCServerDirectoryRepository comment: ''!
!MCServerDirectoryRepository categoriesForClass!Unclassified! !
MCFileTreeRepository guid: (GUID fromString: '{55009128-51C7-4147-86A7-9ABD1FA99F29}')!
MCFileTreeRepository comment: ''!
!MCFileTreeRepository categoriesForClass!Unclassified! !
MCGitHubRepository guid: (GUID fromString: '{FD249A8C-2D38-4C39-88B8-A5C317D8C9CF}')!
MCGitHubRepository comment: ''!
!MCGitHubRepository categoriesForClass!Unclassified! !
!MCGitHubRepository methodsFor!

fullNameOfPackage: packageName versionName: versionName 

	^packageName , '.package'.!

packageList
 
	| list dictionary |
	list := self allVersionInfoNames.
	list := list select: [:each | each endsWith: '.package'].
	dictionary := Dictionary new.
	list do: [:each | 
		| packageName version versions |
		packageName := each copyFrom: 1 to: each size - 8.
		version := 'current'.
		versions := dictionary
			at: packageName
			ifAbsentPut: [OrderedCollection new].
		versions addFirst: version.
	].
	list := OrderedCollection new.
	dictionary keysAndValuesDo: [:eachKey :eachValue | 
		list add: eachKey -> eachValue.
	].
	list := list asSet asSortedCollection.
	list := list collect: [:each | 
		MCPackage new
			name: each key;
			versionNames: each value;
			repository: self;
			yourself.
	].
	^list.
!

versionInfoForPackageNamed: aString version: ignored 

	^self versionInfoForPackageNamed: aString , '.package'.! !
!MCGitHubRepository categoriesFor: #fullNameOfPackage:versionName:!public! !
!MCGitHubRepository categoriesFor: #packageList!public! !
!MCGitHubRepository categoriesFor: #versionInfoForPackageNamed:version:!public! !

MCClassDefinition guid: (GUID fromString: '{4D46B46E-F290-4992-93C5-79CAEE623C11}')!
MCClassDefinition comment: ''!
!MCClassDefinition categoriesForClass!Unclassified! !
!MCClassDefinition methodsFor!

className

	^className.
!

detailsString

	^'Class Definition'.
!

displayMemo

	^definition.
!

displayText

	^'displayText'.
!

initialize: aStream

	| i j |
	definition := (aStream upTo: Character lf) 
		copyReplaceAll: Character cr asString 
		with: Character cr asString , Character lf asString.
	i := definition indexOf: $#.
	j := definition indexOf: Character cr.
	className := definition copyFrom: i + 1 to: j - 1.
! !
!MCClassDefinition categoriesFor: #className!public! !
!MCClassDefinition categoriesFor: #detailsString!public! !
!MCClassDefinition categoriesFor: #displayMemo!public! !
!MCClassDefinition categoriesFor: #displayText!public! !
!MCClassDefinition categoriesFor: #initialize:!public! !

MCMethodDefinition guid: (GUID fromString: '{2FC81943-2846-4631-ABF1-F40BD2DA0274}')!
MCMethodDefinition comment: ''!
!MCMethodDefinition categoriesForClass!Unclassified! !
!MCMethodDefinition methodsFor!

category
	^category!

classIsMeta
	^classIsMeta!

className

	^className , (classIsMeta ifTrue: [' class'] ifFalse: ['']).
!

detailsString

	^category , ' - ' , timeStamp.
!

displayMemo

	^source.
!

displayText

	^category, ' - ' , timeStamp.
!

initialize: aStream

	| size |
	timeStamp:= aStream upTo: Character tab.
	className := aStream upTo: Character tab.
	classIsMeta := (aStream upTo: Character tab) = 'true'.
	category := aStream upTo: Character tab.
	selector := aStream upTo: Character tab.
	size := (aStream upTo: Character tab) asNumber + 1.
	source := (aStream next: size) copyReplacing: Character cr withObject: Character lf.
!

selector
	^selector!

source
	^source!

timeStamp
	^timeStamp! !
!MCMethodDefinition categoriesFor: #category!accessing!public! !
!MCMethodDefinition categoriesFor: #classIsMeta!accessing!public! !
!MCMethodDefinition categoriesFor: #className!accessing!public! !
!MCMethodDefinition categoriesFor: #detailsString!public! !
!MCMethodDefinition categoriesFor: #displayMemo!public! !
!MCMethodDefinition categoriesFor: #displayText!public! !
!MCMethodDefinition categoriesFor: #initialize:!private! !
!MCMethodDefinition categoriesFor: #selector!accessing!public! !
!MCMethodDefinition categoriesFor: #source!accessing!public! !
!MCMethodDefinition categoriesFor: #timeStamp!accessing!public! !

MCOrganizationDefinition guid: (GUID fromString: '{80167200-683E-4723-81D0-1471BCA0142B}')!
MCOrganizationDefinition comment: ''!
!MCOrganizationDefinition categoriesForClass!Unclassified! !
!MCOrganizationDefinition methodsFor!

className

	^''.
!

detailsString

	^'Class Categories'.
!

displayMemo

	| stream |
	stream := WriteStream on: String new.
	categories do: [:each | 
		stream nextPutAll: each; cr.
	].
	^stream contents.
!

displayText

	^'Class Categories'.
!

initialize: aStream

	categories := aStream nextLine subStrings: Character tab.
! !
!MCOrganizationDefinition categoriesFor: #className!public! !
!MCOrganizationDefinition categoriesFor: #detailsString!public! !
!MCOrganizationDefinition categoriesFor: #displayMemo!public! !
!MCOrganizationDefinition categoriesFor: #displayText!public! !
!MCOrganizationDefinition categoriesFor: #initialize:!public! !

MCPatchBrowser guid: (GUID fromString: '{6A36F578-7C9D-4ACE-A3B0-D2E1313FBBCA}')!
MCPatchBrowser comment: ''!
!MCPatchBrowser categoriesForClass!Unclassified! !
!MCPatchBrowser methodsFor!

createComponents

	super createComponents.
	includeIdenticalPresenter	:= self add: BooleanPresenter	new name: 'includeIdenticalSource'.
	operationListPresenter 		:= self add: ListPresenter 			new name: 'operationList'.
	leftTextPresenter				:= self add: TextPresenter			new name: 'leftText'.
	leftMemoPresenter			:= self add: TextPresenter			new name: 'leftMemo'.
	rightTextPresenter			:= self add: TextPresenter			new name: 'rightText'.
	rightMemoPresenter			:= self add: TextPresenter			new name: 'rightMemo'.
!

createSchematicWiring

	super createSchematicWiring.
	includeIdenticalPresenter	when: #'valueChanged'		send: #'refresh'					to: self.
	operationListPresenter 		when: #'selectionChanged' send: #'operationSelected' 	to: self.
!

inspectLine

	| line |
	(line := operationListPresenter selectionOrNil) isNil ifTrue: [^self].
	line halt.
!

logoutRequested: aValueHolder
	"Opportunity to save changes."

	aValueHolder value: true.
!

onViewClosed

	model notNil ifTrue: [
		| temp |
		temp := model.
		model := nil.
		temp removeEventsTriggeredFor: self.
	].
	super onViewClosed.
!

onViewOpened

	super onViewOpened.
	self caption: model name.
	self refresh.
	model gciSession
		when: #'logoutRequested:'	send: #'logoutRequested:'	to: self;
		when: #'logoutPending'		send: #'exit'						to: self;
		yourself.
!

operationSelected

	| operation |
	leftTextPresenter 		value: ''.
	leftMemoPresenter	value: ''.
	rightTextPresenter	value: ''.
	rightMemoPresenter	value: ''.
	operationListPresenter hasSelection ifFalse: [^self].
	operation := operationListPresenter selection.
	leftTextPresenter 		value: operation obsoletionText.
	leftMemoPresenter	value: operation obsoletionMemo.
	rightTextPresenter	value: operation modificationText.
	rightMemoPresenter	value: operation modificationMemo.
	operation obsoletionMemoMarkers do: [:each | 
		leftMemoPresenter view addMarkerType: each key at: each value.
	].
	operation modificationMemoMarkers do: [:each |
		rightMemoPresenter view addMarkerType: each key at: each value.
	].
!

operationsList

	| list |
	list := model operations.
	includeIdenticalPresenter value ifFalse: [
		list := list reject: [:each | each hasEquivalentText].
	].
	^list asSortedCollection asArray.
!

refresh

	operationListPresenter
		resetSelection;
		list: self operationsList;
		yourself! !
!MCPatchBrowser categoriesFor: #createComponents!public! !
!MCPatchBrowser categoriesFor: #createSchematicWiring!public! !
!MCPatchBrowser categoriesFor: #inspectLine!public! !
!MCPatchBrowser categoriesFor: #logoutRequested:!public! !
!MCPatchBrowser categoriesFor: #onViewClosed!public! !
!MCPatchBrowser categoriesFor: #onViewOpened!public! !
!MCPatchBrowser categoriesFor: #operationSelected!public! !
!MCPatchBrowser categoriesFor: #operationsList!public! !
!MCPatchBrowser categoriesFor: #refresh!public! !

!MCPatchBrowser class methodsFor!

icon

	^Icon fromFile: 'icons\GS32x32.ico'.
!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  1601 1201 551 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 0 16 234 256 608 0 0 0 0 0 1 0 0 0 0 1 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 2879 21 530 1201 801 416 706 8 #updateMenuBar 608 416 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 247 7 0 0 154 1 0 0] 98 3 410 8 ##(Smalltalk.ContainerView)  98 15 0 416 98 2 8 1140850688 131073 896 0 0 0 7 0 0 0 896 852230 ##(Smalltalk.FramingLayout)  234 240 98 4 410 8 ##(Smalltalk.ListView)  98 30 0 896 98 2 8 1409355853 1025 1040 590662 2 ##(Smalltalk.ListModel)  202 208 608 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  482 8 4278190080 0 7 265030 4 ##(Smalltalk.Menu)  0 16 98 1 984134 2 ##(Smalltalk.CommandMenuItem)  1 1180998 4 ##(Smalltalk.CommandDescription)  8 #inspectLine 8 'Inspect' 1 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 1040 0 8 4294904169 459270 ##(Smalltalk.Message)  8 #displayString 98 0 8 ##(Smalltalk.IconicListAbstract)  1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 0 202 208 98 4 920646 5 ##(Smalltalk.ListViewColumn)  8 'Type' 201 8 #left 1410 1440 98 0 1410 8 #<= 1632 787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  2 1 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 8 '[:each | each typeString]' 8 #[30 105 226 0 106] 8 #typeString 1696 7 257 0 0 1040 0 1 0 0 1554 8 'Class' 201 1600 1410 1440 1456 8 ##(Smalltalk.SortedCollection)  1682 0 0 1714 2 1 1744 8 'doIt' 8 '[:each | each className]' 8 #[30 105 226 0 106] 8 #className 1888 7 257 0 0 1040 0 1 0 0 1554 8 'Selector' 201 1600 1410 1440 1632 1410 1664 1632 1682 0 0 1714 2 1 1744 8 'doIt' 8 '[:each | each selector]' 8 #[30 105 226 0 106] 8 #selector 2048 7 257 0 0 1040 0 1 0 0 1554 8 'Detail' 569 1600 1410 1440 1632 1410 1664 1632 1682 0 0 1714 2 1 1744 8 'doIt' 8 '[:each | each detailsString]' 8 #[30 105 226 0 106] 8 #detailsString 2208 7 257 0 0 1040 0 3 0 0 8 #report 608 0 131169 0 0 642 202 208 98 3 706 736 98 2 530 1 41 530 1169 313 1040 706 8 #contextMenu: 98 1 1248 1040 706 8 #text: 98 1 8 'Type' 1040 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 72 2 0 0 176 0 0 0] 98 0 530 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 1 2642 8 #fixedParentRight 1 2642 8 #fixedParentTop 41 2642 8 #fixedParentBottom 1 410 8 ##(Smalltalk.CheckBox)  98 16 0 896 98 2 8 1409363203 1 2784 721990 2 ##(Smalltalk.ValueHolder)  0 0 1114118 ##(Smalltalk.NeverSearchPolicy)  32 0 0 7 0 0 0 2784 0 8 4294904245 852486 ##(Smalltalk.NullConverter)  0 0 0 642 202 208 98 2 706 736 98 2 530 5 1 530 511 41 2784 706 2496 98 1 8 'Include methods with identical source' 2784 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 2 0 0 0 0 0 0 0 1 1 0 0 20 0 0 0] 98 0 2592 0 27 2610 2656 5 2642 8 #fixedViewLeft 511 2720 1 2642 8 #fixedViewTop 41 234 256 98 4 1040 8 'operationList' 2784 8 'includeIdenticalSource' 0 642 202 208 98 1 706 736 98 2 530 1 1 530 1169 353 896 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 176 0 0 0] 98 2 2784 1040 2592 0 27 410 8 ##(Smalltalk.Splitter)  98 12 0 416 98 2 8 1140850688 1 3488 0 482 8 4278190080 0 519 0 0 0 3488 642 202 208 98 1 706 736 98 2 530 1 353 530 1169 19 3488 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 176 0 0 0 72 2 0 0 185 0 0 0] 98 0 2592 0 27 410 912 98 15 0 416 98 2 8 1140850688 131073 3760 0 0 0 7 0 0 0 3760 562 234 240 608 32 234 256 608 0 642 202 208 98 1 706 736 98 2 530 1 371 530 1169 355 3760 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 185 0 0 0 72 2 0 0 106 1 0 0] 98 3 410 912 98 15 0 3760 98 2 8 1140850688 131073 4032 0 0 0 7 0 0 0 4032 978 234 240 98 4 410 8 ##(Smalltalk.TextEdit)  98 16 0 4032 98 2 8 1140916352 1025 4144 0 482 8 4278190080 0 7 0 0 0 4144 0 8 4294904227 2946 0 0 3 642 202 208 98 3 706 736 98 2 530 1 1 530 575 39 4144 706 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 4144 706 8 #isTextModified: 98 1 32 4144 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 31 1 0 0 19 0 0 0] 98 0 2592 0 27 2610 2656 1 2688 1 2720 1 3232 39 410 8 ##(Smalltalk.ScintillaView)  98 46 0 4032 98 2 8 1445007428 1025 4592 2866 0 32 1310726 ##(Smalltalk.EqualitySearchPolicy)  0 482 8 4278190080 0 7 0 0 0 4592 0 8 4294904201 2946 0 0 11 0 234 256 98 42 8 #lineNumber 1182726 ##(Smalltalk.ScintillaTextStyle)  67 0 0 1 0 0 0 0 4816 0 0 0 8 #specialSelector 4834 33 196934 1 ##(Smalltalk.RGB)  16646145 0 3 0 0 0 0 4864 0 0 0 8 #global 4834 21 0 0 3 0 0 0 0 4928 0 0 0 8 #normal 4834 1 0 0 1 0 0 0 0 4960 0 0 0 8 #boolean 4834 13 4912 0 3 0 0 0 0 4992 0 0 0 8 #special 4834 25 0 0 3 0 0 0 0 5024 0 0 0 8 #number 4834 5 4898 16711169 0 1 0 0 0 0 5056 0 0 0 8 #nil 4834 19 4912 0 3 0 0 0 0 5104 0 0 0 8 #character 4834 31 4898 16646399 0 3 0 0 0 0 5136 0 0 0 8 #braceHighlight 4834 69 786694 ##(Smalltalk.IndexedColor)  33554465 0 3 0 0 0 0 5184 0 0 0 8 #indentGuide 4834 75 5218 33554447 0 1 0 0 0 0 5248 0 0 0 8 #string 4834 3 4898 16646399 0 129 0 0 0 0 5296 0 0 0 8 #symbol 4834 9 5218 33554443 0 1 0 0 0 0 5344 0 0 0 8 #super 4834 17 4912 0 3 0 0 0 0 5392 0 0 0 8 #comment 4834 7 4898 65025 0 1 0 0 0 0 5424 0 0 0 8 #binary 4834 11 5218 33554433 0 1 0 0 0 0 5472 0 0 0 8 #assignment 4834 29 0 0 3 0 0 0 0 5520 0 0 0 8 #keywordSend 4834 27 5218 33554437 0 3 0 0 0 0 5552 0 0 0 8 #return 4834 23 4898 321 0 3 0 0 0 0 5600 0 0 0 8 #braceMismatch 4834 71 5218 33554459 0 3 0 0 0 0 5648 0 0 0 8 #self 4834 15 4912 0 3 0 0 0 0 5696 0 0 0 98 40 4976 5312 5072 5440 5360 5488 5008 5712 5408 5120 4944 5616 5040 5568 5536 5152 4880 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4848 5200 5664 0 5264 0 0 1245510 1 ##(Smalltalk.NullScintillaStyler)  4960 234 256 98 8 8 #changed 1639942 ##(Smalltalk.ScintillaMarkerDefinition)  7 45 5504 4898 16908287 4592 5808 8 #removed 5826 5 45 5504 4898 16843263 4592 5872 8 #circle 5826 1 1 5504 5218 33554471 4592 5920 8 #added 5826 3 45 5504 4898 16908033 4592 5968 202 208 608 0 63 9215 0 0 0 0 5280 0 0 0 0 0 0 8 '' 3 234 256 98 2 8 #container 234 256 98 2 4960 4834 1 0 0 1 0 0 0 0 4960 0 0 0 0 0 0 0 1 0 234 256 98 6 1 1509190 1 ##(Smalltalk.ScintillaIndicatorStyle)  1 4592 65025 3 32 1 0 3 6178 3 4592 33423361 5 32 3 0 5 6178 5 4592 511 1 32 5 0 642 202 208 98 9 706 736 98 2 530 1 41 530 575 315 4592 706 4416 98 1 4450 3 1 3 4592 706 4496 98 1 32 4592 706 8 #modificationEventMask: 98 1 9215 4592 706 8 #margins: 98 1 98 3 984582 ##(Smalltalk.ScintillaMargin)  1 4592 1 3 32 1 6546 3 4592 33 1 32 67108863 6546 5 4592 1 1 16 -67108863 4592 706 8 #indentationGuides: 98 1 0 4592 706 8 #tabIndents: 98 1 16 4592 706 8 #tabWidth: 98 1 9 4592 706 8 #setLexerLanguage: 98 1 8 #smalltalk 4592 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 31 1 0 0 177 0 0 0] 98 0 2592 0 27 2610 2656 1 2688 1 2720 41 2752 1 234 256 98 4 4144 8 'leftText' 4592 8 'leftMemo' 0 642 202 208 98 1 706 736 98 2 530 1 1 530 575 355 4032 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 31 1 0 0 177 0 0 0] 98 2 4144 4592 2592 0 27 410 3504 98 12 0 3760 98 2 8 1140850688 1 7104 0 482 3584 0 519 0 0 0 7104 642 202 208 98 1 706 736 98 2 530 575 1 530 19 355 7104 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 31 1 0 0 0 0 0 0 40 1 0 0 177 0 0 0] 98 0 2592 0 27 410 912 98 15 0 3760 98 2 8 1140850688 131073 7344 0 0 0 7 0 0 0 7344 978 234 240 98 4 410 4608 98 46 0 7344 98 2 8 1445007428 1025 7456 2866 0 32 4704 0 482 4736 0 7 0 0 0 7456 0 8 4294904201 2946 0 0 11 0 234 256 98 42 4816 4834 67 0 0 1 0 0 0 0 4816 0 0 0 4864 4834 33 4912 0 3 0 0 0 0 4864 0 0 0 4928 4834 21 0 0 3 0 0 0 0 4928 0 0 0 4960 4834 1 0 0 1 0 0 0 0 4960 0 0 0 4992 4834 13 4912 0 3 0 0 0 0 4992 0 0 0 5024 4834 25 0 0 3 0 0 0 0 5024 0 0 0 5056 4834 5 5088 0 1 0 0 0 0 5056 0 0 0 5104 4834 19 4912 0 3 0 0 0 0 5104 0 0 0 5136 4834 31 5168 0 3 0 0 0 0 5136 0 0 0 5184 4834 69 5232 0 3 0 0 0 0 5184 0 0 0 5248 4834 75 5280 0 1 0 0 0 0 5248 0 0 0 5296 4834 3 5328 0 129 0 0 0 0 5296 0 0 0 5344 4834 9 5376 0 1 0 0 0 0 5344 0 0 0 5392 4834 17 4912 0 3 0 0 0 0 5392 0 0 0 5424 4834 7 5456 0 1 0 0 0 0 5424 0 0 0 5472 4834 11 5504 0 1 0 0 0 0 5472 0 0 0 5520 4834 29 0 0 3 0 0 0 0 5520 0 0 0 5552 4834 27 5584 0 3 0 0 0 0 5552 0 0 0 5600 4834 23 5632 0 3 0 0 0 0 5600 0 0 0 5648 4834 71 5680 0 3 0 0 0 0 5648 0 0 0 5696 4834 15 4912 0 3 0 0 0 0 5696 0 0 0 98 40 7664 7792 7712 7840 7808 7856 7680 7936 7824 7728 7648 7904 7696 7888 7872 7744 7632 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 7616 7760 7920 0 7776 0 0 5746 4960 234 256 98 8 5808 5826 7 45 5504 4898 16908287 7456 5808 5872 5826 5 45 5504 4898 16843263 7456 5872 5920 5826 1 1 5504 5952 7456 5920 5968 5826 3 45 5504 4898 16908033 7456 5968 202 208 608 0 63 9215 0 0 0 0 5280 0 0 0 0 0 0 8 '' 3 234 256 98 2 6080 234 256 98 2 4960 4834 1 0 0 1 0 0 0 0 4960 0 0 0 0 0 0 0 1 0 234 256 98 6 1 6178 1 7456 65025 3 32 1 0 3 6178 3 7456 33423361 5 32 3 0 5 6178 5 7456 511 1 32 5 0 642 202 208 98 9 706 736 98 2 530 1 41 530 577 315 7456 706 4416 98 1 4450 3 1 3 7456 706 4496 98 1 32 7456 706 6448 98 1 9215 7456 706 6496 98 1 98 3 6546 1 7456 1 3 32 1 6546 3 7456 33 1 32 67108863 6546 5 7456 1 1 16 -67108863 7456 706 6624 98 1 0 7456 706 6672 98 1 16 7456 706 6720 98 1 9 7456 706 6768 98 1 6800 7456 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 32 1 0 0 177 0 0 0] 98 0 2592 0 27 2610 2656 1 2688 1 2720 41 2752 1 410 4160 98 16 0 7344 98 2 8 1140916352 1025 8832 0 482 4240 0 7 0 0 0 8832 0 8 4294904227 2946 0 0 3 642 202 208 98 3 706 736 98 2 530 1 1 530 577 39 8832 706 4416 98 1 4450 3 1 3 8832 706 4496 98 1 32 8832 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 32 1 0 0 19 0 0 0] 98 0 2592 0 27 2610 2656 1 2688 1 2720 1 3232 39 234 256 98 4 7456 8 'rightMemo' 8832 8 'rightText' 0 642 202 208 98 1 706 736 98 2 530 593 1 530 577 355 7344 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 40 1 0 0 0 0 0 0 72 2 0 0 177 0 0 0] 98 2 8832 7456 2592 0 27 2592 0 27 2592 0 27 )! !
!MCPatchBrowser class categoriesFor: #icon!public! !
!MCPatchBrowser class categoriesFor: #resource_Default_view!public!resources-views! !

MCRepositoryBrowser guid: (GUID fromString: '{ED52D4AC-3C47-4B09-830B-99228086125C}')!
MCRepositoryBrowser comment: ''!
!MCRepositoryBrowser categoriesForClass!Unclassified! !
!MCRepositoryBrowser methodsFor!

addDirectoryRepository

	| path |
	path := BrowseFolderDialog new 
		title: 'Please select a Monticello folder';
		showModal.
	path isNil ifTrue: [^self].
	self model 
		withOopForString: path 
		do: [:oopType |
			self model
				serverPerform: #'mcNewDirectoryRepository:' 
				with: oopType.
	].
	self updateRepositoryList.
!

addFileTreeRepository

	| path |
	(path := Prompter prompt: 'Enter server path:') isNil ifTrue: [^self].
	self model 
		withOopForString: path 
		do: [:oopType |
			self model
				serverPerform: #'mcNewFileTreeRepository:' 
				with: oopType.
	].
	self updateRepositoryList.
!

addGitHubRepository

	| path |
	(path := Prompter prompt: 'Enter location (e.g., ''github://glassdb/zinc:gemstone3.1/repository''):') isNil ifTrue: [^self].
	self model 
		withOopForString: path 
		do: [:oopType |
			self model
				serverPerform: #'mcNewGitHubRepository:' 
				with: oopType.
	].
	self updateRepositoryList.
!

addHttpRepository

	| info string delimiter |
	(info := MCHttpRepositoryInfoDialog showModalOn: MCHttpRepositoryInfo new) isNil ifTrue: [^self].
	delimiter := (Character codePoint: 255) asString.
	string := info location , delimiter , info user , delimiter , info password.
	model 
		withOopForString: string 
		do: [:oopType | 
			model
				serverPerform: #'mcAddHttpRepository:' 
				with: oopType.
		].
	self updateRepositoryList.
!

addRepository

	| type |
	type := ChoicePrompter
		choices: #(#'HTTP' #'Client Directory' #'Server Directory' #'Server Directory with FileTree' #'Server Directory with GitHub' ) 
		caption: 'Choose Repository Type'.
	type isNil ifTrue: [^self].
	type = #'HTTP' ifTrue: [^self addHttpRepository].
	type = #'Client Directory' ifTrue: [^self addDirectoryRepository].
	type = #'Server Directory' ifTrue: [^self addServerDirectoryRepository].
	type = #'Server Directory with FileTree' ifTrue: [^self addFileTreeRepository].
	type = #'Server Directory with GitHub' ifTrue: [^self addGitHubRepository].
	self error: 'Unrecognized repository type: ' , type printString.
!

addServerDirectoryRepository

	| path |
	(path := Prompter prompt: 'Enter server path:') isNil ifTrue: [^self].
	self model 
		withOopForString: path 
		do: [:oopType |
			self model
				serverPerform: #'mcNewServerDirectoryRepository:' 
				with: oopType.
	].
	self updateRepositoryList.
!

compareVersion

	| selections left right patch |
	selections := versionListPresenter selections.
	(selections isEmpty or: [2 < selections size]) ifTrue: [
		MessageBox notify: 'Please select one or two versions!!'. 
		^self.
	].
	left := selections size = 1
		ifTrue: [nil]
		ifFalse: [packageListPresenter selection name , '-' , selections first name].
	right := packageListPresenter selection name , '-' , selections last name.
	right < left ifTrue: [
		| temp |
		temp := right.
		right := left.
		left := temp.
	].
	patch := repositoryListPresenter selection patchFrom: left to: right.
	MCPatchBrowser showOn: patch.
!

createComponents

	super createComponents.
	repositoryListPresenter 						:= self add: ListPresenter new name: 'repositoryList'.
	packageListPresenter 						:= self add: ListPresenter new name: 'packageList'.
	versionListPresenter 							:= self add: ListPresenter new name: 'versionList'.
	repositoryCreationTemplatePresenter	:= self add: TextPresenter new name: 'repositoryCreationTemplate'.
	versionNamePresenter 						:= self add: TextPresenter new name: 'versionName'.
	versionDatePresenter 						:= self add: TextPresenter new name: 'versionDate'.
	versionTimePresenter 						:= self add: TextPresenter new name: 'versionTime'.
	versionAuthorPresenter 					:= self add: TextPresenter new name: 'versionAuthor'.
	versionIDPresenter							:= self add: TextPresenter new name: 'versionID'.
	versionAncestorsPresenter 				:= self add: ListPresenter new name: 'versionAncestors'.
	versionStepChildrenPresenter 			:= self add: ListPresenter new name: 'versionStepChildren'.
	versionMessagePresenter 					:= self add: TextPresenter new name: 'versionMessage'.

!

createSchematicWiring

	super createSchematicWiring.
	repositoryListPresenter		when: #'selectionChanged' send: #'updatePackageList' to: self.
	repositoryListPresenter		when: #'selectionChanged' send: #'updateRepositoryCreationTemplate' to: self.
	packageListPresenter		when: #'selectionChanged' send: #'updateVersionList' to: self.
	versionListPresenter			when: #'selectionChanged' send: #'updateVersionInfo' to: self.
!

editRepository

	MessageBox notify: 'Sorry, we are not yet prepared to do that!!'.
	Keyboard default isShiftDown ifTrue: [self halt].
!

fileTypes

	^Array
		with: #('GemStone Files (*.gs)' '*.gs')
		with: #('Smalltalk Files (*.st)' '*.st')
		with: #('Topaz Files (*.tpz)' '*.tpz')
		with: FileDialog allFilesType.
!

getLoadedVersionNames

	| string list |
	(string := model serverPerform: #'mcLoadedVersionNames') isNil ifTrue: [^self].
	loadedVersionNames := Dictionary new.
	list := string subStrings: Character lf.
	list := list collect: [:each | each subStrings: Character tab].
	list do: [:each | 
		| i name version |
		string := each at: 1.
		i := string findLast: [:char | char = $-].
		name := i = 0 
			ifTrue: [string]
			ifFalse: [string copyFrom: 1 to: i - 1].
		version := i = 0
			ifTrue: ['']
			ifFalse: [string copyFrom: i + 1 to: string size].
		loadedVersionNames
			at: name
			put: version -> ((each at: 2) = 'Y').
	].
!

loadVersion

	| repository package versionName |
	(repository := repositoryListPresenter selectionOrNil) isNil ifTrue: [^self].
	(package := packageListPresenter selectionOrNil) isNil ifTrue: [^self].
	(versionName := versionListPresenter selectionOrNil) isNil ifTrue: [^self].
	repository
		loadPackageNamed: package name
		versionName: versionName name.
	self getLoadedVersionNames.
	versionListPresenter list do: [:each | each isLoaded: false; isModified: false].
	versionName isLoaded: true.
	package loaded: versionName name -> false.
	packageListPresenter view refreshContents.
	packageListPresenter selection: package.
	versionListPresenter selection: versionName.
!

logoutRequested: aValueHolder

	"Private - Opportunity to save changes."

	aValueHolder value: true.
!

mergeVersion

	| repository package versionName versionFullName | 
	(repository := repositoryListPresenter selectionOrNil) isNil ifTrue: [^self].
	(package := packageListPresenter selectionOrNil) isNil ifTrue: [^self].
	(versionName := versionListPresenter selectionOrNil) isNil ifTrue: [^self].
	versionFullName := package name , '-' , versionName name.
	self model 
		withOopForString: versionFullName 
		do: [:oopType |
			self model
				serverPerform: #'mcVersionMerge:from:autoMigrate:' 
				with: oopType
				with: repository
				with: true.
	].
	self getLoadedVersionNames.
	versionListPresenter list do: [:each | each isLoaded: false; isModified: false].
	versionName isLoaded: true.
	package loaded: versionName name -> false.
	packageListPresenter view refreshContents.
	packageListPresenter selection: package.
	versionListPresenter selection: versionName.
!

model: aGciSession

	super model: aGciSession.
	model
		when: #'logoutRequested:'	send: #'logoutRequested:'	to: self;
		when: #'logoutPending'		send: #'exit'						to: self;
		yourself.
!

onViewClosed

	model notNil ifTrue: [
		| temp |
		temp := model.
		model := nil.
		temp removeEventsTriggeredFor: self.
	].
	super onViewClosed.
!

onViewOpened

	super onViewOpened.
	self 
		getLoadedVersionNames;
		updateRepositoryList;
		yourself.
!

queryCommand: aCommandQuery

	(#(#removeRepository) includes: aCommandQuery commandSymbol) ifTrue: [
		aCommandQuery isEnabled: repositoryListPresenter hasSelection.
		^true.
	].
	(#(#removePackage) includes: aCommandQuery commandSymbol) ifTrue: [
		aCommandQuery isEnabled: packageListPresenter hasSelection.
		^true.
	].
	(#(#loadVersion) includes: aCommandQuery commandSymbol) ifTrue: [
		aCommandQuery isEnabled: versionListPresenter hasSelection.
		^true.
	].
	^super queryCommand: aCommandQuery.
!

removeRepository

	| repository |
	repository := repositoryListPresenter selection.
	model
		serverPerform: #'mcRemoveRepository:'
		with: repository.
	self updateRepositoryList.
!

saveAsTopazFileIn

	| repository packageName index fileName path bytes |
	(repository := repositoryListPresenter selectionOrNil) ifNil: [^self].
	packageName := repository 
		fullNameOfPackage: packageListPresenter selection name 
		versionName: versionListPresenter selection name.
	index := (packageName includes: $.)
		ifTrue: [(packageName subStrings: $.) last size]
		ifFalse: [-1].
	fileName := packageName copyFrom: 1 to: packageName size - index - 1.
	path := FileSaveDialog new
		caption: 'File Out ' , fileName;
		fileTypes: self fileTypes;
		defaultExtension: 'gs';
		value: fileName;
		overwritePrompt;
		showModal.
	path isNil ifTrue: [^self].
	bytes := repository topazFrom: packageName.
	(FileStream 
		write: path
		text: false)
		nextPutAll: bytes;
		close.
!

updateCaption

	self caption: (model titleBarFor: 'Monticello Browser').
!

updatePackageList

	| repository list |
	packageListPresenter list: #().
	(repository := repositoryListPresenter selectionOrNil) isNil ifTrue: [^self].
	list := repository packageList.
	list do: [:each | 
		each loaded: (loadedVersionNames 
			at: each name
			ifAbsent: [nil]).
	].
	packageListPresenter list: list.
!

updateRepositoryCreationTemplate

	| repository string i j |
	repositoryCreationTemplatePresenter view ensureVisible.
	repositoryCreationTemplatePresenter value: ''.
	(repository := repositoryListPresenter selectionOrNil) isNil ifTrue: [^self].
	(string := repository creationTemplate) isNil ifTrue: [^self].
	0 < (i := string indexOfSubCollection: 'password:') ifTrue: [
		i := i + 10.
		j := string
			indexOfSubCollection: ''''
			startingAt: i + 1.
		i + 1 < j ifTrue: [
			string := (string copyFrom: 1 to: i) , '*****' , (string copyFrom: j to: string size).
		].
	].
	repositoryCreationTemplatePresenter value: string.
!

updateRepositoryList

	| list |
	repositoryCreationTemplatePresenter view ensureVisible.
	list := MCRepository allIn: self model.
	repositoryListPresenter list: list asSortedCollection.
!

updateVersionInfo

	| repository package versionName version |
	(self view viewNamed: 'versionInfo') ensureVisible.
	versionNamePresenter 				value: ''.
	versionDatePresenter 				value: ''.
	versionTimePresenter 				value: ''.
	versionAuthorPresenter 			value: ''.
	versionIDPresenter 					value: ''.
	versionAncestorsPresenter 		list: #().
	versionStepChildrenPresenter 	list: #().
	versionMessagePresenter 			value: ''.
	(repository := repositoryListPresenter selectionOrNil) isNil ifTrue: [^self].
	(package := packageListPresenter selectionOrNil) isNil ifTrue: [^self].
	(versionName := versionListPresenter selectionOrNil) isNil ifTrue: [^self].
	(version := package infoForVersion: versionName name) isNil ifTrue: [^self].
	versionNamePresenter 				value: 	version name.
	versionDatePresenter 				value: 	version date.
	versionTimePresenter 				value: 	version time.
	versionAuthorPresenter 			value: 	version author.
	versionIDPresenter 					value: 	version id.
	versionAncestorsPresenter 		list: 		version ancestors.
	versionStepChildrenPresenter 	list: 		version stepChildren.
	versionMessagePresenter 			value: 	version message.
!

updateVersionList

	| package list name |
	versionListPresenter list: #().
	(package := packageListPresenter selectionOrNil) isNil ifTrue: [^self].
	list := package versionNames.
	package isLoaded ifTrue: [
		name := package loadedEditionName.
		list do: [:each | 
			each printString = name ifTrue: [
				each 
					isLoaded: true;
					isModified: package isModified;
					yourself.
			].
		].
	].
	versionListPresenter list: list reverse.
! !
!MCRepositoryBrowser categoriesFor: #addDirectoryRepository!public! !
!MCRepositoryBrowser categoriesFor: #addFileTreeRepository!public! !
!MCRepositoryBrowser categoriesFor: #addGitHubRepository!public! !
!MCRepositoryBrowser categoriesFor: #addHttpRepository!public! !
!MCRepositoryBrowser categoriesFor: #addRepository!public! !
!MCRepositoryBrowser categoriesFor: #addServerDirectoryRepository!public! !
!MCRepositoryBrowser categoriesFor: #compareVersion!public! !
!MCRepositoryBrowser categoriesFor: #createComponents!public! !
!MCRepositoryBrowser categoriesFor: #createSchematicWiring!public! !
!MCRepositoryBrowser categoriesFor: #editRepository!public! !
!MCRepositoryBrowser categoriesFor: #fileTypes!public! !
!MCRepositoryBrowser categoriesFor: #getLoadedVersionNames!public! !
!MCRepositoryBrowser categoriesFor: #loadVersion!public! !
!MCRepositoryBrowser categoriesFor: #logoutRequested:!public! !
!MCRepositoryBrowser categoriesFor: #mergeVersion!public! !
!MCRepositoryBrowser categoriesFor: #model:!public! !
!MCRepositoryBrowser categoriesFor: #onViewClosed!public! !
!MCRepositoryBrowser categoriesFor: #onViewOpened!public! !
!MCRepositoryBrowser categoriesFor: #queryCommand:!public! !
!MCRepositoryBrowser categoriesFor: #removeRepository!public! !
!MCRepositoryBrowser categoriesFor: #saveAsTopazFileIn!public! !
!MCRepositoryBrowser categoriesFor: #updateCaption!public! !
!MCRepositoryBrowser categoriesFor: #updatePackageList!public! !
!MCRepositoryBrowser categoriesFor: #updateRepositoryCreationTemplate!public! !
!MCRepositoryBrowser categoriesFor: #updateRepositoryList!public! !
!MCRepositoryBrowser categoriesFor: #updateVersionInfo!public! !
!MCRepositoryBrowser categoriesFor: #updateVersionList!public! !

!MCRepositoryBrowser class methodsFor!

icon

	^Icon fromFile: 'icons\GS32x32.ico'.
!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  1401 1401 551 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 4 410 8 ##(Smalltalk.CardContainer)  98 16 0 416 98 2 8 1409286144 131073 624 0 482 8 4278190080 0 7 0 0 0 624 655878 ##(Smalltalk.CardLayout)  202 208 98 2 721414 ##(Smalltalk.Association)  8 'Version' 410 8 ##(Smalltalk.ContainerView)  98 15 0 624 98 2 8 1140850688 131073 848 0 0 0 7 0 0 0 848 852230 ##(Smalltalk.FramingLayout)  234 240 98 28 410 8 ##(Smalltalk.TextEdit)  98 16 0 848 98 2 8 1140916352 1025 992 0 482 8 4278190080 0 7 0 0 0 992 0 8 4294904477 852486 ##(Smalltalk.NullConverter)  0 0 1 983302 ##(Smalltalk.MessageSequence)  202 208 98 3 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 141 1 530 341 39 992 1218 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 992 1218 8 #isTextModified: 98 1 32 992 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 70 0 0 0 0 0 0 0 240 0 0 0 19 0 0 0] 98 0 530 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 141 1554 8 #fixedViewLeft 341 1554 8 #fixedParentTop 1 1554 8 #fixedViewTop 39 410 1008 98 16 0 848 98 2 8 1140916352 1025 1696 0 482 1088 0 7 0 0 0 1696 0 8 4294904477 1122 0 0 1 1154 202 208 98 3 1218 1248 98 2 530 141 123 530 341 39 1696 1218 1328 98 1 1362 3 1 3 1696 1218 1408 98 1 32 1696 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 70 0 0 0 61 0 0 0 240 0 0 0 80 0 0 0] 98 0 1504 0 27 1522 1568 141 1600 341 1632 123 1664 39 410 8 ##(Smalltalk.ListView)  98 30 0 848 98 2 8 1409355853 1025 2064 590662 2 ##(Smalltalk.ListModel)  202 208 98 0 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  482 8 4278190080 0 7 0 0 0 2064 0 8 4294904065 459270 ##(Smalltalk.Message)  8 #displayString 98 0 0 1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 0 202 208 98 1 920646 5 ##(Smalltalk.ListViewColumn)  8 'Ancestors' 573 8 #left 2290 2320 2336 8 ##(Smalltalk.SortedCollection)  0 0 2064 0 3 0 0 8 #report 2192 0 131169 0 0 1154 202 208 98 2 1218 1248 98 2 530 481 1 530 573 201 2064 1218 8 #text: 98 1 8 'Ancestors' 2064 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 240 0 0 0 0 0 0 0 14 2 0 0 100 0 0 0] 98 0 1504 0 27 1522 1568 481 1554 8 #fixedParentRight -299 1632 1 1664 201 410 1008 98 16 0 848 98 2 8 1140916352 1025 2800 0 482 1088 0 7 0 0 0 2800 0 8 4294904477 1122 0 0 1 1154 202 208 98 3 1218 1248 98 2 530 141 83 530 341 39 2800 1218 1328 98 1 1362 3 1 3 2800 1218 1408 98 1 32 2800 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 70 0 0 0 41 0 0 0 240 0 0 0 60 0 0 0] 98 0 1504 0 27 1522 1568 141 1600 341 1632 83 1664 39 410 8 ##(Smalltalk.StaticText)  98 16 0 848 98 2 8 1140850944 1 3168 0 0 0 7 0 0 0 3168 0 8 4294903569 1122 0 0 0 1154 202 208 98 2 1218 1248 98 2 530 1 1 530 141 39 3168 1218 2656 98 1 8 'Name:' 3168 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 70 0 0 0 19 0 0 0] 98 0 1504 0 27 1522 1568 1 1600 141 1632 1 1664 39 410 3184 98 16 0 848 98 2 8 1140850944 1 3504 0 0 0 7 0 0 0 3504 0 8 4294903569 1122 0 0 0 1154 202 208 98 2 1218 1248 98 2 530 1 201 530 141 39 3504 1218 2656 98 1 8 'Message:' 3504 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 100 0 0 0 70 0 0 0 119 0 0 0] 98 0 1504 0 27 1522 1568 1 1600 141 1632 201 1664 39 410 3184 98 16 0 848 98 2 8 1140850944 1 3824 0 0 0 7 0 0 0 3824 0 8 4294903569 1122 0 0 0 1154 202 208 98 2 1218 1248 98 2 530 1 121 530 141 39 3824 1218 2656 98 1 8 'Author:' 3824 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 60 0 0 0 70 0 0 0 79 0 0 0] 98 0 1504 0 27 1522 1568 1 1600 141 1632 121 1664 39 410 3184 98 16 0 848 98 2 8 1140850944 1 4144 0 0 0 7 0 0 0 4144 0 8 4294903569 1122 0 0 0 1154 202 208 98 2 1218 1248 98 2 530 1 161 530 141 39 4144 1218 2656 98 1 8 'ID:' 4144 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 80 0 0 0 70 0 0 0 99 0 0 0] 98 0 1504 0 27 1522 1568 1 1600 141 1632 161 1664 39 410 3184 98 16 0 848 98 2 8 1140850944 1 4464 0 0 0 7 0 0 0 4464 0 8 4294903569 1122 0 0 0 1154 202 208 98 2 1218 1248 98 2 530 1 81 530 141 39 4464 1218 2656 98 1 8 'Time:' 4464 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 40 0 0 0 70 0 0 0 59 0 0 0] 98 0 1504 0 27 1522 1568 1 1600 141 1632 81 1664 39 410 3184 98 16 0 848 98 2 8 1140850944 1 4784 0 0 0 7 0 0 0 4784 0 8 4294903569 1122 0 0 0 1154 202 208 98 2 1218 1248 98 2 530 1 41 530 141 39 4784 1218 2656 98 1 8 'Date:' 4784 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 70 0 0 0 39 0 0 0] 98 0 1504 0 27 1522 1568 1 1600 141 1632 41 1664 39 410 8 ##(Smalltalk.RichTextEdit)  98 18 0 848 98 2 8 1140920644 1025 5104 0 482 8 4278190080 0 7 265030 4 ##(Smalltalk.Menu)  0 16 98 10 984134 2 ##(Smalltalk.CommandMenuItem)  1 1180998 4 ##(Smalltalk.CommandDescription)  8 #chooseSelectionFont 8 '&Font...' 1 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem)  4097 5266 1 5298 8 #bePlain 8 '&Plain' 1 1 0 0 0 5266 1 5298 8 #toggleBold 8 '&Bold' 1 1 0 0 0 5266 1 5298 8 #toggleItalic 8 '&Italic' 1 1 0 0 0 5266 1 5298 8 #toggleUnderlined 8 '&Underlined' 1 1 0 0 0 5362 4097 5218 0 16 98 3 5266 1025 5298 8 #alignParagraphLeft 8 '&Left' 1 1 0 0 0 5266 1025 5298 8 #alignParagraphCenter 8 '&Centre' 1 1 0 0 0 5266 1025 5298 8 #alignParagraphRight 8 '&Right' 1 1 0 0 0 8 '&Align' 0 1 0 0 0 0 0 5362 4097 5266 1 5298 8 #chooseSelectionColor 8 '&Colour...' 1 1 0 0 0 8 '' 0 1 0 0 0 0 0 0 0 5104 0 8 1772716531 1122 0 0 11 0 655622 ##(Smalltalk.EDITSTREAM)  8 #[0 0 0 0 0 0 0 0 48 0 174 1] 1154 202 208 98 6 1218 1248 98 2 530 141 201 530 1213 469 5104 1218 8 #contextMenu: 98 1 5232 5104 1218 2656 98 1 524550 ##(Smalltalk.RichText)  8 '{\rtf1\ansi\ansicpg1252\deff0\deflang2057{\fonttbl{\f0\froman Times New Roman;}}
\viewkind4\uc1\pard\f0\fs22 
\par }
' 5104 1218 1328 98 1 1362 3 1 3 5104 1218 1408 98 1 32 5104 1218 8 #resetCharFormat 2192 5104 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 70 0 0 0 100 0 0 0 164 2 0 0 78 1 0 0] 98 0 1504 0 27 1522 1568 141 2768 1 1632 201 1554 8 #fixedParentBottom 1 410 1008 98 16 0 848 98 2 8 1140916352 1025 6528 0 482 1088 0 7 0 0 0 6528 0 8 4294904477 1122 0 0 1 1154 202 208 98 3 1218 1248 98 2 530 141 43 530 341 39 6528 1218 1328 98 1 1362 3 1 3 6528 1218 1408 98 1 32 6528 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 70 0 0 0 21 0 0 0 240 0 0 0 40 0 0 0] 98 0 1504 0 27 1522 1568 141 1600 341 1632 43 1664 39 410 1008 98 16 0 848 98 2 8 1140916352 1025 6896 0 482 1088 0 7 0 0 0 6896 0 8 4294904477 1122 0 0 1 1154 202 208 98 3 1218 1248 98 2 530 141 163 530 341 39 6896 1218 1328 98 1 1362 3 1 3 6896 1218 1408 98 1 32 6896 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 70 0 0 0 81 0 0 0 240 0 0 0 100 0 0 0] 98 0 1504 0 27 1522 1568 141 1600 341 1632 163 1664 39 410 2080 98 30 0 848 98 2 8 1409355853 1025 7264 2146 202 208 2192 0 2224 482 2256 0 7 0 0 0 7264 0 8 4294904065 2290 2320 98 0 0 2368 0 0 0 0 0 0 202 208 98 1 2418 8 'Step-Children' 301 2464 2290 2320 7408 2496 0 0 7264 0 3 0 0 2512 2192 0 131169 0 0 1154 202 208 98 2 1218 1248 98 2 530 1053 1 530 301 201 7264 1218 2656 98 1 8 'Step-Children' 7264 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 2 0 0 0 0 0 0 164 2 0 0 100 0 0 0] 98 0 1504 0 27 1522 2768 -299 1600 301 1632 1 1664 201 234 256 98 16 5104 8 'versionMessage' 7264 8 'versionStepChildren' 6528 8 'versionDate' 6896 8 'versionID' 1696 8 'versionAuthor' 992 8 'versionName' 2800 8 'versionTime' 2064 8 'versionAncestors' 0 1154 202 208 98 1 1218 1248 98 2 530 9 49 530 1353 669 848 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 168 2 0 0 102 1 0 0] 98 14 3168 4784 4464 3824 4144 3504 992 6528 2800 1696 6896 2064 7264 5104 1504 0 27 802 8 'Repository Creation Template' 410 5120 98 18 0 624 98 2 8 1140920644 1025 8080 0 482 5200 0 5 5218 0 16 98 10 5266 1 5298 5328 8 '&Font...' 1 1 0 0 0 5362 4097 5266 1 5298 5424 8 '&Plain' 1 1 0 0 0 5266 1 5298 5488 8 '&Bold' 1 1 0 0 0 5266 1 5298 5552 8 '&Italic' 1 1 0 0 0 5266 1 5298 5616 8 '&Underlined' 1 1 0 0 0 5362 4097 5218 0 16 98 3 5266 1025 5298 5728 8 '&Left' 1 1 0 0 0 5266 1025 5298 5792 8 '&Centre' 1 1 0 0 0 5266 1025 5298 5856 8 '&Right' 1 1 0 0 0 8 '&Align' 0 1 0 0 0 0 0 5362 4097 5266 1 5298 5952 8 '&Colour...' 1 1 0 0 0 8 '' 0 1 0 0 0 0 0 0 0 8080 0 8 1772716531 1122 0 0 11 0 6034 8 #[0 0 0 0 0 0 0 0 48 0 174 1] 1154 202 208 98 6 1218 1248 98 2 530 9 49 530 1353 669 8080 1218 6208 98 1 8160 8080 1218 2656 98 1 6274 8 '{\rtf1\ansi\ansicpg1252\deff0\deflang2057{\fonttbl{\f0\froman Times New Roman;}}
\viewkind4\uc1\pard\f0\fs22 
\par }
' 8080 1218 1328 98 1 1362 3 1 3 8080 1218 1408 98 1 32 8080 1218 6416 2192 8080 1442 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 168 2 0 0 102 1 0 0] 98 0 1504 0 27 848 234 256 98 4 8080 8 'repositoryCreationTemplate' 848 8 'versionInfo' 0 410 8 ##(Smalltalk.TabViewXP)  98 28 0 624 98 2 8 1140916736 1 9216 2146 202 208 98 2 8064 832 0 2224 0 0 1 0 0 0 9216 0 8 4294904395 787814 3 ##(Smalltalk.BlockClosure)  0 0 918822 ##(Smalltalk.CompiledMethod)  2 3 8 ##(Smalltalk.ListControlView)  8 #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] 2320 9376 7 257 0 9362 0 0 9394 2 3 8 ##(Smalltalk.IconicListAbstract)  8 #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] 8 #iconImageIndex 9472 7 257 0 2368 0 0 0 0 0 8 #noIcons 0 0 0 0 0 1154 202 208 98 3 1218 1248 98 2 530 1 1 530 1369 725 9216 1218 8 #basicSelectionsByIndex: 98 1 98 1 5 9216 1218 8 #tcmSetExtendedStyle:dwExStyle: 98 2 -1 1 9216 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 172 2 0 0 106 1 0 0] 98 0 1504 0 27 1154 202 208 98 1 1218 1248 98 2 530 1 561 530 1369 725 624 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 24 1 0 0 172 2 0 0 130 2 0 0] 98 3 8080 848 9216 1504 0 27 9 410 864 98 15 0 416 98 2 8 1140850688 131073 10016 0 0 0 7 0 0 0 10016 562 234 240 98 6 410 2080 98 30 0 10016 98 2 8 1409355853 1025 10128 2146 202 208 2192 0 2224 482 8 4278190080 0 7 5218 0 16 98 2 5266 1 5298 8 #addRepository 8 '&Add Repository...' 1 1 0 0 0 5266 1 5298 8 #removeRepository 8 '&Remove Repository...' 1 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 10128 0 8 4294904065 2290 2320 98 0 0 2368 0 0 0 0 0 0 202 208 98 1 2418 8 'Repositories' 1001 2464 2290 2320 10464 2496 0 0 10128 0 1 0 0 2512 2192 0 131169 0 0 1154 202 208 98 3 1218 1248 98 2 530 1 1 530 621 543 10128 1218 6208 98 1 10256 10128 1218 2656 98 1 8 'Repositories' 10128 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 54 1 0 0 15 1 0 0] 98 0 1504 0 27 15 410 2080 98 30 0 10016 98 2 8 1409355849 1025 10800 2146 202 208 2192 0 2224 482 8 4278190080 0 7 5218 0 16 98 4 5266 1 5298 8 #loadVersion 8 '&Load Version' 1 1 0 0 0 5266 1 5298 8 #mergeVersion 8 '&Merge Version' 1 1 0 0 0 5266 1 5298 8 #compareVersion 8 '&Compare' 1 1 0 0 0 5266 1 5298 8 #saveAsTopazFileIn 8 'Save as &Topaz File-In' 1 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 10800 0 8 4294904065 2290 2320 98 0 0 2368 0 0 0 0 0 0 202 208 98 1 2418 8 'Versions' 401 2464 2290 2320 11264 2496 0 0 10800 0 1 0 9362 0 0 1180966 ##(Smalltalk.CompiledExpression)  8 1 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 8 '[:each | each item isLoaded ifTrue: [each font: each font beBold. each item isModified ifTrue: [each font: each font beItalic]]].' 8 #[36 105 226 0 159 221 18 17 226 2 161 180 97 226 0 163 123 17 226 2 164 180 106 60 106 60 106] 8 #item 8 #isLoaded 8 #font 8 #beBold 8 #font: 8 #isModified 8 #beItalic 11360 7 257 0 2512 2192 0 131169 0 0 1154 202 208 98 3 1218 1248 98 2 530 1101 1 530 269 543 10800 1218 6208 98 1 10928 10800 1218 2656 98 1 8 'Versions' 10800 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 38 2 0 0 0 0 0 0 172 2 0 0 15 1 0 0] 98 0 1504 0 27 7 410 2080 98 30 0 10016 98 2 8 1409355853 1025 11824 2146 202 208 2192 0 2224 482 10240 0 7 5218 0 16 98 2 5266 1 5298 8 #addPackage 8 '&Add Package...' 1 1 0 0 0 5266 1 5298 8 #removePackage 8 '&Remove Package' 1 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 11824 0 8 4294904065 2290 2320 98 0 0 2368 0 0 0 0 0 0 202 208 98 1 2418 8 'Packages' 601 2464 9362 0 0 11378 2 1 9360 8 'doIt' 8 '[:each | each name]' 8 #[30 105 226 0 106] 8 #name 12224 7 257 0 2496 0 0 11824 0 1 0 9362 0 0 11378 8 1 9360 8 'doIt' 8 '[:each | each item isLoaded ifTrue: [each font: each font beBold. each item isModified ifTrue: [each font: each font beItalic]]].' 8 #[36 105 226 0 159 221 18 17 226 2 161 180 97 226 0 163 123 17 226 2 164 180 106 60 106 60 106] 11472 11488 11504 11520 11536 11552 11568 12320 7 257 0 2512 2192 0 131169 0 0 1154 202 208 98 3 1218 1248 98 2 530 639 1 530 445 543 11824 1218 6208 98 1 11936 11824 1218 2656 98 1 8 'Packages' 11824 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 63 1 0 0 0 0 0 0 29 2 0 0 15 1 0 0] 98 0 1504 0 27 11 32 234 256 98 6 10128 8 'repositoryList' 10800 8 'versionList' 11824 8 'packageList' 0 1154 202 208 98 1 1218 1248 98 2 530 1 1 530 1369 543 10016 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 172 2 0 0 15 1 0 0] 98 5 10128 410 8 ##(Smalltalk.Splitter)  98 12 0 10016 98 2 8 1140850688 1 12880 0 482 8 4278190080 0 519 0 0 0 12880 1154 202 208 98 1 1218 1248 98 2 530 621 1 530 19 543 12880 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 54 1 0 0 0 0 0 0 63 1 0 0 15 1 0 0] 98 0 1504 0 27 11824 410 12896 98 12 0 10016 98 2 8 1140850688 1 13152 0 482 12976 0 519 0 0 0 13152 1154 202 208 98 1 1218 1248 98 2 530 1083 1 530 19 543 13152 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 29 2 0 0 0 0 0 0 38 2 0 0 15 1 0 0] 98 0 1504 0 27 10800 1504 0 27 7 16 234 256 2192 0 461638 4 ##(Smalltalk.MenuBar)  0 16 98 3 5218 0 16 98 2 5266 1 5298 10320 8 '&Add...' 1 1 0 0 0 5266 1 5298 10384 8 '&Remove...' 1 1 0 0 0 8 '&Repository' 0 134217729 0 0 41903 0 0 5218 0 16 98 2 5266 1 5298 12000 8 '&Add...' 1 1 0 0 0 5266 1 5298 12064 8 'Remove...' 1 1 0 0 0 8 '&Package' 0 134217729 0 0 41909 0 0 5218 0 16 98 1 5266 1 5298 10992 8 '&Load Version' 1 1 0 0 0 8 '&Version' 0 134217729 0 0 41913 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1154 202 208 98 3 1218 1248 98 2 530 3359 21 530 1401 1401 416 1218 2656 98 1 8 'Monticello Repository Browser' 416 1218 8 #updateMenuBar 2192 416 1442 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 143 6 0 0 10 0 0 0 75 9 0 0 198 2 0 0] 98 3 10016 410 12896 98 12 0 416 98 2 8 1140850688 1 14096 0 482 12976 0 519 0 0 0 14096 1154 202 208 98 1 1218 1248 98 2 530 1 543 530 1369 19 14096 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 15 1 0 0 172 2 0 0 24 1 0 0] 98 0 1504 0 27 624 1504 0 27 )! !
!MCRepositoryBrowser class categoriesFor: #icon!public! !
!MCRepositoryBrowser class categoriesFor: #resource_Default_view!public!resources-views! !

MCHttpRepositoryInfoDialog guid: (GUID fromString: '{B2A7D8A5-B9A7-4DB2-BD09-28B3289D28F8}')!
MCHttpRepositoryInfoDialog comment: ''!
!MCHttpRepositoryInfoDialog categoriesForClass!Unclassified! !
!MCHttpRepositoryInfoDialog methodsFor!

createComponents

	super createComponents.
	locationPresenter 		:= self add: TextPresenter new name: 'location'.
	userPresenter			:= self add: TextPresenter new name: 'user'.
	passwordPresenter	:= self add: TextPresenter new name: 'password'.
!

createSchematicWiring

	super createSchematicWiring.
	locationPresenter 		when: #'valueChanged' send: #'locationChanged' 	to: self.
	userPresenter 			when: #'valueChanged' send: #'userChanged' 			to: self.
	passwordPresenter	when: #'valueChanged' send: #'passwordChanged' 	to: self.
!

locationChanged

	model location: locationPresenter value.
!

model: anMCHttpRepositoryInfo

	super model: anMCHttpRepositoryInfo.
	model
		aspectValue: #'location';
		aspectValue: #'user';
		aspectValue: #'password';
		yourself.
!

onViewOpened

	super onViewOpened.
	locationPresenter value: model location.
	userPresenter value: model user.
	passwordPresenter value: model password.
!

passwordChanged

	model password: passwordPresenter value.
!

userChanged

	model user: userPresenter value.
! !
!MCHttpRepositoryInfoDialog categoriesFor: #createComponents!public! !
!MCHttpRepositoryInfoDialog categoriesFor: #createSchematicWiring!public! !
!MCHttpRepositoryInfoDialog categoriesFor: #locationChanged!public! !
!MCHttpRepositoryInfoDialog categoriesFor: #model:!public! !
!MCHttpRepositoryInfoDialog categoriesFor: #onViewOpened!public! !
!MCHttpRepositoryInfoDialog categoriesFor: #passwordChanged!public! !
!MCHttpRepositoryInfoDialog categoriesFor: #userChanged!public! !

!MCHttpRepositoryInfoDialog class methodsFor!

icon

	^Icon fromFile: 'icons\GS32x32.ico'.
!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.DialogView)  98 30 0 0 98 2 26214401 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 167 0 0 0 416 788230 ##(Smalltalk.BorderLayout)  1 1 0 410 8 ##(Smalltalk.ReferenceView)  98 14 0 416 98 2 8 1140850688 131073 560 0 0 0 7 0 0 0 560 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.Presenter)  8 #resource_OK_Cancel_button_block 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  21 107 834 729 71 560 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 53 0 0 0 118 1 0 0 88 0 0 0] 98 0 834 193 193 0 27 0 0 0 234 256 98 6 410 8 ##(Smalltalk.TextEdit)  98 16 0 416 98 2 8 1140916352 1025 992 0 482 8 4278190080 0 7 0 0 0 992 0 8 4294902749 852486 ##(Smalltalk.NullConverter)  0 0 1 706 202 208 98 3 770 800 98 2 834 141 11 834 621 41 992 770 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 992 770 8 #isTextModified: 98 1 32 992 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 70 0 0 0 5 0 0 0 124 1 0 0 25 0 0 0] 98 0 944 0 27 8 'location' 410 1008 98 16 0 416 98 2 8 1140916352 1025 1456 0 482 1088 0 7 0 0 0 1456 0 8 4294902749 1122 0 0 1 706 202 208 98 3 770 800 98 2 834 141 61 834 241 39 1456 770 1280 98 1 1314 3 1 3 1456 770 1360 98 1 32 1456 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 70 0 0 0 30 0 0 0 190 0 0 0 49 0 0 0] 98 0 944 0 27 8 'user' 410 1008 98 16 0 416 98 2 8 1140916384 1025 1824 0 482 8 4278190080 0 7 0 0 0 1824 0 8 4294902749 1122 0 0 1 706 202 208 98 3 770 800 98 2 834 521 61 834 241 39 1824 770 1280 98 1 1314 3 1 3 1824 770 1360 98 1 32 1824 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 1 0 0 30 0 0 0 124 1 0 0 49 0 0 0] 98 0 944 0 27 8 'password' 590342 ##(Smalltalk.Rectangle)  834 21 21 834 21 21 0 0 0 0 9249 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 0 8 2118378815 706 202 208 98 3 770 800 98 2 834 2879 21 834 781 261 416 770 8 #text: 98 1 8 'Monticello HTTP Repository' 416 770 8 #updateMenuBar 928 416 882 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 37 7 0 0 140 0 0 0] 98 7 560 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 2576 0 0 0 7 0 0 0 2576 0 8 4294902779 1122 0 0 0 706 202 208 98 2 770 800 98 2 834 11 11 834 131 41 2576 770 2448 98 1 8 'Location:' 2576 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 5 0 0 0 70 0 0 0 25 0 0 0] 98 0 944 0 27 410 2592 98 16 0 416 98 2 8 1140850944 1 2896 0 0 0 7 0 0 0 2896 0 8 4294902779 1122 0 0 0 706 202 208 98 2 770 800 98 2 834 11 61 834 111 41 2896 770 2448 98 1 8 'User:' 2896 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 30 0 0 0 60 0 0 0 50 0 0 0] 98 0 944 0 27 410 2592 98 16 0 416 98 2 8 1140850944 1 3200 0 0 0 7 0 0 0 3200 0 8 4294902779 1122 0 0 0 706 202 208 98 2 770 800 98 2 834 391 61 834 131 41 3200 770 2448 98 1 8 'Password:' 3200 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 30 0 0 0 4 1 0 0 50 0 0 0] 98 0 944 0 27 992 1456 1824 944 0 27 )! !
!MCHttpRepositoryInfoDialog class categoriesFor: #icon!public! !
!MCHttpRepositoryInfoDialog class categoriesFor: #resource_Default_view!public!resources-views! !

MCVersionDialog guid: (GUID fromString: '{781FA501-11CB-40EB-B958-CBA790F96317}')!
MCVersionDialog comment: ''!
!MCVersionDialog categoriesForClass!Unclassified! !
!MCVersionDialog methodsFor!

apply

	super apply.
	self applyA.
!

applyA

	model value 
		at: #'httpPassword'		put: httpPasswordPresenter value;
		at: #'httpUser'				put: httpUserPresenter value;
		at: #'repository'				put: repositoryListPresenter selection;
		at: #'versionMessage' 	put: messagePresenter value;
		at: #'versionName'		put: namePresenter value;
		yourself.
!

createComponents

	super createComponents.
	namePresenter 				:= self add: TextPresenter 	new name: 'name'.
	messagePresenter 		:= self add: TextPresenter 	new name: 'message'.
	repositoryListPresenter 	:= self add: ListPresenter 		new name: 'repositoryList'.
	httpUserPresenter 		:= self add: TextPresenter 	new name: 'httpUser'.
	httpPasswordPresenter 	:= self add: TextPresenter 	new name: 'httpPassword'.
!

createSchematicWiring

	super createSchematicWiring.
	repositoryListPresenter when: #'selectionChanged' send: #'repositorySelectionChanged' to: self.
!

defaultRepository: aRepository

	repositoryListPresenter selection: aRepository.
!

httpPassword

	^httpPasswordPresenter value.
!

httpUser

	^httpUserPresenter value.
!

onViewOpened

	super onViewOpened.
	self onViewOpened2.
!

onViewOpened2

	| dict |
	dict := model value.
	self caption: 'Save ' , (dict at: #'name').
	(dict at: #'isModified') ifTrue: [
		namePresenter value: (dict at: #'uniqueVersionName').
	] ifFalse: [
		namePresenter value: (dict at: #'versionName').
		messagePresenter value: (dict at: #'versionMessage').
	].
	repositoryListPresenter 
		list: (dict at: #'repositoryList');
		selection: (dict at: #'repository');
		yourself.
!

repositoryList

	^self model repositories.
!

repositorySelectionChanged

	| array |
	array := repositoryListPresenter selection.
	array first = 'MCHttpRepository' ifTrue: [
		self repositoryTypeContainer ensureSubViewVisible: (self view viewNamed: 'httpTab').
		httpPasswordPresenter value: (array at: 4).
		httpUserPresenter value: (array at: 3).
		^self.
	].
	array first = 'MCDictionaryRepository' ifTrue: [
		self repositoryTypeContainer ensureSubViewVisible: (self view viewNamed: 'dictionaryTab').
		^self.
	].
	array first = 'MCDirectoryRepository' ifTrue: [
		self repositoryTypeContainer ensureSubViewVisible: (self view viewNamed: 'directoryTab').
		^self.
	].
	(#('MCServerDirectoryRepository' 'MCFileTreeRepository' 'MCGitHubRepository') includes: array first) ifTrue: [
		self repositoryTypeContainer ensureSubViewVisible: (self view viewNamed: 'directoryTab').
		^self.
	].
	"self halt."
	self error: 'Repository selection changed to unknown type: ' , array first printString.
!

repositoryTypeContainer

	^self view viewNamed: 'repositoryType'.
!

uniqueVersionName

	| string |
	string := self model uniqueVersionName.
	0 = (string indexOfSubCollection: 'seaside') ifTrue: [^string].
	(string := Prompter prompt: 'Author initials?') isNil ifTrue: [
		[self cancel] forkAt: Processor userBackgroundPriority.
		^nil.
	].
	self model authorInitials: string.
	^self model uniqueVersionName.
!

updateDictionaryTabWith: aRepository

	self repositoryTypeContainer ensureSubViewVisible: (self view viewNamed: 'dictionaryTab').
!

updateDirectoryTabWith: aRepository

	self repositoryTypeContainer ensureSubViewVisible: (self view viewNamed: 'directoryTab').
!

updateHttpTabWith: aRepository

	| userAndPassword |
	self repositoryTypeContainer ensureSubViewVisible: (self view viewNamed: 'httpTab').
	userAndPassword := aRepository userAndPassword subStrings: Character tab.
	httpUserPresenter value: userAndPassword first.
	httpPasswordPresenter value: (1 < userAndPassword size
		ifTrue: [userAndPassword at: 2]
		ifFalse: ['']).
! !
!MCVersionDialog categoriesFor: #apply!public! !
!MCVersionDialog categoriesFor: #applyA!public! !
!MCVersionDialog categoriesFor: #createComponents!public! !
!MCVersionDialog categoriesFor: #createSchematicWiring!public! !
!MCVersionDialog categoriesFor: #defaultRepository:!public! !
!MCVersionDialog categoriesFor: #httpPassword!public! !
!MCVersionDialog categoriesFor: #httpUser!public! !
!MCVersionDialog categoriesFor: #onViewOpened!public! !
!MCVersionDialog categoriesFor: #onViewOpened2!public! !
!MCVersionDialog categoriesFor: #repositoryList!public! !
!MCVersionDialog categoriesFor: #repositorySelectionChanged!public! !
!MCVersionDialog categoriesFor: #repositoryTypeContainer!public! !
!MCVersionDialog categoriesFor: #uniqueVersionName!public! !
!MCVersionDialog categoriesFor: #updateDictionaryTabWith:!public! !
!MCVersionDialog categoriesFor: #updateDirectoryTabWith:!public! !
!MCVersionDialog categoriesFor: #updateHttpTabWith:!public! !

!MCVersionDialog class methodsFor!

icon

	^Icon fromFile: 'icons\GS32x32.ico'.
!

package: aPackage defaultRepository: aRepository

	^(self createOn: aPackage) 
		defaultRepository: aRepository;
		showShell.
!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.DialogView)  98 30 0 0 98 2 26738689 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 167 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 16 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 592 0 0 0 7 0 0 0 592 0 8 4294902469 852486 ##(Smalltalk.NullConverter)  0 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  21 345 850 241 39 592 786 8 #text: 98 1 8 'Repository:' 592 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 172 0 0 0 130 0 0 0 191 0 0 0] 98 0 850 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 1 1074 8 #fixedViewLeft 241 1074 8 #fixedParentBottom -359 1074 8 #fixedViewTop 39 410 608 98 16 0 416 98 2 8 1140850944 1 1216 0 0 0 7 0 0 0 1216 0 8 4294902469 690 0 0 0 722 202 208 98 2 786 816 98 2 850 21 111 850 241 39 1216 786 912 98 1 8 'Message:' 1216 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 55 0 0 0 130 0 0 0 74 0 0 0] 98 0 1024 0 27 1042 1088 1 1120 241 1074 8 #fixedParentTop 91 1184 39 410 8 ##(Smalltalk.CardContainer)  98 16 0 416 98 2 8 1409286144 131073 1568 0 482 8 4278190080 0 7 0 0 0 1568 655878 ##(Smalltalk.CardLayout)  202 208 98 3 721414 ##(Smalltalk.Association)  8 'HTTP' 410 8 ##(Smalltalk.ContainerView)  98 15 0 1568 98 2 8 1140850688 131073 1792 0 0 0 5 0 0 0 1792 0 234 256 98 4 410 8 ##(Smalltalk.TextEdit)  98 16 0 1792 98 2 8 1140916352 1025 1904 0 482 8 4278190080 0 5 0 0 0 1904 0 8 4294902723 690 0 0 1 722 202 208 98 3 786 816 98 2 850 141 7 850 241 39 1904 786 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 1904 786 8 #isTextModified: 98 1 32 1904 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 70 0 0 0 3 0 0 0 190 0 0 0 22 0 0 0] 98 0 1024 0 27 8 'httpUser' 410 1920 98 16 0 1792 98 2 8 1140916384 1025 2352 0 482 8 4278190080 0 5 0 0 0 2352 0 8 4294902723 690 0 0 1 722 202 208 98 3 786 816 98 2 850 141 51 850 241 39 2352 786 2176 98 1 2210 3 1 3 2352 786 2256 98 1 32 2352 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 70 0 0 0 25 0 0 0 190 0 0 0 44 0 0 0] 98 0 1024 0 27 8 'httpPassword' 0 722 202 208 98 1 786 816 98 2 850 9 49 850 613 135 1792 962 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 54 1 0 0 91 0 0 0] 98 4 410 608 98 16 0 1792 98 2 8 1140850946 1 2896 0 0 0 5 0 0 0 2896 0 8 4294902469 690 0 0 0 722 202 208 98 2 786 816 98 2 850 1 11 850 141 41 2896 786 912 98 1 8 'User: ' 2896 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 5 0 0 0 70 0 0 0 25 0 0 0] 98 0 1024 0 27 410 608 98 16 0 1792 98 2 8 1140850946 1 3200 0 0 0 5 0 0 0 3200 0 8 4294902469 690 0 0 0 722 202 208 98 2 786 816 98 2 850 11 61 850 131 31 3200 786 912 98 1 8 'Password: ' 3200 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 30 0 0 0 70 0 0 0 45 0 0 0] 98 0 1024 0 27 1904 2352 1024 0 27 1746 8 'Dictionary' 410 1808 98 15 0 1568 98 2 8 1140850688 131073 3536 0 0 0 5 0 0 0 3536 0 234 256 98 0 0 722 202 208 98 1 786 816 98 2 850 9 49 850 613 135 3536 962 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 54 1 0 0 91 0 0 0] 98 0 1024 0 27 1746 8 'Directory' 410 1808 98 15 0 1568 98 2 8 1140850688 131073 3824 0 0 0 7 0 0 0 3824 0 234 256 3616 0 722 202 208 98 1 786 816 98 2 850 9 49 850 613 135 3824 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 54 1 0 0 91 0 0 0] 98 0 1024 0 27 3824 234 256 98 6 1792 8 'httpTab' 3824 8 'directoryTab' 3536 8 'dictionaryTab' 0 410 8 ##(Smalltalk.TabViewXP)  98 28 0 1568 98 2 8 1140916736 1 4144 590662 2 ##(Smalltalk.ListModel)  202 208 98 3 3520 1776 3808 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  0 0 1 0 0 0 4144 0 8 4294903551 787814 3 ##(Smalltalk.BlockClosure)  0 0 918822 ##(Smalltalk.CompiledMethod)  2 3 8 ##(Smalltalk.ListControlView)  8 #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] 8 #displayString 4352 7 257 0 4338 0 0 4370 2 3 8 ##(Smalltalk.IconicListAbstract)  8 #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] 8 #iconImageIndex 4464 7 257 0 1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 8 #noIcons 0 0 0 0 0 722 202 208 98 3 786 816 98 2 850 1 1 850 629 191 4144 786 8 #basicSelectionsByIndex: 98 1 98 1 7 4144 786 8 #tcmSetExtendedStyle:dwExStyle: 98 2 -1 1 4144 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 58 1 0 0 95 0 0 0] 98 0 1024 0 27 722 202 208 98 1 786 816 98 2 850 21 445 850 629 191 1568 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 222 0 0 0 68 1 0 0 61 1 0 0] 98 4 3536 1792 3824 4144 1024 0 27 1042 1088 1 1074 8 #fixedParentRight 1 1152 -259 1152 -69 410 608 98 16 0 416 98 2 8 1140850944 1 5088 0 0 0 7 0 0 0 5088 0 8 4294902469 690 0 0 0 722 202 208 98 2 786 816 98 2 850 21 21 850 241 39 5088 786 912 98 1 8 'Version Name:' 5088 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 10 0 0 0 130 0 0 0 29 0 0 0] 98 0 1024 0 27 1042 1088 1 1120 241 1536 1 1184 39 410 8 ##(Smalltalk.MultilineTextEdit)  98 16 0 416 98 2 8 1143017796 1025 5408 0 482 8 4278190080 0 7 0 0 0 5408 0 8 4294902723 690 0 0 9 722 202 208 98 3 786 816 98 2 850 21 151 850 629 185 5408 786 2176 98 1 2210 3 1 3 5408 786 2256 98 1 32 5408 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 75 0 0 0 68 1 0 0 167 0 0 0] 98 0 1024 0 27 1042 1088 1 5056 1 1536 131 1152 -369 410 8 ##(Smalltalk.ComboBox)  98 17 0 416 98 2 8 1412498947 1025 5808 4226 202 208 3616 0 4304 482 8 4278190080 0 7 0 0 0 5808 0 8 4294902449 4338 0 0 1180966 ##(Smalltalk.CompiledExpression)  1 83886081 8 ##(Smalltalk.Message)  8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 5968 7 257 0 3616 401 722 202 208 98 1 786 816 98 2 850 21 385 850 629 47 5808 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 192 0 0 0 68 1 0 0 215 0 0 0] 98 0 1024 0 27 1042 1088 1 5056 1 1152 -319 1184 47 410 1920 98 16 0 416 98 2 8 1140916352 1025 6256 0 482 2000 0 7 0 0 0 6256 0 8 4294902723 690 0 0 1 722 202 208 98 3 786 816 98 2 850 21 61 850 629 41 6256 786 2176 98 1 2210 3 1 3 6256 786 2256 98 1 32 6256 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 30 0 0 0 68 1 0 0 50 0 0 0] 98 0 1024 0 27 1042 1088 1 5056 1 1536 41 1184 41 410 8 ##(Smalltalk.ReferenceView)  98 14 0 416 98 2 8 1140850688 131073 6624 0 0 0 7 0 0 0 6624 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.Presenter)  8 #resource_OK_Cancel_button_block 0 722 202 208 98 1 786 816 98 2 850 21 635 850 629 71 6624 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 61 1 0 0 68 1 0 0 96 1 0 0] 3616 1024 0 27 1042 1088 1 5056 1 1152 -69 1184 71 234 256 98 8 1568 8 'repositoryType' 5808 8 'repositoryList' 5408 8 'message' 6256 8 'name' 590342 ##(Smalltalk.Rectangle)  850 21 21 850 21 21 0 0 0 0 12051 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 0 8 1969254884 722 202 208 98 2 786 816 98 2 850 2879 21 850 701 801 416 786 8 #updateMenuBar 3616 416 962 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 253 6 0 0 154 1 0 0] 98 8 5088 6256 1216 5408 592 5808 6624 1568 1024 0 27 )! !
!MCVersionDialog class categoriesFor: #icon!public! !
!MCVersionDialog class categoriesFor: #package:defaultRepository:!public! !
!MCVersionDialog class categoriesFor: #resource_Default_view!public!resources-views! !

MCModificationTestCase guid: (GUID fromString: '{C463B1C3-C30B-4A60-9509-7A646157E7DA}')!
MCModificationTestCase comment: ''!
!MCModificationTestCase categoriesForClass!Unclassified! !
!MCModificationTestCase methodsFor!

methods: anArray

	| oldString newString stream |
	oldString := anArray first reject: [:each | each = Character cr].
	newString := anArray last reject: [:each | each = Character cr].
	stream := (WriteStream on: String new)
		nextPutAll: 'M	123	M	JamesFoster 12/23/2010 20:16	ClassName	false	MethodCategory	methodName'; tab;
		nextPutAll: oldString size printString; tab;
		nextPutAll: oldString; 
		nextPut: Character lf;
		nextPutAll: 'M	JamesFoster 12/23/2010 20:16	ClassName	false	MethodCategory	methodName'; tab;
		nextPutAll: newString size printString; tab;
		nextPutAll: newString;
		nextPut: Character lf;
		yourself.
	^MCPatchOperation 
		fromStream: (ReadStream on: stream contents) 
		session: self.
!

methods01

^#(
'aaa
	bbb
	ccc'
'aaa
	BBB
	ccc').!

methods02

^#(
'aaa
	bbb
	ddd'
'aaa
	bbb
	ccc
	ddd').!

methods03

^#(
'aaa
	bbb
	ccc
	ddd'
'aaa
	bbb
	ddd').!

methods04

^#(
'aaa
	bbb
'
'aaa
	bbb

').!

oopTypeWithOop: anObject
!

test01

	| modification oldMemo oldMarkers newMemo newMarkers |
	modification := self methods: self methods01.
	oldMemo := modification obsoletionMemo subStrings: Character lf.
	oldMarkers := modification obsoletionMemoMarkers.
	newMemo := modification modificationMemo subStrings: Character lf.
	newMarkers := modification modificationMemoMarkers.
	self
		assert: oldMemo size = 3;
		assert: oldMarkers = (Array with: #'changed' -> 2);
		assert: newMemo size = 3;
		assert: newMarkers = (Array with: #'changed' -> 2);
		yourself.
!

test02
 
	| modification oldMemo oldMarkers newMemo newMarkers |
	modification := self methods: self methods02.
	oldMemo := modification obsoletionMemo subStrings: Character lf.
	oldMarkers := modification obsoletionMemoMarkers.
	newMemo := modification modificationMemo subStrings: Character lf.
	newMarkers := modification modificationMemoMarkers.
	self
		assert: oldMemo size = 4;
		assert: oldMarkers = #();
		assert: newMemo size = 4;
		assert: newMarkers = (Array with: #'added' -> 3);
		yourself.
!

test03
 
	| modification oldMemo oldMarkers newMemo newMarkers |
	modification := self methods: self methods03.
	oldMemo := modification obsoletionMemo subStrings: Character lf.
	oldMarkers := modification obsoletionMemoMarkers.
	newMemo := modification modificationMemo subStrings: Character lf.
	newMarkers := modification modificationMemoMarkers.
	self
		assert: oldMemo size = 4;
		assert: oldMarkers = (Array with: #'removed' -> 3);
		assert: newMemo size = 4;
		assert: newMarkers = #();
		yourself.
!

test04
 
	| modification oldMemo oldMarkers newMemo newMarkers |
	modification := self methods: self methods04.
	oldMemo := modification obsoletionMemo subStrings: Character lf.
	oldMarkers := modification obsoletionMemoMarkers.
	newMemo := modification modificationMemo subStrings: Character lf.
	newMarkers := modification modificationMemoMarkers.
	self
		assert: oldMemo size = 4;
		assert: oldMarkers = #();
		assert: newMemo size = 4;
		assert: newMarkers = (Array with: #'added' -> 4);
		yourself.
! !
!MCModificationTestCase categoriesFor: #methods:!public! !
!MCModificationTestCase categoriesFor: #methods01!public! !
!MCModificationTestCase categoriesFor: #methods02!public! !
!MCModificationTestCase categoriesFor: #methods03!public! !
!MCModificationTestCase categoriesFor: #methods04!public! !
!MCModificationTestCase categoriesFor: #oopTypeWithOop:!public! !
!MCModificationTestCase categoriesFor: #test01!public! !
!MCModificationTestCase categoriesFor: #test02!public! !
!MCModificationTestCase categoriesFor: #test03!public! !
!MCModificationTestCase categoriesFor: #test04!public! !

"Binary Globals"!


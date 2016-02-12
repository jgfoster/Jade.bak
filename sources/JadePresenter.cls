"Filed out from Dolphin Smalltalk 7"!

Presenter subclass: #JadePresenter
	instanceVariableNames: 'gciSession'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadePresenter guid: (GUID fromString: '{CCC9B223-B3B5-44FD-BF88-60F48B65DC99}')!
JadePresenter comment: ''!
!JadePresenter categoriesForClass!Unclassified! !
!JadePresenter methodsFor!

aboutToEditLabel: anObject accept: aValueHolder
	"Is it okay to start editing a label?"

	aValueHolder value: (self canEditLabel: anObject).
!

canEditLabel: anObject

	^true.
!

createDragDropSchematicWiringFor: aPresenter

	aPresenter
		when: #dragOver: 	send: #onDragOver: 	to: self;
		when: #drop:				send: #onDragDrop:	to: self;
		when: #drag:				send: #onDrag:			to: self;
		when: #dragCut:		send: #onDragCut:	to: self;
		yourself.

!

createLabelEditSchematicWiringFor: aPresenter

	aPresenter
		when: #labelOf:changedTo: 			send: #labelOf:changedTo:				to: self;
		when: #aboutToEditLabel:accept: 	send: #aboutToEditLabel:accept:	to: self;
		when: #labelOf:editedTo:accept: 	send: #labelOf:editedTo:accept:		to: self;
		yourself.
!

createSchematicWiring

	super createSchematicWiring.
	self dragDropPresenters do: [:each | 
		self createDragDropSchematicWiringFor: each.
	].
	self labelEditPresenters do: [:each | 
		self createLabelEditSchematicWiringFor: each.
	].
!

dragDropPresenter

	^self primaryPresenter.
!

dragDropPresenters

	^Set with: self dragDropPresenter.
!

drop: sourceObject on: targetObject description: aString

	MessageBox 
		warning: 'Sorry, drop ' , aString , ' ' , sourceObject printString , ' on ' , targetObject printString , ' in ' , self class name , ' not yet supported.'
		caption: 'Jade System Browser'.
	Keyboard default isShiftDown ifTrue: [self halt].
!

dropClass: anOopType on: aTarget

	self
		drop: anOopType 
		on: aTarget 
		description: 'class'.
!

dropClassCategory: anOopType on: aTarget

	self
		drop: anOopType 
		on: aTarget 
		description: 'class category'.
!

dropMethod: anOopType on: aTarget

	self
		drop: anOopType 
		on: aTarget 
		description: 'method'.
!

dropMethodCategory: anOopType on: aTarget

	self
		drop: anOopType 
		on: aTarget 
		description: 'method category'.
!

dropSymbolDictionary: anOopType on: aTarget

	self
		drop: anOopType 
		on: aTarget 
		description: 'symbol dictionary'.
!

gciSession

	^gciSession
!

gciSession: aGciSession

	aGciSession class == GciSession ifFalse: [self error: 'Wrong class!!'].
	gciSession := aGciSession.
!

labelEditPresenter

	^self primaryPresenter.
!

labelEditPresenters

	^Set with: self labelEditPresenter.
!

labelOf: a changedTo: b
	"Letting us know that the rename occurred"!

labelOf: oldString editedTo: newString accept: aValueHolder
	"Is it okay to do the rename?"

	MessageBox 
		warning: 'Sorry. Item rename not yet supported.' 
		caption: 'Jade System Browser'.
	aValueHolder value: false.
	Keyboard default isShiftDown ifTrue: [self halt].
!

model: anObject

	anObject class == GciSession ifTrue: [gciSession := anObject].
	super model: anObject.
!

onDrag: anInternalDragDropSession 

	self subclassResponsibility.
!

onDragClassCategoriesOver: aSession 

	self
		onDragOver: aSession 
		operations: self supportedClassCategoryDropOperations.
!

onDragClassesOver: aSession 

	self
		onDragOver: aSession 
		operations: self supportedClassDropOperations.
!

onDragCut: aSession 
!

onDragDrop: aSession 

	(aSession isFormatAvailable: #'method') 				ifTrue: [^self onDragDropMethods: aSession].
	(aSession isFormatAvailable: #'methodCategory') 	ifTrue: [^self onDragDropMethodCategories: aSession].
	(aSession isFormatAvailable: #'class') 					ifTrue: [^self onDragDropClasses: aSession].
	(aSession isFormatAvailable: #'classCategory')		ifTrue: [^self onDragDropClassCategories: aSession].
	(aSession isFormatAvailable: #'symbolDictionary')	ifTrue: [^self onDragDropSymbolDictionaries: aSession].
	MessageBox notify: 'Sorry, we are not yet able to drag/drop ' , aSession printString , '!!'.
	Keyboard default isShiftDown ifTrue: [self halt].
!

onDragDropClassCategories: aSession 

	aSession dragObjects do: [:each | 
		self
			dropClassCategory: (each format: #classCategory)
			on: aSession suggestedTarget.
	].
!

onDragDropClasses: aSession 

	aSession dragObjects do: [:each | 
		self
			dropClass: (each format: #class)
			on: aSession suggestedTarget.
	].
!

onDragDropMethodCategories: aSession

	aSession dragObjects do: [:each | 
		self
			dropMethodCategory: (each format: #methodCategory)
			on: aSession suggestedTarget.
	].
!

onDragDropMethods: aSession

	aSession dragObjects do: [:each | 
		self
			dropMethod: (each format: #method)
			on: aSession suggestedTarget.
	].
!

onDragDropSymbolDictionaries: aSession 

	aSession dragObjects do: [:each | 
		self
			dropSymbolDictionary: (each format: #symbolDictionary)
			on: aSession suggestedTarget.
	].
!

onDragDropSymbolLists: aSession 

	aSession dragObjects do: [:each | 
		self
			dropSymbolList: (each format: #symbolList)
			on: aSession suggestedTarget.
	].
!

onDragMethodCategoriesOver: aSession

	self
		onDragOver: aSession 
		operations: self supportedMethodCategoryDropOperations.
!

onDragMethodsOver: aSession

	self
		onDragOver: aSession 
		operations: self supportedMethodDropOperations.
!

onDragOver: aSession

	aSession operation: nil.
	aSession dragObjects isEmpty ifTrue: [^self].
	aSession suggestedTarget isNil ifTrue: [^self].
	(aSession isFormatAvailable: #'method') 				ifTrue: [^self onDragMethodsOver: aSession].
	(aSession isFormatAvailable: #'methodCategory') 	ifTrue: [^self onDragMethodCategoriesOver: aSession].
	(aSession isFormatAvailable: #'class') 					ifTrue: [^self onDragClassesOver: aSession].
	(aSession isFormatAvailable: #'classCategory')		ifTrue: [^self onDragClassCategoriesOver: aSession].
	(aSession isFormatAvailable: #'symbolDictionary')	ifTrue: [^self onDragSymbolDictionariesOver: aSession].
	MessageBox notify: 'Sorry, we are not yet able to drag over ' , aSession printString , '!!'.
	Keyboard default isShiftDown ifTrue: [self halt].
!

onDragOver: aSession operations: aList

	aList isEmpty ifTrue: [^self].
	aSession
		supportedOperations: aList;
		operation: aList first;
		yourself.
!

onDragSymbolDictionariesOver: aSession 

	self
		onDragOver: aSession 
		operations: self supportedSymbolDictionaryDropOperations.
!

primaryPresenter

	^nil.
!

selectionChanging: aSelectionChangingEvent 

	self 
		trigger: #'selectionChanging:'
		with: aSelectionChangingEvent.
!

statusBarText: aString

	self topShell statusBarText: aString.
!

subMenuName

	^nil.
!

subMenuPresenter

	^nil.
!

supportedClassCategoryDropOperations

	^#().
!

supportedClassDropOperations

	^#().
!

supportedMethodCategoryDropOperations

	^#().
!

supportedMethodDropOperations

	^#().
!

supportedSymbolListDropOperations

	^#().
!

updateMenuBar: aMenuBar

	self
		updateMenuBar: aMenuBar 
		withName: self subMenuName 
		itemsFrom: self subMenuPresenter.
!

updateMenuBar: aMenuBar withName: aString itemsFrom: aPresenter

	| contextMenu commandsToRemove items newMenu |
	aString isNil ifTrue: [^self].
	(contextMenu := aPresenter view contextMenu) isNil ifTrue: [^self].
	commandsToRemove := #(#'rename').
	items := contextMenu items reject: [:each | commandsToRemove includes: each command].
	items isEmpty ifTrue: [^self].
	newMenu := aMenuBar addSubmenu: aString.
	1 to: items size do: [:i | 
		| item description |
		item := (items at: i) copy.
		(item isKindOf: CommandMenuItem) ifTrue: [
			description := item commandDescription.
			description := ClosedCommandDescription new
				command: 				description command;
				description: 				description description;
				acceleratorKey: 		description acceleratorKey;
				isModalCommand: 	description isModalCommand;
				image: 						description image;
				receiver:					self;
				queryBlock: 				[:aCommandQuery | self queryCommand: aCommandQuery];
				yourself.
			item commandDescription: description.
		].
		newMenu 
			insertItem: item
			at: i.
	].
!

userSelection

	^nil.
! !
!JadePresenter categoriesFor: #aboutToEditLabel:accept:!label edit!public! !
!JadePresenter categoriesFor: #canEditLabel:!label edit!public! !
!JadePresenter categoriesFor: #createDragDropSchematicWiringFor:!drag & drop!public! !
!JadePresenter categoriesFor: #createLabelEditSchematicWiringFor:!label edit!public! !
!JadePresenter categoriesFor: #createSchematicWiring!drag & drop!label edit!public! !
!JadePresenter categoriesFor: #dragDropPresenter!drag & drop!public! !
!JadePresenter categoriesFor: #dragDropPresenters!drag & drop!public! !
!JadePresenter categoriesFor: #drop:on:description:!drag & drop!public! !
!JadePresenter categoriesFor: #dropClass:on:!drag & drop!public! !
!JadePresenter categoriesFor: #dropClassCategory:on:!drag & drop!public! !
!JadePresenter categoriesFor: #dropMethod:on:!drag & drop!public! !
!JadePresenter categoriesFor: #dropMethodCategory:on:!drag & drop!public! !
!JadePresenter categoriesFor: #dropSymbolDictionary:on:!drag & drop!public! !
!JadePresenter categoriesFor: #gciSession!public! !
!JadePresenter categoriesFor: #gciSession:!public! !
!JadePresenter categoriesFor: #labelEditPresenter!label edit!public! !
!JadePresenter categoriesFor: #labelEditPresenters!label edit!public! !
!JadePresenter categoriesFor: #labelOf:changedTo:!label edit!public! !
!JadePresenter categoriesFor: #labelOf:editedTo:accept:!label edit!public! !
!JadePresenter categoriesFor: #model:!public! !
!JadePresenter categoriesFor: #onDrag:!drag & drop!public! !
!JadePresenter categoriesFor: #onDragClassCategoriesOver:!drag & drop!public! !
!JadePresenter categoriesFor: #onDragClassesOver:!drag & drop!public! !
!JadePresenter categoriesFor: #onDragCut:!drag & drop!public! !
!JadePresenter categoriesFor: #onDragDrop:!drag & drop!public! !
!JadePresenter categoriesFor: #onDragDropClassCategories:!public! !
!JadePresenter categoriesFor: #onDragDropClasses:!public! !
!JadePresenter categoriesFor: #onDragDropMethodCategories:!drag & drop!public! !
!JadePresenter categoriesFor: #onDragDropMethods:!drag & drop!public! !
!JadePresenter categoriesFor: #onDragDropSymbolDictionaries:!public! !
!JadePresenter categoriesFor: #onDragDropSymbolLists:!public! !
!JadePresenter categoriesFor: #onDragMethodCategoriesOver:!drag & drop!public! !
!JadePresenter categoriesFor: #onDragMethodsOver:!drag & drop!public! !
!JadePresenter categoriesFor: #onDragOver:!drag & drop!public! !
!JadePresenter categoriesFor: #onDragOver:operations:!drag & drop!public! !
!JadePresenter categoriesFor: #onDragSymbolDictionariesOver:!drag & drop!public! !
!JadePresenter categoriesFor: #primaryPresenter!drag & drop!label edit!public! !
!JadePresenter categoriesFor: #selectionChanging:!public! !
!JadePresenter categoriesFor: #statusBarText:!public! !
!JadePresenter categoriesFor: #subMenuName!menus!public! !
!JadePresenter categoriesFor: #subMenuPresenter!menus!public! !
!JadePresenter categoriesFor: #supportedClassCategoryDropOperations!drag & drop!public! !
!JadePresenter categoriesFor: #supportedClassDropOperations!drag & drop!public! !
!JadePresenter categoriesFor: #supportedMethodCategoryDropOperations!drag & drop!public! !
!JadePresenter categoriesFor: #supportedMethodDropOperations!drag & drop!public! !
!JadePresenter categoriesFor: #supportedSymbolListDropOperations!drag & drop!public! !
!JadePresenter categoriesFor: #updateMenuBar:!menus!public! !
!JadePresenter categoriesFor: #updateMenuBar:withName:itemsFrom:!menus!public! !
!JadePresenter categoriesFor: #userSelection!public! !

!JadePresenter class methodsFor!

script
"
	JadePresenter script.
"

	| myPackages myPresenters myShells dict stream buildHierarchy reportHierarchy |
	myPackages := PackageManager current packages select: [:each | each packagePathname beginsWith: 'sources\'].
	myPresenters := Presenter allSubclasses select: [:eachPresenter | myPackages anySatisfy: [:eachPackage | eachPackage includesClass: eachPresenter]].
	myShells := myPresenters select: [:each | each inheritsFrom: Shell].
	myPresenters := myPresenters copy reject: [:each | myShells includes: each].
	buildHierarchy := [:eachClass |
		| child parent |
		child := eachClass.
		[
			parent := child superclass.
			parent notNil.
		] whileTrue: [
			(dict at: parent ifAbsentPut: [Set new]) add: child.
			child := parent.
		].
	].
	reportHierarchy := [:class :indent :printBlock| 
		indent timesRepeat: [stream nextPut: $-].
		printBlock value: class.
		(dict at: class ifAbsent: [nil]) ifNotNil: [:aSet | 
			(aSet asSortedCollection: [:a :b | a name <= b name]) do: [:each | reportHierarchy value: each value: indent + 1 value: printBlock].
		].
	].
	dict := Dictionary new.
	myShells do: buildHierarchy.
	"dict := Dictionary new.
	myPresenters do: buildHierarchy."
	stream := WriteStream on: String new.
	reportHierarchy value: Object value: 0 value: [:aClass | 
		stream nextPutAll: aClass name.
		(myPackages anySatisfy: [:eachPackage | eachPackage includesClass: aClass]) ifTrue: [
			stream nextPutAll: ' ( '.
			aClass allReferences do: [:each | 
				((each isKindOf: CompiledMethod) and: [each isClassMethod]) ifTrue: [
					stream nextPutAll: each methodClass instanceClass name.
					stream space.
				].
			].
			stream nextPutAll: ')'.
		].
		stream cr.
	].
	^stream contents.
! !
!JadePresenter class categoriesFor: #script!public! !

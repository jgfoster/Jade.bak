"Filed out from Dolphin Smalltalk 7"!

JadePresenter subclass: #JadePresenterA
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadePresenterA guid: (GUID fromString: '{ccc9b223-b3b5-44fd-bf88-60f48b65dc99}')!
JadePresenterA comment: ''!
!JadePresenterA categoriesForClass!Unclassified! !
!JadePresenterA methodsFor!

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
	SessionManager current pause.
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
	SessionManager current pause.
!

model: anObject

	(anObject isKindOf: GciSession) ifTrue: [self error: 'should call gciSession: method'].
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

	(aSession isFormatAvailable: #'method') 			ifTrue: [^self onDragDropMethods: aSession].
	(aSession isFormatAvailable: #'methodCategory') 	ifTrue: [^self onDragDropMethodCategories: aSession].
	(aSession isFormatAvailable: #'class') 			ifTrue: [^self onDragDropClasses: aSession].
	(aSession isFormatAvailable: #'classCategory')		ifTrue: [^self onDragDropClassCategories: aSession].
	(aSession isFormatAvailable: #'symbolDictionary')	ifTrue: [^self onDragDropSymbolDictionaries: aSession].
	MessageBox notify: 'Sorry, we are not yet able to drag/drop ' , aSession printString , '!!'.
	SessionManager current pause.
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
	(aSession isFormatAvailable: #'method') 			ifTrue: [^self onDragMethodsOver: aSession].
	(aSession isFormatAvailable: #'methodCategory') 	ifTrue: [^self onDragMethodCategoriesOver: aSession].
	(aSession isFormatAvailable: #'class') 			ifTrue: [^self onDragClassesOver: aSession].
	(aSession isFormatAvailable: #'classCategory')		ifTrue: [^self onDragClassCategoriesOver: aSession].
	(aSession isFormatAvailable: #'symbolDictionary')	ifTrue: [^self onDragSymbolDictionariesOver: aSession].
	MessageBox notify: 'Sorry, we are not yet able to drag over ' , aSession printString , '!!'.
	SessionManager current pause.
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

userSelection

	^nil.
! !
!JadePresenterA categoriesFor: #aboutToEditLabel:accept:!label edit!public! !
!JadePresenterA categoriesFor: #canEditLabel:!label edit!public! !
!JadePresenterA categoriesFor: #createDragDropSchematicWiringFor:!drag & drop!public! !
!JadePresenterA categoriesFor: #createLabelEditSchematicWiringFor:!label edit!public! !
!JadePresenterA categoriesFor: #createSchematicWiring!drag & drop!label edit!public! !
!JadePresenterA categoriesFor: #dragDropPresenter!drag & drop!public! !
!JadePresenterA categoriesFor: #dragDropPresenters!drag & drop!public! !
!JadePresenterA categoriesFor: #drop:on:description:!drag & drop!public! !
!JadePresenterA categoriesFor: #dropClass:on:!drag & drop!public! !
!JadePresenterA categoriesFor: #dropClassCategory:on:!drag & drop!public! !
!JadePresenterA categoriesFor: #dropMethod:on:!drag & drop!public! !
!JadePresenterA categoriesFor: #dropMethodCategory:on:!drag & drop!public! !
!JadePresenterA categoriesFor: #dropSymbolDictionary:on:!drag & drop!public! !
!JadePresenterA categoriesFor: #labelEditPresenter!label edit!public! !
!JadePresenterA categoriesFor: #labelEditPresenters!label edit!public! !
!JadePresenterA categoriesFor: #labelOf:changedTo:!label edit!public! !
!JadePresenterA categoriesFor: #labelOf:editedTo:accept:!label edit!public! !
!JadePresenterA categoriesFor: #model:!public! !
!JadePresenterA categoriesFor: #onDrag:!drag & drop!public! !
!JadePresenterA categoriesFor: #onDragClassCategoriesOver:!drag & drop!public! !
!JadePresenterA categoriesFor: #onDragClassesOver:!drag & drop!public! !
!JadePresenterA categoriesFor: #onDragCut:!drag & drop!public! !
!JadePresenterA categoriesFor: #onDragDrop:!drag & drop!public! !
!JadePresenterA categoriesFor: #onDragDropClassCategories:!public! !
!JadePresenterA categoriesFor: #onDragDropClasses:!public! !
!JadePresenterA categoriesFor: #onDragDropMethodCategories:!drag & drop!public! !
!JadePresenterA categoriesFor: #onDragDropMethods:!drag & drop!public! !
!JadePresenterA categoriesFor: #onDragDropSymbolDictionaries:!public! !
!JadePresenterA categoriesFor: #onDragDropSymbolLists:!public! !
!JadePresenterA categoriesFor: #onDragMethodCategoriesOver:!drag & drop!public! !
!JadePresenterA categoriesFor: #onDragMethodsOver:!drag & drop!public! !
!JadePresenterA categoriesFor: #onDragOver:!drag & drop!public! !
!JadePresenterA categoriesFor: #onDragOver:operations:!drag & drop!public! !
!JadePresenterA categoriesFor: #onDragSymbolDictionariesOver:!drag & drop!public! !
!JadePresenterA categoriesFor: #primaryPresenter!drag & drop!label edit!public! !
!JadePresenterA categoriesFor: #selectionChanging:!public! !
!JadePresenterA categoriesFor: #statusBarText:!public! !
!JadePresenterA categoriesFor: #supportedClassCategoryDropOperations!drag & drop!public! !
!JadePresenterA categoriesFor: #supportedClassDropOperations!drag & drop!public! !
!JadePresenterA categoriesFor: #supportedMethodCategoryDropOperations!drag & drop!public! !
!JadePresenterA categoriesFor: #supportedMethodDropOperations!drag & drop!public! !
!JadePresenterA categoriesFor: #supportedSymbolListDropOperations!drag & drop!public! !
!JadePresenterA categoriesFor: #userSelection!public! !

!JadePresenterA class methodsFor!

createIn: aCompositePresenter gciSession: aGciSession
	"Answers an instance of the receiver created as a sub-presenter of aCompositePresenter
	and wired up to a view identified by the resource name aResourceNameString. The new
	presenter is to be connected to aModel. It is assumed that, at this stage, aCompositePresenter
	is already opened in a view. For this reason we must force an #onViewOpened message to
	the newly created presenter"

	| newOne |
	newOne := aCompositePresenter add: (self new gciSession: aGciSession; yourself).
	newOne createView: self defaultView.
	^newOne onViewOpened; yourself
	!

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
!JadePresenterA class categoriesFor: #createIn:gciSession:!public! !
!JadePresenterA class categoriesFor: #script!public! !


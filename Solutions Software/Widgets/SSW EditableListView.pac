| package |
package := Package name: 'SSW EditableListView'.
package paxVersion: 1;
	basicComment: 'Release 1.50 for Dolphin X6

©2006 Solutions Software Limited

http://www.solutionsoft.co.uk/widgets/editablelistview

'.

package basicPackageVersion: '0.001'.


package classNames
	add: #EditableListView;
	add: #EditableListViewColumn;
	add: #EmbeddedCheckBox;
	add: #EmbeddedComboBox;
	add: #EmbeddedFormattedTextEdit;
	add: #EmbeddedMultilineTextEdit;
	add: #EmbeddedTextEdit;
	yourself.

package methodNames
	add: #ImageManager -> #basicImageListWithExtent:;
	add: #ImageManager -> #buildBasicImageListWithExtent:;
	add: #View -> #embeddedValue;
	add: #View -> #embeddedValue:;
	add: 'ListPresenter class' -> #resource_Editable_list_view;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\..\Object Arts\Dolphin\MVP\Presenters\Date Time\Dolphin Date Time Presenters';
	add: '..\..\Object Arts\Dolphin\MVP\Models\List\Dolphin List Models';
	add: '..\..\Object Arts\Dolphin\MVP\Presenters\List\Dolphin List Presenter';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Text Presenter';
	add: '..\..\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	add: '..\..\Object Arts\Dolphin\MVP\Models\Value\Dolphin Value Models';
	add: 'SSW ListView Extensions';
	add: 'SSW Widget Enhancements';
	yourself).

package setManualPrerequisites: #(
	'SSW ListView Extensions').

package!

"Class Definitions"!

ListViewColumn subclass: #EditableListViewColumn
	instanceVariableNames: 'isEditable setContentsBlock editor forecolor backcolor preDrawBlock getSortContentsBlock isEditableBlock cachedWidth cachedIndex'
	classVariableNames: ''
	poolDictionaries: 'CommCtrlConstants'
	classInstanceVariableNames: ''!
ComboBox subclass: #EmbeddedComboBox
	instanceVariableNames: 'choiceModel cellRect'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ListView subclass: #EditableListView
	instanceVariableNames: 'activeColumn multSelectStack _spare0 rowForecolor rowBackcolor rowHeight smallImageExtent hasVirtualGridlines'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TextEdit subclass: #EmbeddedTextEdit
	instanceVariableNames: 'offset'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FormattedTextEdit subclass: #EmbeddedFormattedTextEdit
	instanceVariableNames: 'offset'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MultilineTextEdit subclass: #EmbeddedMultilineTextEdit
	instanceVariableNames: 'column'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
EmulatedCheckBox subclass: #EmbeddedCheckBox
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!ImageManager methodsFor!

basicImageListWithExtent: aPoint 

	"Same as imageListWithExtent: but do not populate"

	| imageList |
	imageList := imageLists at: aPoint ifAbsent: [].
	(imageList isNil or: [imageList isRealized not]) 
		ifTrue: [imageList := self buildBasicImageListWithExtent: aPoint].
	^imageList!

buildBasicImageListWithExtent: aPoint

	"Same as buildImageListWithExtent: but do not populate"

	"Add the new rendering to our dictionary of those available"
	^imageLists at: aPoint put: (self newImageListWithExtent: aPoint)! !
!ImageManager categoriesFor: #basicImageListWithExtent:!accessing!public! !
!ImageManager categoriesFor: #buildBasicImageListWithExtent:!accessing!public! !

!ListPresenter class methodsFor!

resource_Editable_list_view
	"Answer the literal data from which the 'Editable list view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Editable_list_view)
	"

	^#(#'!!STL' 3 788558 10 ##(STBViewProxy)  8 ##(EditableListView)  98 38 0 0 98 2 8 1140920397 1025 416 590662 2 ##(ListModel)  202 208 98 0 0 1114638 ##(STBSingletonProxy)  8 ##(SearchPolicy)  8 #identity 524550 ##(ColorRef)  8 4278190080 0 7 0 0 0 416 0 8 4294902721 8 ##(BasicListAbstract)  8 ##(IconicListAbstract)  570 8 ##(IconImageManager)  8 #current 0 0 0 328198 ##(Point)  65 65 0 0 202 208 98 1 1447494 14 ##(EditableListViewColumn)  8 'Column 1' 201 8 #left 688 8 ##(SortedCollection)  0 0 416 0 1 0 0 16 0 1052998 13 ##(EmbeddedTextEdit)  0 0 98 2 134349057 1 928 721990 2 ##(ValueHolder)  0 32 570 592 8 #equality 0 0 0 5 0 0 0 928 0 0 852486 ##(NullConverter)  0 8 '' 1 0 0 0 0 0 0 0 0 8 #report 544 0 131171 0 0 0 202 208 544 0 0 0 3 0 0 983302 ##(MessageSequence)  202 208 98 2 721670 ##(MessageSend)  8 #createAt:extent: 98 2 770 11 11 770 491 311 416 1170 8 #text: 98 1 8 'Column 1' 416 983302 ##(WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 5 0 0 0 250 0 0 0 160 0 0 0] 98 0 770 193 193 0 29 )! !
!ListPresenter class categoriesFor: #resource_Editable_list_view!public!resources-views! !

!View methodsFor!

embeddedValue

	^self model value!

embeddedValue: anObject

	self model setValue: anObject.
	self refreshContents! !
!View categoriesFor: #embeddedValue!hierarchy!public!sub views! !
!View categoriesFor: #embeddedValue:!hierarchy!public!sub views! !

"End of package definition"!

"Source Globals"!

"Classes"!

EditableListViewColumn guid: (GUID fromString: '{787A1FF5-1809-4381-AB64-22F338ECAA37}')!
EditableListViewColumn comment: ''!
!EditableListViewColumn categoriesForClass!Unclassified! !
!EditableListViewColumn methodsFor!

backcolor
	^backcolor!

backcolor: anObject
	backcolor := anObject!

cachedIndex
	^cachedIndex!

cachedIndex: anObject
	cachedIndex := anObject!

cachedWidth
	^cachedWidth!

cachedWidth: anObject
	cachedWidth := anObject!

createEditorIn: aView

	self hasEditor ifTrue: [self editor createEmbeddedIn: aView].

	self isEditable ifTrue: 
		[self editor model: 
			(ValueHolder new
				when: #valueChanged send: #onValueChangedIn: to: aView with: self;
				yourself)]!

customDrawIcon: aContext

	| image imageRect canvas colBackcolor origin |

	image := self imageFromRow: aContext item.
	image isNil ifTrue: [^self].

	imageRect := aContext view 
		lvmGetSubItemRect: (aContext dwItemSpec@aContext iSubItem) 
		bounding: LVIR_ICON.

	canvas := aContext canvas.

	(self parent backcolor ifNil: [Color default]) isDefault
		ifTrue: [colBackcolor := Color window]
		ifFalse: [colBackcolor := self parent backcolor].

	canvas 
		erase: (imageRect origin extent: imageRect extent - (0@1))
		color: colBackcolor.

	image isInteger
	ifTrue:
		["Regular ListView image index behavior"
		origin := imageRect origin + ((imageRect extent - Icon smallExtent) // 2).
		(self parent imageManager imageListWithExtent: Icon smallExtent) 
			draw: image
			on: canvas
			at: origin
			flags: 0]
	ifFalse:
		["Actual Icon"
		origin := imageRect origin + ((imageRect extent - image extent) // 2).
		image drawOn: canvas at: origin extent: image extent].

	canvas free!

displayBackcolor

	^self parent rowBackcolor ifNil: 
		[self backcolor ifNil: 
			[(self parent backcolor ifNil: [Color default]) isDefault
				ifTrue: [Color window]
				ifFalse: [self parent backcolor]]]!

displayForecolor

	^(self hasEditor not or: [self editor isEnabled])
	ifTrue: 
		[self parent rowForecolor ifNil: 
			[self forecolor ifNil: 
				[self parent forecolor ifNil:
					[Color windowText]]]]
	ifFalse: 
		[Color grayText]!

displayOn: aStream

	super displayOn: aStream.
	self hasEditor ifTrue: [aStream space; nextPutAll: self editor class name]!

editor
	^editor!

editor: anObject
	editor := anObject!

forecolor
	^forecolor!

forecolor: anObject
	forecolor := anObject!

getSortContentsBlock
	^getSortContentsBlock!

getSortContentsBlock: anObject
	getSortContentsBlock := anObject!

hasColumnImage

	^self parent hasColumnImages and: [self hasImageBlock and: [self ~~ self parent primaryColumn]]!

hasEditor

	^self editor notNil!

hasImage

	^(self parent hasColumnImages and: [self hasImageBlock])
		or: [self parent primaryColumn == self and: [self parent hasImageBlock]]!

hasImageBlock

	^self getImageBlock notNil!

hasPreDrawBlock

	^self preDrawBlock notNil!

hide

	self 
		cachedWidth: self width;
		cachedIndex: (self parent allColumns identityIndexOf: self).

	self parent removeColumn: self!

imageFromRow: anObject

	^self parent hasColumnImages
		ifTrue: [super imageFromRow: anObject]
		ifFalse: [self parent imageFromRow: anObject]!

initialize
	"Private - Initialize the state of the receiever."

	super initialize.
	self
		isEditable: true;
		setContentsBlock: nil;
		editor: EmbeddedTextEdit new!

isEditable
	^isEditable!

isEditable: anObject
	isEditable := anObject!

isEditableBlock
	^isEditableBlock!

isEditableBlock: anObject
	isEditableBlock := anObject!

isEditableWith: anObject

	^self isEditable and: [self isEditableBlock isNil or: [self isEditableBlock value: anObject]]!

onSelectionChanged

	"Do nothing"!

ownerDraw: aContext

	| itemRect canvas brush bcolor fcolor hasColumnImage |

	"Self-drawing editor active? It will handle all drawing itself"
	(self hasEditor and: [self editor isDisplayOwnerDrawn not 
		and: [self editor hasVisibleStyle and: [aContext item == self parent selectionOrNil]]]) ifTrue: 
			["...however, need to ensure any areas not covered by the editor are cleared"
			canvas := aContext canvas.
			self parent activeEditorNeedsHighlight ifTrue:
				[(self parent activeEditor cellRect areasOutside: self parent activeEditor rectangle) do:
					[ :each | canvas erase: each color: Color face3d]].
			"...and, if the receiver has an icon, need to draw it manually"
			self parent postDraw: aContext columnIndex: self index. "Implicit canvas free"
			^true].

	aContext forecolor: self displayForecolor; backcolor: self displayBackcolor.
	self hasPreDrawBlock ifTrue: [self preDrawBlock value: aContext].

	hasColumnImage := self hasColumnImage.

	(self hasEditor and: [self editor isDisplayOwnerDrawn or: [hasColumnImage]]) ifFalse: [^false].

	aContext rc width = 0 ifTrue: [^self].

	canvas := aContext canvas.

	"aContext boundingRectangle unreliable"
	itemRect := aContext view 
				lvmGetSubItemRect: (aContext dwItemSpec@aContext iSubItem) 
				bounding: LVIR_LABEL.

	hasColumnImage ifTrue: [itemRect left: ((itemRect left + self parent smallImageExtent x) min: itemRect right)].

	"Handle selection background color"
	aContext view hasFullRowSelect 
	ifTrue:
		[(self parent isSelected: aContext item)
		ifTrue: 
			[self parent hasFocus
			ifTrue: 
				[bcolor := Color highlight.
				fcolor := Color highlightText]
			ifFalse: 
				[self parent showsSelAlways
					ifTrue: [bcolor := Color face3d]
					ifFalse: [bcolor := aContext backcolor]]]
		ifFalse: 
			[bcolor := self displayBackcolor]]
	ifFalse:
		[bcolor := aContext backcolor].

	brush := Brush color: bcolor.
	canvas fillRectangle: itemRect brush: brush.
	brush free.

	self editor 
		display: (self getContentsBlock value: aContext item) 
		in: itemRect 
		on: canvas
		forecolor: (fcolor ifNil: [self displayForecolor])
		backcolor: bcolor.

	hasColumnImage ifTrue: [self customDrawIcon: aContext].

	^true!

preDrawBlock
	^preDrawBlock!

preDrawBlock: anObject
	preDrawBlock := anObject!

rowSortBlock
	"Private - Answer a two argument block that can be used to compare
	two rows based on this column, or nil if the column is not sortable.
	Note that the first time the sort block is accessed, it is returned as originally 
	set up. On the next access it is inverted, and on the next it is inverted again
	(i.e. back to the original). This effectively toggles the sort order between
	ascending (or whatever it was originally) and descending, each time
	the sort block is accessed."

	^self isSortable ifTrue: 
		[self isSortOrderInverted 
			ifTrue: 
				[[:a :b | getSortValueBlock value: (self sortContentFromRow: b) value: (self sortContentFromRow: a)]]
			ifFalse: 
				[[:a :b | getSortValueBlock value: (self sortContentFromRow: a) value: (self sortContentFromRow: b)]]]
!

setContentsBlock

	"setContentsBlock stores a 2-arg block which is evaluated with a list item 
	and an updated value (as returned by the editor) to set the new value in a list"

	^setContentsBlock!

setContentsBlock: anObject

	"setContentsBlock stores a 2-arg block which is evaluated with a list item 
	and an updated value (as returned by the editor) to set the new value in a list"

	setContentsBlock := anObject!

setEditorValueFrom: anObject

	self editor embeddedValue: (self getContentsBlock value: anObject)!

showIn: anEditableListView

	anEditableListView addColumn: self atIndex: self cachedIndex.
	self width: self cachedWidth!

sortContentFromRow: item

	"Return the row attribute to use for sorting"

	^self getSortContentsBlock isNil 
		ifTrue: [self contentFromRow: item]
		ifFalse: [self getSortContentsBlock value: item]!

updateValueIn: anObject

	self setContentsBlock value: anObject value: self editor embeddedValue! !
!EditableListViewColumn categoriesFor: #backcolor!accessing!private! !
!EditableListViewColumn categoriesFor: #backcolor:!accessing!private! !
!EditableListViewColumn categoriesFor: #cachedIndex!accessing!private! !
!EditableListViewColumn categoriesFor: #cachedIndex:!accessing!private! !
!EditableListViewColumn categoriesFor: #cachedWidth!accessing!private! !
!EditableListViewColumn categoriesFor: #cachedWidth:!accessing!private! !
!EditableListViewColumn categoriesFor: #createEditorIn:!initialize/release!private! !
!EditableListViewColumn categoriesFor: #customDrawIcon:!operations!public! !
!EditableListViewColumn categoriesFor: #displayBackcolor!accessing!private! !
!EditableListViewColumn categoriesFor: #displayForecolor!accessing!private! !
!EditableListViewColumn categoriesFor: #displayOn:!operations!private! !
!EditableListViewColumn categoriesFor: #editor!accessing!public! !
!EditableListViewColumn categoriesFor: #editor:!accessing!public! !
!EditableListViewColumn categoriesFor: #forecolor!accessing!private! !
!EditableListViewColumn categoriesFor: #forecolor:!accessing!private! !
!EditableListViewColumn categoriesFor: #getSortContentsBlock!accessing!private! !
!EditableListViewColumn categoriesFor: #getSortContentsBlock:!accessing!private! !
!EditableListViewColumn categoriesFor: #hasColumnImage!public!testing! !
!EditableListViewColumn categoriesFor: #hasEditor!public!testing! !
!EditableListViewColumn categoriesFor: #hasImage!public!testing! !
!EditableListViewColumn categoriesFor: #hasImageBlock!public!testing! !
!EditableListViewColumn categoriesFor: #hasPreDrawBlock!accessing!public! !
!EditableListViewColumn categoriesFor: #hide!operations!public! !
!EditableListViewColumn categoriesFor: #imageFromRow:!public!testing! !
!EditableListViewColumn categoriesFor: #initialize!initialize/release!private! !
!EditableListViewColumn categoriesFor: #isEditable!accessing!public! !
!EditableListViewColumn categoriesFor: #isEditable:!accessing!public! !
!EditableListViewColumn categoriesFor: #isEditableBlock!accessing!private! !
!EditableListViewColumn categoriesFor: #isEditableBlock:!accessing!private! !
!EditableListViewColumn categoriesFor: #isEditableWith:!public!testing! !
!EditableListViewColumn categoriesFor: #onSelectionChanged!event handling!public! !
!EditableListViewColumn categoriesFor: #ownerDraw:!operations!public! !
!EditableListViewColumn categoriesFor: #preDrawBlock!accessing!private! !
!EditableListViewColumn categoriesFor: #preDrawBlock:!accessing!private! !
!EditableListViewColumn categoriesFor: #rowSortBlock!accessing!private! !
!EditableListViewColumn categoriesFor: #setContentsBlock!accessing!public! !
!EditableListViewColumn categoriesFor: #setContentsBlock:!accessing!public! !
!EditableListViewColumn categoriesFor: #setEditorValueFrom:!operations!private! !
!EditableListViewColumn categoriesFor: #showIn:!operations!public! !
!EditableListViewColumn categoriesFor: #sortContentFromRow:!adapters!private! !
!EditableListViewColumn categoriesFor: #updateValueIn:!operations!private! !

!EditableListViewColumn class methodsFor!

stbVersion

	"12: Added isEditableBlock
	14: Added cachedWidth and cachedIndex"

	^14
! !
!EditableListViewColumn class categoriesFor: #stbVersion!public! !

EmbeddedComboBox guid: (GUID fromString: '{37AD4011-9526-4F00-ADBB-A2E73C6B5EBC}')!
EmbeddedComboBox comment: ''!
!EmbeddedComboBox categoriesForClass!Unclassified! !
!EmbeddedComboBox methodsFor!

cbnSelChange

	self choiceModel value: self selectionOrNil.
	^super cbnSelChange!

cellRect

	^cellRect!

cellRect: anObject
	cellRect := anObject!

choiceModel
	^choiceModel!

choiceModel: anObject
	choiceModel := anObject!

choices: aCollection

	self model list: aCollection!

column

	^self parentView allColumns detect: [ :each | each editor == self]!

createEmbeddedIn: aView

	interactor := presenter := self.
	flags := 0.

	self 
		isManaged: true;
		initializeModel;
		parentView: aView;
		create;
		font: self view font;
		yourself!

defaultWindowExStyle
	"Private - Answer the default extended window creation style.
	Use the client edge style for the standard Win95 look."

	^super defaultWindowExStyle bitXor: WS_EX_CLIENTEDGE!

defaultWindowStyle
	"Private - Answer a default style to use when creating an EmbeddedTextEdit."

	^(super defaultWindowStyle) | 128 "ES_AUTOHSCROLL"!

display: anObject in: aRectangle on: aCanvas forecolor: fColor backcolor: bColor 

	| text actualOffset |

	text := self displayStringFor: anObject.
	actualOffset := (self offsetX@self offsetY) + (4@2).
	
	aCanvas 
		forecolor: fColor;
		formatText: text in: ((aRectangle origin + actualOffset) extent: (aRectangle extent - actualOffset))!

displayOnFormats

	^#(#class)!

embeddedValue

	^self choiceModel value!

embeddedValue: anObject

	self selection: anObject.
	self choiceModel setValue: anObject.
	lastSelIndices := #()!

isDisplayOwnerDrawn

	^false!

listModel: aListModel

	super model: aListModel!

model: aValueHolder

	self choiceModel: aValueHolder!

offsetX

	^0!

offsetY

	^self parentView isMultiline
		ifTrue: [(((self actualFont pixelSize) * (self parentView rowHeight - 1)) + 2) //2]
		ifFalse: [0]!

onCursoredLeft

	"The receiver has been entered by a cursor left action"
!

onFullyCreated

	super onFullyCreated.

	"CB_SETEXTENDEDUI"
	self sendMessage: 16r0155 wParam: true asParameter
!

onKillFocus

	self parentView hideActiveEditor.

	^super onKillFocus!

onRequestDeactivate

	"Return if OK"

	^true!

preTranslateKeyboardInput: aMSG

	aMSG message = WM_KEYDOWN ifTrue:
		[| key action |
		key := aMSG wParam.
		key = VK_TAB ifTrue:
			[Keyboard default isShiftDown
				ifTrue: [action := MessageSend receiver: self parentView selector: #fieldExitPrevTabbed: arguments: #(true)]
				ifFalse: [action := MessageSend receiver: self parentView selector: #fieldExitNextTabbed: arguments: #(true)]].

		action notNil ifTrue:
			[SessionManager inputState queueDeferredAction: action.
			^true]].

	^super preTranslateKeyboardInput: aMSG!

showIn: aRectangle

	"need to cache the cellRect since the receiver's height isn't reliable"
	self cellRect: aRectangle.

	self
		rectangle: ((aRectangle origin extent: (aRectangle width@(aRectangle height + self droppedHeight))) 
					insetBy: ((self offsetX@self offsetY) corner: (0@self offsetY)));
		show!

stbSaveOn: anSTBOutFiler

	"If the receiver is unrealized (i.e. just exists in the spec of a column in the view composer),
	save as a plain object"

	handle isNil 
		ifTrue: [anSTBOutFiler saveObject: self]
		ifFalse: [super stbSaveOn: anSTBOutFiler]! !
!EmbeddedComboBox categoriesFor: #cbnSelChange!event handling!public! !
!EmbeddedComboBox categoriesFor: #cellRect!accessing!public! !
!EmbeddedComboBox categoriesFor: #cellRect:!accessing!private! !
!EmbeddedComboBox categoriesFor: #choiceModel!accessing!private! !
!EmbeddedComboBox categoriesFor: #choiceModel:!accessing!private! !
!EmbeddedComboBox categoriesFor: #choices:!public!testing! !
!EmbeddedComboBox categoriesFor: #column!accessing!public! !
!EmbeddedComboBox categoriesFor: #createEmbeddedIn:!initialize/release!public! !
!EmbeddedComboBox categoriesFor: #defaultWindowExStyle!constants!private! !
!EmbeddedComboBox categoriesFor: #defaultWindowStyle!constants!private! !
!EmbeddedComboBox categoriesFor: #display:in:on:forecolor:backcolor:!displaying!public! !
!EmbeddedComboBox categoriesFor: #displayOnFormats!binary filing!public! !
!EmbeddedComboBox categoriesFor: #embeddedValue!accessing!public! !
!EmbeddedComboBox categoriesFor: #embeddedValue:!accessing!public! !
!EmbeddedComboBox categoriesFor: #isDisplayOwnerDrawn!public!testing! !
!EmbeddedComboBox categoriesFor: #listModel:!public!testing! !
!EmbeddedComboBox categoriesFor: #model:!accessing!public! !
!EmbeddedComboBox categoriesFor: #offsetX!accessing!public! !
!EmbeddedComboBox categoriesFor: #offsetY!accessing!public! !
!EmbeddedComboBox categoriesFor: #onCursoredLeft!event handling!public! !
!EmbeddedComboBox categoriesFor: #onFullyCreated!event handling!public! !
!EmbeddedComboBox categoriesFor: #onKillFocus!event handling!public! !
!EmbeddedComboBox categoriesFor: #onRequestDeactivate!event handling!public! !
!EmbeddedComboBox categoriesFor: #preTranslateKeyboardInput:!event handling!public! !
!EmbeddedComboBox categoriesFor: #showIn:!displaying!public! !
!EmbeddedComboBox categoriesFor: #stbSaveOn:!binary filing!public! !

!EmbeddedComboBox class methodsFor!

stbConvertFrom: anSTBClassFormat 
	"Convert from version 9. Version 10 adds cellRect inst var"

	^[:data | 
	| newInstance |
	newInstance := self basicNew.
	1 to: data size do: [:i | newInstance instVarAt: i put: (data at: i)].
	newInstance cellRect: nil.
	newInstance]!

stbVersion

	^10! !
!EmbeddedComboBox class categoriesFor: #stbConvertFrom:!public! !
!EmbeddedComboBox class categoriesFor: #stbVersion!public! !

EditableListView guid: (GUID fromString: '{E2B66E41-55E0-4105-A492-B9DEFD40CD23}')!
EditableListView comment: ''!
!EditableListView categoriesForClass!Unclassified! !
!EditableListView methodsFor!

activateEditorAt: aPoint tabbed: aBoolean

	"Private - aPoint is the row@column coordinates of a subitem
	if aBoolean is true, act as if tabbed into the field
	Return the editor activated, or nil if none"

	| itemRect column index item |

	itemRect := self lvmGetSubItemRect: (aPoint - (1@1)) bounding: LVIR_LABEL.
	index := aPoint x.
	item := self list at: index.

	column := self columnAtIndex: aPoint y.
	(column isEditableWith: item) ifFalse: 
		[column isEditable ifTrue: [Sound bell]. "Bell if the column is disabled for the current item only"
		self enableRedraw. 
		^nil].

	column hasColumnImage ifTrue: [itemRect left: ((itemRect left + self smallImageExtent x) min: itemRect right)].

	self activeEditorCoords: aPoint.

	[self activeEditorNeedsHighlight
		ifTrue: [self activeEditor backcolor: Color face3d]
		ifFalse: [self activeEditor backcolor: Color window].

	self activeColumn setEditorValueFrom: item.

	self activeEditor showIn: itemRect.
	
	aBoolean ifTrue: [self activeEditor tabFocus] ifFalse: [self activeEditor setFocus]]
		ensure: [self enableRedraw].

	^self activeEditor!

activateEditorForColumn: anEditableListViewColumn

	anEditableListViewColumn isEditable ifFalse: [^ self].
	self activateEditorForColumnIndex: (self allColumns indexOf: anEditableListViewColumn)!

activateEditorForColumnIndex: anInteger

	self list isEmpty ifTrue: [^self].
	self selectionByIndex = 0 ifTrue: [self selectionByIndex: self list size].

	((self columnAtIndex: anInteger) isEditableWith: self selection) ifFalse: [^self].

	^self activateEditorAt: (self selectionByIndex@anInteger) tabbed: true!

activateFirstEditor

	"Return the activated editor, or nil if none"

	| coords |

	self list isEmpty ifTrue: [^nil].
	self hasEditableColumn ifFalse: [^nil].

	coords := self firstEditableCoordsOnOrAfter: (self selectionByIndex max: 1).
	coords = nil ifTrue: [^nil].

	self selectionByIndex: coords x.
	^self activateEditorAt: coords tabbed: true!

activateLastEditor

	self selectionByIndex = 0 ifTrue: [self selectionByIndex: self list size].

	^self activateEditorAt: (self selectionByIndex@(self allColumns indexOf: self lastEditableColumn)) tabbed: true!

activeColumn
	^self columnAtIndex: (self activeEditorCoords ifNil: [^nil]) y!

activeColumnIndex

	^self activeEditorCoords y!

activeEditor
	^self activeColumn ifNil: [nil] ifNotNil: [ :col | col editor]!

activeEditorCoords
	^activeColumn!

activeEditorCoords: aPoint
	activeColumn := aPoint!

activeEditorNeedsHighlight

	^self showsSelAlways and: 
		[self hasFullRowSelect or: [self activeEditor == self primaryColumn editor]]!

additionalHorzOffsetFor: anEmbeddedEdit

	"Additional offset needed for embedded editors with column images (except primary col)"

	^(self hasColumnImage: anEmbeddedEdit)
		ifTrue: [self smallImageExtent x]
		ifFalse: [0]!

applyImageLists
	"Private - Set the receiver's image lists from the image managers."

	| largeImList smallImList pixelHeight |

	self isMultiline ifFalse: [^super applyImageLists].

	pixelHeight := (self actualFont pixelSize + 2) * self rowHeight.

	self imageManager notNil ifTrue: [
		largeImList := self imageManager imageListWithExtent: self largeIconExtent.
		smallImList := self imageManager basicImageListWithExtent: (self smallImageExtent x@pixelHeight)].

	smallImList addImage: Icon question.

	self lvmSetImageList: smallImList type: LVSIL_SMALL. 
	self lvmSetImageList: largeImList type: LVSIL_NORMAL!

clearFocusItem

	| anLvItem |

	anLvItem := LVITEM new.
	anLvItem stateMask: LVIS_FOCUSED.
	anLvItem dwState: 0.
	1 to: self itemCount do: [ :i | self lvmSetItem: i-1 state: anLvItem]!

clearFocusRect

	(self propertyAt: #focusRectDrawn ifAbsent: [false]) ifTrue:
		[self drawFocusRect: self clientFocusRectangle.
		self removePropertyAt: #focusRectDrawn]!

click: aMouseEvent activateEditor: aBoolean at: aPoint

	"Private - aPoint is the row@column coordinates of a subitem.
	Return the editor created, or nil if none"

	| row |

	row := aPoint x.

	self hasFocus ifFalse: [self setFocus].

	self validateMultSelectStack.
	(self isMultiSelect and: [aMouseEvent isShiftDown or: [aMouseEvent isCtrlDown]])
	ifTrue:
		[aMouseEvent isShiftDown
			ifTrue: [self handleShiftMultSelectOn: row]
			ifFalse: [self handleCtrlMultSelectOn: row]]
	ifFalse:
		[self handleSingleSelectOn: row].

	^aBoolean
		ifTrue: [self activateEditorAt: aPoint tabbed: false]
		ifFalse: [self enableRedraw. nil]!

clientFocusRectangle

	^self clientRectangle!

columnNamed: aString

	^self allColumns detect: [ :each | each name = aString]!

columnWithEditor: anEmbeddedEdit

	^self allColumns detect: [ :each | each editor == anEmbeddedEdit]!

commitOngoingEdit
	"If there is an active editor, then kill it nicely."

	self hasActiveEditor ifTrue: [self deactivateEditor; enableRedraw; invalidate].!

deactivateEditor

	"Private - Note that redraw is turned off to prevent flickering if the receiver gets focus.
	Clients of this method should ensure redraw is later reactivated"

	self disableRedraw.
	self hideActiveEditor!

drawFocusRect

	self drawFocusRect: self clientFocusRectangle.
	self propertyAt: #focusRectDrawn put: true!

drawHorizontalGridlinesOn: aCanvas from: top to: bottom by: height

	"Private - Special handling in case a line overlaps an active editor"

	| ed edRect |

	ed := self activeEditor.
	ed isNil ifTrue: [^super drawHorizontalGridlinesOn: aCanvas from: top to: bottom by: height].
	
	edRect := ed rectangle.
	top to: bottom by: height do:
		[ :i |
		(i between: edRect top and: edRect bottom)
		ifTrue: 
			[aCanvas 
				lineFrom: (0@i) to: (edRect left@i);
				lineFrom: (edRect right@i) to: (self rectangle width@i)]
		ifFalse:
			[aCanvas lineFrom: (0@i) to: (self rectangle width@i)]]!

drawSubItem: context

	(context iSubItem = 0 and: [self customDrawBlock isNil not]) ifTrue: [self customDrawBlock value: context].

	^context column ownerDraw: context!

fieldExitDown

	self requestDeactivateEditor ifFalse: [^0].

	self selectionByIndex < self list size ifTrue:
		[| row col |
		row := self selectionByIndex + 1.
		col := self activeColumnIndex.
		self 
			deactivateEditor;
			multSelectStack: (OrderedCollection with: row);
			selectionByIndex: self noSelection;
			selectionByIndex: row;
			activateEditorAt: (row@col) tabbed: false]
!

fieldExitNextTabbed: aBoolean

	| next |

	self requestDeactivateEditor ifFalse: [^0].

	next := self nextEditableCoords.
	self hasActiveEditor ifTrue: [self deactivateEditor].

	next isNil ifTrue: [^self enableRedraw; tabNext].

	self selectionByIndex: next x.		
	self activateEditorAt: next tabbed: aBoolean!

fieldExitPrevTabbed: aBoolean

	| nextCoords editor |

	self requestDeactivateEditor ifFalse: [^0].

	nextCoords := self prevEditableCoords.
	self hasActiveEditor ifTrue: [self deactivateEditor].

	nextCoords isNil ifTrue:
		[self enableRedraw.
		^self tabPrev].

	self selectionByIndex: nextCoords x.
	editor := self activateEditorAt: nextCoords tabbed: aBoolean.
	aBoolean ifFalse: [editor onCursoredLeft]!

fieldExitUp

	| row |

	self requestDeactivateEditor ifFalse: [^self].

	(row := self selectionByIndex - 1) > 0 ifTrue:
		[| col |
		col := self activeColumnIndex.
		self 
			deactivateEditor;
			multSelectStack: (OrderedCollection with: row);
			selectionByIndex: self noSelection;
			selectionByIndex: row;
			activateEditorAt: (row@col) tabbed: false]!

firstEditableColumn

	^self allColumns detect: [ :each | each isEditableWith: (self list at: self selectionByIndex)]!

firstEditableCoordsOnOrAfter: anInteger

	anInteger to: self list size do:
		[ :row || item |
		item := self list at: row.
		1 to: self allColumns size do:
			[ :col |
			((self columnAtIndex: col) isEditableWith: item) ifTrue: [^row@col]]].

	^nil!

handleCtrlMultSelectOn: anInteger

	"anInteger is the row clicked"

	| set |

	set := (self multSelectStack includes: anInteger) not.

	self selectIndex: anInteger set: set.
	self multSelectStack remove: anInteger ifAbsent: [].
	set ifTrue: [self multSelectStack addLast: anInteger]!

handleShiftMultSelectOn: anInteger

	"anInteger is the row clicked"

	| start newSelections |

	self multSelectStack isEmpty ifTrue: [^self handleSingleSelectOn: anInteger].

	start := self multSelectStack first.
	start < anInteger
		ifTrue: [newSelections := start to: anInteger]
		ifFalse: [newSelections := anInteger to: start].

	(self multSelectStack difference: newSelections) do:
		[ :unsetRow |
		self selectIndex: unsetRow set: false].

	newSelections do:
		[ :setRow |
		self selectIndex: setRow set: true].

	start < anInteger
		ifTrue: [self multSelectStack: newSelections asOrderedCollection]
		ifFalse: [self multSelectStack: newSelections asOrderedCollection reverse]!

handleSingleSelectOn: anInteger

	"anInteger is the row clicked"

	self selectionByIndex = anInteger ifFalse:
		[self 
			multSelectStack: (OrderedCollection with: anInteger);
			selectionByIndex: self noSelection;
			selectionByIndex: anInteger]!

hasActiveEditor

	^self activeColumn notNil!

hasColumnImage: anEmbeddedEdit

	"Additional offset needed for embedded editors with column images (except primary col)"

	| column |

	self hasColumnImages ifFalse: [^false].

	column := self columnWithEditor: anEmbeddedEdit.

	^(column ~~ self primaryColumn and: [column hasImageBlock])!

hasEditableColumn

	^self allColumns anySatisfy: [ :each | each isEditable]
!

hasEditableColumnInCurrentRow

	| current |

	current := self selection.

	^self allColumns anySatisfy: [ :each | each isEditableWith: current]
!

hasImageBlock

	^self getImageBlock notNil!

hasVirtualGridlines
	^hasVirtualGridlines ifNil: [hasVirtualGridlines := true]!

hasVirtualGridlines: anObject
	hasVirtualGridlines := anObject!

hideActiveEditor

	self activeEditor ifNotNil:
		[ :editor |
		editor hide. 
		self activeEditorCoords: nil]!

isMultiline

	^self rowHeight > 1!

isSelected: anObject

	^self selectionsByIndex includes: (self list identityIndexOf: anObject)!

itemIndex: anInteger hasFocus: aBoolean

	| anLvItem |

	anLvItem := LVITEM new.
	anLvItem stateMask: LVIS_FOCUSED.
	anLvItem dwState: aBoolean.
	self lvmSetItem: anInteger-1 state: anLvItem!

itemIndexWithFocus

	^(1 to: self itemCount) detect: [ :i | (self lvmGetItemState: (i - 1) mask: LVIS_FOCUSED) = 1] ifNone: [nil]!

itemIndexWithFocus: anInteger

	| anLvItem |

	anLvItem := LVITEM new.
	anLvItem stateMask: LVIS_FOCUSED.
	anLvItem dwState: 1.
	self lvmSetItem: anInteger-1 state: anLvItem!

lastEditableColumn

	^self allColumns reverse detect: [ :each | each isEditable]!

lastEditableCoordsOnOrBefore: anInteger

	anInteger to: 1 by: -1 do:
		[ :row || item |
		item := self list at: row.
		self allColumns size to: 1 by: -1 do:
			[ :col |
			((self columnAtIndex: col) isEditableWith: item) ifTrue: [^row@col]]].

	^nil!

lvmSetColumn: anLvColumn at: columnIndex

	"Ignore if the receiver is not open"

	^self isOpen ifTrue: [super lvmSetColumn: anLvColumn at: columnIndex]!

multSelectStack
	^multSelectStack ifNil: [multSelectStack := OrderedCollection new]!

multSelectStack: anObject
	multSelectStack := anObject!

nextEditableColumn

	(self activeColumnIndex + 1) to: self allColumns size do:
		[ :index || column |
		column := self allColumns at: index.
		column isEditable ifTrue: [^column]].

	^nil!

nextEditableCoords

	^self nextEditableCoordsInOrAfter: (self selectionByIndex@(self activeColumnIndex + 1))!

nextEditableCoordsInOrAfter: coords

	| row item | 

	row := coords x.
	item := self list at: row.
	coords y to: self allColumns size do:
		[ :col |
		((self columnAtIndex: col) isEditableWith: item) ifTrue: [^row@col]].

	^self firstEditableCoordsOnOrAfter: row + 1!

nmCustomDraw: pNMHDR

	"Override to trap or queue a (sub)item post-paint notification"

	| context columnIndex drawStage res |

	context := self customDrawContextClass fromAddress: pNMHDR.
	columnIndex := context iSubItem + 1.

	"Only needed to manually draw icons when multiline or the editor is active"
	(self isMultiline or: [(columnIndex >= 1) and: [columnIndex <= columns size and: [(self columnAtIndex: columnIndex) editor ifNil: [false] ifNotNil: [ :ed | ed hasVisibleStyle]]]]) ifFalse: [^super nmCustomDraw: pNMHDR].

	"Postdraw notification (see below)?"
	((drawStage := pNMHDR dwordAtOffset: 12) allMask: 65538 "CDDS_ITEMPOSTPAINT") ifTrue:
		[columnIndex > columns size ifTrue: [^CDRF_DODEFAULT].
		context rc width = 0 ifTrue: [^CDRF_DODEFAULT].
		context
			item: (self objectFromHandle: context itemHandle ifAbsent: [^CDRF_DODEFAULT]);
			view: self.	"The control attempts to draw stuff that isn't there on occassion"
		self postDraw: context columnIndex: columnIndex.
		^CDRF_DODEFAULT].

	res := super nmCustomDraw: pNMHDR.

	"Request postdraw notification - see above"
	^((drawStage allMask: CDDS_ITEMPREPAINT) and: [(res = CDRF_DODEFAULT)
		and: [(self hasColumnImages and: [drawStage allMask: CDDS_SUBITEM])
				or: [self hasColumnImages not and: [self hasImageBlock and: [columnIndex = 1]]]]])
					ifTrue: [16r10 "CDRF_NOTIFYPOSTPAINT"]
					ifFalse: [res]!

onDisplayDetailsRequired: lvitem 
	"Private - Get the display info for the receiver's row identified by the <LVITEM>, lvitem."

	"N.B. This is a callback request from the ListView so setting a breakpoint in here may bring
	your image to its knees."

	"Implementation Note: If in report mode then the task of supplying the text/images is
	delegated to the particular column, otherwise the valuables local to the receiver are used.
	This may seem inconsistent, but it allows different text/images to be displayed for the
	primary column if the application requires that the view be dynamically switchable between
	#report mode and the other modes."

	| rowObject mask column columnIdx |
	rowObject := self objectFromHandle: lvitem handle ifAbsent: [].
	"List sometimes asks for lvitem we no longer have, answer nil to accept default processing"
	rowObject isNil ifTrue: [^nil].
	self isReportMode 
		ifTrue: 
			[columnIdx := lvitem iSubItem + 1.
			column := self columnAtIndex: columnIdx].
	mask := lvitem mask.


	"Image Request? Fake it for multiline (icons drawn manually, see nmCustomDraw:)"
	(mask allMask: LVIF_IMAGE) 
		ifTrue: 
			[| imgIdx |
			self isMultiline 
				ifTrue: [imgIdx := -1]
				ifFalse: 
					[imgIdx := ((column notNil and: [self hasColumnImages]) ifTrue: [column] ifFalse: [self]) 
								imageFromRow: rowObject.
					imgIdx notNil ifTrue: [lvitem image: imgIdx - 1]]].

	"Text request?"
	(mask allMask: LVIF_TEXT) 
		ifTrue: 
			["If in report mode the column's get text block is used unless the request
			 is for the primary column and its text block is nil, in which case the view
			 level block is used"
			lvitem 
				textInBuffer: (((column notNil and: [columnIdx > 1 or: [column getTextBlock notNil]]) 
						ifTrue: [column]
						ifFalse: [self]) textFromRow: rowObject)].
	(mask allMask: LVIF_INDENT) 
		ifTrue: 
			["Indenting is only supported for the whole row, not on a per-column basis"
			lvitem indent: (self indentFromRow: rowObject)].
	^0	"suppress default processing"!

onHeaderBeginTrack

	self hideActiveEditor!

onKillFocus

	self clearFocusRect.

	^super onKillFocus!

onLeftButtonDoubleClicked: aMouseEvent

	self onLeftButtonPressed: aMouseEvent.
	^super onLeftButtonDoubleClicked: aMouseEvent!

onLeftButtonPressed: aMouseEvent

	| point itemStruct itemCoords selectOnly |

	self requestDeactivateEditor ifFalse: [^0].

	point := aMouseEvent x@aMouseEvent y.
	itemStruct := self fullItemFromPoint: point.
	itemStruct iItem negative 
	ifTrue: 
		["Check for pseudo full row select"
		(self hasFullRowSelect or: [point x > self allColumns first width]) ifTrue: 
			[self clearFocusItem.
			^super onLeftButtonPressed: aMouseEvent].
		itemStruct := self fullItemFromPoint: (point + (self allColumns first width@0)).
		(itemStruct isNil or: [itemStruct iItem negative]) ifTrue: [^self].
		itemCoords := (itemStruct iItem @ itemStruct iSubItem) + (1@0)]
	ifFalse:
		[itemCoords := (itemStruct iItem @ itemStruct iSubItem) + (1@1)].

	self hasActiveEditor ifTrue: [self deactivateEditor].
	self presenter trigger: #leftButtonPressed: with: aMouseEvent.

	"If clicked the icon, then select only"
	selectOnly := (self lvmGetSubItemRect: (itemCoords - (1@1)) bounding: LVIR_ICON) containsPoint: point.

	^(self click: aMouseEvent activateEditor: selectOnly not at: itemCoords) 
		ifNil: [0]
		ifNotNil: [ :editor | SessionManager inputState queueDeferredAction: [aMouseEvent resendTo: editor]. 0]	!

onSelChanged: anObject

	self validateMultSelectStack.
	self primaryColumn onSelectionChanged.
	^super onSelChanged: anObject!

onSetFocus

	self hasSelection ifFalse: 
		[self 
			clearFocusItem;
			drawFocusRect].

	^super onSetFocus!

onValueChangedIn: aColumn

	aColumn updateValueIn: self selection.

	"attempt to ensure that changes are triggered off the list model"
	self model updateAtIndex: (self selectionByIndex)!

onViewOpened

	super onViewOpened.

	self headerControl when: #beginTrack send: #onHeaderBeginTrack to: self.

	self allColumns do: 
		[ :each | 
		each 
			createEditorIn: self;
			customDrawBlock: (MessageSend receiver: self selector: #drawSubItem: arguments: (Array with: nil))]!

postDraw: aContext columnIndex: anInteger

	((anInteger = 1 and: [self hasImageBlock]) or: [self hasColumnImages]) 
		ifTrue: [(self columnAtIndex: anInteger) customDrawIcon: aContext]
		ifFalse: [aContext canvas free]!

preTranslateKeyboardInput: aMSG

	| key |

	key := aMSG wParam.

	aMSG message = WM_KEYDOWN ifTrue:
		[(key = VK_RIGHT and: [self itemCount > 0]) ifTrue:
			[self hasSelection 
				ifTrue: [self activeColumn isNil ifTrue: 
						[self activateFirstEditor notNil ifTrue: [^true]]]
				ifFalse: [self selectionByIndex: 1. ^true]].

		self hasEditableColumn ifTrue:
			[| action |
			key = VK_TAB ifTrue:
				[Keyboard default isShiftDown
					ifTrue: [action := MessageSend receiver: self selector: #activateLastEditor]
					ifFalse: [action := MessageSend receiver: self selector: #activateFirstEditor]].
			action notNil ifTrue:
				[SessionManager inputState queueDeferredAction: action.
				^true]]].

	^super preTranslateKeyboardInput: aMSG!

prevEditableColumn

	((self activeColumnIndex) - 1) to: 1 by: -1 do:
		[ :index || column |
		column := self allColumns at: index.
		column isEditable ifTrue: [^column]].

	^nil!

prevEditableCoords

	^self prevEditableCoordsInOrBefore: (self selectionByIndex@(self activeColumnIndex - 1))!

prevEditableCoordsInOrBefore: coords

	| row item | 

	row := coords x.
	item := self list at: row.
	coords y to: 1 by: -1 do:
		[ :col |
		((self columnAtIndex: col) isEditableWith: item) ifTrue: [^row@col]].

	^self lastEditableCoordsOnOrBefore: row - 1!

requestDeactivateEditor

	^(self hasActiveEditor not or: [self activeEditor onRequestDeactivate])
		ifTrue: [true]
		ifFalse: [self activeEditor show; tabFocus. false]!

rowBackcolor
	^rowBackcolor!

rowBackcolor: anObject
	rowBackcolor := anObject!

rowForecolor
	^rowForecolor!

rowForecolor: anObject
	rowForecolor := anObject!

rowHeight

	^rowHeight!

rowHeight: anObject
	rowHeight := anObject!

rowPixelHeight

	^(self rowHeight ifNil: [1]) = 1
		ifTrue: [super rowPixelHeight]
		ifFalse: [((self actualFont pixelSize + 2) * self rowHeight) truncated + 1]!

selectionByIndex

	^self multSelectStack isEmpty
		ifTrue: [super selectionByIndex]
		ifFalse: [self multSelectStack last]!

selectionByIndex: anInteger

	self multSelectStack: (OrderedCollection with: anInteger).
	^super selectionByIndex: anInteger!

selectionsByIndex: aCollectionOfIntegers

	"cancel any outstanding edit first"
	self
		commitOngoingEdit;
		basicSelectionsByIndex: aCollectionOfIntegers;
		multSelectStack: aCollectionOfIntegers asOrderedCollection;
		onSelChanged: nil!

setColumnIcon: anImage atIndex: anInteger

	"Only use to clear image - proper header sort images are set in sortOnColumn:"

	anImage isNil ifTrue: [self headerControl clearSortIconForColumnAtIndex: anInteger]!

smallImageExtent
	"Private - Answer the value of the receiver's 'smallImageExtent' instance variable."

	^smallImageExtent ifNil: [Icon smallExtent]!

smallImageExtent: anObject
	"Private - Set the value of the receiver's 'smallImageExtent' instance variable to the argument."

	smallImageExtent := anObject!

sortAscending: aBoolean onColumnIndex: anInteger

	| column |

	column := self columnAtIndex: anInteger.
	column isSortOrderInverted: aBoolean not.

	self 
		forgetLastClickedColumn;
		sortOnColumn: column;
		setColumnIcon: (column headerIcon: true) atIndex: anInteger.

	lastClickedColIndex := anInteger!

sortColumnIndex

	^lastClickedColIndex!

sortOnColumn: aListViewColumn

	self headerControl setSortIconForColumn: aListViewColumn.

	^super sortOnColumn: aListViewColumn!

stbSaveOn: anSTBOutFiler

	"If the receiver is unrealized (i.e. just exists in the spec of an object in the view composer),
	save as a plain object"

	handle isNil 
		ifTrue: [anSTBOutFiler saveObject: self]
		ifFalse: [super stbSaveOn: anSTBOutFiler]!

tabPrev

	| parent prev |
	parent := self parentView.
	[parent notNil and: [parent ~~ self class desktop and: [prev isNil]]] whileTrue: [
		prev := parent tabNextSiblingFrom: self forward: false.
		parent := parent parentView].
	prev isNil ifFalse: [prev tabFocus]!

validateMultSelectStack

	| shouldBe |

	shouldBe := self selectionsByIndex.

	((shouldBe size = 1 and: [self multSelectStack size = 1 and: [shouldBe first = self multSelectStack first]]) or: [shouldBe = self multSelectStack asSortedCollection asArray]) ifFalse:
		[self multSelectStack: shouldBe asOrderedCollection]!

wmHScroll: message wParam: wParam lParam: lParam 

	self hasActiveEditor ifTrue: [self deactivateEditor; enableRedraw].
	^super wmHScroll: message wParam: wParam lParam: lParam !

wmMouseWheel: message wParam: wParam lParam: lParam 

	self hasActiveEditor ifTrue: [self deactivateEditor; enableRedraw].
	^super wmMouseWheel: message wParam: wParam lParam: lParam !

wmPaint: message wParam: wParam lParam: lParam

	self clearFocusRect.

	^super wmPaint: message wParam: wParam lParam: lParam!

wmVScroll: message wParam: wParam lParam: lParam 

	self hasActiveEditor ifTrue: [self deactivateEditor; enableRedraw].
	^super wmVScroll: message wParam: wParam lParam: lParam ! !
!EditableListView categoriesFor: #activateEditorAt:tabbed:!helpers!public! !
!EditableListView categoriesFor: #activateEditorForColumn:!operations!public! !
!EditableListView categoriesFor: #activateEditorForColumnIndex:!operations!public! !
!EditableListView categoriesFor: #activateFirstEditor!operations!public! !
!EditableListView categoriesFor: #activateLastEditor!operations!public! !
!EditableListView categoriesFor: #activeColumn!accessing!public! !
!EditableListView categoriesFor: #activeColumnIndex!accessing!public! !
!EditableListView categoriesFor: #activeEditor!accessing!public! !
!EditableListView categoriesFor: #activeEditorCoords!accessing!private! !
!EditableListView categoriesFor: #activeEditorCoords:!accessing!private! !
!EditableListView categoriesFor: #activeEditorNeedsHighlight!public!testing! !
!EditableListView categoriesFor: #additionalHorzOffsetFor:!accessing!public! !
!EditableListView categoriesFor: #applyImageLists!image management!private! !
!EditableListView categoriesFor: #clearFocusItem!helpers!public! !
!EditableListView categoriesFor: #clearFocusRect!helpers!public! !
!EditableListView categoriesFor: #click:activateEditor:at:!helpers!private! !
!EditableListView categoriesFor: #clientFocusRectangle!helpers!public! !
!EditableListView categoriesFor: #columnNamed:!accessing!public! !
!EditableListView categoriesFor: #columnWithEditor:!accessing!public! !
!EditableListView categoriesFor: #commitOngoingEdit!operations!public! !
!EditableListView categoriesFor: #deactivateEditor!helpers!private! !
!EditableListView categoriesFor: #drawFocusRect!helpers!public! !
!EditableListView categoriesFor: #drawHorizontalGridlinesOn:from:to:by:!helpers!private! !
!EditableListView categoriesFor: #drawSubItem:!helpers!public! !
!EditableListView categoriesFor: #fieldExitDown!operations!public! !
!EditableListView categoriesFor: #fieldExitNextTabbed:!operations!public! !
!EditableListView categoriesFor: #fieldExitPrevTabbed:!operations!public! !
!EditableListView categoriesFor: #fieldExitUp!operations!public! !
!EditableListView categoriesFor: #firstEditableColumn!accessing!public! !
!EditableListView categoriesFor: #firstEditableCoordsOnOrAfter:!helpers!private! !
!EditableListView categoriesFor: #handleCtrlMultSelectOn:!helpers!private! !
!EditableListView categoriesFor: #handleShiftMultSelectOn:!helpers!private! !
!EditableListView categoriesFor: #handleSingleSelectOn:!helpers!private! !
!EditableListView categoriesFor: #hasActiveEditor!public!testing! !
!EditableListView categoriesFor: #hasColumnImage:!public!testing! !
!EditableListView categoriesFor: #hasEditableColumn!public!testing! !
!EditableListView categoriesFor: #hasEditableColumnInCurrentRow!public!testing! !
!EditableListView categoriesFor: #hasImageBlock!public!testing! !
!EditableListView categoriesFor: #hasVirtualGridlines!accessing!private! !
!EditableListView categoriesFor: #hasVirtualGridlines:!accessing!private! !
!EditableListView categoriesFor: #hideActiveEditor!helpers!public! !
!EditableListView categoriesFor: #isMultiline!public!testing! !
!EditableListView categoriesFor: #isSelected:!public!testing! !
!EditableListView categoriesFor: #itemIndex:hasFocus:!helpers!public! !
!EditableListView categoriesFor: #itemIndexWithFocus!accessing!public! !
!EditableListView categoriesFor: #itemIndexWithFocus:!accessing!public! !
!EditableListView categoriesFor: #lastEditableColumn!accessing!public! !
!EditableListView categoriesFor: #lastEditableCoordsOnOrBefore:!helpers!private! !
!EditableListView categoriesFor: #lvmSetColumn:at:!columns!private! !
!EditableListView categoriesFor: #multSelectStack!accessing!private! !
!EditableListView categoriesFor: #multSelectStack:!accessing!private! !
!EditableListView categoriesFor: #nextEditableColumn!accessing!public! !
!EditableListView categoriesFor: #nextEditableCoords!helpers!private! !
!EditableListView categoriesFor: #nextEditableCoordsInOrAfter:!helpers!private! !
!EditableListView categoriesFor: #nmCustomDraw:!event handling!public! !
!EditableListView categoriesFor: #onDisplayDetailsRequired:!event handling!private! !
!EditableListView categoriesFor: #onHeaderBeginTrack!event handling!public! !
!EditableListView categoriesFor: #onKillFocus!event handling!public! !
!EditableListView categoriesFor: #onLeftButtonDoubleClicked:!event handling!public! !
!EditableListView categoriesFor: #onLeftButtonPressed:!event handling!public! !
!EditableListView categoriesFor: #onSelChanged:!event handling!public! !
!EditableListView categoriesFor: #onSetFocus!event handling!public! !
!EditableListView categoriesFor: #onValueChangedIn:!event handling!public! !
!EditableListView categoriesFor: #onViewOpened!event handling!public! !
!EditableListView categoriesFor: #postDraw:columnIndex:!helpers!private! !
!EditableListView categoriesFor: #preTranslateKeyboardInput:!event handling!public! !
!EditableListView categoriesFor: #prevEditableColumn!accessing!public! !
!EditableListView categoriesFor: #prevEditableCoords!helpers!private! !
!EditableListView categoriesFor: #prevEditableCoordsInOrBefore:!helpers!private! !
!EditableListView categoriesFor: #requestDeactivateEditor!helpers!private! !
!EditableListView categoriesFor: #rowBackcolor!accessing!private! !
!EditableListView categoriesFor: #rowBackcolor:!accessing!private! !
!EditableListView categoriesFor: #rowForecolor!accessing!private! !
!EditableListView categoriesFor: #rowForecolor:!accessing!private! !
!EditableListView categoriesFor: #rowHeight!accessing!private! !
!EditableListView categoriesFor: #rowHeight:!accessing!private! !
!EditableListView categoriesFor: #rowPixelHeight!accessing!public! !
!EditableListView categoriesFor: #selectionByIndex!public!selection! !
!EditableListView categoriesFor: #selectionByIndex:!public!selection! !
!EditableListView categoriesFor: #selectionsByIndex:!public!selection! !
!EditableListView categoriesFor: #setColumnIcon:atIndex:!helpers!public! !
!EditableListView categoriesFor: #smallImageExtent!accessing!public! !
!EditableListView categoriesFor: #smallImageExtent:!accessing!public! !
!EditableListView categoriesFor: #sortAscending:onColumnIndex:!columns!private! !
!EditableListView categoriesFor: #sortColumnIndex!columns!private! !
!EditableListView categoriesFor: #sortOnColumn:!helpers!public! !
!EditableListView categoriesFor: #stbSaveOn:!binary filing!public! !
!EditableListView categoriesFor: #tabPrev!operations!public! !
!EditableListView categoriesFor: #validateMultSelectStack!helpers!public! !
!EditableListView categoriesFor: #wmHScroll:wParam:lParam:!event handling!public! !
!EditableListView categoriesFor: #wmMouseWheel:wParam:lParam:!event handling!public! !
!EditableListView categoriesFor: #wmPaint:wParam:lParam:!event handling!public! !
!EditableListView categoriesFor: #wmVScroll:wParam:lParam:!event handling!public! !

!EditableListView class methodsFor!

columnClass
	"Answer the class of object used to represent the columns of the receiver."

	^EditableListViewColumn!

stbConvert: instVarArray fromVersion: verInteger

	| origInstVars instVars newArray offset |

	origInstVars := super stbConvert: instVarArray fromVersion: verInteger.
	instVars := origInstVars.
	offset := 0.

	verInteger < 12 ifTrue: 
		[| new |
		"Added rowHeight"
		new := Array new: instVars size + 1.
		new replaceFrom: 1 to: (##(self) instSize - 3) with: instVars startingAt: 1.
		new at: (##(self) instSize - 2) put: 1.
		instVars := new.
		offset := offset + 1].

	verInteger < 14 ifTrue:
		[| new |
		"Added _spares"
		new := Array new: instVars size + 2.
		new replaceFrom: 1 to: (##(self) instSize - 2) with: instVars startingAt: 1.
		new 
			at: ##(self) instSize put: nil;
			at: (##(self) instSize - 1) put: nil.

		offset := offset + 2.

		"Copy inst vars of any subclass"
		self instSize > ##(self) instSize ifTrue:
			[new 
				replaceFrom: (##(self) instSize + 1) 
				to: new size 
				with: origInstVars 
				startingAt: (##(self) instSize - offset) + 1].

		instVars := new].

	^instVars!

stbVersion

	"12 - added rowHeight
	14 - added _spares"

	^14! !
!EditableListView class categoriesFor: #columnClass!constants!private! !
!EditableListView class categoriesFor: #stbConvert:fromVersion:!constants!public! !
!EditableListView class categoriesFor: #stbVersion!constants!private! !

EmbeddedTextEdit guid: (GUID fromString: '{B24A4423-0072-4380-BD67-1E317410EBF8}')!
EmbeddedTextEdit comment: ''!
!EmbeddedTextEdit categoriesForClass!Unclassified! !
!EmbeddedTextEdit methodsFor!

actualOffset

	^self offset ifNil: [self defaultOffset]!

cellRect

	"Private - Return the cell rectangle equivalent to the receiver's edit rectangle"

	^(self rectangle topLeft - self actualOffset) extent: (self extent + (self actualOffset + (0@1)))!

column

	^self parentView allColumns detect: [ :each | each editor == self]!

createEmbeddedIn: aView

	interactor := presenter := self.
	flags := 0.

	self 
		isManaged: true;
		initializeModel;
		parentView: aView;
		create;
		font: self view font;
		yourself!

defaultOffset

	"Works for several fonts (set an explicit offset if not)"

	| x y |

	(self parentView primaryColumn editor == self)
		ifTrue: [x := -1]
		ifFalse: [x := 3].

	y := (10 - (self actualFont pointSize)) max: 0.
	self parentView isMultiline ifTrue:
		[y := (((self actualFont pixelSize + 2) * (self parentView rowHeight - 1)) + 1) //2].

	^x@y!

defaultWindowExStyle
	"Private - Answer the default extended window creation style.
	Use the client edge style for the standard Win95 look."

	^super defaultWindowExStyle bitXor: WS_EX_CLIENTEDGE!

defaultWindowStyle
	"Private - Answer a default style to use when creating an EmbeddedTextEdit."

	^(super defaultWindowStyle) | ES_AUTOHSCROLL!

display: aString in: aRectangle on: aCanvas forecolor: fColor backcolor: bColor 

	| text |

	text := aString ifNil: [''].
	offset := self actualOffset.
	
	aCanvas 
		forecolor: fColor;
		formatText: text in: ((aRectangle origin + offset) extent: (aRectangle extent - offset))!

displayOnFormats

	^#(#class)!

editRectFor: aRectangle

	"Private - Return the rectangle to use for the editor within the grid cell aRectangle"

	^(aRectangle topLeft + self actualOffset) extent: (aRectangle extent - (self actualOffset + (0@1)))!

isDisplayOwnerDrawn

	^false!

offset

	^offset
!

offset: aPoint

	offset := aPoint

!

onCursoredLeft

	"The receiver has been entered by a cursor left action"

	self caretPosition: (self plainText size + 1)!

onKillFocus

	self parentView hideActiveEditor.

	^super onKillFocus!

onRequestDeactivate

	"Return if OK"

	^true!

preTranslateKeyboardInput: aMSG

	aMSG message = WM_KEYDOWN ifTrue:
		[| key action |
		key := aMSG wParam.
		key = VK_TAB ifTrue:
			[Keyboard default isShiftDown
				ifTrue: [action := MessageSend receiver: self parentView selector: #fieldExitPrevTabbed: arguments: #(true)]
				ifFalse: [action := MessageSend receiver: self parentView selector: #fieldExitNextTabbed: arguments: #(true)]].
		(key = VK_RIGHT and: [self caretPosition > self plainText size]) ifTrue: [action := MessageSend receiver: self parentView selector: #fieldExitNextTabbed: arguments: #(false)].

		key = VK_LEFT ifTrue:
			[(self selectionRange isEmpty and: [self caretPosition = 1]) ifTrue: 
				[action := MessageSend receiver: self parentView selector: #fieldExitPrevTabbed: arguments: #(false)].
			(Keyboard default isShiftDown not and: [self basicSelectionRange = (0 to: self textLength)]) ifTrue: 
				[action := MessageSend receiver: self selector: #caretPosition: arguments: #(1)]].

		key = VK_UP ifTrue: [action := MessageSend receiver: self parentView selector: #fieldExitUp].
		key = VK_DOWN ifTrue: [action := MessageSend receiver: self parentView selector: #fieldExitDown].
		key = VK_ESCAPE ifTrue: [action := [self undo; selectAll]].
		action notNil ifTrue:
			[SessionManager inputState queueDeferredAction: action.
			^true]].

	^super preTranslateKeyboardInput: aMSG!

show

	self caretPosition: 1.
	^super show!

showIn: aRectangle

	self 
		rectangle: (self editRectFor: aRectangle);
		show!

stbSaveOn: anSTBOutFiler

	"If the receiver is unrealized (i.e. just exists in the spec of a column in the view composer),
	save as a plain object"

	handle isNil 
		ifTrue: [anSTBOutFiler saveObject: self]
		ifFalse: [super stbSaveOn: anSTBOutFiler]! !
!EmbeddedTextEdit categoriesFor: #actualOffset!accessing!public! !
!EmbeddedTextEdit categoriesFor: #cellRect!displaying!private! !
!EmbeddedTextEdit categoriesFor: #column!accessing!public! !
!EmbeddedTextEdit categoriesFor: #createEmbeddedIn:!initializing!public! !
!EmbeddedTextEdit categoriesFor: #defaultOffset!accessing!public! !
!EmbeddedTextEdit categoriesFor: #defaultWindowExStyle!constants!private! !
!EmbeddedTextEdit categoriesFor: #defaultWindowStyle!constants!private! !
!EmbeddedTextEdit categoriesFor: #display:in:on:forecolor:backcolor:!displaying!public! !
!EmbeddedTextEdit categoriesFor: #displayOnFormats!binary filing!public! !
!EmbeddedTextEdit categoriesFor: #editRectFor:!displaying!private! !
!EmbeddedTextEdit categoriesFor: #isDisplayOwnerDrawn!public!testing! !
!EmbeddedTextEdit categoriesFor: #offset!accessing!public! !
!EmbeddedTextEdit categoriesFor: #offset:!accessing!public! !
!EmbeddedTextEdit categoriesFor: #onCursoredLeft!event handling!public! !
!EmbeddedTextEdit categoriesFor: #onKillFocus!event handling!public! !
!EmbeddedTextEdit categoriesFor: #onRequestDeactivate!event handling!public! !
!EmbeddedTextEdit categoriesFor: #preTranslateKeyboardInput:!event handling!public! !
!EmbeddedTextEdit categoriesFor: #show!displaying!public! !
!EmbeddedTextEdit categoriesFor: #showIn:!displaying!public! !
!EmbeddedTextEdit categoriesFor: #stbSaveOn:!binary filing!public! !

EmbeddedFormattedTextEdit guid: (GUID fromString: '{4EF6ADF7-0642-4BAC-970D-10E4A69EC7F0}')!
EmbeddedFormattedTextEdit comment: ''!
!EmbeddedFormattedTextEdit categoriesForClass!Unclassified! !
!EmbeddedFormattedTextEdit methodsFor!

actualOffset

	^self offset ifNil: [self defaultOffset]!

cellRect

	"Private - Return the cell rectangle equivalent to the receiver's edit rectangle"

	^(self rectangle topLeft - self actualOffset) extent: (self extent + (self actualOffset + (0@1)))!

column

	^self parentView allColumns detect: [ :each | each editor == self]!

createEmbeddedIn: aView

	interactor := presenter := self.
	flags := 0.

	self 
		isManaged: true;
		initializeModel;
		parentView: aView;
		create;
		font: aView font;
		yourself!

defaultOffset

	"Works for several fonts (set an explicit offset if not)"
	
	| x y |

	(self parentView primaryColumn editor == self)
		ifTrue: [x := -1]
		ifFalse: [x := 3].

	y := (10 - (self actualFont pointSize)) max: 0.
	self parentView isMultiline ifTrue:
		[y := (((self actualFont pixelSize + 2) * (self parentView rowHeight - 1)) + 1) //2].

	^x@y!

defaultWindowExStyle
	"Private - Answer the default extended window creation style.
	Use the client edge style for the standard Win95 look."

	^super defaultWindowExStyle bitXor: WS_EX_CLIENTEDGE!

defaultWindowStyle
	"Private - Answer a default style to use when creating an EmbeddedTextEdit."

	^(super defaultWindowStyle) | ES_AUTOHSCROLL!

display: anObject in: aRectangle on: aCanvas forecolor: fColor backcolor: bColor 

	| text actualOffset |

	text := self typeconverter convertFromLeftToRight: anObject.
	actualOffset := self actualOffset + (2@0).
	
	aCanvas 
		forecolor: fColor;
		formatText: text in: ((aRectangle origin + actualOffset) extent: (aRectangle extent - actualOffset))!

displayOnFormats

	^#(#class)!

editRectFor: aRectangle

	"Private - Return the rectangle to use for the editor within the grid cell aRectangle"

	^(aRectangle topLeft + self actualOffset) extent: (aRectangle extent - (self actualOffset + (0@1)))!

isDisplayOwnerDrawn

	^false!

offset

	^offset
!

offset: aPoint

	offset := aPoint

!

onCursoredLeft

	"The receiver has been entered by a cursor left action"

	self caretPosition: (self plainText size + 1)!

onKillFocus

	self parentView hideActiveEditor.

	^super onKillFocus!

onRequestDeactivate

	"Return if OK"

	^self isTextValid
		ifTrue: [true]
		ifFalse: [self errorTextInvalid. false]!

preTranslateKeyboardInput: aMSG

	aMSG message = WM_KEYDOWN ifTrue:
		[| key action |
		key := aMSG wParam.
		key = VK_TAB ifTrue:
			[Keyboard default isShiftDown
				ifTrue: [action := MessageSend receiver: self parentView selector: #fieldExitPrevTabbed: arguments: #(true)]
				ifFalse: [action := MessageSend receiver: self parentView selector: #fieldExitNextTabbed: arguments: #(true)]].
		(key = VK_RIGHT and: [self caretPosition > self plainText size]) ifTrue: [action := MessageSend receiver: self parentView selector: #fieldExitNextTabbed: arguments: #(false)].

		key = VK_LEFT ifTrue:
			[(self selectionRange isEmpty and: [self caretPosition = 1]) ifTrue: 
				[action := MessageSend receiver: self parentView selector: #fieldExitPrevTabbed: arguments: #(false)].
			(Keyboard default isShiftDown not and: [self basicSelectionRange = (0 to: self textLength)]) ifTrue: 
				[action := MessageSend receiver: self selector: #caretPosition: arguments: #(1)]].

		key = VK_UP ifTrue: [action := MessageSend receiver: self parentView selector: #fieldExitUp].
		key = VK_DOWN ifTrue: [action := MessageSend receiver: self parentView selector: #fieldExitDown].
		key = VK_ESCAPE ifTrue: [action := [self undo; selectAll]].
		action notNil ifTrue:
			[SessionManager inputState queueDeferredAction: action.
			^true]].

	^super preTranslateKeyboardInput: aMSG!

show

	self caretPosition: 1.
	^super show!

showIn: aRectangle

	self 
		rectangle: (self editRectFor: aRectangle);
		show!

stbSaveOn: anSTBOutFiler

	"If the receiver is unrealized (i.e. just exists in the spec of a column in the view composer),
	save as a plain object"

	handle isNil 
		ifTrue: [anSTBOutFiler saveObject: self]
		ifFalse: [super stbSaveOn: anSTBOutFiler]! !
!EmbeddedFormattedTextEdit categoriesFor: #actualOffset!accessing!public! !
!EmbeddedFormattedTextEdit categoriesFor: #cellRect!displaying!private! !
!EmbeddedFormattedTextEdit categoriesFor: #column!accessing!public! !
!EmbeddedFormattedTextEdit categoriesFor: #createEmbeddedIn:!initialize/release!public! !
!EmbeddedFormattedTextEdit categoriesFor: #defaultOffset!accessing!public! !
!EmbeddedFormattedTextEdit categoriesFor: #defaultWindowExStyle!constants!private! !
!EmbeddedFormattedTextEdit categoriesFor: #defaultWindowStyle!constants!private! !
!EmbeddedFormattedTextEdit categoriesFor: #display:in:on:forecolor:backcolor:!displaying!public! !
!EmbeddedFormattedTextEdit categoriesFor: #displayOnFormats!binary filing!public! !
!EmbeddedFormattedTextEdit categoriesFor: #editRectFor:!displaying!private! !
!EmbeddedFormattedTextEdit categoriesFor: #isDisplayOwnerDrawn!public!testing! !
!EmbeddedFormattedTextEdit categoriesFor: #offset!accessing!public! !
!EmbeddedFormattedTextEdit categoriesFor: #offset:!accessing!public! !
!EmbeddedFormattedTextEdit categoriesFor: #onCursoredLeft!event handling!public! !
!EmbeddedFormattedTextEdit categoriesFor: #onKillFocus!event handling!public! !
!EmbeddedFormattedTextEdit categoriesFor: #onRequestDeactivate!event handling!public! !
!EmbeddedFormattedTextEdit categoriesFor: #preTranslateKeyboardInput:!event handling!public! !
!EmbeddedFormattedTextEdit categoriesFor: #show!displaying!public! !
!EmbeddedFormattedTextEdit categoriesFor: #showIn:!displaying!public! !
!EmbeddedFormattedTextEdit categoriesFor: #stbSaveOn:!binary filing!public! !

!EmbeddedFormattedTextEdit class methodsFor!

newForDate

	^super new 
		initialize;
		separatorChars: '/';
		placeholderChar: $_;
		format: '__/__/____';
		typeconverter: DateToText new;
		yourself! !
!EmbeddedFormattedTextEdit class categoriesFor: #newForDate!constants!development!public! !

EmbeddedMultilineTextEdit guid: (GUID fromString: '{F464E637-7C1A-4A4A-A066-653A45DA17FD}')!
EmbeddedMultilineTextEdit comment: ''!
!EmbeddedMultilineTextEdit categoriesForClass!Unclassified! !
!EmbeddedMultilineTextEdit methodsFor!

column
	^column!

column: anObject
	column := anObject!

createEmbeddedIn: aView

	self
		initialize;
		parentView: aView;
		column: (aView allColumns detect: [ :each | each editor == self]);
		create;
		font: self view font;
		yourself!

defaultWindowExStyle
	"Private - Answer the default extended window creation style.
	Use the client edge style for the standard Win95 look."

	^super defaultWindowExStyle bitXor: WS_EX_CLIENTEDGE!

defaultWindowStyle
	"Private - Answer a default style to use when creating an EmbeddedMultilineTextEdit."

	^super defaultWindowStyle | WS_VSCROLL | ES_AUTOVSCROLL
!

display: aString in: aRectangle on: aCanvas forecolor: fColor backcolor: bColor 

	| text textHeight offset |

	text := aString ifNil: [''].
	textHeight := (aCanvas textExtent: 'X') y * ((text occurrencesOfSubCollection: String lineDelimiter) + 1).
	textHeight < aRectangle height 
		ifTrue: [offset := (aRectangle height - textHeight) //2]
		ifFalse: [offset := 0].

	aCanvas 
		forecolor: fColor;
		formatText: text in: ((aRectangle origin + (self horzOffset@offset)) extent: (aRectangle extent - (self horzOffset@offset)))!

displayOnFormats

	^#(#class)!

hasColumnImage

	| isPrimary |

	isPrimary := self parentView primaryColumn editor == self.

	^(isPrimary not and: [self parentView hasColumnImages and: [self column hasImageBlock]])!

horzOffset

	^3!

isDisplayOwnerDrawn

	^true!

onCursoredLeft

	"The receiver has been entered by a cursor left action"

	self caretPosition: (self plainText size + 1)!

onKillFocus

	self parentView hideActiveEditor.

	^super onKillFocus!

onRequestDeactivate

	"Return if OK"

	^true!

preTranslateKeyboardInput: aMSG

	aMSG message = WM_KEYDOWN ifTrue:
		[| key action |
		key := aMSG wParam.
		key = VK_TAB ifTrue:
			[Keyboard default isShiftDown
				ifTrue: [action := MessageSend receiver: self parentView selector: #fieldExitPrevTabbed: arguments: #(true)]
				ifFalse: [action := MessageSend receiver: self parentView selector: #fieldExitNextTabbed: arguments: #(true)]].
		(key = VK_RIGHT and: [self caretPosition > self plainText size]) ifTrue: [action := MessageSend receiver: self parentView selector: #fieldExitNextTabbed: arguments: #(false)].

		key = VK_LEFT ifTrue:
			[(self selectionRange isEmpty and: [self caretPosition = 1]) ifTrue: 
				[action := MessageSend receiver: self parentView selector: #fieldExitPrevTabbed: arguments: #(false)].
			(Keyboard default isShiftDown not and: [self basicSelectionRange = (0 to: self textLength)]) ifTrue: 
				[action := MessageSend receiver: self selector: #caretPosition: arguments: #(1)]].

		(key = VK_UP and: [self currentLine = 1]) ifTrue: [action := MessageSend receiver: self parentView selector: #fieldExitUp].
		(key = VK_DOWN and: [self currentLine = self lineCount]) ifTrue: [action := MessageSend receiver: self parentView selector: #fieldExitDown].
		key = VK_ESCAPE ifTrue: [action := [self undo; selectAll]].
		action notNil ifTrue:
			[SessionManager inputState queueDeferredAction: action.
			^true]].

	^super preTranslateKeyboardInput: aMSG!

show

	self caretPosition: 1.
	^super show!

showIn: aRectangle

	self
		position: (aRectangle topLeft+(self horzOffset@0));
		height: (aRectangle height - 1);
		width: (aRectangle width - self horzOffset);
		show!

stbSaveOn: anSTBOutFiler

	"If the receiver is unrealized (i.e. just exists in the spec of a column in the view composer),
	save as a plain object"

	handle isNil 
		ifTrue: [anSTBOutFiler saveObject: self]
		ifFalse: [super stbSaveOn: anSTBOutFiler]! !
!EmbeddedMultilineTextEdit categoriesFor: #column!accessing!private! !
!EmbeddedMultilineTextEdit categoriesFor: #column:!accessing!private! !
!EmbeddedMultilineTextEdit categoriesFor: #createEmbeddedIn:!initialize/release!public! !
!EmbeddedMultilineTextEdit categoriesFor: #defaultWindowExStyle!constants!private! !
!EmbeddedMultilineTextEdit categoriesFor: #defaultWindowStyle!constants!private! !
!EmbeddedMultilineTextEdit categoriesFor: #display:in:on:forecolor:backcolor:!displaying!public! !
!EmbeddedMultilineTextEdit categoriesFor: #displayOnFormats!binary filing!public! !
!EmbeddedMultilineTextEdit categoriesFor: #hasColumnImage!public!testing! !
!EmbeddedMultilineTextEdit categoriesFor: #horzOffset!constants!public! !
!EmbeddedMultilineTextEdit categoriesFor: #isDisplayOwnerDrawn!public!testing! !
!EmbeddedMultilineTextEdit categoriesFor: #onCursoredLeft!event handling!public! !
!EmbeddedMultilineTextEdit categoriesFor: #onKillFocus!event handling!public! !
!EmbeddedMultilineTextEdit categoriesFor: #onRequestDeactivate!event handling!public! !
!EmbeddedMultilineTextEdit categoriesFor: #preTranslateKeyboardInput:!event handling!public! !
!EmbeddedMultilineTextEdit categoriesFor: #show!displaying!public! !
!EmbeddedMultilineTextEdit categoriesFor: #showIn:!displaying!public! !
!EmbeddedMultilineTextEdit categoriesFor: #stbSaveOn:!binary filing!public! !

EmbeddedCheckBox guid: (GUID fromString: '{99DEC189-99A2-4781-9681-FC04AE58678D}')!
EmbeddedCheckBox comment: ''!
!EmbeddedCheckBox categoriesForClass!Unclassified! !
!EmbeddedCheckBox methodsFor!

createEmbeddedIn: aView

	interactor := presenter := self.
	flags := 0.

	self 
		isManaged: true;
		initializeModel;
		parentView: aView;
		create;
		font: aView font.

	(aView hasFullRowSelect and: [aView showsSelAlways]) ifTrue: [self backcolor: Color face3d]!

display: aValue in: aRectangle on: aCanvas forecolor: fcolor backcolor: bcolor

	self 
		draw: aValue transition: false 
		in: aRectangle on: aCanvas 
		forecolor: fcolor backcolor: bcolor 
		focus: false highlight: false
!

displayOnFormats

	^#(#class)!

isDisplayOwnerDrawn

	^true!

isHighlighted

	"Override to always highlight when hasFocus"

	^self hasFocus or: [super isHighlighted]!

onCursoredLeft

	"The receiver has been entered by a cursor left action"

	^self!

onKillFocus

	self parentView hideActiveEditor.

	^super onKillFocus!

onRequestDeactivate

	"Return if OK"

	^true!

preTranslateKeyboardInput: aMSG

	aMSG message = WM_KEYDOWN ifTrue:
		[| key action |
		key := aMSG wParam.
		key = VK_TAB ifTrue:
			[Keyboard default isShiftDown
				ifTrue: [action := MessageSend receiver: self parentView selector: #fieldExitPrevTabbed: arguments: #(true)]
				ifFalse: [action := MessageSend receiver: self parentView selector: #fieldExitNextTabbed: arguments: #(true)]].
		key = VK_RIGHT ifTrue: [action := MessageSend receiver: self parentView selector: #fieldExitNextTabbed: arguments: #(false)].
		key = VK_LEFT ifTrue: [action := MessageSend receiver: self parentView selector: #fieldExitPrevTabbed: arguments: #(false)].
		key = VK_UP ifTrue: [action := MessageSend receiver: self parentView selector: #fieldExitUp].
		key = VK_DOWN ifTrue: [action := MessageSend receiver: self parentView selector: #fieldExitDown].
		action notNil ifTrue:
			[SessionManager inputState queueDeferredAction: action.
			^true]].

	^super preTranslateKeyboardInput: aMSG!

showIn: aRectangle

	self
		position: (aRectangle topLeft+(1@1));
		height: (aRectangle height - 2);
		width: (aRectangle width - 2);
		show!

stbSaveOn: anSTBOutFiler

	"If the receiver is unrealized (i.e. just exists in the spec of a column in the view composer),
	save as a plain object"

	handle isNil 
		ifTrue: [anSTBOutFiler saveObject: self]
		ifFalse: [super stbSaveOn: anSTBOutFiler]! !
!EmbeddedCheckBox categoriesFor: #createEmbeddedIn:!initialize/release!public! !
!EmbeddedCheckBox categoriesFor: #display:in:on:forecolor:backcolor:!displaying!public! !
!EmbeddedCheckBox categoriesFor: #displayOnFormats!binary filing!public! !
!EmbeddedCheckBox categoriesFor: #isDisplayOwnerDrawn!public!testing! !
!EmbeddedCheckBox categoriesFor: #isHighlighted!displaying!public! !
!EmbeddedCheckBox categoriesFor: #onCursoredLeft!event handling!public! !
!EmbeddedCheckBox categoriesFor: #onKillFocus!event handling!public! !
!EmbeddedCheckBox categoriesFor: #onRequestDeactivate!event handling!public! !
!EmbeddedCheckBox categoriesFor: #preTranslateKeyboardInput:!event handling!public! !
!EmbeddedCheckBox categoriesFor: #showIn:!displaying!public! !
!EmbeddedCheckBox categoriesFor: #stbSaveOn:!binary filing!public! !

"Binary Globals"!


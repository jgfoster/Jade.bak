| package |
package := Package name: 'SSW ListView Extensions'.
package paxVersion: 1;
	basicComment: '©2006 Solutions Software Limited'.

package basicPackageVersion: '0.001'.


package classNames
	add: #HDITEM;
	add: #HeaderView;
	yourself.

package methodNames
	add: #ListView -> #drawGridlines;
	add: #ListView -> #drawGridlinesOn:;
	add: #ListView -> #drawHorizontalGridlinesOn:from:to:by:;
	add: #ListView -> #drawVerticalGridlinesOn:from:to:;
	add: #ListView -> #fullItemFromPoint:;
	add: #ListView -> #hasVirtualGridLines;
	add: #ListView -> #hasVirtualGridLines:;
	add: #ListView -> #headerControl;
	add: #ListView -> #headerRect;
	add: #ListView -> #horzScrollPos;
	add: #ListView -> #isHorzPaging;
	add: #ListView -> #isHorzPaging:;
	add: #ListView -> #lvmGetItemCount;
	add: #ListView -> #lvmGetSubItemRect:bounding:;
	add: #ListView -> #lvmGetTopIndex;
	add: #ListView -> #nmCustomDraw:;
	add: #ListView -> #onHScroll:;
	add: #ListView -> #onViewOpened;
	add: #ListView -> #rowPixelHeight;
	add: #ListView -> #vertScrollPos;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	yourself).

package!

"Class Definitions"!

CCITEM subclass: #HDITEM
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ControlView subclass: #HeaderView
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!ListView methodsFor!

drawGridlines

	| canvas |

	canvas := self canvas.

	self drawGridlinesOn: canvas.

	canvas free!

drawGridlinesOn: aCanvas

	| headerHeight first height brush top bottom |

	headerHeight := self headerRect height.

	"Work out start point and spacing"
	self lvmGetItemCount > 0
	ifTrue:
		[| rect |
		rect := self itemRect: ((self lvmGetTopIndex) + 1).
		first := rect bottom.
		height := rect height]
	ifFalse:
		[height := self rowPixelHeight.
		first := headerHeight + height].

	top := first - 1.
	bottom := self rectangle bottom.

	brush := Color face3d pen.
	aCanvas pen: brush.

	self drawHorizontalGridlinesOn: aCanvas from: top to: bottom by: height.

	self isHorzPaging ifFalse: 
		[top := top - height.
		self drawVerticalGridlinesOn: aCanvas from: top to: bottom].

	brush free!

drawHorizontalGridlinesOn: aCanvas from: top to: bottom by: height

	top to: bottom by: height do:
		[ :i |
		aCanvas lineFrom: (0@i) to: (self rectangle width@i)]
!

drawVerticalGridlinesOn: aCanvas from: top to: bottom

	| left allColumns |

	left := 0 - self horzScrollPos.
	allColumns := self allColumns.

	self columnOrder do:
		[ :index || col |
		col := allColumns at: index.
		left := left + col width.
		aCanvas lineFrom: (left@top) to: (left@bottom)]!

fullItemFromPoint: coord
	"Private - Answer a LVHITTESTINFO populated by the control  for the
	client coordinate represented by the <POINTL>, coord."

	| struct |
	struct := LVHITTESTINFO new.
	struct pt: coord asParameter.
	self sendMessage: (LVM_HITTEST + 39) "LVM_SUBITEMHITTEST" wParam: 0 lpParam: struct.
	^struct!

hasVirtualGridLines

	^self propertyAt: #hasVirtualGridLines ifAbsent: [false]!

hasVirtualGridLines: aBoolean

	self propertyAt: #hasVirtualGridLines put: aBoolean!

headerControl

	^self propertyAt: #headerControl ifAbsent:
		[| hHandle |
		hHandle := self lvmGetHeader.
		self propertyAt: #headerControl put:
			((HeaderView fromHandle: hHandle)
				subclassWindow: hHandle;
				yourself)]
!

headerRect

	| rect |

	rect := RECT new.

	UserLibrary default getWindowRect: self lvmGetHeader lpRect: rect.

	^rect
!

horzScrollPos

	| struct |

	struct := SCROLLINFO new maskIn: SIF_POS; yourself.

	self view getScrollInfo: struct bar: SB_HORZ.

	^struct pos!

isHorzPaging

	^self propertyAt: #isHorzPaging ifAbsent: [false]!

isHorzPaging: aBoolean

	self propertyAt: #isHorzPaging put: aBoolean!

lvmGetItemCount

	^self sendMessage: LVM_GETITEMCOUNT!

lvmGetSubItemRect: pointIndex bounding: boundingValue
	"Private - Answer a bounding Rectangle for all or part of a subitem.
	pointIndex is itemIndex@subitemIndex"

	| rect |
	rect := RECT new.
	rect 
		top: pointIndex y; 
		left: boundingValue.

	(self sendMessage: "LVM_GETSUBITEMRECT"LVM_HITTEST+38 wParam: pointIndex x lpParam: rect) == 0
		ifTrue: [^self errorInCommonControlCall: pointIndex x].

	^rect asRectangle!

lvmGetTopIndex

	^self sendMessage: ##(16r1000 "LVM_FIRST" + 39) "LVM_GETTOPINDEX"!

nmCustomDraw: pNMHDR

	"Override to request or trap a post-paint notification"

	| context drawStage res |

	"Only required for drawing gridlines"
	self hasVirtualGridLines ifFalse: [^super nmCustomDraw: pNMHDR].

	context := self customDrawContextClass fromAddress: pNMHDR.
	drawStage := context dwDrawStage.

	"Overall postpaint notification? Draw the gridlines"
	((drawStage allMask: 2 "CDDS_POSTPAINT") and: [drawStage < 65536 "NOT CDDS_ITEM"]) ifTrue:
		[self drawGridlinesOn: context canvas.
		^CDRF_DODEFAULT].

	res := super nmCustomDraw: pNMHDR.

	"Request postdraw notification for the above"
	^((drawStage allMask: CDDS_PREPAINT) and: [drawStage < 65536 "NOT CDDS_ITEM"]) 
		ifTrue: [res | 16r10 "CDRF_NOTIFYPOSTPAINT"]
		ifFalse: [res]!

onHScroll: aScrollEvent 

	"Annoyingly, vertical virtual grildines scar on horizontal page scrolling... 
	have to suppress vertical gridlines when page scrolling..."
	self hasVirtualGridLines ifTrue:
		[(aScrollEvent pageLeft or: [aScrollEvent pageRight])
		ifTrue: 
			[self isHorzPaging: true]
		ifFalse: 
			[(self isHorzPaging and: [aScrollEvent endScroll]) ifTrue: 
				[self isHorzPaging: false.
				self drawGridlines]]].

	^super onHScroll: aScrollEvent!

onViewOpened

	"Turn off real gridlines and activate virtual gridlines on XP to handle corruption when scrolling"
	(self hasGridLines and: [OSVERSIONINFO current isWinXP]) ifTrue:
		[self 
			hasGridLines: false;
			hasVirtualGridLines: true].

	^super onViewOpened!

rowPixelHeight

	"Estimated"

	^(self smallImageExtent y + 1) max: (self actualFont pixelSize + 6)!

vertScrollPos

	| struct |

	struct := SCROLLINFO new maskIn: SIF_POS; yourself.

	self view getScrollInfo: struct bar: SB_VERT.

	^struct pos! !
!ListView categoriesFor: #drawGridlines!helpers!public! !
!ListView categoriesFor: #drawGridlinesOn:!helpers!public! !
!ListView categoriesFor: #drawHorizontalGridlinesOn:from:to:by:!helpers!private! !
!ListView categoriesFor: #drawVerticalGridlinesOn:from:to:!helpers!private! !
!ListView categoriesFor: #fullItemFromPoint:!accessing!private! !
!ListView categoriesFor: #hasVirtualGridLines!accessing!public! !
!ListView categoriesFor: #hasVirtualGridLines:!accessing!public! !
!ListView categoriesFor: #headerControl!public!updating! !
!ListView categoriesFor: #headerRect!public!updating! !
!ListView categoriesFor: #horzScrollPos!enquiries!public! !
!ListView categoriesFor: #isHorzPaging!accessing!public! !
!ListView categoriesFor: #isHorzPaging:!accessing!public! !
!ListView categoriesFor: #lvmGetItemCount!accessing!public! !
!ListView categoriesFor: #lvmGetSubItemRect:bounding:!geometry!private! !
!ListView categoriesFor: #lvmGetTopIndex!accessing!public! !
!ListView categoriesFor: #nmCustomDraw:!event handling!public! !
!ListView categoriesFor: #onHScroll:!event handling!public! !
!ListView categoriesFor: #onViewOpened!event handling!public! !
!ListView categoriesFor: #rowPixelHeight!helpers!public! !
!ListView categoriesFor: #vertScrollPos!enquiries!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

HDITEM guid: (GUID fromString: '{CFF5DA39-BB38-4499-9527-66449B212B87}')!
HDITEM comment: ''!
!HDITEM categoriesForClass!Unclassified! !
!HDITEM methodsFor!

cchTextMax
	"Answer the receiver's cchTextMax field as a Smalltalk object."

	^(bytes sdwordAtOffset: 16)!

cchTextMax: anObject
	"Set the receiver's cchTextMax field to the value of anObject."

	bytes sdwordAtOffset: 16 put: anObject!

cxy
	"Answer the receiver's cxy field as a Smalltalk object."

	^(bytes sdwordAtOffset: 4)!

cxy: anObject
	"Set the receiver's cxy field to the value of anObject."

	bytes sdwordAtOffset: 4 put: anObject!

dwState
	"Answer the receiver's dwState field as a Smalltalk object."

	^self shouldNotImplement!

dwState: anObject
	"Set the receiver's dwState field to the value of anObject."

	^self shouldNotImplement!

fmt
	"Answer the receiver's fmt field as a Smalltalk object."

	^(bytes sdwordAtOffset: 20)!

fmt: anObject
	"Set the receiver's fmt field to the value of anObject."

	bytes sdwordAtOffset: 20 put: anObject!

handle
	"Answer the 'handle' of the item described by the receiver."

	^self shouldNotImplement!

hbm
	"Answer the receiver's hbm field as a Smalltalk object."

	^(bytes dwordAtOffset: 12) asExternalHandle!

hbm: anObject
	"Set the receiver's hbm field to the value of anObject."

	bytes dwordAtOffset: 12 put: anObject!

iImage
	"Answer the receiver's iImage field as a Smalltalk object."

	^(bytes sdwordAtOffset: 28)!

iImage: anObject
	"Set the receiver's iImage field to the value of anObject."

	bytes sdwordAtOffset: 28 put: anObject!

imageValidMask
	"Private - Answer the bit mask the receiver uses to denote that the iImage
	field has valid contents."

	^16r0020 "HDI_IMAGE"!

iOrder
	"Answer the receiver's iOrder field as a Smalltalk object."

	^(bytes sdwordAtOffset: 32)!

iOrder: anObject
	"Set the receiver's iOrder field to the value of anObject."

	bytes sdwordAtOffset: 32 put: anObject!

lParam
	"Answer the receiver's lParam field as a Smalltalk object."

	^(bytes dwordAtOffset: 24)!

lParam: anObject
	"Set the receiver's lParam field to the value of anObject."

	bytes dwordAtOffset: 24 put: anObject!

mask
	"Answer the receiver's mask field as a Smalltalk object."

	^(bytes dwordAtOffset: 0)!

mask: anObject
	"Set the receiver's mask field to the value of anObject."

	bytes dwordAtOffset: 0 put: anObject!

paramValidMask
	"Private - Answer the bit mask the receiver uses to denote that the pszText
	field has valid contents."

	^self subclassResponsibility!

pszText
	"Answer the receiver's pszText field as a Smalltalk object."

	^String fromAddress: (bytes sdwordAtOffset: 8)!

pszText: anObject
	"Set the receiver's pszText field to the value of anObject."

	bytes dwordAtOffset: 8 put: anObject yourAddress!

pvFilter
	"Answer the receiver's pvFilter field as a Smalltalk object."

	^(bytes dwordAtOffset: 40)!

pvFilter: anObject
	"Set the receiver's pvFilter field to the value of anObject."

	bytes dwordAtOffset: 40 put: anObject!

stateMask: anObject
	"Set the receiver's stateMask field to the value of anObject."

	^self shouldNotImplement!

stateValidMask
	"Private - Answer the bit mask the receiver uses to denote that the dwState
	field has valid contents."

	^self shouldNotImplement!

text: aString
	"Sets contents' pszMember to aString. We also hang on to it in our
	text instance var to ensure that its lifetime is sufficiently long
	for us to safely use it."

	text := aString.
	self
		pszText: aString;
		cchTextMax: aString size;
		maskIn: self textValidMask!

textPointerOffset
	"Private - Answer the offset of the text pointer in the receiver. "

	^8!

textValidMask
	"Private - Answer the bit mask the receiver uses to denote that the pszText
	field has valid contents."

	^16r0002	"HDI_TEXT"!

type
	"Answer the receiver's type field as a Smalltalk object."

	^(bytes dwordAtOffset: 36)!

type: anObject
	"Set the receiver's type field to the value of anObject."

	bytes dwordAtOffset: 36 put: anObject! !
!HDITEM categoriesFor: #cchTextMax!**compiled accessors**!public! !
!HDITEM categoriesFor: #cchTextMax:!**compiled accessors**!public! !
!HDITEM categoriesFor: #cxy!**compiled accessors**!public! !
!HDITEM categoriesFor: #cxy:!**compiled accessors**!public! !
!HDITEM categoriesFor: #dwState!**compiled accessors**!public! !
!HDITEM categoriesFor: #dwState:!**compiled accessors**!public! !
!HDITEM categoriesFor: #fmt!**compiled accessors**!public! !
!HDITEM categoriesFor: #fmt:!**compiled accessors**!public! !
!HDITEM categoriesFor: #handle!accessing!public! !
!HDITEM categoriesFor: #hbm!**compiled accessors**!public! !
!HDITEM categoriesFor: #hbm:!**compiled accessors**!public! !
!HDITEM categoriesFor: #iImage!**compiled accessors**!public! !
!HDITEM categoriesFor: #iImage:!**compiled accessors**!public! !
!HDITEM categoriesFor: #imageValidMask!constants!private! !
!HDITEM categoriesFor: #iOrder!**compiled accessors**!public! !
!HDITEM categoriesFor: #iOrder:!**compiled accessors**!public! !
!HDITEM categoriesFor: #lParam!**compiled accessors**!public! !
!HDITEM categoriesFor: #lParam:!accessing!public! !
!HDITEM categoriesFor: #mask!accessing!public! !
!HDITEM categoriesFor: #mask:!accessing!public! !
!HDITEM categoriesFor: #paramValidMask!constants!private! !
!HDITEM categoriesFor: #pszText!**compiled accessors**!public! !
!HDITEM categoriesFor: #pszText:!**compiled accessors**!public! !
!HDITEM categoriesFor: #pvFilter!**compiled accessors**!public! !
!HDITEM categoriesFor: #pvFilter:!**compiled accessors**!public! !
!HDITEM categoriesFor: #stateMask:!**compiled accessors**!public! !
!HDITEM categoriesFor: #stateValidMask!constants!private! !
!HDITEM categoriesFor: #text:!accessing!public! !
!HDITEM categoriesFor: #textPointerOffset!constants!private! !
!HDITEM categoriesFor: #textValidMask!constants!private! !
!HDITEM categoriesFor: #type!**compiled accessors**!public! !
!HDITEM categoriesFor: #type:!**compiled accessors**!public! !

!HDITEM class methodsFor!

defineFields
	"Define the fields of the MENUITEMINFOA structure.
		HDITEM compileDefinition
	
		typedef 		struct _HDITEM {
			UINT    mask; 
			int     cxy; 
			LPTSTR  pszText; 
			HBITMAP hbm; 
			int     cchTextMax; 
			int     fmt; 
			LPARAM  lParam; 
			#if (_WIN32_IE >= 0x0300)
				int     iImage;
				int     iOrder;
			#endif
			#if (_WIN32_IE >= 0x0500)
				UINT    type;
				LPVOID  pvFilter;
			#endif
		} HDITEM,  *LPHDITEM;
	"

	self
		defineField: #mask type: DWORDField new offset: 0;	"Rename to override superclass accessor???"
		defineField: #cxy type: SDWORDField new offset: 4;
		defineField: #pszText type: (PointerField type: String) offset: 8;
		defineField: #hbm type: HANDLEField new offset: 12;
		defineField: #cchTextMax type: SDWORDField new offset: 16;
		defineField: #fmt type: SDWORDField new offset: 20;
		defineField: #lParam type: DWORDField new offset: 24;
		defineField: #iImage type: SDWORDField new offset: 28;
		defineField: #iOrder type: SDWORDField new offset: 32;
		defineField: #type type: DWORDField new offset: 36;
		defineField: #pvFilter type: DWORDField new offset: 40.

	self byteSize: 44! !
!HDITEM class categoriesFor: #defineFields!initializing!public! !

HeaderView guid: (GUID fromString: '{7A6B97FA-0FD4-424D-99DE-515000F88886}')!
HeaderView comment: '#define HDI_WIDTH               0x0001
#define HDI_HEIGHT              HDI_WIDTH
#define HDI_TEXT                0x0002
#define HDI_FORMAT              0x0004
#define HDI_LPARAM              0x0008
#define HDI_BITMAP              0x0010
#if (_WIN32_IE >= 0x0300)
#define HDI_IMAGE               0x0020
#define HDI_DI_SETITEM          0x0040
#define HDI_ORDER               0x0080
#endif
#if (_WIN32_IE >= 0x0500)
#define HDI_FILTER              0x0100
#endif

#define HDF_LEFT                0x0000
#define HDF_RIGHT               0x0001
#define HDF_CENTER              0x0002
#define HDF_JUSTIFYMASK         0x0003
#define HDF_RTLREADING          0x0004

#define HDF_OWNERDRAW           0x8000
#define HDF_STRING              0x4000
#define HDF_BITMAP              0x2000
#if (_WIN32_IE >= 0x0300)
#define HDF_BITMAP_ON_RIGHT     0x1000
#define HDF_IMAGE               0x0800
#endif

#if (_WIN32_WINNT >= 0x501)
#define HDF_SORTUP              0x0400
#define HDF_SORTDOWN            0x0200
#endif'!
!HeaderView categoriesForClass!Unclassified! !
!HeaderView methodsFor!

clearSortIconForColumnAtIndex: anInteger

	| header |

	header := self headerForColumnIndex: anInteger mask: 16r0004. "HDI_FORMAT"

	header fmt: ((header fmt maskClear: 16r400 "HDF_SORTUP") maskClear: 16r0200) "HDF_SORTDOWN".

	self setHeader: header!

hdmGetItem: aHdItem index: index

	(self sendMessage: ##(16r1200+3) "HDM_GETITEMA" wParam: index lpParam: aHdItem) == 0
		ifTrue: [^self errorInCommonControlCall]
!

hdmSetItem: aHdItem index: index

	(self sendMessage: ##(16r1200+4) "HDM_SETITEMA" wParam: index lpParam: aHdItem) == 0
		ifTrue: [^self errorInCommonControlCall]
!

headerForColumn: aListViewColumn mask: anInteger

	"Private - Stuff the header index into the lparam for easy resetting with setHeader:"

	^self headerForColumnIndex: aListViewColumn index mask: anInteger!

headerForColumnIndex: aListViewColumnIndex mask: anInteger

	"Private - Stuff the header index into the lparam for easy resetting with setHeader:
	NB aListViewColumnIndex is 1-based; internal indexing is 0-based"

	| index item |

	index := aListViewColumnIndex - 1.
	item := HDITEM new.
	item 
		mask: anInteger;
		lParam: index.

	self hdmGetItem: item index: index.

	^item!

nmNotify: anNMHDR
	| msg |

	msg := anNMHDR sdwordAtOffset: 8.

	msg = (-300 "HDN_FIRST" -26) "HDN_BEGINTRACK" ifTrue: [self trigger: #beginTrack].
	msg = (-300 "HDN_FIRST" -27) "HDN_ENDTRACK" ifTrue: [self trigger: #endTrack].
	msg = (-300 "HDN_FIRST" -25) "DIVIDERDBLCLICK" ifTrue: [self trigger: #dividerDoubleClick].
	msg = (-300 "HDN_FIRST" -11) "HDN_ENDDRAG" ifTrue: [self trigger: #endDrag].

	^super nmNotify: anNMHDR
!

setAlignment: aSymbol forColumn: aListViewColumn

	"Set the alignment of the header item for the given column (distinct from the column's alignment"

	| header alignmentMask |

	alignmentMask := 
		##(Dictionary new
			at: #left	put: 16r0000; "HDF_LEFT"
			at: #right	put: 16r0001; "HDF_RIGHT"
			at: #center	put: 16r0002; "HDF_CENTER"
			yourself) at: aSymbol.

	header := self headerForColumn: aListViewColumn mask: 16r0004. "HDI_FORMAT"

	header fmt: ((header fmt maskClear: 16r0003 "HDF_JUSTIFYMASK") maskSet: alignmentMask).

	self setHeader: header!

setHeader: aHDITEM

	"Private - Assume the header item index is stuffed in lParam (see headerForColumn:mask:)"

	self hdmSetItem: aHDITEM index: aHDITEM lParam!

setSortIconForColumn: aListViewColumn

	| header |

	header := self headerForColumn: aListViewColumn mask: 16r0004. "HDI_FORMAT"

	aListViewColumn isSortOrderInverted
		ifTrue: [header fmt: ((header fmt maskClear: 16r400 "HDF_SORTUP") maskSet: 16r0200) "HDF_SORTDOWN"]
		ifFalse: [header fmt: ((header fmt maskClear: 16r200 "HDF_SORTDOWN") maskSet: 16r0400) "HDF_SORTUP"].

	self setHeader: header! !
!HeaderView categoriesFor: #clearSortIconForColumnAtIndex:!helpers!public! !
!HeaderView categoriesFor: #hdmGetItem:index:!accessing!public! !
!HeaderView categoriesFor: #hdmSetItem:index:!accessing!public! !
!HeaderView categoriesFor: #headerForColumn:mask:!helpers!private! !
!HeaderView categoriesFor: #headerForColumnIndex:mask:!helpers!private! !
!HeaderView categoriesFor: #nmNotify:!event handling-win32!private! !
!HeaderView categoriesFor: #setAlignment:forColumn:!helpers!public! !
!HeaderView categoriesFor: #setHeader:!helpers!private! !
!HeaderView categoriesFor: #setSortIconForColumn:!helpers!public! !

!HeaderView class methodsFor!

winClassName
	^'BUTTON'! !
!HeaderView class categoriesFor: #winClassName!public! !

"Binary Globals"!


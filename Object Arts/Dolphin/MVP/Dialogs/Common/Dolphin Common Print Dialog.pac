| package |
package := Package name: 'Dolphin Common Print Dialog'.
package paxVersion: 1;
	basicComment: 'Dolphin Smalltalk Windows Common Print Dialog.
Copyright (c) Object Arts Ltd. 1997-2005. Portions Copyright (c) CGI Group (Europe) Ltd. 1997.

This package contains Dialog presenter wrappers for the Windows common printer PrintDialog.'.

package basicPackageVersion: '0.001'.


package classNames
	add: #PrintDialog;
	add: #PRINTDLG;
	add: #PRINTDLGEX;
	add: #PRINTPAGERANGE;
	yourself.

package methodNames
	add: #ComDlgLibrary -> #printDlgEx:;
	add: 'PrinterCanvas class' -> #choose;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Base\Dolphin';
	add: 'Dolphin Common Dialogs';
	add: '..\..\Base\Dolphin MVP Base';
	add: '..\..\..\ActiveX\COM\OLE COM';
	yourself).

package!

"Class Definitions"!

Win32Structure subclass: #PRINTPAGERANGE
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CommonDialogStructure subclass: #PRINTDLG
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CommonDialogStructure subclass: #PRINTDLGEX
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CommonDialog subclass: #PrintDialog
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!ComDlgLibrary methodsFor!

ric not]) and: [
		next + subString size > string size or: [(string at: next + subString size) isAlphaNumeric not]]) ifTrue: [
				count := count + 1.
		].
	].
	^count.
! !
!ComDlgLibrary categoriesFor: #printDlgEx:!**auto generated**!public! !

!PrinterCanvas class methodsFor!

g.
	self updateCaption.
! !
!PrinterCanvas class categoriesFor: #choose!instance creation!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

PRINTPAGERANGE guid: (GUID fromString: '{C8DDE437-3C85-4A13-9304-3399D25AF2B4}')!
PRINTPAGERANGE comment: '&Terminate'!
!PRINTPAGERANGE categoriesForClass!Unclassified! !
!PRINTPAGERANGE methodsFor!

ection value.
	].
	documentPresenter value: string.
!

m which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  801 601 551 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 0 32 234 256 98 4 410 8 ##(Smalltalk.ListBox)  98 17 0 416 98 2 8 1144062209 1025 656 590662 2 ##(Smalltalk.ListModel)  202 208 608 0 1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.SearchPolicy)  8 #identity 482 8 4278190080 0 7 0 0 0 656 0 8 4294902469 459270 ##(Smalltalk.Message)  8 #key 608 608 32 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 1 1 530 387 533 656 1010 8 #horizontalExtent: 98 1 1 656 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 193 0 0 0 10 1 0 0] 98 0 530 193 193 0 27 8 'instVarList' 410 8 ##(Smalltalk.ScintillaView)  98 46 0 416 98 2 8 1176571972 1025 1248 721990 2 ##(Smalltalk.ValueHolder)  0 32 794 816 8 #equality 0 482 8 4278190080 0 7 0 0 0 1248 0 52910733 852486 ##(Smalltalk.NullConverter)  0 0 9 0 234 256 98 2 8 #normal 1182726 ##(Smalltalk.ScintillaTextStyle)  1 0 0 1 0 0 0 0 1488 0 0 0 98 40 1520 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1245510 1 ##(Smalltalk.NullScintillaStyler)  1488 234 256 98 2 8 #default 1639942 ##(Smalltalk.ScintillaMarkerDefinition)  1 1 786694 ##(Smalltalk.IndexedColor)  33554433 1666 33554471 1248 8 #circle 202 208 608 0 63 0 0 0 0 0 1666 33554447 0 0 0 0 0 0 8 '' 3 234 256 98 2 8 #container 1456 0 0 0 0 1 0 0 946 202 208 98 8 1010 1040 98 2 530 397 1 530 389 533 1248 1010 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 1248 1010 8 #isTextModified: 98 1 32 1248 1010 8 #modificationEventMask: 98 1 9215 1248 1010 8 #indicatorDefinitions: 98 1 98 3 1836038 ##(Smalltalk.ScintillaIndicatorDefinition)  1 1248 65025 3 2178 3 1248 33423361 5 2178 5 1248 511 1 1248 1010 8 #margins: 98 1 98 3 984582 ##(Smalltalk.ScintillaMargin)  1 1248 1 3 32 1 2306 3 1248 33 1 16 67108863 2306 5 1248 1 1 16 -67108863 1248 1010 8 #tabIndents: 98 1 16 1248 1010 8 #tabWidth: 98 1 9 1248 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 198 0 0 0 0 0 0 0 136 1 0 0 10 1 0 0] 98 0 1216 0 27 8 'document' 0 0 0 0 0 1 0 0 0 0 1 0 0 946 202 208 98 3 1010 1040 98 2 530 2559 21 530 801 601 416 1010 8 #text: 98 1 8 'Jade Object Inspector' 416 1010 8 #updateMenuBar 608 416 1154 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 10 0 0 0 143 6 0 0 54 1 0 0] 98 3 656 410 8 ##(Smalltalk.Splitter)  98 12 0 416 98 2 8 1140850688 1 2784 0 482 8 4278190080 0 519 0 0 0 2784 946 202 208 98 1 1010 1040 98 2 530 387 1 530 11 533 2784 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 193 0 0 0 0 0 0 0 198 0 0 0 10 1 0 0] 98 0 1216 0 27 1248 1216 0 27 )!

)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  801 601 551 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 0 32 234 256 98 4 410 8 ##(Smalltalk.ListBox)  98 17 0 416 98 2 8 1144062209 1025 656 590662 2 ##(Smalltalk.ListModel)  202 208 608 0 1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.SearchPolicy)  8 #identity 482 8 4278190080 0 7 0 0 0 656 0 8 4294902469 459270 ##(Smalltalk.Message)  8 #key 608 608 32 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 1 1 530 387 533 656 1010 8 #horizontalExtent: 98 1 1 656 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 193 0 0 0 10 1 0 0] 98 0 530 193 193 0 27 8 'instVarList' 410 8 ##(Smalltalk.ScintillaView)  98 46 0 416 98 2 8 1176571972 1025 1248 721990 2 ##(Smalltalk.ValueHolder)  0 32 794 816 8 #equality 0 482 8 4278190080 0 7 0 0 0 1248 0 52910733 852486 ##(Smalltalk.NullConverter)  0 0 9 0 234 256 98 2 8 #normal 1182726 ##(Smalltalk.ScintillaTextStyle)  1 0 0 1 0 0 0 0 1488 0 0 0 98 40 1520 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1245510 1 ##(Smalltalk.NullScintillaStyler)  1488 234 256 98 2 8 #default 1639942 ##(Smalltalk.ScintillaMarkerDefinition)  1 1 786694 ##(Smalltalk.IndexedColor)  33554433 1666 33554471 1248 8 #circle 202 208 608 0 63 0 0 0 0 0 1666 33554447 0 0 0 0 0 0 8 '' 3 234 256 98 2 8 #container 1456 0 0 0 0 1 0 0 946 202 208 98 8 1010 1040 98 2 530 397 1 530 389 533 1248 1010 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 1248 1010 8 #isTextModified: 98 1 32 1248 1010 8 #modificationEventMask: 98 1 9215 1248 1010 8 #indicatorDefinitions: 98 1 98 3 1836038 ##(Smalltalk.ScintillaIndicatorDefinition)  1 1248 65025 3 2178 3 1248 33423361 5 2178 5 1248 511 1 1248 1010 8 #margins: 98 1 98 3 984582 ##(Smalltalk.ScintillaMargin)  1 1248 1 3 32 1 2306 3 1248 33 1 16 67108863 2306 5 1248 1 1 16 -67108863 1248 1010 8 #tabIndents: 98 1 16 1248 1010 8 #tabWidth: 98 1 9 1248 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 198 0 0 0 0 0 0 0 136 1 0 0 10 1 0 0] 98 0 1216 0 27 8 'document' 0 0 0 0 0 1 0 0 0 0 1 0 0 946 202 208 98 3 1010 1040 98 2 530 2559 21 530 801 601 416 1010 8 #text: 98 1 8 'Jade Object Inspector' 416 1010 8 #updateMenuBar 608 416 1154 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 10 0 0 0 143 6 0 0 54 1 0 0] 98 3 656 410 8 ##(Smalltalk.Splitter)  98 12 0 416 98 2 8 1140850688 1 2784 0 482 8 4278190080 0 519 0 0 0 2784 946 202 208 98 1 1010 1040 98 2 530 387 1 530 11 533 2784 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 193 0 0 0 0 0 0 0 198 0 0 0 10 1 0 0] 98 0 1216 0 27 1248 1216 0 27 )!

0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 0 32 234 256 98 4 410 8 ##(Smalltalk.ListBox)  98 17 0 416 98 2 8 1144062209 1025 656 590662 2 ##(Smalltalk.ListModel)  202 208 608 0 1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.SearchPolicy)  8 #identity 482 8 4278190080 0 7 0 0 0 656 0 8 4294902469 459270 ##(Smalltalk.Message)  8 #key 608 608 32 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 1 1 530 387 533 656 1010 8 #horizontalExtent: 98 1 1 656 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 193 0 0 0 10 1 0 0] 98 0 530 193 193 0 27 8 'instVarList' 410 8 ##(Smalltalk.ScintillaView)  98 46 0 416 98 2 8 1176571972 1025 1248 721990 2 ##(Smalltalk.ValueHolder)  0 32 794 816 8 #equality 0 482 8 4278190080 0 7 0 0 0 1248 0 52910733 852486 ##(Smalltalk.NullConverter)  0 0 9 0 234 256 98 2 8 #normal 1182726 ##(Smalltalk.ScintillaTextStyle)  1 0 0 1 0 0 0 0 1488 0 0 0 98 40 1520 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1245510 1 ##(Smalltalk.NullScintillaStyler)  1488 234 256 98 2 8 #default 1639942 ##(Smalltalk.ScintillaMarkerDefinition)  1 1 786694 ##(Smalltalk.IndexedColor)  33554433 1666 33554471 1248 8 #circle 202 208 608 0 63 0 0 0 0 0 1666 33554447 0 0 0 0 0 0 8 '' 3 234 256 98 2 8 #container 1456 0 0 0 0 1 0 0 946 202 208 98 8 1010 1040 98 2 530 397 1 530 389 533 1248 1010 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 1248 1010 8 #isTextModified: 98 1 32 1248 1010 8 #modificationEventMask: 98 1 9215 1248 1010 8 #indicatorDefinitions: 98 1 98 3 1836038 ##(Smalltalk.ScintillaIndicatorDefinition)  1 1248 65025 3 2178 3 1248 33423361 5 2178 5 1248 511 1 1248 1010 8 #margins: 98 1 98 3 984582 ##(Smalltalk.ScintillaMargin)  1 1248 1 3 32 1 2306 3 1248 33 1 16 67108863 2306 5 1248 1 1 16 -67108863 1248 1010 8 #tabIndents: 98 1 16 1248 1010 8 #tabWidth: 98 1 9 1248 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 198 0 0 0 0 0 0 0 136 1 0 0 10 1 0 0] 98 0 1216 0 27 8 'document' 0 0 0 0 0 1 0 0 0 0 1 0 0 946 202 208 98 3 1010 1040 98 2 530 2559 21 530 801 601 416 1010 8 #text: 98 1 8 'Jade Object Inspector' 416 1010 8 #updateMenuBar 608 416 1154 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 10 0 0 0 143 6 0 0 54 1 0 0] 98 3 656 410 8 ##(Smalltalk.Splitter)  98 12 0 416 98 2 8 1140850688 1 2784 0 482 8 4278190080 0 519 0 0 0 2784 946 202 208 98 1 1010 1040 98 2 530 387 1 530 11 533 2784 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 193 0 0 0 0 0 0 0 198 0 0 0 10 1 0 0] 98 0 1216 0 27 1248 1216 0 27 )! !
!PRINTPAGERANGE categoriesFor: #nFromPage!**compiled accessors**!public! !
!PRINTPAGERANGE categoriesFor: #nFromPage:!**compiled accessors**!public! !
!PRINTPAGERANGE categoriesFor: #nToPage!**compiled accessors**!public! !
!PRINTPAGERANGE categoriesFor: #nToPage:!**compiled accessors**!public! !

!PRINTPAGERANGE class methodsFor!

8 ##(Smalltalk.SearchPolicy)  8 #identity 482 8 4278190080 0 7 0 0 0 656 0 8 4294902469 459270 ##(Smalltalk.Message)  8 #key 608 608 32 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 1 1 530 387 533 656 1010 8 #horizontalExtent: 98 1 1 656 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 193 0 0 0 10 1 0 0] 98 0 530 193 193 0 27 8 'instVarList' 410 8 ##(Smalltalk.ScintillaView)  98 46 0 416 98 2 8 1176571972 1025 1248 721990 2 ##(Smalltalk.ValueHolder)  0 32 794 816 8 #equality 0 482 8 4278190080 0 7 0 0 0 1248 0 52910733 852486 ##(Smalltalk.NullConverter)  0 0 9 0 234 256 98 2 8 #normal 1182726 ##(Smalltalk.ScintillaTextStyle)  1 0 0 1 0 0 0 0 1488 0 0 0 98 40 1520 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1245510 1 ##(Smalltalk.NullScintillaStyler)  1488 234 256 98 2 8 #default 1639942 ##(Smalltalk.ScintillaMarkerDefinition)  1 1 786694 ##(Smalltalk.IndexedColor)  33554433 1666 33554471 1248 8 #circle 202 208 608 0 63 0 0 0 0 0 1666 33554447 0 0 0 0 0 0 8 '' 3 234 256 98 2 8 #container 1456 0 0 0 0 1 0 0 946 202 208 98 8 1010 1040 98 2 530 397 1 530 389 533 1248 1010 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 1248 1010 8 #isTextModified: 98 1 32 1248 1010 8 #modificationEventMask: 98 1 9215 1248 1010 8 #indicatorDefinitions: 98 1 98 3 1836038 ##(Smalltalk.ScintillaIndicatorDefinition)  1 1248 65025 3 2178 3 1248 33423361 5 2178 5 1248 511 1 1248 1010 8 #margins: 98 1 98 3 984582 ##(Smalltalk.ScintillaMargin)  1 1248 1 3 32 1 2306 3 1248 33 1 16 67108863 2306 5 1248 1 1 16 -67108863 1248 1010 8 #tabIndents: 98 1 16 1248 1010 8 #tabWidth: 98 1 9 1248 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 198 0 0 0 0 0 0 0 136 1 0 0 10 1 0 0] 98 0 1216 0 27 8 'document' 0 0 0 0 0 1 0 0 0 0 1 0 0 946 202 208 98 3 1010 1040 98 2 530 2559 21 530 801 601 416 1010 8 #text: 98 1 8 'Jade Object Inspector' 416 1010 8 #updateMenuBar 608 416 1154 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 10 0 0 0 143 6 0 0 54 1 0 0] 98 3 656 410 8 ##(Smalltalk.Splitter)  98 12 0 416 98 2 8 1140850688 1 2784 0 482 8 4278190080 0 519 0 0 0 2784 946 202 208 98 1 1010 1040 98 2 530 387 1 530 11 533 2784 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 193 0 0 0 0 0 0 0 198 0 0 0 10 1 0 0] 98 0 1216 0 27 1248 1216 0 27 )! !
!PRINTPAGERANGE class categoriesFor: #defineFields!**auto generated**!initializing!public! !

PRINTDLG guid: (GUID fromString: '{B8A0733A-7AD6-442A-ACC5-F73156818152}')!
PRINTDLG comment: 'message'!
!PRINTDLG categoriesForClass!Unclassified! !
!PRINTDLG methodsFor!

248 0 482 512 0 7 0 0 0 3248 0 8 4294902797 2434 8 #doResume 8 '&Resume' 1189 1 0 0 32 930 202 208 98 3 994 1024 98 2 530 211 463 530 171 61 3248 994 2624 98 1 32 3248 994 2672 98 1 8 '&Resume' 3248 1218 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 105 0 0 0 231 0 0 0 190 0 0 0 5 1 0 0] 98 0 1280 0 27 2130 2176 211 2784 171 2288 -69 2816 61 410 2336 98 17 0 416 98 2 8 1140924416 1 3648 0 482 512 0 7 0 0 0 3648 0 8 4294902797 2434 8 #doCopy 8 '&Copy' 1159 1 0 0 32 930 202 208 98 3 994 1024 98 2 530 611 463 530 161 61 3648 994 2624 98 1 32 3648 994 2672 98 1 8 '&Copy' 3648 1218 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 49 1 0 0 231 0 0 0 129 1 0 0 5 1 0 0] 98 0 1280 0 27 2130 2176 611 2784 161 2288 -69 2816 61 234 256 98 0 590342 ##(Smalltalk.Rectangle)  530 1 1 530 1 1 0 0 0 0 13627 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 32 8 2010572111 930 202 208 98 2 994 1024 98 2 530 2559 21 530 801 601 416 994 8 #updateMenuBar 4064 416 1218 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 10 0 0 0 143 6 0 0 54 1 0 0] 98 5 2848 3248 2320 3648 624 1280 0 27 )!

0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 105 0 0 0 231 0 0 0 190 0 0 0 5 1 0 0] 98 0 1280 0 27 2130 2176 211 2784 171 2288 -69 2816 61 410 2336 98 17 0 416 98 2 8 1140924416 1 3648 0 482 512 0 7 0 0 0 3648 0 8 4294902797 2434 8 #doCopy 8 '&Copy' 1159 1 0 0 32 930 202 208 98 3 994 1024 98 2 530 611 463 530 161 61 3648 994 2624 98 1 32 3648 994 2672 98 1 8 '&Copy' 3648 1218 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 49 1 0 0 231 0 0 0 129 1 0 0 5 1 0 0] 98 0 1280 0 27 2130 2176 611 2784 161 2288 -69 2816 61 234 256 98 0 590342 ##(Smalltalk.Rectangle)  530 1 1 530 1 1 0 0 0 0 13627 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 32 8 2010572111 930 202 208 98 2 994 1024 98 2 530 2559 21 530 801 601 416 994 8 #updateMenuBar 4064 416 1218 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 10 0 0 0 143 6 0 0 54 1 0 0] 98 5 2848 3248 2320 3648 624 1280 0 27 )!

6 98 2 8 1140924416 1 3648 0 482 512 0 7 0 0 0 3648 0 8 4294902797 2434 8 #doCopy 8 '&Copy' 1159 1 0 0 32 930 202 208 98 3 994 1024 98 2 530 611 463 530 161 61 3648 994 2624 98 1 32 3648 994 2672 98 1 8 '&Copy' 3648 1218 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 49 1 0 0 231 0 0 0 129 1 0 0 5 1 0 0] 98 0 1280 0 27 2130 2176 611 2784 161 2288 -69 2816 61 234 256 98 0 590342 ##(Smalltalk.Rectangle)  530 1 1 530 1 1 0 0 0 0 13627 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 32 8 2010572111 930 202 208 98 2 994 1024 98 2 530 2559 21 530 801 601 416 994 8 #updateMenuBar 4064 416 1218 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 10 0 0 0 143 6 0 0 54 1 0 0] 98 5 2848 3248 2320 3648 624 1280 0 27 )!

1218 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 49 1 0 0 231 0 0 0 129 1 0 0 5 1 0 0] 98 0 1280 0 27 2130 2176 611 2784 161 2288 -69 2816 61 234 256 98 0 590342 ##(Smalltalk.Rectangle)  530 1 1 530 1 1 0 0 0 0 13627 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 32 8 2010572111 930 202 208 98 2 994 1024 98 2 530 2559 21 530 801 601 416 994 8 #updateMenuBar 4064 416 1218 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 10 0 0 0 143 6 0 0 54 1 0 0] 98 5 2848 3248 2320 3648 624 1280 0 27 )!

6 61 234 256 98 0 590342 ##(Smalltalk.Rectangle)  530 1 1 530 1 1 0 0 0 0 13627 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 32 8 2010572111 930 202 208 98 2 994 1024 98 2 530 2559 21 530 801 601 416 994 8 #updateMenuBar 4064 416 1218 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 10 0 0 0 143 6 0 0 54 1 0 0] 98 5 2848 3248 2320 3648 624 1280 0 27 )!

4 8 #updateMenuBar 4064 416 1218 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 10 0 0 0 143 6 0 0 54 1 0 0] 98 5 2848 3248 2320 3648 624 1280 0 27 )!

5 255 255 255 4 0 0 10 0 0 0 143 6 0 0 54 1 0 0] 98 5 2848 3248 2320 3648 624 1280 0 27 )!

339B99-4380-48FD-94FA-1B7BFEEF120C}')!

: ''!

or: #defineClass!

inCategory: category.
!

ne subStrings: $:) at: 2) trimBlanks
		inPackageNamed: packageName
		inCategory: category.
!

FileIn categoriesFor: #doIt!

: stream 
			forBrowser: aBrowser.
	] ensure: [
		stream close.
	].
!

= aBrowser.
	browser fileInStart: packageName.
	[
		stream := aStream.
		self read.
	] ensure: [
		browser fileInEnd: packageName.
	].
!

r:!

leInStream: (ReadStream on: aString) 
		forBrowser: aBrowser.

! !
!PRINTDLG categoriesFor: #dwSize:!**compiled accessors**!public! !
!PRINTDLG categoriesFor: #flags!**compiled accessors**!public! !
!PRINTDLG categoriesFor: #flags:!**compiled accessors**!public! !
!PRINTDLG categoriesFor: #hDC!**compiled accessors**!public! !
!PRINTDLG categoriesFor: #hDC:!**compiled accessors**!public! !
!PRINTDLG categoriesFor: #hookFlag!accessing!public! !
!PRINTDLG categoriesFor: #hwndOwner:!**compiled accessors**!public! !
!PRINTDLG categoriesFor: #lpfnHook:!**compiled accessors**!public! !
!PRINTDLG categoriesFor: #nCopies!**compiled accessors**!public! !
!PRINTDLG categoriesFor: #nCopies:!**compiled accessors**!public! !
!PRINTDLG categoriesFor: #nFromPage!**compiled accessors**!public! !
!PRINTDLG categoriesFor: #nFromPage:!**compiled accessors**!public! !
!PRINTDLG categoriesFor: #nMaxPage:!**compiled accessors**!public! !
!PRINTDLG categoriesFor: #nMinPage:!**compiled accessors**!public! !
!PRINTDLG categoriesFor: #nToPage!**compiled accessors**!public! !
!PRINTDLG categoriesFor: #nToPage:!**compiled accessors**!public! !
!PRINTDLG categoriesFor: #ownerView:!accessing!public! !

!PRINTDLG class methodsFor!

ng: aString.
	source removeSPort.
	source := source asTopazFileIn.
	(JadeWorkspace showOn: aBrowser gciSession)
		caption: 'Jade Workspace - GemStone File-In';
		fileIn: source;
		yourself.
!

"Source currently unavailable"! !
!PRINTDLG class categoriesFor: #defineFields!initializing!public! !
!PRINTDLG class categoriesFor: #packing!constants!public! !

PRINTDLGEX guid: (GUID fromString: '{9ADEA4B1-A392-47B6-904F-0FE669D13E46}')!
PRINTDLGEX comment: 'message'!
!PRINTDLGEX categoriesForClass!Unclassified! !
!PRINTDLGEX methodsFor!

t: [:each | 
		(each getAttribute: 'offset') asNumber -> (each text , ' (#' , (each getAttribute: 'number') , ')').
	].
	readStream := ReadStream on: text.
	writeStream := WriteStream on: String new.
	begin := 1.
	errors asSortedCollection do: [:each |
		writeStream 
			nextPutAll: (readStream next: each key - begin);
			nextPutAll: '{';
			nextPutAll: each value;
			nextPutAll: '}';
			yourself.
		begin := each key.
	].
	writeStream nextPutAll: readStream upToEnd.	
	MessageBox notify: writeStream contents.
!

rs asSortedCollection do: [:each |
		writeStream 
			nextPutAll: (readStream next: each key - begin);
			nextPutAll: '{';
			nextPutAll: each value;
			nextPutAll: '}';
			yourself.
		begin := each key.
	].
	writeStream nextPutAll: readStream upToEnd.	
	MessageBox notify: writeStream contents.
!

f isPostloadScript 		ifTrue: [^self readPostloadScript	].
	self doIt.
!

:= path 
		copyReplaceAll: '$l2tests'
		with: '\\samba\denile2\users\jfoster\checkouts\gss64bit11\tests'.
	path := path
		copyReplaceAll: '/'
		with: '\'.
	browser fileInPath: path.
!

tream upToEnd.	
	MessageBox notify: writeStream contents.
!

: $') at: 2.
!

JadeFileIn methodsFor!

: text
		inPackageNamed: packageName.
!

tream upTo: Character lf.
	text := readStream upToEnd.
	browser
		preLoadScript: text
		inPackageNamed: packageName.
!

"Source currently unavailable"!

s: #JadeFindClassDialog
	instanceVariableNames: 'classListPresenter nameEntryPresenter availableClasses'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

nceVariableNames: ''!

new name: 'nameEntry'.
!

formed send: #ok							to: self.
	nameEntryPresenter		when: #valueChanged 	send: #updateClassList 	to: self.
!

leTrue: [
		writeStream nextPutAll: nextLine; lf.
	].
	string := writeStream contents.
	^string.
!

"Source currently unavailable"!

elf model value: classListPresenter selection.
	super ok.
!

lue asSortedCollection.
	self model value: nil.
	nameEntryPresenter value: '*'.
!

"Source currently unavailable"!

rceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.DialogView)  98 30 0 0 98 2 26214401 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 133 0 0 0 416 788230 ##(Smalltalk.BorderLayout)  1 1 0 410 8 ##(Smalltalk.ReferenceView)  98 14 0 416 98 2 8 1140850688 131073 560 0 0 0 5 0 0 0 560 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.Presenter)  8 #resource_OK_Cancel_button_block 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  21 437 834 469 71 560 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 218 0 0 0 244 0 0 0 253 0 0 0] 98 0 834 193 193 0 27 0 0 0 234 256 98 4 410 8 ##(Smalltalk.TextEdit)  98 16 0 416 98 2 8 1140916352 1025 992 0 482 512 0 5 0 0 0 992 0 8 4294902529 852486 ##(Smalltalk.NullConverter)  0 0 5 706 202 208 98 4 770 800 98 2 834 211 1 834 301 41 992 770 8 #text: 98 1 8 '*' 992 770 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 992 770 8 #isTextModified: 98 1 32 992 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 105 0 0 0 0 0 0 0 255 0 0 0 20 0 0 0] 98 0 944 0 27 8 'nameEntry' 410 8 ##(Smalltalk.ListView)  98 30 0 416 98 2 8 1140920397 1025 1504 590662 2 ##(Smalltalk.ListModel)  202 208 928 0 1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.SearchPolicy)  8 #identity 482 512 0 5 0 0 0 1504 0 8 4294902617 8 ##(Smalltalk.BasicListAbstract)  8 ##(Smalltalk.IconicListAbstract)  1642 8 ##(Smalltalk.IconImageManager)  8 #current 0 0 0 0 0 0 202 208 98 2 920646 5 ##(Smalltalk.ListViewColumn)  8 'Name' 353 8 #left 1728 8 ##(Smalltalk.SortedCollection)  787814 3 ##(Smalltalk.BlockClosure)  0 459302 ##(Smalltalk.Context)  1 1 0 0 1180966 ##(Smalltalk.CompiledExpression)  1 9 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 98 2 8 '[:each | each key]' 98 1 202 8 ##(Smalltalk.PoolDictionary)  928 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #key 17 257 0 0 1504 0 3 0 0 1842 8 'Dictionary' 151 1888 1728 1904 1922 0 1954 1 1 0 0 1986 0 9 2016 8 'doIt' 98 2 8 '[:each | each value]' 98 1 202 2112 928 8 #[252 1 0 1 1 5 0 17 229 32 142 106 105] 17 257 0 0 1504 0 1 0 0 8 #report 928 0 131169 0 0 706 202 208 98 2 770 800 98 2 834 1 41 834 511 391 1504 770 1264 98 1 8 'Name' 1504 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 255 0 0 0 215 0 0 0] 98 0 944 0 27 8 'classList' 590342 ##(Smalltalk.Rectangle)  834 21 21 834 21 21 0 0 0 0 3 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 0 8 2010572111 706 202 208 98 3 770 800 98 2 834 1421 755 834 521 591 416 770 1264 98 1 8 'Jade Find Class Dialog' 416 770 8 #updateMenuBar 928 416 882 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 198 2 0 0 121 1 0 0 202 3 0 0 160 2 0 0] 98 4 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 2928 0 0 0 5 0 0 0 2928 0 8 4294902707 1106 0 0 0 706 202 208 98 2 770 800 98 2 834 1 1 834 211 41 2928 770 1264 98 1 8 'Partial Name:' 2928 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 105 0 0 0 20 0 0 0] 98 0 944 0 27 992 1504 560 944 0 27 )!

5 255 255 255 105 0 0 0 0 0 0 0 255 0 0 0 20 0 0 0] 98 0 944 0 27 8 'nameEntry' 410 8 ##(Smalltalk.ListView)  98 30 0 416 98 2 8 1140920397 1025 1504 590662 2 ##(Smalltalk.ListModel)  202 208 928 0 1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.SearchPolicy)  8 #identity 482 512 0 5 0 0 0 1504 0 8 4294902617 8 ##(Smalltalk.BasicListAbstract)  8 ##(Smalltalk.IconicListAbstract)  1642 8 ##(Smalltalk.IconImageManager)  8 #current 0 0 0 0 0 0 202 208 98 2 920646 5 ##(Smalltalk.ListViewColumn)  8 'Name' 353 8 #left 1728 8 ##(Smalltalk.SortedCollection)  787814 3 ##(Smalltalk.BlockClosure)  0 459302 ##(Smalltalk.Context)  1 1 0 0 1180966 ##(Smalltalk.CompiledExpression)  1 9 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 98 2 8 '[:each | each key]' 98 1 202 8 ##(Smalltalk.PoolDictionary)  928 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #key 17 257 0 0 1504 0 3 0 0 1842 8 'Dictionary' 151 1888 1728 1904 1922 0 1954 1 1 0 0 1986 0 9 2016 8 'doIt' 98 2 8 '[:each | each value]' 98 1 202 2112 928 8 #[252 1 0 1 1 5 0 17 229 32 142 106 105] 17 257 0 0 1504 0 1 0 0 8 #report 928 0 131169 0 0 706 202 208 98 2 770 800 98 2 834 1 41 834 511 391 1504 770 1264 98 1 8 'Name' 1504 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 255 0 0 0 215 0 0 0] 98 0 944 0 27 8 'classList' 590342 ##(Smalltalk.Rectangle)  834 21 21 834 21 21 0 0 0 0 3 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 0 8 2010572111 706 202 208 98 3 770 800 98 2 834 1421 755 834 521 591 416 770 1264 98 1 8 'Jade Find Class Dialog' 416 770 8 #updateMenuBar 928 416 882 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 198 2 0 0 121 1 0 0 202 3 0 0 160 2 0 0] 98 4 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 2928 0 0 0 5 0 0 0 2928 0 8 4294902707 1106 0 0 0 706 202 208 98 2 770 800 98 2 834 1 1 834 211 41 2928 770 1264 98 1 8 'Partial Name:' 2928 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 105 0 0 0 20 0 0 0] 98 0 944 0 27 992 1504 560 944 0 27 )!

hPolicy)  8 #identity 482 512 0 5 0 0 0 1504 0 8 4294902617 8 ##(Smalltalk.BasicListAbstract)  8 ##(Smalltalk.IconicListAbstract)  1642 8 ##(Smalltalk.IconImageManager)  8 #current 0 0 0 0 0 0 202 208 98 2 920646 5 ##(Smalltalk.ListViewColumn)  8 'Name' 353 8 #left 1728 8 ##(Smalltalk.SortedCollection)  787814 3 ##(Smalltalk.BlockClosure)  0 459302 ##(Smalltalk.Context)  1 1 0 0 1180966 ##(Smalltalk.CompiledExpression)  1 9 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 98 2 8 '[:each | each key]' 98 1 202 8 ##(Smalltalk.PoolDictionary)  928 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #key 17 257 0 0 1504 0 3 0 0 1842 8 'Dictionary' 151 1888 1728 1904 1922 0 1954 1 1 0 0 1986 0 9 2016 8 'doIt' 98 2 8 '[:each | each value]' 98 1 202 2112 928 8 #[252 1 0 1 1 5 0 17 229 32 142 106 105] 17 257 0 0 1504 0 1 0 0 8 #report 928 0 131169 0 0 706 202 208 98 2 770 800 98 2 834 1 41 834 511 391 1504 770 1264 98 1 8 'Name' 1504 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 255 0 0 0 215 0 0 0] 98 0 944 0 27 8 'classList' 590342 ##(Smalltalk.Rectangle)  834 21 21 834 21 21 0 0 0 0 3 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 0 8 2010572111 706 202 208 98 3 770 800 98 2 834 1421 755 834 521 591 416 770 1264 98 1 8 'Jade Find Class Dialog' 416 770 8 #updateMenuBar 928 416 882 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 198 2 0 0 121 1 0 0 202 3 0 0 160 2 0 0] 98 4 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 2928 0 0 0 5 0 0 0 2928 0 8 4294902707 1106 0 0 0 706 202 208 98 2 770 800 98 2 834 1 1 834 211 41 2928 770 1264 98 1 8 'Partial Name:' 2928 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 105 0 0 0 20 0 0 0] 98 0 944 0 27 992 1504 560 944 0 27 )!

88230 ##(Smalltalk.BorderLayout)  1 1 0 410 8 ##(Smalltalk.ReferenceView)  98 14 0 416 98 2 8 1140850688 131073 560 0 0 0 5 0 0 0 560 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.Presenter)  8 #resource_OK_Cancel_button_block 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  21 437 834 469 71 560 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 218 0 0 0 244 0 0 0 253 0 0 0] 98 0 834 193 193 0 27 0 0 0 234 256 98 4 410 8 ##(Smalltalk.TextEdit)  98 16 0 416 98 2 8 1140916352 1025 992 0 482 512 0 5 0 0 0 992 0 8 4294902529 852486 ##(Smalltalk.NullConverter)  0 0 5 706 202 208 98 4 770 800 98 2 834 211 1 834 301 41 992 770 8 #text: 98 1 8 '*' 992 770 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 992 770 8 #isTextModified: 98 1 32 992 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 105 0 0 0 0 0 0 0 255 0 0 0 20 0 0 0] 98 0 944 0 27 8 'nameEntry' 410 8 ##(Smalltalk.ListView)  98 30 0 416 98 2 8 1140920397 1025 1504 590662 2 ##(Smalltalk.ListModel)  202 208 928 0 1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.SearchPolicy)  8 #identity 482 512 0 5 0 0 0 1504 0 8 4294902617 8 ##(Smalltalk.BasicListAbstract)  8 ##(Smalltalk.IconicListAbstract)  1642 8 ##(Smalltalk.IconImageManager)  8 #current 0 0 0 0 0 0 202 208 98 2 920646 5 ##(Smalltalk.ListViewColumn)  8 'Name' 353 8 #left 1728 8 ##(Smalltalk.SortedCollection)  787814 3 ##(Smalltalk.BlockClosure)  0 459302 ##(Smalltalk.Context)  1 1 0 0 1180966 ##(Smalltalk.CompiledExpression)  1 9 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 98 2 8 '[:each | each key]' 98 1 202 8 ##(Smalltalk.PoolDictionary)  928 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #key 17 257 0 0 1504 0 3 0 0 1842 8 'Dictionary' 151 1888 1728 1904 1922 0 1954 1 1 0 0 1986 0 9 2016 8 'doIt' 98 2 8 '[:each | each value]' 98 1 202 2112 928 8 #[252 1 0 1 1 5 0 17 229 32 142 106 105] 17 257 0 0 1504 0 1 0 0 8 #report 928 0 131169 0 0 706 202 208 98 2 770 800 98 2 834 1 41 834 511 391 1504 770 1264 98 1 8 'Name' 1504 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 255 0 0 0 215 0 0 0] 98 0 944 0 27 8 'classList' 590342 ##(Smalltalk.Rectangle)  834 21 21 834 21 21 0 0 0 0 3 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 0 8 2010572111 706 202 208 98 3 770 800 98 2 834 1421 755 834 521 591 416 770 1264 98 1 8 'Jade Find Class Dialog' 416 770 8 #updateMenuBar 928 416 882 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 198 2 0 0 121 1 0 0 202 3 0 0 160 2 0 0] 98 4 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 2928 0 0 0 5 0 0 0 2928 0 8 4294902707 1106 0 0 0 706 202 208 98 2 770 800 98 2 834 1 1 834 211 41 2928 770 1264 98 1 8 'Partial Name:' 2928 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 105 0 0 0 20 0 0 0] 98 0 944 0 27 992 1504 560 944 0 27 )!

##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  21 437 834 469 71 560 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 218 0 0 0 244 0 0 0 253 0 0 0] 98 0 834 193 193 0 27 0 0 0 234 256 98 4 410 8 ##(Smalltalk.TextEdit)  98 16 0 416 98 2 8 1140916352 1025 992 0 482 512 0 5 0 0 0 992 0 8 4294902529 852486 ##(Smalltalk.NullConverter)  0 0 5 706 202 208 98 4 770 800 98 2 834 211 1 834 301 41 992 770 8 #text: 98 1 8 '*' 992 770 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 992 770 8 #isTextModified: 98 1 32 992 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 105 0 0 0 0 0 0 0 255 0 0 0 20 0 0 0] 98 0 944 0 27 8 'nameEntry' 410 8 ##(Smalltalk.ListView)  98 30 0 416 98 2 8 1140920397 1025 1504 590662 2 ##(Smalltalk.ListModel)  202 208 928 0 1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.SearchPolicy)  8 #identity 482 512 0 5 0 0 0 1504 0 8 4294902617 8 ##(Smalltalk.BasicListAbstract)  8 ##(Smalltalk.IconicListAbstract)  1642 8 ##(Smalltalk.IconImageManager)  8 #current 0 0 0 0 0 0 202 208 98 2 920646 5 ##(Smalltalk.ListViewColumn)  8 'Name' 353 8 #left 1728 8 ##(Smalltalk.SortedCollection)  787814 3 ##(Smalltalk.BlockClosure)  0 459302 ##(Smalltalk.Context)  1 1 0 0 1180966 ##(Smalltalk.CompiledExpression)  1 9 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 98 2 8 '[:each | each key]' 98 1 202 8 ##(Smalltalk.PoolDictionary)  928 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #key 17 257 0 0 1504 0 3 0 0 1842 8 'Dictionary' 151 1888 1728 1904 1922 0 1954 1 1 0 0 1986 0 9 2016 8 'doIt' 98 2 8 '[:each | each value]' 98 1 202 2112 928 8 #[252 1 0 1 1 5 0 17 229 32 142 106 105] 17 257 0 0 1504 0 1 0 0 8 #report 928 0 131169 0 0 706 202 208 98 2 770 800 98 2 834 1 41 834 511 391 1504 770 1264 98 1 8 'Name' 1504 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 255 0 0 0 215 0 0 0] 98 0 944 0 27 8 'classList' 590342 ##(Smalltalk.Rectangle)  834 21 21 834 21 21 0 0 0 0 3 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 0 8 2010572111 706 202 208 98 3 770 800 98 2 834 1421 755 834 521 591 416 770 1264 98 1 8 'Jade Find Class Dialog' 416 770 8 #updateMenuBar 928 416 882 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 198 2 0 0 121 1 0 0 202 3 0 0 160 2 0 0] 98 4 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 2928 0 0 0 5 0 0 0 2928 0 8 4294902707 1106 0 0 0 706 202 208 98 2 770 800 98 2 834 1 1 834 211 41 2928 770 1264 98 1 8 'Partial Name:' 2928 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 105 0 0 0 20 0 0 0] 98 0 944 0 27 992 1504 560 944 0 27 )!

255 255 255 255 255 255 255 255 10 0 0 0 218 0 0 0 244 0 0 0 253 0 0 0] 98 0 834 193 193 0 27 0 0 0 234 256 98 4 410 8 ##(Smalltalk.TextEdit)  98 16 0 416 98 2 8 1140916352 1025 992 0 482 512 0 5 0 0 0 992 0 8 4294902529 852486 ##(Smalltalk.NullConverter)  0 0 5 706 202 208 98 4 770 800 98 2 834 211 1 834 301 41 992 770 8 #text: 98 1 8 '*' 992 770 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 992 770 8 #isTextModified: 98 1 32 992 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 105 0 0 0 0 0 0 0 255 0 0 0 20 0 0 0] 98 0 944 0 27 8 'nameEntry' 410 8 ##(Smalltalk.ListView)  98 30 0 416 98 2 8 1140920397 1025 1504 590662 2 ##(Smalltalk.ListModel)  202 208 928 0 1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.SearchPolicy)  8 #identity 482 512 0 5 0 0 0 1504 0 8 4294902617 8 ##(Smalltalk.BasicListAbstract)  8 ##(Smalltalk.IconicListAbstract)  1642 8 ##(Smalltalk.IconImageManager)  8 #current 0 0 0 0 0 0 202 208 98 2 920646 5 ##(Smalltalk.ListViewColumn)  8 'Name' 353 8 #left 1728 8 ##(Smalltalk.SortedCollection)  787814 3 ##(Smalltalk.BlockClosure)  0 459302 ##(Smalltalk.Context)  1 1 0 0 1180966 ##(Smalltalk.CompiledExpression)  1 9 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 98 2 8 '[:each | each key]' 98 1 202 8 ##(Smalltalk.PoolDictionary)  928 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #key 17 257 0 0 1504 0 3 0 0 1842 8 'Dictionary' 151 1888 1728 1904 1922 0 1954 1 1 0 0 1986 0 9 2016 8 'doIt' 98 2 8 '[:each | each value]' 98 1 202 2112 928 8 #[252 1 0 1 1 5 0 17 229 32 142 106 105] 17 257 0 0 1504 0 1 0 0 8 #report 928 0 131169 0 0 706 202 208 98 2 770 800 98 2 834 1 41 834 511 391 1504 770 1264 98 1 8 'Name' 1504 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 255 0 0 0 215 0 0 0] 98 0 944 0 27 8 'classList' 590342 ##(Smalltalk.Rectangle)  834 21 21 834 21 21 0 0 0 0 3 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 0 8 2010572111 706 202 208 98 3 770 800 98 2 834 1421 755 834 521 591 416 770 1264 98 1 8 'Jade Find Class Dialog' 416 770 8 #updateMenuBar 928 416 882 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 198 2 0 0 121 1 0 0 202 3 0 0 160 2 0 0] 98 4 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 2928 0 0 0 5 0 0 0 2928 0 8 4294902707 1106 0 0 0 706 202 208 98 2 770 800 98 2 834 1 1 834 211 41 2928 770 1264 98 1 8 'Partial Name:' 2928 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 105 0 0 0 20 0 0 0] 98 0 944 0 27 992 1504 560 944 0 27 )!

5 706 202 208 98 4 770 800 98 2 834 211 1 834 301 41 992 770 8 #text: 98 1 8 '*' 992 770 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 992 770 8 #isTextModified: 98 1 32 992 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 105 0 0 0 0 0 0 0 255 0 0 0 20 0 0 0] 98 0 944 0 27 8 'nameEntry' 410 8 ##(Smalltalk.ListView)  98 30 0 416 98 2 8 1140920397 1025 1504 590662 2 ##(Smalltalk.ListModel)  202 208 928 0 1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.SearchPolicy)  8 #identity 482 512 0 5 0 0 0 1504 0 8 4294902617 8 ##(Smalltalk.BasicListAbstract)  8 ##(Smalltalk.IconicListAbstract)  1642 8 ##(Smalltalk.IconImageManager)  8 #current 0 0 0 0 0 0 202 208 98 2 920646 5 ##(Smalltalk.ListViewColumn)  8 'Name' 353 8 #left 1728 8 ##(Smalltalk.SortedCollection)  787814 3 ##(Smalltalk.BlockClosure)  0 459302 ##(Smalltalk.Context)  1 1 0 0 1180966 ##(Smalltalk.CompiledExpression)  1 9 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 98 2 8 '[:each | each key]' 98 1 202 8 ##(Smalltalk.PoolDictionary)  928 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #key 17 257 0 0 1504 0 3 0 0 1842 8 'Dictionary' 151 1888 1728 1904 1922 0 1954 1 1 0 0 1986 0 9 2016 8 'doIt' 98 2 8 '[:each | each value]' 98 1 202 2112 928 8 #[252 1 0 1 1 5 0 17 229 32 142 106 105] 17 257 0 0 1504 0 1 0 0 8 #report 928 0 131169 0 0 706 202 208 98 2 770 800 98 2 834 1 41 834 511 391 1504 770 1264 98 1 8 'Name' 1504 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 255 0 0 0 215 0 0 0] 98 0 944 0 27 8 'classList' 590342 ##(Smalltalk.Rectangle)  834 21 21 834 21 21 0 0 0 0 3 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 0 8 2010572111 706 202 208 98 3 770 800 98 2 834 1421 755 834 521 591 416 770 1264 98 1 8 'Jade Find Class Dialog' 416 770 8 #updateMenuBar 928 416 882 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 198 2 0 0 121 1 0 0 202 3 0 0 160 2 0 0] 98 4 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 2928 0 0 0 5 0 0 0 2928 0 8 4294902707 1106 0 0 0 706 202 208 98 2 770 800 98 2 834 1 1 834 211 41 2928 770 1264 98 1 8 'Partial Name:' 2928 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 105 0 0 0 20 0 0 0] 98 0 944 0 27 992 1504 560 944 0 27 )!

lableClasses select: [:each | nameEntryPresenter value , '*' match: each key].
	classListPresenter list: list.
	list size >= 1 ifTrue: [
		classListPresenter selectionByIndex: 1.
	].
!

categoriesFor: #updateClassList!

t 1728 8 ##(Smalltalk.SortedCollection)  787814 3 ##(Smalltalk.BlockClosure)  0 459302 ##(Smalltalk.Context)  1 1 0 0 1180966 ##(Smalltalk.CompiledExpression)  1 9 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 98 2 8 '[:each | each key]' 98 1 202 8 ##(Smalltalk.PoolDictionary)  928 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #key 17 257 0 0 1504 0 3 0 0 1842 8 'Dictionary' 151 1888 1728 1904 1922 0 1954 1 1 0 0 1986 0 9 2016 8 'doIt' 98 2 8 '[:each | each value]' 98 1 202 2112 928 8 #[252 1 0 1 1 5 0 17 229 32 142 106 105] 17 257 0 0 1504 0 1 0 0 8 #report 928 0 131169 0 0 706 202 208 98 2 770 800 98 2 834 1 41 834 511 391 1504 770 1264 98 1 8 'Name' 1504 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 255 0 0 0 215 0 0 0] 98 0 944 0 27 8 'classList' 590342 ##(Smalltalk.Rectangle)  834 21 21 834 21 21 0 0 0 0 3 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 0 8 2010572111 706 202 208 98 3 770 800 98 2 834 1421 755 834 521 591 416 770 1264 98 1 8 'Jade Find Class Dialog' 416 770 8 #updateMenuBar 928 416 882 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 198 2 0 0 121 1 0 0 202 3 0 0 160 2 0 0] 98 4 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 2928 0 0 0 5 0 0 0 2928 0 8 4294902707 1106 0 0 0 706 202 208 98 2 770 800 98 2 834 1 1 834 211 41 2928 770 1264 98 1 8 'Partial Name:' 2928 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 105 0 0 0 20 0 0 0] 98 0 944 0 27 992 1504 560 944 0 27 )!

t' 98 2 8 '[:each | each key]' 98 1 202 8 ##(Smalltalk.PoolDictionary)  928 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #key 17 257 0 0 1504 0 3 0 0 1842 8 'Dictionary' 151 1888 1728 1904 1922 0 1954 1 1 0 0 1986 0 9 2016 8 'doIt' 98 2 8 '[:each | each value]' 98 1 202 2112 928 8 #[252 1 0 1 1 5 0 17 229 32 142 106 105] 17 257 0 0 1504 0 1 0 0 8 #report 928 0 131169 0 0 706 202 208 98 2 770 800 98 2 834 1 41 834 511 391 1504 770 1264 98 1 8 'Name' 1504 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 255 0 0 0 215 0 0 0] 98 0 944 0 27 8 'classList' 590342 ##(Smalltalk.Rectangle)  834 21 21 834 21 21 0 0 0 0 3 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 0 8 2010572111 706 202 208 98 3 770 800 98 2 834 1421 755 834 521 591 416 770 1264 98 1 8 'Jade Find Class Dialog' 416 770 8 #updateMenuBar 928 416 882 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 198 2 0 0 121 1 0 0 202 3 0 0 160 2 0 0] 98 4 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 2928 0 0 0 5 0 0 0 2928 0 8 4294902707 1106 0 0 0 706 202 208 98 2 770 800 98 2 834 1 1 834 211 41 2928 770 1264 98 1 8 'Partial Name:' 2928 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 105 0 0 0 20 0 0 0] 98 0 944 0 27 992 1504 560 944 0 27 )!

'doIt' 98 2 8 '[:each | each value]' 98 1 202 2112 928 8 #[252 1 0 1 1 5 0 17 229 32 142 106 105] 17 257 0 0 1504 0 1 0 0 8 #report 928 0 131169 0 0 706 202 208 98 2 770 800 98 2 834 1 41 834 511 391 1504 770 1264 98 1 8 'Name' 1504 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 255 0 0 0 215 0 0 0] 98 0 944 0 27 8 'classList' 590342 ##(Smalltalk.Rectangle)  834 21 21 834 21 21 0 0 0 0 3 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 0 8 2010572111 706 202 208 98 3 770 800 98 2 834 1421 755 834 521 591 416 770 1264 98 1 8 'Jade Find Class Dialog' 416 770 8 #updateMenuBar 928 416 882 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 198 2 0 0 121 1 0 0 202 3 0 0 160 2 0 0] 98 4 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 2928 0 0 0 5 0 0 0 2928 0 8 4294902707 1106 0 0 0 706 202 208 98 2 770 800 98 2 834 1 1 834 211 41 2928 770 1264 98 1 8 'Partial Name:' 2928 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 105 0 0 0 20 0 0 0] 98 0 944 0 27 992 1504 560 944 0 27 )!

70 1264 98 1 8 'Name' 1504 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 255 0 0 0 215 0 0 0] 98 0 944 0 27 8 'classList' 590342 ##(Smalltalk.Rectangle)  834 21 21 834 21 21 0 0 0 0 3 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 0 8 2010572111 706 202 208 98 3 770 800 98 2 834 1421 755 834 521 591 416 770 1264 98 1 8 'Jade Find Class Dialog' 416 770 8 #updateMenuBar 928 416 882 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 198 2 0 0 121 1 0 0 202 3 0 0 160 2 0 0] 98 4 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 2928 0 0 0 5 0 0 0 2928 0 8 4294902707 1106 0 0 0 706 202 208 98 2 770 800 98 2 834 1 1 834 211 41 2928 770 1264 98 1 8 'Partial Name:' 2928 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 105 0 0 0 20 0 0 0] 98 0 944 0 27 992 1504 560 944 0 27 )!

21 834 21 21 0 0 0 0 3 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 0 8 2010572111 706 202 208 98 3 770 800 98 2 834 1421 755 834 521 591 416 770 1264 98 1 8 'Jade Find Class Dialog' 416 770 8 #updateMenuBar 928 416 882 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 198 2 0 0 121 1 0 0 202 3 0 0 160 2 0 0] 98 4 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 2928 0 0 0 5 0 0 0 2928 0 8 4294902707 1106 0 0 0 706 202 208 98 2 770 800 98 2 834 1 1 834 211 41 2928 770 1264 98 1 8 'Partial Name:' 2928 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 105 0 0 0 20 0 0 0] 98 0 944 0 27 992 1504 560 944 0 27 )!

882 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 198 2 0 0 121 1 0 0 202 3 0 0 160 2 0 0] 98 4 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 2928 0 0 0 5 0 0 0 2928 0 8 4294902707 1106 0 0 0 706 202 208 98 2 770 800 98 2 834 1 1 834 211 41 2928 770 1264 98 1 8 'Partial Name:' 2928 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 105 0 0 0 20 0 0 0] 98 0 944 0 27 992 1504 560 944 0 27 )!

1106 0 0 0 706 202 208 98 2 770 800 98 2 834 1 1 834 211 41 2928 770 1264 98 1 8 'Partial Name:' 2928 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 105 0 0 0 20 0 0 0] 98 0 944 0 27 992 1504 560 944 0 27 )!

0 0 0 0 105 0 0 0 20 0 0 0] 98 0 944 0 27 992 1504 560 944 0 27 )!

A-8BE0-49345512C4F9}')!

spector class instanceVariableNames: ''!

mponents!

#actionPerformed send: #inspectInstVar to: self.
!

ifTrue: [string := (string copyFrom: 1 to: 500) , ''...''].
string]'.
!

stream list string |
(stream := WriteStream on: String new)
	nextPutAll: object class name; tab.
list := object class allInstVarNames.
1 to: list size do: [:i | 
	stream nextPutAll: (list at: i); nextPut: $-.
	(object instVarAt: i) asOop printOn: stream.
	stream tab.
].
object class format > 0 ifTrue: [
	1 to: (object _basicSize min: 100) do: [:i | 
		i printOn: stream.
		stream nextPut: $-.
		(object _at: i) asOop printOn: stream.
		stream tab.
	].
].
(string := object printString) size > 5000 ifTrue: [string := (string copyFrom: 1 to: 5000) , ''...''].
stream cr; nextPutAll: string; contents]'.
! !
!PRINTDLGEX categoriesFor: #dwResultAction!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #dwResultAction:!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #ExclusionFlags!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #ExclusionFlags:!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #flags!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #flags:!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #Flags2!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #Flags2:!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #HDC!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #HDC:!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #hDevMode!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #hDevMode:!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #hDevNames!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #hDevNames:!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #HINSTANCE!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #HINSTANCE:!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #hwndOwner!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #hwndOwner:!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #lpCallback!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #lpCallback:!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #lphPropertyPages!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #lphPropertyPages:!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #lpPageRanges!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #lpPageRanges:!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #lpPrintTemplateName!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #lpPrintTemplateName:!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #lStructSize!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #lStructSize:!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #nCopies!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #nCopies:!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #nMaxPage!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #nMaxPage:!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #nMaxPageRanges!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #nMaxPageRanges:!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #nMinPage!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #nMinPage:!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #nPageRanges!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #nPageRanges:!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #nPropertyPages!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #nPropertyPages:!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #nStartPage!**compiled accessors**!public! !
!PRINTDLGEX categoriesFor: #nStartPage:!**compiled accessors**!public! !

!PRINTDLGEX class methodsFor!

sOop printOn: stream.
	stream tab.
].
object class format > 0 ifTrue: [
	1 to: (object _basicSize min: 100) do: [:i | 
		i printOn: stream.
		stream nextPut: $-.
		(object _at: i) asOop printOn: stream.
		stream tab.
	].
].
(string := object printString) size > 5000 ifTrue: [string := (string copyFrom: 1 to: 5000) , ''...''].
stream cr; nextPutAll: string; contents]'.
! !
!PRINTDLGEX class categoriesFor: #defineFields!**auto generated**!initializing!public! !

PrintDialog guid: (GUID fromString: '{CDCC38B9-8189-4D88-A850-4D6C8845DB51}')!
PrintDialog comment: '&Debug'!
!PrintDialog categoriesForClass!Unclassified! !
!PrintDialog methodsFor!

er)  0 0 9 0 234 256 98 2 8 #normal 1182726 ##(Smalltalk.ScintillaTextStyle)  1 0 0 1 0 0 0 0 1488 0 0 0 98 40 1520 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1245510 1 ##(Smalltalk.NullScintillaStyler)  1488 234 256 98 2 8 #default 1639942 ##(Smalltalk.ScintillaMarkerDefinition)  1 1 786694 ##(Smalltalk.IndexedColor)  33554433 1666 33554471 1248 8 #circle 202 208 608 0 63 0 0 0 0 0 1666 33554447 0 0 0 0 0 0 8 '' 3 234 256 98 2 8 #container 1456 0 0 0 0 1 0 0 946 202 208 98 8 1010 1040 98 2 530 397 1 530 389 533 1248 1010 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 1248 1010 8 #isTextModified: 98 1 32 1248 1010 8 #modificationEventMask: 98 1 9215 1248 1010 8 #indicatorDefinitions: 98 1 98 3 1836038 ##(Smalltalk.ScintillaIndicatorDefinition)  1 1248 65025 3 2178 3 1248 33423361 5 2178 5 1248 511 1 1248 1010 8 #margins: 98 1 98 3 984582 ##(Smalltalk.ScintillaMargin)  1 1248 1 3 32 1 2306 3 1248 33 1 16 67108863 2306 5 1248 1 1 16 -67108863 1248 1010 8 #tabIndents: 98 1 16 1248 1010 8 #tabWidth: 98 1 9 1248 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 198 0 0 0 0 0 0 0 136 1 0 0 10 1 0 0] 98 0 1216 0 27 8 'document' 0 0 0 0 0 1 0 0 0 0 1 0 0 946 202 208 98 3 1010 1040 98 2 530 2559 21 530 801 601 416 1010 8 #text: 98 1 8 'Jade Object Inspector' 416 1010 8 #updateMenuBar 608 416 1154 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 10 0 0 0 143 6 0 0 54 1 0 0] 98 3 656 410 8 ##(Smalltalk.Splitter)  98 12 0 416 98 2 8 1140850688 1 2784 0 482 8 4278190080 0 519 0 0 0 2784 946 202 208 98 1 1010 1040 98 2 530 387 1 530 11 533 2784 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 193 0 0 0 0 0 0 0 198 0 0 0 10 1 0 0] 98 0 1216 0 27 1248 1216 0 27 )!

6 33554471 1248 8 #circle 202 208 608 0 63 0 0 0 0 0 1666 33554447 0 0 0 0 0 0 8 '' 3 234 256 98 2 8 #container 1456 0 0 0 0 1 0 0 946 202 208 98 8 1010 1040 98 2 530 397 1 530 389 533 1248 1010 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 1248 1010 8 #isTextModified: 98 1 32 1248 1010 8 #modificationEventMask: 98 1 9215 1248 1010 8 #indicatorDefinitions: 98 1 98 3 1836038 ##(Smalltalk.ScintillaIndicatorDefinition)  1 1248 65025 3 2178 3 1248 33423361 5 2178 5 1248 511 1 1248 1010 8 #margins: 98 1 98 3 984582 ##(Smalltalk.ScintillaMargin)  1 1248 1 3 32 1 2306 3 1248 33 1 16 67108863 2306 5 1248 1 1 16 -67108863 1248 1010 8 #tabIndents: 98 1 16 1248 1010 8 #tabWidth: 98 1 9 1248 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 198 0 0 0 0 0 0 0 136 1 0 0 10 1 0 0] 98 0 1216 0 27 8 'document' 0 0 0 0 0 1 0 0 0 0 1 0 0 946 202 208 98 3 1010 1040 98 2 530 2559 21 530 801 601 416 1010 8 #text: 98 1 8 'Jade Object Inspector' 416 1010 8 #updateMenuBar 608 416 1154 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 10 0 0 0 143 6 0 0 54 1 0 0] 98 3 656 410 8 ##(Smalltalk.Splitter)  98 12 0 416 98 2 8 1140850688 1 2784 0 482 8 4278190080 0 519 0 0 0 2784 946 202 208 98 1 1010 1040 98 2 530 387 1 530 11 533 2784 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 193 0 0 0 0 0 0 0 198 0 0 0 10 1 0 0] 98 0 1216 0 27 1248 1216 0 27 )!

1 530 389 533 1248 1010 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 1248 1010 8 #isTextModified: 98 1 32 1248 1010 8 #modificationEventMask: 98 1 9215 1248 1010 8 #indicatorDefinitions: 98 1 98 3 1836038 ##(Smalltalk.ScintillaIndicatorDefinition)  1 1248 65025 3 2178 3 1248 33423361 5 2178 5 1248 511 1 1248 1010 8 #margins: 98 1 98 3 984582 ##(Smalltalk.ScintillaMargin)  1 1248 1 3 32 1 2306 3 1248 33 1 16 67108863 2306 5 1248 1 1 16 -67108863 1248 1010 8 #tabIndents: 98 1 16 1248 1010 8 #tabWidth: 98 1 9 1248 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 198 0 0 0 0 0 0 0 136 1 0 0 10 1 0 0] 98 0 1216 0 27 8 'document' 0 0 0 0 0 1 0 0 0 0 1 0 0 946 202 208 98 3 1010 1040 98 2 530 2559 21 530 801 601 416 1010 8 #text: 98 1 8 'Jade Object Inspector' 416 1010 8 #updateMenuBar 608 416 1154 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 10 0 0 0 143 6 0 0 54 1 0 0] 98 3 656 410 8 ##(Smalltalk.Splitter)  98 12 0 416 98 2 8 1140850688 1 2784 0 482 8 4278190080 0 519 0 0 0 2784 946 202 208 98 1 1010 1040 98 2 530 387 1 530 11 533 2784 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 193 0 0 0 0 0 0 0 198 0 0 0 10 1 0 0] 98 0 1216 0 27 1248 1216 0 27 )!

efinitions: 98 1 98 3 1836038 ##(Smalltalk.ScintillaIndicatorDefinition)  1 1248 65025 3 2178 3 1248 33423361 5 2178 5 1248 511 1 1248 1010 8 #margins: 98 1 98 3 984582 ##(Smalltalk.ScintillaMargin)  1 1248 1 3 32 1 2306 3 1248 33 1 16 67108863 2306 5 1248 1 1 16 -67108863 1248 1010 8 #tabIndents: 98 1 16 1248 1010 8 #tabWidth: 98 1 9 1248 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 198 0 0 0 0 0 0 0 136 1 0 0 10 1 0 0] 98 0 1216 0 27 8 'document' 0 0 0 0 0 1 0 0 0 0 1 0 0 946 202 208 98 3 1010 1040 98 2 530 2559 21 530 801 601 416 1010 8 #text: 98 1 8 'Jade Object Inspector' 416 1010 8 #updateMenuBar 608 416 1154 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 10 0 0 0 143 6 0 0 54 1 0 0] 98 3 656 410 8 ##(Smalltalk.Splitter)  98 12 0 416 98 2 8 1140850688 1 2784 0 482 8 4278190080 0 519 0 0 0 2784 946 202 208 98 1 1010 1040 98 2 530 387 1 530 11 533 2784 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 193 0 0 0 0 0 0 0 198 0 0 0 10 1 0 0] 98 0 1216 0 27 1248 1216 0 27 )!

255 255 255 255 255 255 255 255 255 255 255 255 255 255 198 0 0 0 0 0 0 0 136 1 0 0 10 1 0 0] 98 0 1216 0 27 8 'document' 0 0 0 0 0 1 0 0 0 0 1 0 0 946 202 208 98 3 1010 1040 98 2 530 2559 21 530 801 601 416 1010 8 #text: 98 1 8 'Jade Object Inspector' 416 1010 8 #updateMenuBar 608 416 1154 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 10 0 0 0 143 6 0 0 54 1 0 0] 98 3 656 410 8 ##(Smalltalk.Splitter)  98 12 0 416 98 2 8 1140850688 1 2784 0 482 8 4278190080 0 519 0 0 0 2784 946 202 208 98 1 1010 1040 98 2 530 387 1 530 11 533 2784 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 193 0 0 0 0 0 0 0 198 0 0 0 10 1 0 0] 98 0 1216 0 27 1248 1216 0 27 )!

Jade Object Inspector' 416 1010 8 #updateMenuBar 608 416 1154 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 10 0 0 0 143 6 0 0 54 1 0 0] 98 3 656 410 8 ##(Smalltalk.Splitter)  98 12 0 416 98 2 8 1140850688 1 2784 0 482 8 4278190080 0 519 0 0 0 2784 946 202 208 98 1 1010 1040 98 2 530 387 1 530 11 533 2784 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 193 0 0 0 0 0 0 0 198 0 0 0 10 1 0 0] 98 0 1216 0 27 1248 1216 0 27 )!

1140850688 1 2784 0 482 8 4278190080 0 519 0 0 0 2784 946 202 208 98 1 1010 1040 98 2 530 387 1 530 11 533 2784 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 193 0 0 0 0 0 0 0 198 0 0 0 10 1 0 0] 98 0 1216 0 27 1248 1216 0 27 )!

198 0 0 0 10 1 0 0] 98 0 1216 0 27 1248 1216 0 27 )!

'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''! !
!PrintDialog categoriesFor: #basicShowModal!private!realizing/unrealizing! !
!PrintDialog categoriesFor: #copies!accessing!public! !
!PrintDialog categoriesFor: #copies:!accessing!public! !
!PrintDialog categoriesFor: #extractResult:!helpers!private! !
!PrintDialog categoriesFor: #initialize!public! !
!PrintDialog categoriesFor: #pageRange:!accessing!public! !
!PrintDialog categoriesFor: #printRange!accessing!public! !
!PrintDialog categoriesFor: #printRange:!accessing!public! !
!PrintDialog categoriesFor: #winStructClass!constants!private! !

"Binary Globals"!


| package |
package := Package name: 'Dolphin Scintilla View'.
package paxVersion: 1;
	basicComment: 'Dolphin Scintilla Control Wrapper
Copyright (c) Object Arts Ltd, 2002-2007.

**Built against Scintilla version 1.72**

This package implements a Dolphin <textView>, <ScintillaView>, that wraps the open-source Scintilla programmers'' editor control. ScintillaView provides at least a basic interface (generated from the ''scintilla.iface'' file supplied with the source) to all of Scintilla''s features. It, and the supporting classes in the package, also provide higher level access to the majority of those features such as margins, margin markers, indicators, auto-completion lists, and text styling. Some other powerful features such as call tips may be wrapped at a higher level in a future release.

Scintilla is now used as the view for the development system''s workspaces, having completely replaced the use of RichTextEdit and MultilineTextEdit, with which the SmalltalkWorkspace presenter is no longer compatible. This carries many advantages, not the least of which is that syntax colouring of Smalltalk source code now takes place on the fly, not just when code is compiled. 

See http://www.scintilla.org for further details of Scintilla, including complete API documentation. Check the package version number to see which version of Scintilla it was built against. New Scintilla releases are often not entirely backwards compatible so we recommend that you stick with version of the Scintilla DLL''s (SciLexer.dll and Scintilla.dll) that match the package.

The underlying Scintilla control is Copyright 1998-2006 by Neil Hodgson <neilh@scintilla.org>, license terms for Scintilla:

"All Rights Reserved 

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that 
both that copyright notice and this permission notice appear in 
supporting documentation. 

NEIL HODGSON DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS 
SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY 
AND FITNESS, IN NO EVENT SHALL NEIL HODGSON BE LIABLE FOR ANY 
SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES 
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER 
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE 
OR PERFORMANCE OF THIS SOFTWARE."'.

package basicPackageVersion: '6.1'.


package classNames
	add: #NullScintillaStyler;
	add: #QueryableScintillaAttribute;
	add: #ScintillaAttribute;
	add: #ScintillaIndicator;
	add: #ScintillaIndicatorStyle;
	add: #ScintillaKeyBinding;
	add: #ScintillaLibrary;
	add: #ScintillaMargin;
	add: #ScintillaMarker;
	add: #ScintillaMarkerDefinition;
	add: #ScintillaStyler;
	add: #ScintillaStylesCollectionPresenter;
	add: #ScintillaTextStyle;
	add: #ScintillaTextStylePresenter;
	add: #ScintillaTextStylesDialog;
	add: #ScintillaView;
	add: #SCNotification;
	yourself.

package methodNames
	add: #CRTLibrary -> #isalnum:;
	add: 'TextPresenter class' -> #resource_Scintilla_view;
	yourself.

package globalNames
	add: #ScintillaConstants;
	add: #ScintillaIndicatorDefinition;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	add: #ScintillaIndicatorDefinition;
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Base\Dolphin';
	add: '..\..\Presenters\Boolean\Dolphin Boolean Presenter';
	add: '..\..\Presenters\Choice\Dolphin Choice Presenter';
	add: '..\..\Presenters\Collection\Dolphin Collection Presenters';
	add: '..\..\Presenters\Color\Dolphin Color Presenter';
	add: '..\Common Controls\Dolphin Common Controls';
	add: '..\..\Dialogs\Common\Dolphin Common Dialogs';
	add: '..\Control Bars\Dolphin Control Bars';
	add: '..\..\Presenters\Date Time\Dolphin Date Time Presenters';
	add: '..\..\Models\List\Dolphin List Models';
	add: '..\..\Presenters\List\Dolphin List Presenter';
	add: '..\..\Base\Dolphin MVP Base';
	add: '..\..\Presenters\Number\Dolphin Number Presenter';
	add: '..\SpinButton\Dolphin SpinButton Control';
	add: '..\..\Presenters\Text\Dolphin Text Presenter';
	add: '..\..\Type Converters\Dolphin Type Converters';
	add: '..\..\Models\Value\Dolphin Value Models';
	yourself).

package!

"Class Definitions"!

Object subclass: #ScintillaAttribute
	instanceVariableNames: 'id'
	classVariableNames: ''
	poolDictionaries: 'ScintillaConstants'
	classInstanceVariableNames: 'attributes'!
Object subclass: #ScintillaIndicator
	instanceVariableNames: 'styleName range tag'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #ScintillaKeyBinding
	instanceVariableNames: 'acceleratorKey message'
	classVariableNames: 'Commands VirtualKeyMap'
	poolDictionaries: 'ScintillaConstants Win32Constants'
	classInstanceVariableNames: ''!
Object subclass: #ScintillaMarker
	instanceVariableNames: 'view definition line handle'
	classVariableNames: ''
	poolDictionaries: 'ScintillaConstants'
	classInstanceVariableNames: ''!
Object subclass: #ScintillaStyler
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'ScintillaConstants'
	classInstanceVariableNames: ''!
ExternalLibrary subclass: #ScintillaLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
NMHDR subclass: #SCNotification
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'ScintillaConstants'
	classInstanceVariableNames: ''!
ValueDialog subclass: #ScintillaTextStylesDialog
	instanceVariableNames: 'stylesPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ValuePresenter subclass: #ScintillaStylesCollectionPresenter
	instanceVariableNames: 'collectionPresenter detailPresenter settingSelection'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ValuePresenter subclass: #ScintillaTextStylePresenter
	instanceVariableNames: 'facePresenter pointSizePresenter forecolorPresenter backcolorPresenter isItalicPresenter isBoldPresenter isUnderlinedPresenter casePresenter characterSetPresenter previewPresenter fillToEndOfLinePresenter normalStyle defaultStyle isInvisiblePresenter isHotspotPresenter isReadOnlyPresenter namePresenter idPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ScintillaAttribute subclass: #QueryableScintillaAttribute
	instanceVariableNames: 'view'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'getMessages'!
ScintillaAttribute subclass: #ScintillaMarkerDefinition
	instanceVariableNames: 'code forecolor backcolor view name'
	classVariableNames: 'FolderNames'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ScintillaAttribute subclass: #ScintillaTextStyle
	instanceVariableNames: 'forecolor backcolor flags faceName pointSize characterSet case name description _reserved1 _reserved2'
	classVariableNames: 'BoldMask EolFilledMask HotspotMask InvisibleMask ItalicMask PredefinedStyleNames ReadOnlyMask UnderlinedMask'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
QueryableScintillaAttribute subclass: #ScintillaIndicatorStyle
	instanceVariableNames: 'forecolor style under name _reserved2'
	classVariableNames: 'StyleNames'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
QueryableScintillaAttribute subclass: #ScintillaMargin
	instanceVariableNames: 'width type isSensitive mask'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ScintillaStyler subclass: #NullScintillaStyler
	instanceVariableNames: 'normalStyleName'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MultilineTextEdit subclass: #ScintillaView
	instanceVariableNames: 'this currentTextStyles styleIdMap styler markerDefinitions markers wordChars styleMask modificationEventMask autoCStops autoCFillups whitespaceBackcolor whitespaceForecolor selectionBackcolor selectionForecolor indicators callTipBackcolor callTipForecolor callTipHighlightColor braceChars whitespaces scFlags allTextStyles foldMarginColor foldMarginHiColor foldMarkerStyle foldFlags extraStyleBits keyBindings indicatorStyles'
	classVariableNames: 'BackgroundDwellEvents BraceHilightingMask CaretStyles CodePages DefaultKeyBindings DefaultTextStyles FoldingMask FoldMarkerStyles IndentationGuideStyles Lexers ScnMap Whitespaces'
	poolDictionaries: 'ScintillaConstants Win32Constants'
	classInstanceVariableNames: ''!

"Global Aliases"!

ScintillaIndicatorDefinition := ScintillaIndicatorStyle!


"Loose Methods"!

!CRTLibrary methodsFor!

isalnum: aCharacter 
	<cdecl: bool isalnum char>
	^self invalidCall! !
!CRTLibrary categoriesFor: #isalnum:!CRT functions-character classification!public! !

!TextPresenter class methodsFor!

resource_Scintilla_view
	"Answer the literal data from which the 'Scintilla view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Scintilla_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ScintillaView)  98 46 0 0 98 2 8 1445007428 1025 416 721990 2 ##(Smalltalk.ValueHolder)  0 32 1310726 ##(Smalltalk.EqualitySearchPolicy)  0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 5 0 0 0 416 0 8 4294903673 852486 ##(Smalltalk.NullConverter)  0 0 9 0 234 256 98 2 8 #normal 1182726 ##(Smalltalk.ScintillaTextStyle)  1 0 0 1 0 0 0 0 688 0 0 0 98 40 720 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1245510 1 ##(Smalltalk.NullScintillaStyler)  688 234 256 98 2 8 #default 1639942 ##(Smalltalk.ScintillaMarkerDefinition)  1 1 786694 ##(Smalltalk.IndexedColor)  33554433 866 33554471 416 8 #circle 202 208 98 0 0 63 9215 0 0 0 0 866 33554447 0 0 0 0 0 0 8 '' 3 234 256 98 2 8 #container 656 0 0 0 0 1 0 234 256 98 6 1 1509190 1 ##(Smalltalk.ScintillaIndicatorStyle)  1 416 65025 3 32 1 0 3 1074 3 416 33423361 5 32 3 0 5 1074 5 416 511 1 32 5 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 8 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  3359 21 1266 631 501 416 1202 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 416 1202 8 #isTextModified: 98 1 32 416 1202 8 #modificationEventMask: 98 1 9215 416 1202 8 #margins: 98 1 98 3 984582 ##(Smalltalk.ScintillaMargin)  1 416 1 3 32 1 1554 3 416 33 1 16 67108863 1554 5 416 1 1 16 -67108863 416 1202 8 #indentationGuides: 98 1 0 416 1202 8 #tabIndents: 98 1 16 416 1202 8 #tabWidth: 98 1 9 416 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 143 6 0 0 10 0 0 0 202 7 0 0 4 1 0 0] 98 0 1266 193 193 0 27 )! !
!TextPresenter class categoriesFor: #resource_Scintilla_view!public!resources-views! !

"End of package definition"!

"Source Globals"!

Smalltalk at: #ScintillaConstants put: (PoolConstantsDictionary named: #ScintillaConstants)!
ScintillaConstants at: 'CARET_CENTER' put: 16r2!
ScintillaConstants at: 'CARET_EVEN' put: 16r8!
ScintillaConstants at: 'CARET_JUMPS' put: 16r10!
ScintillaConstants at: 'CARET_SLOP' put: 16r1!
ScintillaConstants at: 'CARET_STRICT' put: 16r4!
ScintillaConstants at: 'CARET_XEVEN' put: 16r8!
ScintillaConstants at: 'CARET_XJUMPS' put: 16r10!
ScintillaConstants at: 'CARETSTYLE_BLOCK' put: 16r2!
ScintillaConstants at: 'CARETSTYLE_INVISIBLE' put: 16r0!
ScintillaConstants at: 'CARETSTYLE_LINE' put: 16r1!
ScintillaConstants at: 'EDGE_BACKGROUND' put: 16r2!
ScintillaConstants at: 'EDGE_LINE' put: 16r1!
ScintillaConstants at: 'EDGE_NONE' put: 16r0!
ScintillaConstants at: 'INDIC_BOX' put: 16r6!
ScintillaConstants at: 'INDIC_CONTAINER' put: 16r8!
ScintillaConstants at: 'INDIC_DIAGONAL' put: 16r3!
ScintillaConstants at: 'INDIC_HIDDEN' put: 16r5!
ScintillaConstants at: 'INDIC_MAX' put: 16r1F!
ScintillaConstants at: 'INDIC_PLAIN' put: 16r0!
ScintillaConstants at: 'INDIC_ROUNDBOX' put: 16r7!
ScintillaConstants at: 'INDIC_SQUIGGLE' put: 16r1!
ScintillaConstants at: 'INDIC_STRIKE' put: 16r4!
ScintillaConstants at: 'INDIC_TT' put: 16r2!
ScintillaConstants at: 'INDIC0_MASK' put: 16r20!
ScintillaConstants at: 'INDIC1_MASK' put: 16r40!
ScintillaConstants at: 'INDIC2_MASK' put: 16r80!
ScintillaConstants at: 'INDICS_MASK' put: 16rE0!
ScintillaConstants at: 'INVALID_POSITION' put: -16r1!
ScintillaConstants at: 'KEYWORDSET_MAX' put: 16r8!
ScintillaConstants at: 'MARKER_MAX' put: 16r1F!
ScintillaConstants at: 'SC_ALPHA_NOALPHA' put: 16r100!
ScintillaConstants at: 'SC_ALPHA_OPAQUE' put: 16rFF!
ScintillaConstants at: 'SC_ALPHA_TRANSPARENT' put: 16r0!
ScintillaConstants at: 'SC_CACHE_CARET' put: 16r1!
ScintillaConstants at: 'SC_CACHE_DOCUMENT' put: 16r3!
ScintillaConstants at: 'SC_CACHE_NONE' put: 16r0!
ScintillaConstants at: 'SC_CACHE_PAGE' put: 16r2!
ScintillaConstants at: 'SC_CASE_LOWER' put: 16r2!
ScintillaConstants at: 'SC_CASE_MIXED' put: 16r0!
ScintillaConstants at: 'SC_CASE_UPPER' put: 16r1!
ScintillaConstants at: 'SC_CHARSET_8859_15' put: 16r3E8!
ScintillaConstants at: 'SC_CHARSET_ANSI' put: 16r0!
ScintillaConstants at: 'SC_CHARSET_ARABIC' put: 16rB2!
ScintillaConstants at: 'SC_CHARSET_BALTIC' put: 16rBA!
ScintillaConstants at: 'SC_CHARSET_CHINESEBIG5' put: 16r88!
ScintillaConstants at: 'SC_CHARSET_CYRILLIC' put: 16r4E3!
ScintillaConstants at: 'SC_CHARSET_DEFAULT' put: 16r1!
ScintillaConstants at: 'SC_CHARSET_EASTEUROPE' put: 16rEE!
ScintillaConstants at: 'SC_CHARSET_GB2312' put: 16r86!
ScintillaConstants at: 'SC_CHARSET_GREEK' put: 16rA1!
ScintillaConstants at: 'SC_CHARSET_HANGUL' put: 16r81!
ScintillaConstants at: 'SC_CHARSET_HEBREW' put: 16rB1!
ScintillaConstants at: 'SC_CHARSET_JOHAB' put: 16r82!
ScintillaConstants at: 'SC_CHARSET_MAC' put: 16r4D!
ScintillaConstants at: 'SC_CHARSET_OEM' put: 16rFF!
ScintillaConstants at: 'SC_CHARSET_RUSSIAN' put: 16rCC!
ScintillaConstants at: 'SC_CHARSET_SHIFTJIS' put: 16r80!
ScintillaConstants at: 'SC_CHARSET_SYMBOL' put: 16r2!
ScintillaConstants at: 'SC_CHARSET_THAI' put: 16rDE!
ScintillaConstants at: 'SC_CHARSET_TURKISH' put: 16rA2!
ScintillaConstants at: 'SC_CHARSET_VIETNAMESE' put: 16rA3!
ScintillaConstants at: 'SC_CP_DBCS' put: 16r1!
ScintillaConstants at: 'SC_CP_UTF8' put: 16rFDE9!
ScintillaConstants at: 'SC_CURSORNORMAL' put: -16r1!
ScintillaConstants at: 'SC_CURSORWAIT' put: 16r4!
ScintillaConstants at: 'SC_EOL_CR' put: 16r1!
ScintillaConstants at: 'SC_EOL_CRLF' put: 16r0!
ScintillaConstants at: 'SC_EOL_LF' put: 16r2!
ScintillaConstants at: 'SC_FOLDFLAG_BOX' put: 16r1!
ScintillaConstants at: 'SC_FOLDFLAG_LEVELNUMBERS' put: 16r40!
ScintillaConstants at: 'SC_FOLDFLAG_LINEAFTER_CONTRACTED' put: 16r10!
ScintillaConstants at: 'SC_FOLDFLAG_LINEAFTER_EXPANDED' put: 16r8!
ScintillaConstants at: 'SC_FOLDFLAG_LINEBEFORE_CONTRACTED' put: 16r4!
ScintillaConstants at: 'SC_FOLDFLAG_LINEBEFORE_EXPANDED' put: 16r2!
ScintillaConstants at: 'SC_FOLDLEVELBASE' put: 16r400!
ScintillaConstants at: 'SC_FOLDLEVELBOXFOOTERFLAG' put: 16r8000!
ScintillaConstants at: 'SC_FOLDLEVELBOXHEADERFLAG' put: 16r4000!
ScintillaConstants at: 'SC_FOLDLEVELCONTRACTED' put: 16r10000!
ScintillaConstants at: 'SC_FOLDLEVELHEADERFLAG' put: 16r2000!
ScintillaConstants at: 'SC_FOLDLEVELNUMBERMASK' put: 16rFFF!
ScintillaConstants at: 'SC_FOLDLEVELUNINDENT' put: 16r20000!
ScintillaConstants at: 'SC_FOLDLEVELWHITEFLAG' put: 16r1000!
ScintillaConstants at: 'SC_IV_LOOKBOTH' put: 16r3!
ScintillaConstants at: 'SC_IV_LOOKFORWARD' put: 16r2!
ScintillaConstants at: 'SC_IV_NONE' put: 16r0!
ScintillaConstants at: 'SC_IV_REAL' put: 16r1!
ScintillaConstants at: 'SC_LASTSTEPINUNDOREDO' put: 16r100!
ScintillaConstants at: 'SC_MARGIN_BACK' put: 16r2!
ScintillaConstants at: 'SC_MARGIN_FORE' put: 16r3!
ScintillaConstants at: 'SC_MARGIN_NUMBER' put: 16r1!
ScintillaConstants at: 'SC_MARGIN_SYMBOL' put: 16r0!
ScintillaConstants at: 'SC_MARK_ARROW' put: 16r2!
ScintillaConstants at: 'SC_MARK_ARROWDOWN' put: 16r6!
ScintillaConstants at: 'SC_MARK_ARROWS' put: 16r18!
ScintillaConstants at: 'SC_MARK_BACKGROUND' put: 16r16!
ScintillaConstants at: 'SC_MARK_BOXMINUS' put: 16rE!
ScintillaConstants at: 'SC_MARK_BOXMINUSCONNECTED' put: 16rF!
ScintillaConstants at: 'SC_MARK_BOXPLUS' put: 16rC!
ScintillaConstants at: 'SC_MARK_BOXPLUSCONNECTED' put: 16rD!
ScintillaConstants at: 'SC_MARK_CHARACTER' put: 16r2710!
ScintillaConstants at: 'SC_MARK_CIRCLE' put: 16r0!
ScintillaConstants at: 'SC_MARK_CIRCLEMINUS' put: 16r14!
ScintillaConstants at: 'SC_MARK_CIRCLEMINUSCONNECTED' put: 16r15!
ScintillaConstants at: 'SC_MARK_CIRCLEPLUS' put: 16r12!
ScintillaConstants at: 'SC_MARK_CIRCLEPLUSCONNECTED' put: 16r13!
ScintillaConstants at: 'SC_MARK_DOTDOTDOT' put: 16r17!
ScintillaConstants at: 'SC_MARK_EMPTY' put: 16r5!
ScintillaConstants at: 'SC_MARK_FULLRECT' put: 16r1A!
ScintillaConstants at: 'SC_MARK_LCORNER' put: 16rA!
ScintillaConstants at: 'SC_MARK_LCORNERCURVE' put: 16r10!
ScintillaConstants at: 'SC_MARK_MINUS' put: 16r7!
ScintillaConstants at: 'SC_MARK_PIXMAP' put: 16r19!
ScintillaConstants at: 'SC_MARK_PLUS' put: 16r8!
ScintillaConstants at: 'SC_MARK_ROUNDRECT' put: 16r1!
ScintillaConstants at: 'SC_MARK_SHORTARROW' put: 16r4!
ScintillaConstants at: 'SC_MARK_SMALLRECT' put: 16r3!
ScintillaConstants at: 'SC_MARK_TCORNER' put: 16rB!
ScintillaConstants at: 'SC_MARK_TCORNERCURVE' put: 16r11!
ScintillaConstants at: 'SC_MARK_VLINE' put: 16r9!
ScintillaConstants at: 'SC_MARKNUM_FOLDER' put: 16r1E!
ScintillaConstants at: 'SC_MARKNUM_FOLDEREND' put: 16r19!
ScintillaConstants at: 'SC_MARKNUM_FOLDERMIDTAIL' put: 16r1B!
ScintillaConstants at: 'SC_MARKNUM_FOLDEROPEN' put: 16r1F!
ScintillaConstants at: 'SC_MARKNUM_FOLDEROPENMID' put: 16r1A!
ScintillaConstants at: 'SC_MARKNUM_FOLDERSUB' put: 16r1D!
ScintillaConstants at: 'SC_MARKNUM_FOLDERTAIL' put: 16r1C!
ScintillaConstants at: 'SC_MASK_FOLDERS' put: 16rFE000000!
ScintillaConstants at: 'SC_MOD_BEFOREDELETE' put: 16r800!
ScintillaConstants at: 'SC_MOD_BEFOREINSERT' put: 16r400!
ScintillaConstants at: 'SC_MOD_CHANGEFOLD' put: 16r8!
ScintillaConstants at: 'SC_MOD_CHANGEINDICATOR' put: 16r4000!
ScintillaConstants at: 'SC_MOD_CHANGELINESTATE' put: 16r8000!
ScintillaConstants at: 'SC_MOD_CHANGEMARKER' put: 16r200!
ScintillaConstants at: 'SC_MOD_CHANGESTYLE' put: 16r4!
ScintillaConstants at: 'SC_MOD_DELETETEXT' put: 16r2!
ScintillaConstants at: 'SC_MOD_INSERTTEXT' put: 16r1!
ScintillaConstants at: 'SC_MODEVENTMASKALL' put: 16rFFFF!
ScintillaConstants at: 'SC_MULTILINEUNDOREDO' put: 16r1000!
ScintillaConstants at: 'SC_MULTISTEPUNDOREDO' put: 16r80!
ScintillaConstants at: 'SC_PERFORMED_REDO' put: 16r40!
ScintillaConstants at: 'SC_PERFORMED_UNDO' put: 16r20!
ScintillaConstants at: 'SC_PERFORMED_USER' put: 16r10!
ScintillaConstants at: 'SC_PRINT_BLACKONWHITE' put: 16r2!
ScintillaConstants at: 'SC_PRINT_COLOURONWHITE' put: 16r3!
ScintillaConstants at: 'SC_PRINT_COLOURONWHITEDEFAULTBG' put: 16r4!
ScintillaConstants at: 'SC_PRINT_INVERTLIGHT' put: 16r1!
ScintillaConstants at: 'SC_PRINT_NORMAL' put: 16r0!
ScintillaConstants at: 'SC_SEL_LINES' put: 16r2!
ScintillaConstants at: 'SC_SEL_RECTANGLE' put: 16r1!
ScintillaConstants at: 'SC_SEL_STREAM' put: 16r0!
ScintillaConstants at: 'SC_STARTACTION' put: 16r2000!
ScintillaConstants at: 'SC_TIME_FOREVER' put: 16r989680!
ScintillaConstants at: 'SC_WRAP_CHAR' put: 16r2!
ScintillaConstants at: 'SC_WRAP_NONE' put: 16r0!
ScintillaConstants at: 'SC_WRAP_WORD' put: 16r1!
ScintillaConstants at: 'SC_WRAPVISUALFLAG_END' put: 16r1!
ScintillaConstants at: 'SC_WRAPVISUALFLAG_NONE' put: 16r0!
ScintillaConstants at: 'SC_WRAPVISUALFLAG_START' put: 16r2!
ScintillaConstants at: 'SC_WRAPVISUALFLAGLOC_DEFAULT' put: 16r0!
ScintillaConstants at: 'SC_WRAPVISUALFLAGLOC_END_BY_TEXT' put: 16r1!
ScintillaConstants at: 'SC_WRAPVISUALFLAGLOC_START_BY_TEXT' put: 16r2!
ScintillaConstants at: 'SCE_FS_ASM' put: 16r13!
ScintillaConstants at: 'SCE_FS_BINNUMBER' put: 16r17!
ScintillaConstants at: 'SCE_FS_COMMENT' put: 16r1!
ScintillaConstants at: 'SCE_FS_COMMENTDOC' put: 16r3!
ScintillaConstants at: 'SCE_FS_COMMENTDOCKEYWORD' put: 16r5!
ScintillaConstants at: 'SCE_FS_COMMENTDOCKEYWORDERROR' put: 16r6!
ScintillaConstants at: 'SCE_FS_COMMENTLINE' put: 16r2!
ScintillaConstants at: 'SCE_FS_COMMENTLINEDOC' put: 16r4!
ScintillaConstants at: 'SCE_FS_CONSTANT' put: 16r12!
ScintillaConstants at: 'SCE_FS_DATE' put: 16r10!
ScintillaConstants at: 'SCE_FS_DEFAULT' put: 16r0!
ScintillaConstants at: 'SCE_FS_ERROR' put: 16r15!
ScintillaConstants at: 'SCE_FS_HEXNUMBER' put: 16r16!
ScintillaConstants at: 'SCE_FS_IDENTIFIER' put: 16rF!
ScintillaConstants at: 'SCE_FS_KEYWORD' put: 16r7!
ScintillaConstants at: 'SCE_FS_KEYWORD2' put: 16r8!
ScintillaConstants at: 'SCE_FS_KEYWORD3' put: 16r9!
ScintillaConstants at: 'SCE_FS_KEYWORD4' put: 16rA!
ScintillaConstants at: 'SCE_FS_LABEL' put: 16r14!
ScintillaConstants at: 'SCE_FS_NUMBER' put: 16rB!
ScintillaConstants at: 'SCE_FS_OPERATOR' put: 16rE!
ScintillaConstants at: 'SCE_FS_PREPROCESSOR' put: 16rD!
ScintillaConstants at: 'SCE_FS_STRING' put: 16rC!
ScintillaConstants at: 'SCE_FS_STRINGEOL' put: 16r11!
ScintillaConstants at: 'SCE_GC_ATTRIBUTE' put: 16r5!
ScintillaConstants at: 'SCE_GC_COMMAND' put: 16r7!
ScintillaConstants at: 'SCE_GC_COMMENTBLOCK' put: 16r2!
ScintillaConstants at: 'SCE_GC_COMMENTLINE' put: 16r1!
ScintillaConstants at: 'SCE_GC_CONTROL' put: 16r6!
ScintillaConstants at: 'SCE_GC_DEFAULT' put: 16r0!
ScintillaConstants at: 'SCE_GC_EVENT' put: 16r4!
ScintillaConstants at: 'SCE_GC_GLOBAL' put: 16r3!
ScintillaConstants at: 'SCE_GC_OPERATOR' put: 16r9!
ScintillaConstants at: 'SCE_GC_STRING' put: 16r8!
ScintillaConstants at: 'SCEN_CHANGE' put: 16r300!
ScintillaConstants at: 'SCEN_KILLFOCUS' put: 16r100!
ScintillaConstants at: 'SCEN_SETFOCUS' put: 16r200!
ScintillaConstants at: 'SCFIND_MATCHCASE' put: 16r4!
ScintillaConstants at: 'SCFIND_POSIX' put: 16r400000!
ScintillaConstants at: 'SCFIND_REGEXP' put: 16r200000!
ScintillaConstants at: 'SCFIND_WHOLEWORD' put: 16r2!
ScintillaConstants at: 'SCFIND_WORDSTART' put: 16r100000!
ScintillaConstants at: 'SCI_ADDREFDOCUMENT' put: 16r948!
ScintillaConstants at: 'SCI_ADDSTYLEDTEXT' put: 16r7D2!
ScintillaConstants at: 'SCI_ADDTEXT' put: 16r7D1!
ScintillaConstants at: 'SCI_ALLOCATE' put: 16r98E!
ScintillaConstants at: 'SCI_APPENDTEXT' put: 16r8EA!
ScintillaConstants at: 'SCI_ASSIGNCMDKEY' put: 16r816!
ScintillaConstants at: 'SCI_AUTOCACTIVE' put: 16r836!
ScintillaConstants at: 'SCI_AUTOCCANCEL' put: 16r835!
ScintillaConstants at: 'SCI_AUTOCCOMPLETE' put: 16r838!
ScintillaConstants at: 'SCI_AUTOCGETAUTOHIDE' put: 16r847!
ScintillaConstants at: 'SCI_AUTOCGETCANCELATSTART' put: 16r83F!
ScintillaConstants at: 'SCI_AUTOCGETCHOOSESINGLE' put: 16r842!
ScintillaConstants at: 'SCI_AUTOCGETCURRENT' put: 16r98D!
ScintillaConstants at: 'SCI_AUTOCGETDROPRESTOFWORD' put: 16r8DF!
ScintillaConstants at: 'SCI_AUTOCGETIGNORECASE' put: 16r844!
ScintillaConstants at: 'SCI_AUTOCGETMAXHEIGHT' put: 16r8A3!
ScintillaConstants at: 'SCI_AUTOCGETMAXWIDTH' put: 16r8A1!
ScintillaConstants at: 'SCI_AUTOCGETSEPARATOR' put: 16r83B!
ScintillaConstants at: 'SCI_AUTOCGETTYPESEPARATOR' put: 16r8ED!
ScintillaConstants at: 'SCI_AUTOCPOSSTART' put: 16r837!
ScintillaConstants at: 'SCI_AUTOCSELECT' put: 16r83C!
ScintillaConstants at: 'SCI_AUTOCSETAUTOHIDE' put: 16r846!
ScintillaConstants at: 'SCI_AUTOCSETCANCELATSTART' put: 16r83E!
ScintillaConstants at: 'SCI_AUTOCSETCHOOSESINGLE' put: 16r841!
ScintillaConstants at: 'SCI_AUTOCSETDROPRESTOFWORD' put: 16r8DE!
ScintillaConstants at: 'SCI_AUTOCSETFILLUPS' put: 16r840!
ScintillaConstants at: 'SCI_AUTOCSETIGNORECASE' put: 16r843!
ScintillaConstants at: 'SCI_AUTOCSETMAXHEIGHT' put: 16r8A2!
ScintillaConstants at: 'SCI_AUTOCSETMAXWIDTH' put: 16r8A0!
ScintillaConstants at: 'SCI_AUTOCSETSEPARATOR' put: 16r83A!
ScintillaConstants at: 'SCI_AUTOCSETTYPESEPARATOR' put: 16r8EE!
ScintillaConstants at: 'SCI_AUTOCSHOW' put: 16r834!
ScintillaConstants at: 'SCI_AUTOCSTOPS' put: 16r839!
ScintillaConstants at: 'SCI_BACKTAB' put: 16r918!
ScintillaConstants at: 'SCI_BEGINUNDOACTION' put: 16r81E!
ScintillaConstants at: 'SCI_BRACEBADLIGHT' put: 16r930!
ScintillaConstants at: 'SCI_BRACEHIGHLIGHT' put: 16r92F!
ScintillaConstants at: 'SCI_BRACEMATCH' put: 16r931!
ScintillaConstants at: 'SCI_CALLTIPACTIVE' put: 16r89A!
ScintillaConstants at: 'SCI_CALLTIPCANCEL' put: 16r899!
ScintillaConstants at: 'SCI_CALLTIPPOSSTART' put: 16r89B!
ScintillaConstants at: 'SCI_CALLTIPSETBACK' put: 16r89D!
ScintillaConstants at: 'SCI_CALLTIPSETFORE' put: 16r89E!
ScintillaConstants at: 'SCI_CALLTIPSETFOREHLT' put: 16r89F!
ScintillaConstants at: 'SCI_CALLTIPSETHLT' put: 16r89C!
ScintillaConstants at: 'SCI_CALLTIPSHOW' put: 16r898!
ScintillaConstants at: 'SCI_CALLTIPUSESTYLE' put: 16r8A4!
ScintillaConstants at: 'SCI_CANCEL' put: 16r915!
ScintillaConstants at: 'SCI_CANPASTE' put: 16r87D!
ScintillaConstants at: 'SCI_CANREDO' put: 16r7E0!
ScintillaConstants at: 'SCI_CANUNDO' put: 16r87E!
ScintillaConstants at: 'SCI_CHARLEFT' put: 16r900!
ScintillaConstants at: 'SCI_CHARLEFTEXTEND' put: 16r901!
ScintillaConstants at: 'SCI_CHARLEFTRECTEXTEND' put: 16r97C!
ScintillaConstants at: 'SCI_CHARRIGHT' put: 16r902!
ScintillaConstants at: 'SCI_CHARRIGHTEXTEND' put: 16r903!
ScintillaConstants at: 'SCI_CHARRIGHTRECTEXTEND' put: 16r97D!
ScintillaConstants at: 'SCI_CHOOSECARETX' put: 16r95F!
ScintillaConstants at: 'SCI_CLEAR' put: 16r884!
ScintillaConstants at: 'SCI_CLEARALL' put: 16r7D4!
ScintillaConstants at: 'SCI_CLEARALLCMDKEYS' put: 16r818!
ScintillaConstants at: 'SCI_CLEARCMDKEY' put: 16r817!
ScintillaConstants at: 'SCI_CLEARDOCUMENTSTYLE' put: 16r7D5!
ScintillaConstants at: 'SCI_CLEARREGISTEREDIMAGES' put: 16r968!
ScintillaConstants at: 'SCI_COLOURISE' put: 16rFA3!
ScintillaConstants at: 'SCI_CONVERTEOLS' put: 16r7ED!
ScintillaConstants at: 'SCI_COPY' put: 16r882!
ScintillaConstants at: 'SCI_COPYRANGE' put: 16r973!
ScintillaConstants at: 'SCI_COPYTEXT' put: 16r974!
ScintillaConstants at: 'SCI_CREATEDOCUMENT' put: 16r947!
ScintillaConstants at: 'SCI_CUT' put: 16r881!
ScintillaConstants at: 'SCI_DELETEBACK' put: 16r916!
ScintillaConstants at: 'SCI_DELETEBACKNOTLINE' put: 16r928!
ScintillaConstants at: 'SCI_DELLINELEFT' put: 16r95B!
ScintillaConstants at: 'SCI_DELLINERIGHT' put: 16r95C!
ScintillaConstants at: 'SCI_DELWORDLEFT' put: 16r91F!
ScintillaConstants at: 'SCI_DELWORDRIGHT' put: 16r920!
ScintillaConstants at: 'SCI_DELWORDRIGHTEND' put: 16r9D6!
ScintillaConstants at: 'SCI_DOCLINEFROMVISIBLE' put: 16r8AD!
ScintillaConstants at: 'SCI_DOCUMENTEND' put: 16r90E!
ScintillaConstants at: 'SCI_DOCUMENTENDEXTEND' put: 16r90F!
ScintillaConstants at: 'SCI_DOCUMENTSTART' put: 16r90C!
ScintillaConstants at: 'SCI_DOCUMENTSTARTEXTEND' put: 16r90D!
ScintillaConstants at: 'SCI_EDITTOGGLEOVERTYPE' put: 16r914!
ScintillaConstants at: 'SCI_EMPTYUNDOBUFFER' put: 16r87F!
ScintillaConstants at: 'SCI_ENCODEDFROMUTF8' put: 16r991!
ScintillaConstants at: 'SCI_ENDUNDOACTION' put: 16r81F!
ScintillaConstants at: 'SCI_ENSUREVISIBLE' put: 16r8B8!
ScintillaConstants at: 'SCI_ENSUREVISIBLEENFORCEPOLICY' put: 16r8BA!
ScintillaConstants at: 'SCI_FINDCOLUMN' put: 16r998!
ScintillaConstants at: 'SCI_FINDTEXT' put: 16r866!
ScintillaConstants at: 'SCI_FORMATRANGE' put: 16r867!
ScintillaConstants at: 'SCI_FORMFEED' put: 16r91A!
ScintillaConstants at: 'SCI_GETANCHOR' put: 16r7D9!
ScintillaConstants at: 'SCI_GETBACKSPACEUNINDENTS' put: 16r8D7!
ScintillaConstants at: 'SCI_GETBUFFEREDDRAW' put: 16r7F2!
ScintillaConstants at: 'SCI_GETCARETFORE' put: 16r85A!
ScintillaConstants at: 'SCI_GETCARETLINEBACK' put: 16r831!
ScintillaConstants at: 'SCI_GETCARETLINEBACKALPHA' put: 16r9A7!
ScintillaConstants at: 'SCI_GETCARETLINEVISIBLE' put: 16r82F!
ScintillaConstants at: 'SCI_GETCARETPERIOD' put: 16r81B!
ScintillaConstants at: 'SCI_GETCARETSTICKY' put: 16r999!
ScintillaConstants at: 'SCI_GETCARETSTYLE' put: 16r9D1!
ScintillaConstants at: 'SCI_GETCARETWIDTH' put: 16r88D!
ScintillaConstants at: 'SCI_GETCHARAT' put: 16r7D7!
ScintillaConstants at: 'SCI_GETCODEPAGE' put: 16r859!
ScintillaConstants at: 'SCI_GETCOLUMN' put: 16r851!
ScintillaConstants at: 'SCI_GETCONTROLCHARSYMBOL' put: 16r955!
ScintillaConstants at: 'SCI_GETCURLINE' put: 16r7EB!
ScintillaConstants at: 'SCI_GETCURRENTPOS' put: 16r7D8!
ScintillaConstants at: 'SCI_GETCURSOR' put: 16r953!
ScintillaConstants at: 'SCI_GETDIRECTFUNCTION' put: 16r888!
ScintillaConstants at: 'SCI_GETDIRECTPOINTER' put: 16r889!
ScintillaConstants at: 'SCI_GETDOCPOINTER' put: 16r935!
ScintillaConstants at: 'SCI_GETEDGECOLOUR' put: 16r93C!
ScintillaConstants at: 'SCI_GETEDGECOLUMN' put: 16r938!
ScintillaConstants at: 'SCI_GETEDGEMODE' put: 16r93A!
ScintillaConstants at: 'SCI_GETENDATLASTLINE' put: 16r8E6!
ScintillaConstants at: 'SCI_GETENDSTYLED' put: 16r7EC!
ScintillaConstants at: 'SCI_GETEOLMODE' put: 16r7EE!
ScintillaConstants at: 'SCI_GETFIRSTVISIBLELINE' put: 16r868!
ScintillaConstants at: 'SCI_GETFOCUS' put: 16r94D!
ScintillaConstants at: 'SCI_GETFOLDEXPANDED' put: 16r8B6!
ScintillaConstants at: 'SCI_GETFOLDLEVEL' put: 16r8AF!
ScintillaConstants at: 'SCI_GETFOLDPARENT' put: 16r8B1!
ScintillaConstants at: 'SCI_GETHIGHLIGHTGUIDE' put: 16r857!
ScintillaConstants at: 'SCI_GETHOTSPOTACTIVEBACK' put: 16r9BF!
ScintillaConstants at: 'SCI_GETHOTSPOTACTIVEFORE' put: 16r9BE!
ScintillaConstants at: 'SCI_GETHOTSPOTACTIVEUNDERLINE' put: 16r9C0!
ScintillaConstants at: 'SCI_GETHOTSPOTSINGLELINE' put: 16r9C1!
ScintillaConstants at: 'SCI_GETHSCROLLBAR' put: 16r853!
ScintillaConstants at: 'SCI_GETINDENT' put: 16r84B!
ScintillaConstants at: 'SCI_GETINDENTATIONGUIDES' put: 16r855!
ScintillaConstants at: 'SCI_GETINDICATORCURRENT' put: 16r9C5!
ScintillaConstants at: 'SCI_GETINDICATORVALUE' put: 16r9C7!
ScintillaConstants at: 'SCI_GETLASTCHILD' put: 16r8B0!
ScintillaConstants at: 'SCI_GETLAYOUTCACHE' put: 16r8E1!
ScintillaConstants at: 'SCI_GETLENGTH' put: 16r7D6!
ScintillaConstants at: 'SCI_GETLEXER' put: 16rFA2!
ScintillaConstants at: 'SCI_GETLINE' put: 16r869!
ScintillaConstants at: 'SCI_GETLINECOUNT' put: 16r86A!
ScintillaConstants at: 'SCI_GETLINEENDPOSITION' put: 16r858!
ScintillaConstants at: 'SCI_GETLINEINDENTATION' put: 16r84F!
ScintillaConstants at: 'SCI_GETLINEINDENTPOSITION' put: 16r850!
ScintillaConstants at: 'SCI_GETLINESELENDPOSITION' put: 16r979!
ScintillaConstants at: 'SCI_GETLINESELSTARTPOSITION' put: 16r978!
ScintillaConstants at: 'SCI_GETLINESTATE' put: 16r82D!
ScintillaConstants at: 'SCI_GETLINEVISIBLE' put: 16r8B4!
ScintillaConstants at: 'SCI_GETMARGINLEFT' put: 16r86C!
ScintillaConstants at: 'SCI_GETMARGINMASKN' put: 16r8C5!
ScintillaConstants at: 'SCI_GETMARGINRIGHT' put: 16r86E!
ScintillaConstants at: 'SCI_GETMARGINSENSITIVEN' put: 16r8C7!
ScintillaConstants at: 'SCI_GETMARGINTYPEN' put: 16r8C1!
ScintillaConstants at: 'SCI_GETMARGINWIDTHN' put: 16r8C3!
ScintillaConstants at: 'SCI_GETMAXLINESTATE' put: 16r82E!
ScintillaConstants at: 'SCI_GETMODEVENTMASK' put: 16r94A!
ScintillaConstants at: 'SCI_GETMODIFY' put: 16r86F!
ScintillaConstants at: 'SCI_GETMOUSEDOWNCAPTURES' put: 16r951!
ScintillaConstants at: 'SCI_GETMOUSEDWELLTIME' put: 16r8D9!
ScintillaConstants at: 'SCI_GETOVERTYPE' put: 16r88B!
ScintillaConstants at: 'SCI_GETPASTECONVERTENDINGS' put: 16r9A4!
ScintillaConstants at: 'SCI_GETPOSITIONCACHE' put: 16r9D3!
ScintillaConstants at: 'SCI_GETPRINTCOLOURMODE' put: 16r865!
ScintillaConstants at: 'SCI_GETPRINTMAGNIFICATION' put: 16r863!
ScintillaConstants at: 'SCI_GETPRINTWRAPMODE' put: 16r967!
ScintillaConstants at: 'SCI_GETPROPERTY' put: 16rFA8!
ScintillaConstants at: 'SCI_GETPROPERTYEXPANDED' put: 16rFA9!
ScintillaConstants at: 'SCI_GETPROPERTYINT' put: 16rFAA!
ScintillaConstants at: 'SCI_GETREADONLY' put: 16r85C!
ScintillaConstants at: 'SCI_GETSCROLLWIDTH' put: 16r8E3!
ScintillaConstants at: 'SCI_GETSCROLLWIDTHTRACKING' put: 16r9D5!
ScintillaConstants at: 'SCI_GETSEARCHFLAGS' put: 16r897!
ScintillaConstants at: 'SCI_GETSELALPHA' put: 16r9AD!
ScintillaConstants at: 'SCI_GETSELECTIONEND' put: 16r861!
ScintillaConstants at: 'SCI_GETSELECTIONMODE' put: 16r977!
ScintillaConstants at: 'SCI_GETSELECTIONSTART' put: 16r85F!
ScintillaConstants at: 'SCI_GETSELEOLFILLED' put: 16r9AF!
ScintillaConstants at: 'SCI_GETSELTEXT' put: 16r871!
ScintillaConstants at: 'SCI_GETSTATUS' put: 16r94F!
ScintillaConstants at: 'SCI_GETSTYLEAT' put: 16r7DA!
ScintillaConstants at: 'SCI_GETSTYLEBITS' put: 16r82B!
ScintillaConstants at: 'SCI_GETSTYLEBITSNEEDED' put: 16rFAB!
ScintillaConstants at: 'SCI_GETSTYLEDTEXT' put: 16r7DF!
ScintillaConstants at: 'SCI_GETTABINDENTS' put: 16r8D5!
ScintillaConstants at: 'SCI_GETTABWIDTH' put: 16r849!
ScintillaConstants at: 'SCI_GETTARGETEND' put: 16r891!
ScintillaConstants at: 'SCI_GETTARGETSTART' put: 16r88F!
ScintillaConstants at: 'SCI_GETTEXT' put: 16r886!
ScintillaConstants at: 'SCI_GETTEXTLENGTH' put: 16r887!
ScintillaConstants at: 'SCI_GETTEXTRANGE' put: 16r872!
ScintillaConstants at: 'SCI_GETTWOPHASEDRAW' put: 16r8EB!
ScintillaConstants at: 'SCI_GETUNDOCOLLECTION' put: 16r7E3!
ScintillaConstants at: 'SCI_GETUSEPALETTE' put: 16r85B!
ScintillaConstants at: 'SCI_GETUSETABS' put: 16r84D!
ScintillaConstants at: 'SCI_GETVIEWEOL' put: 16r933!
ScintillaConstants at: 'SCI_GETVIEWWS' put: 16r7E4!
ScintillaConstants at: 'SCI_GETVSCROLLBAR' put: 16r8E9!
ScintillaConstants at: 'SCI_GETWRAPMODE' put: 16r8DD!
ScintillaConstants at: 'SCI_GETWRAPSTARTINDENT' put: 16r9A1!
ScintillaConstants at: 'SCI_GETWRAPVISUALFLAGS' put: 16r99D!
ScintillaConstants at: 'SCI_GETWRAPVISUALFLAGSLOCATION' put: 16r99F!
ScintillaConstants at: 'SCI_GETXOFFSET' put: 16r95E!
ScintillaConstants at: 'SCI_GETZOOM' put: 16r946!
ScintillaConstants at: 'SCI_GOTOLINE' put: 16r7E8!
ScintillaConstants at: 'SCI_GOTOPOS' put: 16r7E9!
ScintillaConstants at: 'SCI_GRABFOCUS' put: 16r960!
ScintillaConstants at: 'SCI_HIDELINES' put: 16r8B3!
ScintillaConstants at: 'SCI_HIDESELECTION' put: 16r873!
ScintillaConstants at: 'SCI_HOME' put: 16r908!
ScintillaConstants at: 'SCI_HOMEDISPLAY' put: 16r929!
ScintillaConstants at: 'SCI_HOMEDISPLAYEXTEND' put: 16r92A!
ScintillaConstants at: 'SCI_HOMEEXTEND' put: 16r909!
ScintillaConstants at: 'SCI_HOMERECTEXTEND' put: 16r97E!
ScintillaConstants at: 'SCI_HOMEWRAP' put: 16r92D!
ScintillaConstants at: 'SCI_HOMEWRAPEXTEND' put: 16r992!
ScintillaConstants at: 'SCI_INDICATORALLONFOR' put: 16r9CA!
ScintillaConstants at: 'SCI_INDICATORCLEARRANGE' put: 16r9C9!
ScintillaConstants at: 'SCI_INDICATOREND' put: 16r9CD!
ScintillaConstants at: 'SCI_INDICATORFILLRANGE' put: 16r9C8!
ScintillaConstants at: 'SCI_INDICATORSTART' put: 16r9CC!
ScintillaConstants at: 'SCI_INDICATORVALUEAT' put: 16r9CB!
ScintillaConstants at: 'SCI_INDICGETFORE' put: 16r823!
ScintillaConstants at: 'SCI_INDICGETSTYLE' put: 16r821!
ScintillaConstants at: 'SCI_INDICGETUNDER' put: 16r9CF!
ScintillaConstants at: 'SCI_INDICSETFORE' put: 16r822!
ScintillaConstants at: 'SCI_INDICSETSTYLE' put: 16r820!
ScintillaConstants at: 'SCI_INDICSETUNDER' put: 16r9CE!
ScintillaConstants at: 'SCI_INSERTTEXT' put: 16r7D3!
ScintillaConstants at: 'SCI_LEXER_START' put: 16rFA0!
ScintillaConstants at: 'SCI_LINECOPY' put: 16r997!
ScintillaConstants at: 'SCI_LINECUT' put: 16r921!
ScintillaConstants at: 'SCI_LINEDELETE' put: 16r922!
ScintillaConstants at: 'SCI_LINEDOWN' put: 16r8FC!
ScintillaConstants at: 'SCI_LINEDOWNEXTEND' put: 16r8FD!
ScintillaConstants at: 'SCI_LINEDOWNRECTEXTEND' put: 16r97A!
ScintillaConstants at: 'SCI_LINEDUPLICATE' put: 16r964!
ScintillaConstants at: 'SCI_LINEEND' put: 16r90A!
ScintillaConstants at: 'SCI_LINEENDDISPLAY' put: 16r92B!
ScintillaConstants at: 'SCI_LINEENDDISPLAYEXTEND' put: 16r92C!
ScintillaConstants at: 'SCI_LINEENDEXTEND' put: 16r90B!
ScintillaConstants at: 'SCI_LINEENDRECTEXTEND' put: 16r980!
ScintillaConstants at: 'SCI_LINEENDWRAP' put: 16r993!
ScintillaConstants at: 'SCI_LINEENDWRAPEXTEND' put: 16r994!
ScintillaConstants at: 'SCI_LINEFROMPOSITION' put: 16r876!
ScintillaConstants at: 'SCI_LINELENGTH' put: 16r92E!
ScintillaConstants at: 'SCI_LINESCROLL' put: 16r878!
ScintillaConstants at: 'SCI_LINESCROLLDOWN' put: 16r926!
ScintillaConstants at: 'SCI_LINESCROLLUP' put: 16r927!
ScintillaConstants at: 'SCI_LINESJOIN' put: 16r8F0!
ScintillaConstants at: 'SCI_LINESONSCREEN' put: 16r942!
ScintillaConstants at: 'SCI_LINESSPLIT' put: 16r8F1!
ScintillaConstants at: 'SCI_LINETRANSPOSE' put: 16r923!
ScintillaConstants at: 'SCI_LINEUP' put: 16r8FE!
ScintillaConstants at: 'SCI_LINEUPEXTEND' put: 16r8FF!
ScintillaConstants at: 'SCI_LINEUPRECTEXTEND' put: 16r97B!
ScintillaConstants at: 'SCI_LOADLEXERLIBRARY' put: 16rFA7!
ScintillaConstants at: 'SCI_LOWERCASE' put: 16r924!
ScintillaConstants at: 'SCI_MARKERADD' put: 16r7FB!
ScintillaConstants at: 'SCI_MARKERADDSET' put: 16r9A2!
ScintillaConstants at: 'SCI_MARKERDEFINE' put: 16r7F8!
ScintillaConstants at: 'SCI_MARKERDEFINEPIXMAP' put: 16r801!
ScintillaConstants at: 'SCI_MARKERDELETE' put: 16r7FC!
ScintillaConstants at: 'SCI_MARKERDELETEALL' put: 16r7FD!
ScintillaConstants at: 'SCI_MARKERDELETEHANDLE' put: 16r7E2!
ScintillaConstants at: 'SCI_MARKERGET' put: 16r7FE!
ScintillaConstants at: 'SCI_MARKERLINEFROMHANDLE' put: 16r7E1!
ScintillaConstants at: 'SCI_MARKERNEXT' put: 16r7FF!
ScintillaConstants at: 'SCI_MARKERPREVIOUS' put: 16r800!
ScintillaConstants at: 'SCI_MARKERSETALPHA' put: 16r9AC!
ScintillaConstants at: 'SCI_MARKERSETBACK' put: 16r7FA!
ScintillaConstants at: 'SCI_MARKERSETFORE' put: 16r7F9!
ScintillaConstants at: 'SCI_MOVECARETINSIDEVIEW' put: 16r961!
ScintillaConstants at: 'SCI_NEWLINE' put: 16r919!
ScintillaConstants at: 'SCI_NULL' put: 16r87C!
ScintillaConstants at: 'SCI_OPTIONAL_START' put: 16rBB8!
ScintillaConstants at: 'SCI_PAGEDOWN' put: 16r912!
ScintillaConstants at: 'SCI_PAGEDOWNEXTEND' put: 16r913!
ScintillaConstants at: 'SCI_PAGEDOWNRECTEXTEND' put: 16r982!
ScintillaConstants at: 'SCI_PAGEUP' put: 16r910!
ScintillaConstants at: 'SCI_PAGEUPEXTEND' put: 16r911!
ScintillaConstants at: 'SCI_PAGEUPRECTEXTEND' put: 16r981!
ScintillaConstants at: 'SCI_PARADOWN' put: 16r96D!
ScintillaConstants at: 'SCI_PARADOWNEXTEND' put: 16r96E!
ScintillaConstants at: 'SCI_PARAUP' put: 16r96F!
ScintillaConstants at: 'SCI_PARAUPEXTEND' put: 16r970!
ScintillaConstants at: 'SCI_PASTE' put: 16r883!
ScintillaConstants at: 'SCI_POINTXFROMPOSITION' put: 16r874!
ScintillaConstants at: 'SCI_POINTYFROMPOSITION' put: 16r875!
ScintillaConstants at: 'SCI_POSITIONAFTER' put: 16r972!
ScintillaConstants at: 'SCI_POSITIONBEFORE' put: 16r971!
ScintillaConstants at: 'SCI_POSITIONFROMLINE' put: 16r877!
ScintillaConstants at: 'SCI_POSITIONFROMPOINT' put: 16r7E6!
ScintillaConstants at: 'SCI_POSITIONFROMPOINTCLOSE' put: 16r7E7!
ScintillaConstants at: 'SCI_REDO' put: 16r7DB!
ScintillaConstants at: 'SCI_REGISTERIMAGE' put: 16r965!
ScintillaConstants at: 'SCI_RELEASEDOCUMENT' put: 16r949!
ScintillaConstants at: 'SCI_REPLACESEL' put: 16r87A!
ScintillaConstants at: 'SCI_REPLACETARGET' put: 16r892!
ScintillaConstants at: 'SCI_REPLACETARGETRE' put: 16r893!
ScintillaConstants at: 'SCI_SCROLLCARET' put: 16r879!
ScintillaConstants at: 'SCI_SEARCHANCHOR' put: 16r93E!
ScintillaConstants at: 'SCI_SEARCHINTARGET' put: 16r895!
ScintillaConstants at: 'SCI_SEARCHNEXT' put: 16r93F!
ScintillaConstants at: 'SCI_SEARCHPREV' put: 16r940!
ScintillaConstants at: 'SCI_SELECTALL' put: 16r7DD!
ScintillaConstants at: 'SCI_SELECTIONDUPLICATE' put: 16r9A5!
ScintillaConstants at: 'SCI_SELECTIONISRECTANGLE' put: 16r944!
ScintillaConstants at: 'SCI_SETANCHOR' put: 16r7EA!
ScintillaConstants at: 'SCI_SETBACKSPACEUNINDENTS' put: 16r8D6!
ScintillaConstants at: 'SCI_SETBUFFEREDDRAW' put: 16r7F3!
ScintillaConstants at: 'SCI_SETCARETFORE' put: 16r815!
ScintillaConstants at: 'SCI_SETCARETLINEBACK' put: 16r832!
ScintillaConstants at: 'SCI_SETCARETLINEBACKALPHA' put: 16r9A6!
ScintillaConstants at: 'SCI_SETCARETLINEVISIBLE' put: 16r830!
ScintillaConstants at: 'SCI_SETCARETPERIOD' put: 16r81C!
ScintillaConstants at: 'SCI_SETCARETPOLICY' put: 16r941!
ScintillaConstants at: 'SCI_SETCARETSTICKY' put: 16r99A!
ScintillaConstants at: 'SCI_SETCARETSTYLE' put: 16r9D0!
ScintillaConstants at: 'SCI_SETCARETWIDTH' put: 16r88C!
ScintillaConstants at: 'SCI_SETCHARSDEFAULT' put: 16r98C!
ScintillaConstants at: 'SCI_SETCODEPAGE' put: 16r7F5!
ScintillaConstants at: 'SCI_SETCONTROLCHARSYMBOL' put: 16r954!
ScintillaConstants at: 'SCI_SETCURRENTPOS' put: 16r85D!
ScintillaConstants at: 'SCI_SETCURSOR' put: 16r952!
ScintillaConstants at: 'SCI_SETDOCPOINTER' put: 16r936!
ScintillaConstants at: 'SCI_SETEDGECOLOUR' put: 16r93D!
ScintillaConstants at: 'SCI_SETEDGECOLUMN' put: 16r939!
ScintillaConstants at: 'SCI_SETEDGEMODE' put: 16r93B!
ScintillaConstants at: 'SCI_SETENDATLASTLINE' put: 16r8E5!
ScintillaConstants at: 'SCI_SETEOLMODE' put: 16r7EF!
ScintillaConstants at: 'SCI_SETFOCUS' put: 16r94C!
ScintillaConstants at: 'SCI_SETFOLDEXPANDED' put: 16r8B5!
ScintillaConstants at: 'SCI_SETFOLDFLAGS' put: 16r8B9!
ScintillaConstants at: 'SCI_SETFOLDLEVEL' put: 16r8AE!
ScintillaConstants at: 'SCI_SETFOLDMARGINCOLOUR' put: 16r8F2!
ScintillaConstants at: 'SCI_SETFOLDMARGINHICOLOUR' put: 16r8F3!
ScintillaConstants at: 'SCI_SETHIGHLIGHTGUIDE' put: 16r856!
ScintillaConstants at: 'SCI_SETHOTSPOTACTIVEBACK' put: 16r96B!
ScintillaConstants at: 'SCI_SETHOTSPOTACTIVEFORE' put: 16r96A!
ScintillaConstants at: 'SCI_SETHOTSPOTACTIVEUNDERLINE' put: 16r96C!
ScintillaConstants at: 'SCI_SETHOTSPOTSINGLELINE' put: 16r975!
ScintillaConstants at: 'SCI_SETHSCROLLBAR' put: 16r852!
ScintillaConstants at: 'SCI_SETINDENT' put: 16r84A!
ScintillaConstants at: 'SCI_SETINDENTATIONGUIDES' put: 16r854!
ScintillaConstants at: 'SCI_SETINDICATORCURRENT' put: 16r9C4!
ScintillaConstants at: 'SCI_SETINDICATORVALUE' put: 16r9C6!
ScintillaConstants at: 'SCI_SETKEYWORDS' put: 16rFA5!
ScintillaConstants at: 'SCI_SETLAYOUTCACHE' put: 16r8E0!
ScintillaConstants at: 'SCI_SETLENGTHFORENCODE' put: 16r990!
ScintillaConstants at: 'SCI_SETLEXER' put: 16rFA1!
ScintillaConstants at: 'SCI_SETLEXERLANGUAGE' put: 16rFA6!
ScintillaConstants at: 'SCI_SETLINEINDENTATION' put: 16r84E!
ScintillaConstants at: 'SCI_SETLINESTATE' put: 16r82C!
ScintillaConstants at: 'SCI_SETMARGINLEFT' put: 16r86B!
ScintillaConstants at: 'SCI_SETMARGINMASKN' put: 16r8C4!
ScintillaConstants at: 'SCI_SETMARGINRIGHT' put: 16r86D!
ScintillaConstants at: 'SCI_SETMARGINSENSITIVEN' put: 16r8C6!
ScintillaConstants at: 'SCI_SETMARGINTYPEN' put: 16r8C0!
ScintillaConstants at: 'SCI_SETMARGINWIDTHN' put: 16r8C2!
ScintillaConstants at: 'SCI_SETMODEVENTMASK' put: 16r937!
ScintillaConstants at: 'SCI_SETMOUSEDOWNCAPTURES' put: 16r950!
ScintillaConstants at: 'SCI_SETMOUSEDWELLTIME' put: 16r8D8!
ScintillaConstants at: 'SCI_SETOVERTYPE' put: 16r88A!
ScintillaConstants at: 'SCI_SETPASTECONVERTENDINGS' put: 16r9A3!
ScintillaConstants at: 'SCI_SETPOSITIONCACHE' put: 16r9D2!
ScintillaConstants at: 'SCI_SETPRINTCOLOURMODE' put: 16r864!
ScintillaConstants at: 'SCI_SETPRINTMAGNIFICATION' put: 16r862!
ScintillaConstants at: 'SCI_SETPRINTWRAPMODE' put: 16r966!
ScintillaConstants at: 'SCI_SETPROPERTY' put: 16rFA4!
ScintillaConstants at: 'SCI_SETREADONLY' put: 16r87B!
ScintillaConstants at: 'SCI_SETSAVEPOINT' put: 16r7DE!
ScintillaConstants at: 'SCI_SETSCROLLWIDTH' put: 16r8E2!
ScintillaConstants at: 'SCI_SETSCROLLWIDTHTRACKING' put: 16r9D4!
ScintillaConstants at: 'SCI_SETSEARCHFLAGS' put: 16r896!
ScintillaConstants at: 'SCI_SETSEL' put: 16r870!
ScintillaConstants at: 'SCI_SETSELALPHA' put: 16r9AE!
ScintillaConstants at: 'SCI_SETSELBACK' put: 16r814!
ScintillaConstants at: 'SCI_SETSELECTIONEND' put: 16r860!
ScintillaConstants at: 'SCI_SETSELECTIONMODE' put: 16r976!
ScintillaConstants at: 'SCI_SETSELECTIONSTART' put: 16r85E!
ScintillaConstants at: 'SCI_SETSELEOLFILLED' put: 16r9B0!
ScintillaConstants at: 'SCI_SETSELFORE' put: 16r813!
ScintillaConstants at: 'SCI_SETSTATUS' put: 16r94E!
ScintillaConstants at: 'SCI_SETSTYLEBITS' put: 16r82A!
ScintillaConstants at: 'SCI_SETSTYLING' put: 16r7F1!
ScintillaConstants at: 'SCI_SETSTYLINGEX' put: 16r819!
ScintillaConstants at: 'SCI_SETTABINDENTS' put: 16r8D4!
ScintillaConstants at: 'SCI_SETTABWIDTH' put: 16r7F4!
ScintillaConstants at: 'SCI_SETTARGETEND' put: 16r890!
ScintillaConstants at: 'SCI_SETTARGETSTART' put: 16r88E!
ScintillaConstants at: 'SCI_SETTEXT' put: 16r885!
ScintillaConstants at: 'SCI_SETTWOPHASEDRAW' put: 16r8EC!
ScintillaConstants at: 'SCI_SETUNDOCOLLECTION' put: 16r7DC!
ScintillaConstants at: 'SCI_SETUSEPALETTE' put: 16r7F7!
ScintillaConstants at: 'SCI_SETUSETABS' put: 16r84C!
ScintillaConstants at: 'SCI_SETVIEWEOL' put: 16r934!
ScintillaConstants at: 'SCI_SETVIEWWS' put: 16r7E5!
ScintillaConstants at: 'SCI_SETVISIBLEPOLICY' put: 16r95A!
ScintillaConstants at: 'SCI_SETVSCROLLBAR' put: 16r8E8!
ScintillaConstants at: 'SCI_SETWHITESPACEBACK' put: 16r825!
ScintillaConstants at: 'SCI_SETWHITESPACECHARS' put: 16r98B!
ScintillaConstants at: 'SCI_SETWHITESPACEFORE' put: 16r824!
ScintillaConstants at: 'SCI_SETWORDCHARS' put: 16r81D!
ScintillaConstants at: 'SCI_SETWRAPMODE' put: 16r8DC!
ScintillaConstants at: 'SCI_SETWRAPSTARTINDENT' put: 16r9A0!
ScintillaConstants at: 'SCI_SETWRAPVISUALFLAGS' put: 16r99C!
ScintillaConstants at: 'SCI_SETWRAPVISUALFLAGSLOCATION' put: 16r99E!
ScintillaConstants at: 'SCI_SETXCARETPOLICY' put: 16r962!
ScintillaConstants at: 'SCI_SETXOFFSET' put: 16r95D!
ScintillaConstants at: 'SCI_SETYCARETPOLICY' put: 16r963!
ScintillaConstants at: 'SCI_SETZOOM' put: 16r945!
ScintillaConstants at: 'SCI_SHOWLINES' put: 16r8B2!
ScintillaConstants at: 'SCI_START' put: 16r7D0!
ScintillaConstants at: 'SCI_STARTRECORD' put: 16rBB9!
ScintillaConstants at: 'SCI_STARTSTYLING' put: 16r7F0!
ScintillaConstants at: 'SCI_STOPRECORD' put: 16rBBA!
ScintillaConstants at: 'SCI_STUTTEREDPAGEDOWN' put: 16r985!
ScintillaConstants at: 'SCI_STUTTEREDPAGEDOWNEXTEND' put: 16r986!
ScintillaConstants at: 'SCI_STUTTEREDPAGEUP' put: 16r983!
ScintillaConstants at: 'SCI_STUTTEREDPAGEUPEXTEND' put: 16r984!
ScintillaConstants at: 'SCI_STYLECLEARALL' put: 16r802!
ScintillaConstants at: 'SCI_STYLEGETBACK' put: 16r9B2!
ScintillaConstants at: 'SCI_STYLEGETBOLD' put: 16r9B3!
ScintillaConstants at: 'SCI_STYLEGETCASE' put: 16r9B9!
ScintillaConstants at: 'SCI_STYLEGETCHANGEABLE' put: 16r9BC!
ScintillaConstants at: 'SCI_STYLEGETCHARACTERSET' put: 16r9BA!
ScintillaConstants at: 'SCI_STYLEGETEOLFILLED' put: 16r9B7!
ScintillaConstants at: 'SCI_STYLEGETFONT' put: 16r9B6!
ScintillaConstants at: 'SCI_STYLEGETFORE' put: 16r9B1!
ScintillaConstants at: 'SCI_STYLEGETHOTSPOT' put: 16r9BD!
ScintillaConstants at: 'SCI_STYLEGETITALIC' put: 16r9B4!
ScintillaConstants at: 'SCI_STYLEGETSIZE' put: 16r9B5!
ScintillaConstants at: 'SCI_STYLEGETUNDERLINE' put: 16r9B8!
ScintillaConstants at: 'SCI_STYLEGETVISIBLE' put: 16r9BB!
ScintillaConstants at: 'SCI_STYLERESETDEFAULT' put: 16r80A!
ScintillaConstants at: 'SCI_STYLESETBACK' put: 16r804!
ScintillaConstants at: 'SCI_STYLESETBOLD' put: 16r805!
ScintillaConstants at: 'SCI_STYLESETCASE' put: 16r80C!
ScintillaConstants at: 'SCI_STYLESETCHANGEABLE' put: 16r833!
ScintillaConstants at: 'SCI_STYLESETCHARACTERSET' put: 16r812!
ScintillaConstants at: 'SCI_STYLESETEOLFILLED' put: 16r809!
ScintillaConstants at: 'SCI_STYLESETFONT' put: 16r808!
ScintillaConstants at: 'SCI_STYLESETFORE' put: 16r803!
ScintillaConstants at: 'SCI_STYLESETHOTSPOT' put: 16r969!
ScintillaConstants at: 'SCI_STYLESETITALIC' put: 16r806!
ScintillaConstants at: 'SCI_STYLESETSIZE' put: 16r807!
ScintillaConstants at: 'SCI_STYLESETUNDERLINE' put: 16r80B!
ScintillaConstants at: 'SCI_STYLESETVISIBLE' put: 16r81A!
ScintillaConstants at: 'SCI_TAB' put: 16r917!
ScintillaConstants at: 'SCI_TARGETASUTF8' put: 16r98F!
ScintillaConstants at: 'SCI_TARGETFROMSELECTION' put: 16r8EF!
ScintillaConstants at: 'SCI_TEXTHEIGHT' put: 16r8E7!
ScintillaConstants at: 'SCI_TEXTWIDTH' put: 16r8E4!
ScintillaConstants at: 'SCI_TOGGLECARETSTICKY' put: 16r99B!
ScintillaConstants at: 'SCI_TOGGLEFOLD' put: 16r8B7!
ScintillaConstants at: 'SCI_UNDO' put: 16r880!
ScintillaConstants at: 'SCI_UPPERCASE' put: 16r925!
ScintillaConstants at: 'SCI_USEPOPUP' put: 16r943!
ScintillaConstants at: 'SCI_USERLISTSHOW' put: 16r845!
ScintillaConstants at: 'SCI_VCHOME' put: 16r91B!
ScintillaConstants at: 'SCI_VCHOMEEXTEND' put: 16r91C!
ScintillaConstants at: 'SCI_VCHOMERECTEXTEND' put: 16r97F!
ScintillaConstants at: 'SCI_VCHOMEWRAP' put: 16r995!
ScintillaConstants at: 'SCI_VCHOMEWRAPEXTEND' put: 16r996!
ScintillaConstants at: 'SCI_VISIBLEFROMDOCLINE' put: 16r8AC!
ScintillaConstants at: 'SCI_WORDENDPOSITION' put: 16r8DB!
ScintillaConstants at: 'SCI_WORDLEFT' put: 16r904!
ScintillaConstants at: 'SCI_WORDLEFTEND' put: 16r987!
ScintillaConstants at: 'SCI_WORDLEFTENDEXTEND' put: 16r988!
ScintillaConstants at: 'SCI_WORDLEFTEXTEND' put: 16r905!
ScintillaConstants at: 'SCI_WORDPARTLEFT' put: 16r956!
ScintillaConstants at: 'SCI_WORDPARTLEFTEXTEND' put: 16r957!
ScintillaConstants at: 'SCI_WORDPARTRIGHT' put: 16r958!
ScintillaConstants at: 'SCI_WORDPARTRIGHTEXTEND' put: 16r959!
ScintillaConstants at: 'SCI_WORDRIGHT' put: 16r906!
ScintillaConstants at: 'SCI_WORDRIGHTEND' put: 16r989!
ScintillaConstants at: 'SCI_WORDRIGHTENDEXTEND' put: 16r98A!
ScintillaConstants at: 'SCI_WORDRIGHTEXTEND' put: 16r907!
ScintillaConstants at: 'SCI_WORDSTARTPOSITION' put: 16r8DA!
ScintillaConstants at: 'SCI_WRAPCOUNT' put: 16r8BB!
ScintillaConstants at: 'SCI_ZOOMIN' put: 16r91D!
ScintillaConstants at: 'SCI_ZOOMOUT' put: 16r91E!
ScintillaConstants at: 'SCK_ADD' put: 16r136!
ScintillaConstants at: 'SCK_BACK' put: 16r8!
ScintillaConstants at: 'SCK_DELETE' put: 16r134!
ScintillaConstants at: 'SCK_DIVIDE' put: 16r138!
ScintillaConstants at: 'SCK_DOWN' put: 16r12C!
ScintillaConstants at: 'SCK_END' put: 16r131!
ScintillaConstants at: 'SCK_ESCAPE' put: 16r7!
ScintillaConstants at: 'SCK_HOME' put: 16r130!
ScintillaConstants at: 'SCK_INSERT' put: 16r135!
ScintillaConstants at: 'SCK_LEFT' put: 16r12E!
ScintillaConstants at: 'SCK_MENU' put: 16r13B!
ScintillaConstants at: 'SCK_NEXT' put: 16r133!
ScintillaConstants at: 'SCK_PRIOR' put: 16r132!
ScintillaConstants at: 'SCK_RETURN' put: 16rD!
ScintillaConstants at: 'SCK_RIGHT' put: 16r12F!
ScintillaConstants at: 'SCK_RWIN' put: 16r13A!
ScintillaConstants at: 'SCK_SUBTRACT' put: 16r137!
ScintillaConstants at: 'SCK_TAB' put: 16r9!
ScintillaConstants at: 'SCK_UP' put: 16r12D!
ScintillaConstants at: 'SCK_WIN' put: 16r139!
ScintillaConstants at: 'SCLEX_ABAQUS' put: 16r54!
ScintillaConstants at: 'SCLEX_ADA' put: 16r14!
ScintillaConstants at: 'SCLEX_APDL' put: 16r3D!
ScintillaConstants at: 'SCLEX_ASM' put: 16r22!
ScintillaConstants at: 'SCLEX_ASN1' put: 16r3F!
ScintillaConstants at: 'SCLEX_ASP' put: 16r1D!
ScintillaConstants at: 'SCLEX_ASYMPTOTE' put: 16r55!
ScintillaConstants at: 'SCLEX_AU3' put: 16r3C!
ScintillaConstants at: 'SCLEX_AUTOMATIC' put: 16r3E8!
ScintillaConstants at: 'SCLEX_AVE' put: 16r13!
ScintillaConstants at: 'SCLEX_BAAN' put: 16r1F!
ScintillaConstants at: 'SCLEX_BASH' put: 16r3E!
ScintillaConstants at: 'SCLEX_BATCH' put: 16rC!
ScintillaConstants at: 'SCLEX_BLITZBASIC' put: 16r42!
ScintillaConstants at: 'SCLEX_BULLANT' put: 16r1B!
ScintillaConstants at: 'SCLEX_CAML' put: 16r41!
ScintillaConstants at: 'SCLEX_CLW' put: 16r2D!
ScintillaConstants at: 'SCLEX_CLWNOCASE' put: 16r2E!
ScintillaConstants at: 'SCLEX_CMAKE' put: 16r50!
ScintillaConstants at: 'SCLEX_CONF' put: 16r11!
ScintillaConstants at: 'SCLEX_CONTAINER' put: 16r0!
ScintillaConstants at: 'SCLEX_CPP' put: 16r3!
ScintillaConstants at: 'SCLEX_CPPNOCASE' put: 16r23!
ScintillaConstants at: 'SCLEX_CSOUND' put: 16r4A!
ScintillaConstants at: 'SCLEX_CSS' put: 16r26!
ScintillaConstants at: 'SCLEX_D' put: 16r4F!
ScintillaConstants at: 'SCLEX_DIFF' put: 16r10!
ScintillaConstants at: 'SCLEX_EIFFEL' put: 16r17!
ScintillaConstants at: 'SCLEX_EIFFELKW' put: 16r18!
ScintillaConstants at: 'SCLEX_ERLANG' put: 16r35!
ScintillaConstants at: 'SCLEX_ERRORLIST' put: 16rA!
ScintillaConstants at: 'SCLEX_ESCRIPT' put: 16r29!
ScintillaConstants at: 'SCLEX_F77' put: 16r25!
ScintillaConstants at: 'SCLEX_FLAGSHIP' put: 16r49!
ScintillaConstants at: 'SCLEX_FORTH' put: 16r34!
ScintillaConstants at: 'SCLEX_FORTRAN' put: 16r24!
ScintillaConstants at: 'SCLEX_FREEBASIC' put: 16r4B!
ScintillaConstants at: 'SCLEX_GAP' put: 16r51!
ScintillaConstants at: 'SCLEX_GUI4CLI' put: 16r3A!
ScintillaConstants at: 'SCLEX_HASKELL' put: 16r44!
ScintillaConstants at: 'SCLEX_HTML' put: 16r4!
ScintillaConstants at: 'SCLEX_INNOSETUP' put: 16r4C!
ScintillaConstants at: 'SCLEX_KIX' put: 16r39!
ScintillaConstants at: 'SCLEX_LATEX' put: 16rE!
ScintillaConstants at: 'SCLEX_LISP' put: 16r15!
ScintillaConstants at: 'SCLEX_LOT' put: 16r2F!
ScintillaConstants at: 'SCLEX_LOUT' put: 16r28!
ScintillaConstants at: 'SCLEX_LUA' put: 16rF!
ScintillaConstants at: 'SCLEX_MAKEFILE' put: 16rB!
ScintillaConstants at: 'SCLEX_MATLAB' put: 16r20!
ScintillaConstants at: 'SCLEX_METAPOST' put: 16r32!
ScintillaConstants at: 'SCLEX_MMIXAL' put: 16r2C!
ScintillaConstants at: 'SCLEX_MSSQL' put: 16r37!
ScintillaConstants at: 'SCLEX_NNCRONTAB' put: 16r1A!
ScintillaConstants at: 'SCLEX_NSIS' put: 16r2B!
ScintillaConstants at: 'SCLEX_NULL' put: 16r1!
ScintillaConstants at: 'SCLEX_OCTAVE' put: 16r36!
ScintillaConstants at: 'SCLEX_OPAL' put: 16r4D!
ScintillaConstants at: 'SCLEX_PASCAL' put: 16r12!
ScintillaConstants at: 'SCLEX_PERL' put: 16r6!
ScintillaConstants at: 'SCLEX_PHP' put: 16r1E!
ScintillaConstants at: 'SCLEX_PHPSCRIPT' put: 16r45!
ScintillaConstants at: 'SCLEX_PLM' put: 16r52!
ScintillaConstants at: 'SCLEX_POV' put: 16r27!
ScintillaConstants at: 'SCLEX_POWERBASIC' put: 16r33!
ScintillaConstants at: 'SCLEX_PROGRESS' put: 16r53!
ScintillaConstants at: 'SCLEX_PROPERTIES' put: 16r9!
ScintillaConstants at: 'SCLEX_PS' put: 16r2A!
ScintillaConstants at: 'SCLEX_PUREBASIC' put: 16r43!
ScintillaConstants at: 'SCLEX_PYTHON' put: 16r2!
ScintillaConstants at: 'SCLEX_R' put: 16r56!
ScintillaConstants at: 'SCLEX_REBOL' put: 16r47!
ScintillaConstants at: 'SCLEX_RUBY' put: 16r16!
ScintillaConstants at: 'SCLEX_SCRIPTOL' put: 16r21!
ScintillaConstants at: 'SCLEX_SMALLTALK' put: 16r48!
ScintillaConstants at: 'SCLEX_SPECMAN' put: 16r3B!
ScintillaConstants at: 'SCLEX_SPICE' put: 16r4E!
ScintillaConstants at: 'SCLEX_SQL' put: 16r7!
ScintillaConstants at: 'SCLEX_TADS3' put: 16r46!
ScintillaConstants at: 'SCLEX_TCL' put: 16r19!
ScintillaConstants at: 'SCLEX_TEX' put: 16r31!
ScintillaConstants at: 'SCLEX_VB' put: 16r8!
ScintillaConstants at: 'SCLEX_VBSCRIPT' put: 16r1C!
ScintillaConstants at: 'SCLEX_VERILOG' put: 16r38!
ScintillaConstants at: 'SCLEX_VHDL' put: 16r40!
ScintillaConstants at: 'SCLEX_XCODE' put: 16rD!
ScintillaConstants at: 'SCLEX_XML' put: 16r5!
ScintillaConstants at: 'SCLEX_YAML' put: 16r30!
ScintillaConstants at: 'SCMOD_ALT' put: 16r4!
ScintillaConstants at: 'SCMOD_CTRL' put: 16r2!
ScintillaConstants at: 'SCMOD_NORM' put: 16r0!
ScintillaConstants at: 'SCMOD_SHIFT' put: 16r1!
ScintillaConstants at: 'SCN_AUTOCSELECTION' put: 16r7E6!
ScintillaConstants at: 'SCN_CALLTIPCLICK' put: 16r7E5!
ScintillaConstants at: 'SCN_CHARADDED' put: 16r7D1!
ScintillaConstants at: 'SCN_CHECKBRACE' put: 16r7D7!
ScintillaConstants at: 'SCN_DOUBLECLICK' put: 16r7D6!
ScintillaConstants at: 'SCN_DWELLEND' put: 16r7E1!
ScintillaConstants at: 'SCN_DWELLSTART' put: 16r7E0!
ScintillaConstants at: 'SCN_HOTSPOTCLICK' put: 16r7E3!
ScintillaConstants at: 'SCN_HOTSPOTDOUBLECLICK' put: 16r7E4!
ScintillaConstants at: 'SCN_INDICATORCLICK' put: 16r7E7!
ScintillaConstants at: 'SCN_INDICATORRELEASE' put: 16r7E8!
ScintillaConstants at: 'SCN_KEY' put: 16r7D5!
ScintillaConstants at: 'SCN_MACRORECORD' put: 16r7D9!
ScintillaConstants at: 'SCN_MARGINCLICK' put: 16r7DA!
ScintillaConstants at: 'SCN_MODIFIED' put: 16r7D8!
ScintillaConstants at: 'SCN_MODIFYATTEMPTRO' put: 16r7D4!
ScintillaConstants at: 'SCN_NEEDSHOWN' put: 16r7DB!
ScintillaConstants at: 'SCN_PAINTED' put: 16r7DD!
ScintillaConstants at: 'SCN_POSCHANGED' put: 16r7DC!
ScintillaConstants at: 'SCN_SAVEPOINTLEFT' put: 16r7D3!
ScintillaConstants at: 'SCN_SAVEPOINTREACHED' put: 16r7D2!
ScintillaConstants at: 'SCN_STYLENEEDED' put: 16r7D0!
ScintillaConstants at: 'SCN_UPDATEUI' put: 16r7D7!
ScintillaConstants at: 'SCN_URIDROPPED' put: 16r7DF!
ScintillaConstants at: 'SCN_USERLISTSELECTION' put: 16r7DE!
ScintillaConstants at: 'SCN_ZOOM' put: 16r7E2!
ScintillaConstants at: 'SCWS_INVISIBLE' put: 16r0!
ScintillaConstants at: 'SCWS_VISIBLEAFTERINDENT' put: 16r2!
ScintillaConstants at: 'SCWS_VISIBLEALWAYS' put: 16r1!
ScintillaConstants at: 'STYLE_BRACEBAD' put: 16r23!
ScintillaConstants at: 'STYLE_BRACELIGHT' put: 16r22!
ScintillaConstants at: 'STYLE_CALLTIP' put: 16r26!
ScintillaConstants at: 'STYLE_CONTROLCHAR' put: 16r24!
ScintillaConstants at: 'STYLE_DEFAULT' put: 16r20!
ScintillaConstants at: 'STYLE_INDENTGUIDE' put: 16r25!
ScintillaConstants at: 'STYLE_LASTPREDEFINED' put: 16r27!
ScintillaConstants at: 'STYLE_LINENUMBER' put: 16r21!
ScintillaConstants at: 'STYLE_MAX' put: 16rFF!
ScintillaConstants at: 'STYLE_NORMAL' put: 16r0!
ScintillaConstants at: 'VISIBLE_SLOP' put: 16r1!
ScintillaConstants at: 'VISIBLE_STRICT' put: 16r4!
ScintillaConstants shrink!

"Classes"!

ScintillaAttribute guid: (GUID fromString: '{B73F94FE-0BB9-4C23-AEE5-E77B3CF408AF}')!
ScintillaAttribute comment: 'ScintillaAttribute is the abstract superclass of objects that wrap various attributes of <ScintillaView>s. It provides a generic mechanism for setting the attribute value into the underlying control.

Instance Variables:
	id		<integer>. Identifier of the attribute instance.

Class Instance Variables:
	setMessages	<IdentityDictionary>



'!
!ScintillaAttribute categoriesForClass!MVP-Views-Support! !
!ScintillaAttribute methodsFor!

= aScintillaAttribute 
	^self class == aScintillaAttribute and: [self name = aScintillaAttribute name]!

applyToView: aScintillaView 
	self view: aScintillaView.
	self attributes 
		ifNotNil: 
			[:map | 
			map keysAndValuesDo: 
					[:eachGetter :eachSetMessage | 
					(self perform: eachGetter) 
						ifNotNil: 
							[:value | 
							aScintillaView 
								sendMessage: eachSetMessage
								wParam: self id
								lParam: value asDword]]]!

applyToView: aScintillaView at: anInteger 
	self id: anInteger.
	self applyToView: aScintillaView!

asParameter
	^self id!

attributes
	^self class attributes!

hash
	^self name hash!

id
	^id!

id: anInteger 
	id := anInteger!

name
	^self id!

printableAttributes
	^self attributes keys!

printAttributesOn: aStream 
	self printableAttributes do: 
			[:each | 
			(self perform: each) 
				ifNotNil: 
					[:attrib | 
					aStream
						nextPutAll: ', ';
						nextPutAll: each;
						nextPutAll: '=';
						print: attrib]]!

printOn: aStream 
	"Append a short textual description of the receiver to aStream."

	aStream
		basicPrint: self;
		nextPut: $(;
		print: self id;
		nextPut: $:;
		print: self name.
	self printAttributesOn: aStream.
	aStream nextPut: $)!

storeableAttributes
	^(Array with: #id) , self class attributes keys asArray!

storeOn: aStream 
	"Append to the <puttableStream> argument an expression which when evaluated will answer a
	collection similar to the receiver."

	aStream
		nextPutAll: '((';
		print: self class;
		nextPutAll: ' new)';
		space.
	self storeableAttributes do: 
			[:each | 
			(self perform: each) 
				ifNotNil: 
					[:attrib | 
					aStream
						display: each;
						nextPut: $:;
						space.
					attrib storeOn: aStream.
					aStream
						nextPut: $;;
						space]].
	aStream
		display: #yourself;
		nextPut: $)!

updateViewAttribute: aSymbol 
	self view 
		ifNotNil: 
			[:scintilla | 
			scintilla 
				sendMessage: (self attributes at: aSymbol)
				wParam: self id
				lParam: (self perform: aSymbol) asDword]!

view
	^nil!

view: aScintillaView 
	^self! !
!ScintillaAttribute categoriesFor: #=!comparing!public! !
!ScintillaAttribute categoriesFor: #applyToView:!helpers!private! !
!ScintillaAttribute categoriesFor: #applyToView:at:!helpers!private! !
!ScintillaAttribute categoriesFor: #asParameter!converting!public! !
!ScintillaAttribute categoriesFor: #attributes!constants!private! !
!ScintillaAttribute categoriesFor: #hash!comparing!public! !
!ScintillaAttribute categoriesFor: #id!accessing!private! !
!ScintillaAttribute categoriesFor: #id:!accessing!private! !
!ScintillaAttribute categoriesFor: #name!accessing!public! !
!ScintillaAttribute categoriesFor: #printableAttributes!development!helpers!printing!private! !
!ScintillaAttribute categoriesFor: #printAttributesOn:!development!helpers!printing!private! !
!ScintillaAttribute categoriesFor: #printOn:!development!printing!public! !
!ScintillaAttribute categoriesFor: #storeableAttributes!development!printing!private! !
!ScintillaAttribute categoriesFor: #storeOn:!development!printing!public! !
!ScintillaAttribute categoriesFor: #updateViewAttribute:!helpers!private! !
!ScintillaAttribute categoriesFor: #view!accessing!private! !
!ScintillaAttribute categoriesFor: #view:!accessing!private! !

!ScintillaAttribute class methodsFor!

attributes
	^attributes!

icon
	^ScintillaView icon!

sortByIdBlock
	^[:a :b | a id ifNil: [true] ifNotNil: [:aId | b id ifNil: [false] ifNotNil: [:bId | aId <= bId]]]! !
!ScintillaAttribute class categoriesFor: #attributes!constants!private! !
!ScintillaAttribute class categoriesFor: #icon!constants!development!public! !
!ScintillaAttribute class categoriesFor: #sortByIdBlock!constants!public! !

ScintillaIndicator guid: (GUID fromString: '{D8F9D545-39F0-4F15-B405-D14DFE355B3D}')!
ScintillaIndicator comment: 'ScintillaIndicators represent the indicators displayed as various styles of underlining in a <ScintillaView>.

Instance Variables:
	type	<Integer>. Id, normally in the range 1..3, of the corresponding <ScintillaIndicatorDefinition>.
	range	<Interval> in the text which is so highlighted.
	tag		<Object>. Any user-defined object to be associated with the indicator, e.g. for bubble help.'!
!ScintillaIndicator categoriesForClass!MVP-Views-Support! !
!ScintillaIndicator methodsFor!

printOn: aStream 
	aStream
		basicPrint: self;
		nextPut: $(;
		display: self type;
		nextPutAll: ': ';
		display: self range;
		nextPut: $)!

range
	"Answer the <Interval> of text highlighted by this indicator."

	^range!

setStyleName: aSymbolOrInteger range: anInterval tag: anObject 
	styleName := aSymbolOrInteger.
	range := anInterval.
	tag := anObject!

styleName
	"Answer the symbolic name of the style of indicator to be used to represent the receiver in
	the <ScintillaView>. It is expected that the view will have a <ScintillaIndicatorStyle>
	defined of the same name that specifies the style of indicator drawn, it's colour, etc. If
	there is no such indicator style, then the standard indicator 0 is used. The name could also
	be an integer id from 0 to 7 if identifying one of the styles reserved for use by lexers, in
	which case no corresponding style need be defined in the view."

	^styleName!

tag
	"Answer a user-defined <Object> associated with the indicator. This could be used, for
	example, to provide bubble help explaining the indicator."

	^tag!

type
	#deprecated.
	^self styleName! !
!ScintillaIndicator categoriesFor: #printOn:!printing!public! !
!ScintillaIndicator categoriesFor: #range!accessing!public! !
!ScintillaIndicator categoriesFor: #setStyleName:range:tag:!initializing!private! !
!ScintillaIndicator categoriesFor: #styleName!public! !
!ScintillaIndicator categoriesFor: #tag!accessing!public! !
!ScintillaIndicator categoriesFor: #type!accessing!public! !

!ScintillaIndicator class methodsFor!

icon
	^ScintillaView icon!

styleName: aSymbolOrInteger range: anInterval 
	^self 
		styleName: aSymbolOrInteger
		range: anInterval
		tag: nil!

styleName: aSymbolOrInteger range: anInterval tag: anObject 
	^(self new)
		setStyleName: aSymbolOrInteger
			range: anInterval
			tag: anObject;
		yourself!

type: anInteger range: anInterval 
	#deprecated.
	^self styleName: anInteger range: anInterval!

type: anInteger range: anInterval tag: anObject 
	#deprecated.
	^self 
		styleName: anInteger
		range: anInterval
		tag: anObject! !
!ScintillaIndicator class categoriesFor: #icon!constants!development!public! !
!ScintillaIndicator class categoriesFor: #styleName:range:!instance creation!public! !
!ScintillaIndicator class categoriesFor: #styleName:range:tag:!instance creation!public! !
!ScintillaIndicator class categoriesFor: #type:range:!instance creation!public! !
!ScintillaIndicator class categoriesFor: #type:range:tag:!instance creation!public! !

ScintillaKeyBinding guid: (GUID fromString: '{BF35C065-FEF0-4AA1-8DCD-E40E73C5447A}')!
ScintillaKeyBinding comment: 'ScintillaKeyBinding is a support class for the ScintillaView, used to represent the key bindings defined in a particular instance. The control itself defines a large set of default mappings, but these can be replaced and/or augmented.

Instance Variables:
	acceleratorKey		<integer>. Dolphin format accelerator key combination (combines a VK_xxx, and FALT/FSHIFT/FCONTROL flags)
	message			<iInteger> from the valid SCI_xxx messages that can be used as commands

Class Variables:
	VirtualKeyMap		<Array> of <integer>s used to map Windows VK_xxx codes to Scintilla SCK_xxx codes.
	Commands		<IdentityDictionary> mapping Dolphin accelerator key codes to command symbols.

'!
!ScintillaKeyBinding categoriesForClass!MVP-Views-Support! !
!ScintillaKeyBinding methodsFor!

= comparand 
	^self species == comparand species 
		and: [self acceleratorKey = comparand acceleratorKey and: [self message = comparand message]]!

acceleratorKey
	"Answers the <integer> accelerator key code for the receiver, or zero if there is none."

	^acceleratorKey!

acceleratorKey: code
	"Set the receiver's accelerator key code to the <integer> argument, code."

	acceleratorKey := code!

acceleratorKeyString
	"Answers the <readableString> accelerator key string for the receiver, 
	or an empty <String> if there is none."

	^AcceleratorTable keyStringFromCode: self acceleratorKey!

acceleratorKeyString: keys
	"Sets the accelerator key to the key code generated from parsing the <readableString> 
	accelerator key description, keys. The string is assumed to be in English."

	self acceleratorKey: (AcceleratorTable keyCodeFromString: keys)!

commandSymbol
	"Answers the <Symbol>ic command forwarded to a command target when the receiver's
	accelerator key combination is pressed."

	^message ifNotNil: [Commands at: message]!

commandSymbol: aSymbol 
	"Set the <Symbol>ic command forwarded to a command target when the receiver's
	accelerator key combination is pressed."

	message := Commands keyAtValue: aSymbol!

displayOn: target 
	target
		display: self acceleratorKeyString;
		nextPutAll: ' -> ';
		print: self commandSymbol!

hash
	^acceleratorKey hash!

message
	^message!

message: anInteger 
	message := anInteger!

printOn: target 
	super printOn: target.
	target
		nextPut: $(;
		display: self;
		nextPut: $)!

scintillaKeyCode
	| codes |
	codes := AcceleratorTable splitKeyCode: self acceleratorKey.
	^(self translateVirtualKey: codes first) + ((codes last bitAnd: 16r1C) << 14)!

translateVirtualKey: vkInteger 
	"Private - Translate from a Windows Virtual Key code (VK_xxx) to the corresponding Scintilla
	key code (SCK_xxx)"

	^VirtualKeyMap at: (vkInteger bitAnd: 16rFF) + 1! !
!ScintillaKeyBinding categoriesFor: #=!comparing!public! !
!ScintillaKeyBinding categoriesFor: #acceleratorKey!accessing!public! !
!ScintillaKeyBinding categoriesFor: #acceleratorKey:!accessing!public! !
!ScintillaKeyBinding categoriesFor: #acceleratorKeyString!accessing!public! !
!ScintillaKeyBinding categoriesFor: #acceleratorKeyString:!accessing!public! !
!ScintillaKeyBinding categoriesFor: #commandSymbol!accessing!public! !
!ScintillaKeyBinding categoriesFor: #commandSymbol:!accessing!public! !
!ScintillaKeyBinding categoriesFor: #displayOn:!printing!public! !
!ScintillaKeyBinding categoriesFor: #hash!comparing!public! !
!ScintillaKeyBinding categoriesFor: #message!accessing!private! !
!ScintillaKeyBinding categoriesFor: #message:!accessing!private! !
!ScintillaKeyBinding categoriesFor: #printOn:!printing!public! !
!ScintillaKeyBinding categoriesFor: #scintillaKeyCode!accessing!private! !
!ScintillaKeyBinding categoriesFor: #translateVirtualKey:!helpers!private! !

!ScintillaKeyBinding class methodsFor!

icon
	^ScintillaView icon!

initialize
	"Private - Initialize the receiver's class variables, etc
		self initialize
	"

	self initializeVirtualKeyMap.
	self initializeCommands!

initializeCommands
	Commands := (IdentityDictionary new)
				at: SCI_BACKTAB put: #unindent;
				at: SCI_CANCEL put: #cancelModes;
				at: SCI_CHARLEFT put: #moveLeft;
				at: SCI_CHARLEFTEXTEND put: #extendLeft;
				at: SCI_CHARLEFTRECTEXTEND put: #extendRectangleLeft;
				at: SCI_CHARRIGHT put: #moveRight;
				at: SCI_CHARRIGHTEXTEND put: #extendRight;
				at: SCI_CHARRIGHTRECTEXTEND put: #extendRectangleRight;
				at: SCI_CLEAR put: #basicClearSelection;
				at: SCI_CLEARALL put: #clearAll;
				at: SCI_COPY put: #copySelection;
				at: SCI_CUT put: #cutSelection;
				at: SCI_DELETEBACK put: #backspace;
				at: SCI_DELETEBACKNOTLINE put: #backspaceNoLine;
				at: SCI_DELLINELEFT put: #deleteToStartOfLine;
				at: SCI_DELLINERIGHT put: #deleteToEndOfLine;
				at: SCI_DELWORDLEFT put: #deleteToStartOfWord;
				at: SCI_DELWORDRIGHT put: #deleteToNextWord;
				at: SCI_DOCUMENTEND put: #moveToEndOfDocument;
				at: SCI_DOCUMENTENDEXTEND put: #extendToEndOfDocument;
				at: SCI_DOCUMENTSTART put: #moveToStartOfDocument;
				at: SCI_DOCUMENTSTARTEXTEND put: #extendToStartOfDocument;
				at: SCI_EDITTOGGLEOVERTYPE put: #toggleOvertype;
				at: SCI_FORMFEED put: #formFeed;
				at: SCI_HOME put: #moveToStartOfLine;
				at: SCI_HOMEDISPLAY put: #moveToStartOfDisplayLine;
				at: SCI_HOMEDISPLAYEXTEND put: #extendToStartOfDisplayLine;
				at: SCI_HOMEEXTEND put: #extendToStartOfLine;
				at: SCI_HOMERECTEXTEND put: #extendRectangleToStartOfLine;
				at: SCI_HOMEWRAP put: #moveToStartOfWrappedLine;
				at: SCI_HOMEWRAPEXTEND put: #extendToStartOfWrappedLine;
				at: SCI_LINECOPY put: #copyLine;
				at: SCI_LINECUT put: #cutLine;
				at: SCI_LINEDELETE put: #deleteLine;
				at: SCI_LINEDOWN put: #moveDown;
				at: SCI_LINEDOWNEXTEND put: #extendDown;
				at: SCI_LINEDOWNRECTEXTEND put: #extendRectangleDown;
				at: SCI_LINEDUPLICATE put: #duplicateLine;
				at: SCI_LINEEND put: #moveToEndOfLine;
				at: SCI_LINEENDDISPLAY put: #moveToEndOfDisplayLine;
				at: SCI_LINEENDDISPLAYEXTEND put: #extendToEndOfDisplayLine;
				at: SCI_LINEENDEXTEND put: #extendToEndOfLine;
				at: SCI_LINEENDRECTEXTEND put: #extendRectangleToEndOfLine;
				at: SCI_LINEENDWRAP put: #moveToEndOfWrappedLine;
				at: SCI_LINEENDWRAPEXTEND put: #extendToEndOfWrappedLine;
				at: SCI_LINESCROLLDOWN put: #scrollDown;
				at: SCI_LINESCROLLUP put: #scrollUp;
				at: SCI_LINESJOIN put: #joinTarget;
				at: SCI_LINETRANSPOSE put: #twiddleLines;
				at: SCI_LINEUP put: #moveUp;
				at: SCI_LINEUPEXTEND put: #extendUp;
				at: SCI_LINEUPRECTEXTEND put: #extendRectangleUp;
				at: SCI_LOWERCASE put: #convertToLowercase;
				at: SCI_MOVECARETINSIDEVIEW put: #moveCaretInsideView;
				at: SCI_NEWLINE put: #newLine;
				at: SCI_PAGEDOWN put: #movePageDown;
				at: SCI_PAGEDOWNEXTEND put: #extendPageDown;
				at: SCI_PAGEDOWNRECTEXTEND put: #extendRectanglePageDown;
				at: SCI_PAGEUP put: #movePageUp;
				at: SCI_PAGEUPEXTEND put: #extendPageUp;
				at: SCI_PAGEUPRECTEXTEND put: #extendRectanglePageUp;
				at: SCI_PARADOWN put: #moveParaDown;
				at: SCI_PARADOWNEXTEND put: #extendParaDown;
				at: SCI_PARAUP put: #moveParaUp;
				at: SCI_PARAUPEXTEND put: #extendParaUp;
				at: SCI_PASTE put: #pasteClipboard;
				at: SCI_REDO put: #redo;
				at: SCI_SEARCHANCHOR put: #sciSearchAnchor;
				at: SCI_SELECTALL put: #basicSelectAll;
				at: SCI_SETSAVEPOINT put: #sciSetSavePoint;
				at: SCI_STARTRECORD put: #startRecording;
				at: SCI_STOPRECORD put: #stopRecording;
				at: SCI_STUTTEREDPAGEDOWN put: #moveStutteredPageDown;
				at: SCI_STUTTEREDPAGEDOWNEXTEND put: #extendStutteredPageDown;
				at: SCI_STUTTEREDPAGEUP put: #moveStutteredPageUp;
				at: SCI_STUTTEREDPAGEUPEXTEND put: #extendStutteredPageUp;
				at: SCI_TAB put: #indent;
				at: SCI_TARGETFROMSELECTION put: #setTargetRangeFromSelection;
				at: SCI_TOGGLECARETSTICKY put: #sciToggleCaretSticky;
				at: SCI_UNDO put: #basicUndo;
				at: SCI_UPPERCASE put: #convertToUppercase;
				at: SCI_VCHOME put: #moveToVcHome;
				at: SCI_VCHOMEEXTEND put: #extendToVcHome;
				at: SCI_VCHOMERECTEXTEND put: #extendRectangleToVcHome;
				at: SCI_VCHOMEWRAP put: #moveToWrappedVcHome;
				at: SCI_VCHOMEWRAPEXTEND put: #extendToWrappedVcHome;
				at: SCI_WORDLEFT put: #moveToStartOfWord;
				at: SCI_WORDLEFTEND put: #moveToEndOfPreviousWord;
				at: SCI_WORDLEFTENDEXTEND put: #extendToEndOfPreviousWord;
				at: SCI_WORDLEFTEXTEND put: #extendToStartOfWord;
				at: SCI_WORDPARTLEFT put: #moveToStartOfWordPart;
				at: SCI_WORDPARTLEFTEXTEND put: #extendToStartOfWordPart;
				at: SCI_WORDPARTRIGHT put: #moveToEndOfWordPart;
				at: SCI_WORDPARTRIGHTEXTEND put: #extendToEndOfWordPart;
				at: SCI_WORDRIGHT put: #moveToEndOfWord;
				at: SCI_WORDRIGHTEND put: #moveToEndOfNextWord;
				at: SCI_WORDRIGHTENDEXTEND put: #extendToEndOfNextWord;
				at: SCI_WORDRIGHTEXTEND put: #extendToEndOfWord;
				at: SCI_ZOOMIN put: #zoomIn;
				at: SCI_ZOOMOUT put: #zoomOut;
				at: SCI_SETZOOM put: #resetZoom;
				at: SCI_NULL put: #yourself;
				shrink;
				isImmutable: true;
				yourself!

initializeVirtualKeyMap
	VirtualKeyMap := (0 to: 255) collect: [:each | each].
	VirtualKeyMap
		at: VK_DOWN + 1 put: SCK_DOWN;
		at: VK_UP + 1 put: SCK_UP;
		at: VK_LEFT + 1 put: SCK_LEFT;
		at: VK_RIGHT + 1 put: SCK_RIGHT;
		at: VK_HOME + 1 put: SCK_HOME;
		at: VK_END + 1 put: SCK_END;
		at: VK_PRIOR + 1 put: SCK_PRIOR;
		at: VK_NEXT + 1 put: SCK_NEXT;
		at: VK_DELETE + 1 put: SCK_DELETE;
		at: VK_INSERT + 1 put: SCK_INSERT;
		at: VK_ESCAPE + 1 put: SCK_ESCAPE;
		at: VK_BACK + 1 put: SCK_BACK;
		at: VK_TAB + 1 put: SCK_TAB;
		at: VK_RETURN + 1 put: SCK_RETURN;
		at: VK_ADD + 1 put: SCK_ADD;
		at: VK_SUBTRACT + 1 put: SCK_SUBTRACT;
		at: VK_DIVIDE + 1 put: SCK_DIVIDE;
		at: VK_MENU + 1 put: SCK_MENU;
		at: VK_LWIN + 1 put: SCK_WIN;
		at: VK_RWIN + 1 put: SCK_RWIN;
		at: VK_OEM_2 + 1 put: $/ codePoint;
		at: VK_OEM_3 + 1 put: $` codePoint;
		at: VK_OEM_4 + 1 put: $[ codePoint;
		at: VK_OEM_5 + 1 put: $\ codePoint;
		at: VK_OEM_6 + 1 put: $] codePoint;
		isImmutable: true!

new
	^self newAcceleratorKey: 0 message: SCI_NULL!

newAcceleratorKey: keyInteger message: sciInteger 
	^(self basicNew)
		acceleratorKey: keyInteger;
		message: sciInteger;
		yourself! !
!ScintillaKeyBinding class categoriesFor: #icon!constants!development!public! !
!ScintillaKeyBinding class categoriesFor: #initialize!development!initializing!private! !
!ScintillaKeyBinding class categoriesFor: #initializeCommands!**auto generated**!development!private!scintilla interface! !
!ScintillaKeyBinding class categoriesFor: #initializeVirtualKeyMap!development!initializing!private! !
!ScintillaKeyBinding class categoriesFor: #new!instance creation!public! !
!ScintillaKeyBinding class categoriesFor: #newAcceleratorKey:message:!instance creation!public! !

ScintillaMarker guid: (GUID fromString: '{2AAD3A52-111C-4781-B79B-22F044E4A6F7}')!
ScintillaMarker comment: 'ScintillaMarkers represent the visible ''markers'' displayed in the margins of <ScintillaView>s.

Instance Variables:
	view		<ScintillaView>. View in which currently installed, or nil if inactive.
	definition		<ScintillaMarkerDefinition>. "Type" of this marker (defines glyph, etc).
	line		<integer>. Line number on which the marker is displayed.
	handle		<integer>. Handle allocated by Scintilla to uniquely identify the marker.

'!
!ScintillaMarker categoriesForClass!MVP-Views-Support! !
!ScintillaMarker methodsFor!

addToView: aScintillaView 
	"Add this marker to the view at its currently recorded line"

	view := aScintillaView.
	handle := view 
				sendMessage: SCI_MARKERADD
				wParam: line - 1
				lParam: self definition id!

cacheCurrentLine
	"Set the line number stored in the receiver to the current line of the associated marker."

	line := self currentLine!

currentLine
	"Answer the one-based <integer> line number with which this marker is currently associated
	(the marker may have moved if the text has been edited such that lines have been shuffled)."

	^view isNil 
		ifTrue: [line]
		ifFalse: [(view sendMessage: SCI_MARKERLINEFROMHANDLE wParam: handle) + 1]!

currentPosition
	"Answer the one-based <integer> character position of the start of the line on with which this
	marker is currently associated."

	^view positionAtLine: self currentLine!

currentRange
	"Answer the <Interval> of source positions occupied by the line currently marked
	by the receiver."

	^view lineRange: self currentLine!

definition
	^definition!

handle
	^handle!

line
	"Answer the one-based <integer> line number with which this marker was originally associated.
	Note that the marker may have moved if the text has been edited causing lines to be shuffled."

	^line!

removedFromView
	view := handle := nil!

removeFromView
	view isNil ifFalse: [view removeMarker: self]!

setDefinition: aScintillaMarkerDefinition line: anInteger 
	definition := aScintillaMarkerDefinition.
	line := anInteger.
	handle := nil!

type
	^definition name!

view
	^view!

view: anObject
	view := anObject! !
!ScintillaMarker categoriesFor: #addToView:!helpers!public! !
!ScintillaMarker categoriesFor: #cacheCurrentLine!accessing!public! !
!ScintillaMarker categoriesFor: #currentLine!accessing!public! !
!ScintillaMarker categoriesFor: #currentPosition!accessing!public! !
!ScintillaMarker categoriesFor: #currentRange!accessing!public! !
!ScintillaMarker categoriesFor: #definition!helpers!public! !
!ScintillaMarker categoriesFor: #handle!accessing!private! !
!ScintillaMarker categoriesFor: #line!accessing!public! !
!ScintillaMarker categoriesFor: #removedFromView!private!removing! !
!ScintillaMarker categoriesFor: #removeFromView!public!removing! !
!ScintillaMarker categoriesFor: #setDefinition:line:!initializing!private! !
!ScintillaMarker categoriesFor: #type!accessing!public! !
!ScintillaMarker categoriesFor: #view!accessing!private! !
!ScintillaMarker categoriesFor: #view:!accessing!private! !

!ScintillaMarker class methodsFor!

definition: aScintillaMarkerDefinition line: anInteger 
	^(self new)
		setDefinition: aScintillaMarkerDefinition line: anInteger;
		yourself!

icon
	^ScintillaView icon! !
!ScintillaMarker class categoriesFor: #definition:line:!instance creation!public! !
!ScintillaMarker class categoriesFor: #icon!constants!development!public! !

ScintillaStyler guid: (GUID fromString: '{7DC234BA-44C2-490A-A7A1-7AAFD6B577EC}')!
ScintillaStyler comment: 'ScintillaStyler is the abstract class of objects responsible for dynamically ''colouring'' the text in a <ScintillaView>. This is done by applying text styles in runs between start and end points. Initially the control will request that the entire text be styled, then as the view is edited it will send further requests to style modified text. Concrete subclasses define the manner in which the text is styled, for example to do syntax colouring for a particular programming language.
'!
!ScintillaStyler categoriesForClass!Kernel-Objects! !
!ScintillaStyler methodsFor!

blockSize
	"Private - Style in blocks of a maximum of 16k characters. This allows the editor to remain
	responsive while the large bodies of text are styled incrementally."

	^##(32 * 1024)!

colorText: aString in: aScintillaView startingAt: aSmallInteger 
	"Colour the <String> of text from the <ScintillaView>, starting at the character position
	identified by the <Integer> argument. Note that the initial styling position has already
	been set."

	^self subclassResponsibility!

colorTextFrom: startInteger to: endInteger in: aScintillaView 
	self 
		colorText: (aScintillaView plainTextFrom: startInteger to: endInteger)
		in: aScintillaView
		startingAt: startInteger!

onStyleNeeded: aScintillaView from: startInteger to: stopInteger 
	"Callback from Scintilla requesting that the specified text range be coloured."

	| startPos |
	startPos := self stylingStartBefore: startInteger in: aScintillaView.
	self 
		resetStylingIn: aScintillaView
		from: startPos
		to: stopInteger.
	self 
		colorTextFrom: startPos
		to: (startInteger + self blockSize min: stopInteger)
		in: aScintillaView!

prepareToStyleView: aScintillaView
	"The receiver has been set up as the styler for the specified <ScintillaView>. This is an
	opportunity to initialise that view appropriately for this styler."!

resetStylingIn: aScintillaView from: startPos to: anInteger 
	aScintillaView startStylingFrom: startPos!

stylingStartBefore: startInteger in: aScintillaView 
	"Locate the position before the <integer>, startInteger, from which to start styling in the
	<ScintillaView>, aScintillaView. This needs to be a position from which we can safely start
	the scanner from its start state."

	"Implementation Note: Step back over any whitespace until we find the end of a token, and
	then back until we find further whitespace before that. This is necessary to cover the case
	where backspace is used to delete the last character in the token thus changing its
	interpretation (e.g. deleting the close quote of a string), or when the interpretation of a
	token changes when a further character is typed (e.g. typing a multi-part keyword literal
	and entering the colon on the second keyword). We must then step back to the start of the
	token so that we may kick off the lexer from its start state. We are more interested in
	speed than elegance here."

	| prev mask |
	startInteger <= 1 ifTrue: [^1].
	prev := startInteger - 1.
	mask := aScintillaView maxStyle.
	(aScintillaView styleNamed: #whitespace) 
		ifNil: 
			["No whitespace style, so must look for whitespace characters in the #normal style"
			| white |
			white := aScintillaView whitespaces.
			
			[prev > 0 and: 
					[((aScintillaView styleMaskAt: prev) bitAnd: mask) == 0 
						and: [white identityIncludes: (aScintillaView characterAt: prev)]]] 
					whileTrue: [prev := prev - 1].
			
			[prev == 0 or: 
					[((aScintillaView styleMaskAt: prev) bitAnd: mask) == 0 
						and: [white identityIncludes: (aScintillaView characterAt: prev)]]] 
					whileFalse: [prev := prev - 1]]
		ifNotNil: 
			[:style | 
			| id |
			id := style id.
			[prev > 0 and: [((aScintillaView styleMaskAt: prev) bitAnd: mask) == id]] 
				whileTrue: [prev := prev - 1].
			[prev == 0 or: [((aScintillaView styleMaskAt: prev) bitAnd: mask) == id]] 
				whileFalse: [prev := prev - 1]].
	^prev + 1! !
!ScintillaStyler categoriesFor: #blockSize!constants!private! !
!ScintillaStyler categoriesFor: #colorText:in:startingAt:!operations!private! !
!ScintillaStyler categoriesFor: #colorTextFrom:to:in:!event handling!public! !
!ScintillaStyler categoriesFor: #onStyleNeeded:from:to:!event handling!public! !
!ScintillaStyler categoriesFor: #prepareToStyleView:!initializing!public! !
!ScintillaStyler categoriesFor: #resetStylingIn:from:to:!helpers!private! !
!ScintillaStyler categoriesFor: #stylingStartBefore:in:!event handling!public! !

!ScintillaStyler class methodsFor!

icon
	^ScintillaView icon!

isAbstract
	^self == ##(self)!

new
	^super new initialize! !
!ScintillaStyler class categoriesFor: #icon!constants!development!public! !
!ScintillaStyler class categoriesFor: #isAbstract!public!Testing! !
!ScintillaStyler class categoriesFor: #new!instance creation!public! !

ScintillaLibrary guid: (GUID fromString: '{846BC1F2-AD71-411E-8C63-C99780A5A626}')!
ScintillaLibrary comment: 'ExternalLibrary class to wrap the Scintilla DLL (SciLexer.dll).'!
!ScintillaLibrary categoriesForClass!External-Libraries! !
!ScintillaLibrary methodsFor!

directFunction: sciThis msg: msg wParam: wParam lParam: lParam 
	<stdcall: sdword Scintilla_DirectFunction dword dword dword dword>
	^self invalidCall!

versionFormatString
	"Private - Answer a String containing the version format used by the receiver.
	The arguments than can be inserted into the string are:
		1) Product name
		2) Product major high word
		3) Product major low word
		4) Product minor high word
		5) Product minor low word
	"

	^'%2!!d!!.%3!!d!!.%4!!d!!.%5!!d!!'! !
!ScintillaLibrary categoriesFor: #directFunction:msg:wParam:lParam:!operations!private! !
!ScintillaLibrary categoriesFor: #versionFormatString!constants!private! !

!ScintillaLibrary class methodsFor!

default
	"Answer the default instance of the receiver."

	"Implementation Note: Override for optimum performance (assume always open like a permanent library)."

	^default!

fileName
	"Answer the host system file name of the external library which the receiver represents"

	^'SciLexer'!

realize
	super default! !
!ScintillaLibrary class categoriesFor: #default!accessing!public! !
!ScintillaLibrary class categoriesFor: #fileName!constants!public! !
!ScintillaLibrary class categoriesFor: #realize!public!realizing/unrealizing! !

SCNotification guid: (GUID fromString: '{B4E498B9-AD7B-4519-86F5-1E6232FBD0BD}')!
SCNotification comment: '<SCNotification> is an <ExternalStructure> class to wrap the struct ''ScintillaLib.SCNotification'' from type information in the ''Scintiall 1.46 Type Library'' library.

Note that the ''text'' field is not null terminated (the number of characters is specified by the ''length'' field), and is only valid for SCN_MODIFIED notifications where the SC_MOD_DELETETEXT or SC_MOD_INSERTTEXT ''modificationType'' is specified.'!
!SCNotification categoriesForClass!ScintillaLib-Structs! !
!SCNotification methodsFor!

ch
	"Answer the receiver's ch field as a Smalltalk object."

	^(bytes sdwordAtOffset: 16)!

character
	"Answer the receiver's <Character> who's codePoint is stored in the 'ch' field."

	^Character codePoint: self ch!

foldLevelNow
	"Answer the receiver's foldLevelNow field as a Smalltalk object."

	^(bytes sdwordAtOffset: 56)!

foldLevelPrev
	"Answer the receiver's foldLevelPrev field as a Smalltalk object."

	^(bytes sdwordAtOffset: 60)!

getValidFields
	"Private - Answer a <sequencedReadableCollection> of the fields defined 
	in the receiver's template, sorted in ascending order of offset from the start of the
	structure, that are valid in this particular instance."

	"From the Scintilla docs:

	struct SCNotification {
		NMHDR	nmhdr;
		int position;	// SCN_STYLENEEDED, SCN_MODIFIED, SCN_DWELLSTART, SCN_DWELLEND
		int ch;		// SCN_CHARADDED, SCN_KEY
		int modifiers;	// SCN_KEY
		int modificationType;// SCN_MODIFIED
		const char *text;	// SCN_MODIFIED
		int length;		// SCN_MODIFIED
		int linesAdded;	// SCN_MODIFIED
		int message;	// SCN_MACRORECORD
		uptr_t wParam;	// SCN_MACRORECORD
		sptr_t lParam;	// SCN_MACRORECORD
		int line;		// SCN_MODIFIED
		int foldLevelNow;	// SCN_MODIFIED
		int foldLevelPrev;	// SCN_MODIFIED
		int margin;	// SCN_MARGINCLICK
		int listType;	// SCN_USERLISTSELECTION
		int x;		// SCN_DWELLSTART, SCN_DWELLEND
		int y;		// SCN_DWELLSTART, SCN_DWELLEND
	};
	"

	^#(#code) 
		, (##((IdentityDictionary new)
				at: SCN_CHARADDED put: #(#ch);
				at: SCN_DWELLEND put: #(#scPosition #x #y);
				at: SCN_DWELLSTART put: #(#scPosition #x #y);
				at: SCN_KEY put: #(#ch #modifiers);
				at: SCN_MACRORECORD put: #(#message #wParam #lParam);
				at: SCN_MARGINCLICK put: #(#margin #scPosition);
				at: SCN_MODIFIED
					put: #(#scPosition #text #length #linesAdded #scLine #foldLevelNow #foldLevelPrev);
				at: SCN_STYLENEEDED put: #(#scPosition);
				at: SCN_USERLISTSELECTION put: #(#listType);
				shrink) at: self code ifAbsent: [#()])!

isAltKeyDown
	"Answer whether the ALT key was down when the event was raised.
	Only relevant for some notifications such as SCN_KEY, and SCN_HOTSPOT[DOUBLE]CLICK."

	^self modifiers allMask: SCMOD_ALT!

isShiftDown
	"Answer whether the shift key was down when the event was raised.
	Only relevant for some notifications such as SCN_KEY, and SCN_HOTSPOT[DOUBLE]CLICK."

	^self modifiers allMask: SCMOD_SHIFT!

length
	"Answer the receiver's length field as a Smalltalk object."

	^(bytes sdwordAtOffset: 32)!

line
	"Answer the one-based <integer> line number associated with the notification."

	^self scLine + 1!

linesAdded
	"Answer the receiver's linesAdded field as a Smalltalk object."

	^(bytes sdwordAtOffset: 36)!

listType
	"Answer the receiver's listType field as a Smalltalk object."

	^(bytes sdwordAtOffset: 68)!

lParam
	"Answer the receiver's lParam field as a Smalltalk object."

	^(bytes sdwordAtOffset: 48)!

margin
	"Answer the receiver's margin field as a Smalltalk object."

	^(bytes sdwordAtOffset: 64)!

message
	"Answer the receiver's message field as a Smalltalk object."

	^(bytes sdwordAtOffset: 40)!

modificationType
	"Answer the receiver's modificationType field as a Smalltalk object."

	^(bytes sdwordAtOffset: 24)!

modifiers
	"Answer the receiver's modifiers field as a Smalltalk object."

	^(bytes sdwordAtOffset: 20)!

point
	^self x @ self y!

position
	"Answer the receiver's position field as a Smalltalk object."

	^self scPosition + 1!

scLine
	"Answer the receiver's scLine field as a Smalltalk object."

	^(bytes sdwordAtOffset: 52)!

scPosition
	"Answer the receiver's scPosition field as a Smalltalk object."

	^(bytes sdwordAtOffset: 12)!

text
	"Answer the receiver's text field as a Smalltalk object."

	^String fromAddress: (bytes sdwordAtOffset: 28)!

text: anObject
	"Set the receiver's text field to the value of anObject."

	bytes dwordAtOffset: 28 put: anObject yourAddress!

wParam
	"Answer the receiver's wParam field as a Smalltalk object."

	^(bytes dwordAtOffset: 44)!

x
	"Answer the receiver's x field as a Smalltalk object."

	^(bytes sdwordAtOffset: 72)!

y
	"Answer the receiver's y field as a Smalltalk object."

	^(bytes sdwordAtOffset: 76)! !
!SCNotification categoriesFor: #ch!**compiled accessors**!public! !
!SCNotification categoriesFor: #character!accessing!public! !
!SCNotification categoriesFor: #foldLevelNow!**compiled accessors**!public! !
!SCNotification categoriesFor: #foldLevelPrev!**compiled accessors**!public! !
!SCNotification categoriesFor: #getValidFields!accessing!private! !
!SCNotification categoriesFor: #isAltKeyDown!public!testing! !
!SCNotification categoriesFor: #isShiftDown!public!testing! !
!SCNotification categoriesFor: #length!**compiled accessors**!public! !
!SCNotification categoriesFor: #line!accessing!public! !
!SCNotification categoriesFor: #linesAdded!**compiled accessors**!public! !
!SCNotification categoriesFor: #listType!**compiled accessors**!public! !
!SCNotification categoriesFor: #lParam!**compiled accessors**!public! !
!SCNotification categoriesFor: #margin!**compiled accessors**!public! !
!SCNotification categoriesFor: #message!**compiled accessors**!public! !
!SCNotification categoriesFor: #modificationType!**compiled accessors**!public! !
!SCNotification categoriesFor: #modifiers!**compiled accessors**!public! !
!SCNotification categoriesFor: #point!accessing!public! !
!SCNotification categoriesFor: #position!accessing!public! !
!SCNotification categoriesFor: #scLine!**compiled accessors**!public! !
!SCNotification categoriesFor: #scPosition!**compiled accessors**!public! !
!SCNotification categoriesFor: #text!**compiled accessors**!public! !
!SCNotification categoriesFor: #text:!**compiled accessors**!public! !
!SCNotification categoriesFor: #wParam!**compiled accessors**!public! !
!SCNotification categoriesFor: #x!**compiled accessors**!public! !
!SCNotification categoriesFor: #y!**compiled accessors**!public! !

!SCNotification class methodsFor!

defineFields
	"Define the fields of the SCNotification structure.
		SCNotification compileDefinition
	"

	super defineFields.
	self
		defineField: #scPosition type: SDWORDField readOnly offset: 12;
		defineField: #ch type: SDWORDField readOnly offset: 16;
		defineField: #modifiers type: SDWORDField readOnly offset: 20;
		defineField: #modificationType type: SDWORDField readOnly offset: 24;
		defineField: #text type: (PointerField type: String) offset: 28;
		defineField: #length type: SDWORDField readOnly offset: 32;
		defineField: #linesAdded type: SDWORDField readOnly offset: 36;
		defineField: #message type: SDWORDField readOnly offset: 40;
		defineField: #wParam type: DWORDField readOnly offset: 44;
		defineField: #lParam type: SDWORDField readOnly offset: 48;
		defineField: #scLine type: SDWORDField readOnly offset: 52;
		defineField: #foldLevelNow type: SDWORDField readOnly offset: 56;
		defineField: #foldLevelPrev type: SDWORDField readOnly offset: 60;
		defineField: #margin type: SDWORDField readOnly offset: 64;
		defineField: #listType type: SDWORDField readOnly offset: 68;
		defineField: #x type: SDWORDField readOnly offset: 72;
		defineField: #y type: SDWORDField readOnly offset: 76.
	self byteSize: 80! !
!SCNotification class categoriesFor: #defineFields!initializing!public! !

ScintillaTextStylesDialog guid: (GUID fromString: '{E768B8D3-CA27-4C94-B60D-7FBF3DD4E34A}')!
ScintillaTextStylesDialog comment: 'ScintillaTextStylesDialog is a <valueDialogPresenter> that can be used to edit a collection of <ScintillaTextStyles> in order to configure the visual styles in a <ScintillaView>. It is intended for use in applications, and so does not allow the addition or removal of styles, nor the editing of style names. However this could be changed by defining a different view that used the ScintillaStylesCollectionPresenter ''Developer view'' instead of the ''Default view''.

For example usage see the XmlPad sample.

Instance Variables:
	stylesPresenter		<ScintillaStylesCollectionPresenter>
'!
!ScintillaTextStylesDialog categoriesForClass!MVP-Presenters! !
!ScintillaTextStylesDialog methodsFor!

createComponents
	"Private - Create the presenters contained by the receiver"

	super createComponents.
	stylesPresenter := self add: ScintillaStylesCollectionPresenter new name: 'styles'!

defaultStyle
	^stylesPresenter defaultStyle!

defaultStyle: aScintillaTextStyle 
	"Set the default style to be used for style facets for which no setting is specified, i.e.
	this style specifies the style facets that will be inherited from the view settings
	Typically this will include the font and background color. These style facets are then
	overridden by first the #normal style, and then any specific style. "

	stylesPresenter defaultStyle: aScintillaTextStyle!

model: aSubjectModel 
	"Connect the receiver to aSubjectModel. The reply presenter shares this
	same model since it is responsible for editing it's value"

	| styles |
	super model: aSubjectModel.
	styles := ((self model value collect: [:each | each copy]) 
				asSortedCollection: [:a :b | a name <= b name]) asValue.
	#todo.	"Sort out CollectionPresenter so don't need to use NeverSearchPolicy - problem is that it won't trigger any changes
	without this when the list is edited, since it compares the current value against itself"
	styles comparisonPolicy: SearchPolicy never.
	stylesPresenter model: styles.
	styles 
		when: #valueChanged
		send: #onStylesChanged
		to: self!

onStylesChanged
	"The style collection (a copy) has been changed, so transfer this across to the receiver's value buffer."

	self value: stylesPresenter value! !
!ScintillaTextStylesDialog categoriesFor: #createComponents!initializing!private! !
!ScintillaTextStylesDialog categoriesFor: #defaultStyle!accessing!public! !
!ScintillaTextStylesDialog categoriesFor: #defaultStyle:!accessing!public! !
!ScintillaTextStylesDialog categoriesFor: #model:!accessing!public! !
!ScintillaTextStylesDialog categoriesFor: #onStylesChanged!event handling!private! !

!ScintillaTextStylesDialog class methodsFor!

defaultModel
	^OrderedCollection with: ScintillaTextStyle normal!

icon
	"Answers an Icon that can be used to represent this class."

	^ScintillaTextStyle icon
!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(STBViewProxy)  8 ##(DialogView)  98 30 0 0 98 2 26738689 131073 416 0 524550 ##(ColorRef)  8 4278190080 0 133 0 0 0 416 788230 ##(BorderLayout)  1 21 0 410 8 ##(ContainerView)  98 15 0 416 98 2 8 1140850688 131073 560 0 0 0 5 0 0 0 560 530 1 1 410 8 ##(StaticRectangle)  98 14 0 560 98 2 8 1140850960 1 656 0 0 0 5 0 0 0 656 0 8 4294906439 983302 ##(MessageSequence)  202 208 98 2 721670 ##(MessageSend)  8 #createAt:extent: 98 2 328198 ##(Point)  1 1 882 837 5 656 818 8 #isEnabled: 98 1 32 656 983302 ##(WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 162 1 0 0 2 0 0 0] 98 0 882 193 193 0 27 0 0 0 410 8 ##(ReferenceView)  98 14 0 560 98 2 8 1140850688 131073 1056 0 0 0 5 0 0 0 1056 1180166 ##(ResourceIdentifier)  8 ##(Presenter)  8 #resource_OK_Cancel_button_block 0 754 202 208 98 1 818 848 98 2 882 1 5 882 837 77 1056 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 2 0 0 0 162 1 0 0 40 0 0 0] 98 0 1040 0 27 234 256 1344 0 754 202 208 98 1 818 848 98 2 882 21 761 882 837 81 560 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 124 1 0 0 172 1 0 0 164 1 0 0] 98 2 656 1056 1040 0 27 0 0 410 1072 98 14 0 416 98 2 8 1140850688 131073 1536 0 0 0 5 0 0 0 1536 1138 8 ##(ScintillaStylesCollectionPresenter)  8 #resource_Default_view 0 754 202 208 98 1 818 848 98 2 882 21 21 882 837 721 1536 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 10 0 0 0 172 1 0 0 114 1 0 0] 1344 1040 0 27 234 256 98 2 1536 8 'styles' 590342 ##(Rectangle)  882 21 21 882 21 21 0 0 0 0 14555 0 0 0 882 885 915 1 0 0 590598 ##(Semaphore)  0 0 1 0 8 2010572111 754 202 208 98 3 818 848 98 2 882 1155 743 882 893 915 416 818 8 #text: 98 1 8 'Text Styles' 416 818 8 #menuBar: 98 1 0 416 978 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 65 2 0 0 115 1 0 0 255 3 0 0 60 3 0 0] 98 2 1536 560 1040 0 27 )! !
!ScintillaTextStylesDialog class categoriesFor: #defaultModel!constants!public! !
!ScintillaTextStylesDialog class categoriesFor: #icon!accessing!constants!public! !
!ScintillaTextStylesDialog class categoriesFor: #resource_Default_view!public!resources-views! !

ScintillaStylesCollectionPresenter guid: (GUID fromString: '{DFA721D9-7DE6-4FE7-9998-18DD7B52C0C4}')!
ScintillaStylesCollectionPresenter comment: 'ScintillaStylesCollectionPresenter provides a master-detail presentation of a collection of <ScintillaTextStyle>s (e.g  those configured for a particular ScintillaView). It is composed of a <CollectionPresenter>, to present the list of styles, and a <ScintillaTextStylePresenter>, to present the detail view. A ScintillaStylesCollectionPresenter can be used to add new styles, and remove and modify existing styles, and as such is used for the #textStyles aspect of a <ScintillaView> when inspected with the Published Aspect Inspector, and also within the <ScintillaTextStylesDialog>. The latter is intended to allow for configuration of text styles in an end-user application that employs Scintilla.

Instance Variables:
	collectionPresenter	<CollectionPresenter>
	detailPresenter		<ScintillaTextStylePresenter>

'!
!ScintillaStylesCollectionPresenter categoriesForClass!MVP-Presenters!MVP-Resources-IDE Tools! !
!ScintillaStylesCollectionPresenter methodsFor!

addItem
	^collectionPresenter addItem!

applyDefaultStyle
	| style list |
	(collectionPresenter notNil and: [collectionPresenter isOpen]) ifFalse: [^self].
	list := collectionPresenter listPresenter view.
	style := self defaultStyle.
	list font: style font.
	style forecolor ifNotNil: [:colour | list forecolor: colour].
	style backcolor ifNotNil: [:colour | list backcolor: colour]!

createComponents
	"Private - Create the presenters contained by the receiver"

	super createComponents.
	collectionPresenter := self add: CollectionPresenter new name: 'styles'.
	collectionPresenter setAddItemBlock: [ScintillaTextStyle new].
	detailPresenter := self add: ScintillaTextStylePresenter new name: 'detail'!

createSchematicWiring
	"Create the trigger wiring for the receiver. At this stage the initialization
	is complete and the view is open"

	super createSchematicWiring.
	collectionPresenter 
		when: #selectionChanged
		send: #onSelectionChanged
		to: self.
	detailPresenter 
		when: #valueChanged
		send: #onStyleChanged
		to: self!

defaultStyle
	^detailPresenter defaultStyle!

defaultStyle: aScintillaTextStyle 
	"Set the default style to be used for style facets for which no setting is specified, i.e.
	this style specifies the style facets that will be inherited from the view settings
	Typically this will include the font and background color. These style facets are then
	overridden by first the #normal style, and then any specific style. "

	detailPresenter defaultStyle: aScintillaTextStyle.
	self applyDefaultStyle!

model: aValueModel
	"Set the model of the receiver to be aValueModel. We intercept a change
	notification so that the list selection can track this value."

	super model: aValueModel.
	self onValueChanged
!

onSelectionChanged
	| selection |
	selection := collectionPresenter selectionOrNil.
	detailPresenter model value: selection.
	selection isNil 
		ifTrue: [detailPresenter isEnabled: false]
		ifFalse: [detailPresenter isEnabled: true]!

onStyleChanged
	collectionPresenter listModel refresh: collectionPresenter selectionOrNil!

onValueChanged
	"Private - The value has been changed in the receiver's model. Transfer the value to the
	listModel"

	| list normal |
	collectionPresenter model: self model.
	collectionPresenter listModel searchPolicy: SearchPolicy equality.
	list := self model value.
	normal := list detect: [:each | each name == #normal].
	detailPresenter normalStyle: normal!

onViewOpened
	super onViewOpened.
	collectionPresenter selectionOrNil: detailPresenter normalStyle! !
!ScintillaStylesCollectionPresenter categoriesFor: #addItem!commands!public! !
!ScintillaStylesCollectionPresenter categoriesFor: #applyDefaultStyle!private!updating! !
!ScintillaStylesCollectionPresenter categoriesFor: #createComponents!initializing!private! !
!ScintillaStylesCollectionPresenter categoriesFor: #createSchematicWiring!initializing!public! !
!ScintillaStylesCollectionPresenter categoriesFor: #defaultStyle!accessing!public! !
!ScintillaStylesCollectionPresenter categoriesFor: #defaultStyle:!accessing!public! !
!ScintillaStylesCollectionPresenter categoriesFor: #model:!accessing!public! !
!ScintillaStylesCollectionPresenter categoriesFor: #onSelectionChanged!event handling!private! !
!ScintillaStylesCollectionPresenter categoriesFor: #onStyleChanged!event handling!private! !
!ScintillaStylesCollectionPresenter categoriesFor: #onValueChanged!event handling!private! !
!ScintillaStylesCollectionPresenter categoriesFor: #onViewOpened!event handling!public! !

!ScintillaStylesCollectionPresenter class methodsFor!

customDraw: aNMLVCUSTOMDRAW 
	| style |
	style := aNMLVCUSTOMDRAW item.
	style fontName isNil 
		ifTrue: 
			[| font |
			font := aNMLVCUSTOMDRAW font.
			style restyleFont: font]
		ifFalse: [aNMLVCUSTOMDRAW font: style font].
	style forecolor ifNotNil: [:color | aNMLVCUSTOMDRAW forecolor: color].
	style backcolor ifNotNil: [:color | aNMLVCUSTOMDRAW backcolor: color]!

defaultModel
	^OrderedCollection with: ScintillaTextStyle normal!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ContainerView)  98 15 0 0 98 2 8 1409286144 131073 416 0 0 0 5 0 0 0 416 788230 ##(Smalltalk.BorderLayout)  17 11 0 0 0 410 432 98 15 0 416 98 2 8 1140850688 131073 528 0 0 0 5 0 0 0 528 498 1 1 410 8 ##(Smalltalk.StaticText)  98 16 0 528 98 2 8 1140850944 1 608 0 0 0 5 0 0 0 608 0 8 4294903625 852486 ##(Smalltalk.NullConverter)  0 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  1 1 866 321 39 608 802 8 #text: 98 1 8 '&Style Name:' 608 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 160 0 0 0 19 0 0 0] 98 0 866 193 193 0 27 0 0 0 410 8 ##(Smalltalk.ListView)  98 30 0 528 98 2 8 1140936781 1025 1056 590662 2 ##(Smalltalk.ListModel)  202 208 98 0 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  524550 ##(Smalltalk.ColorRef)  8 4278190080 0 5 0 0 0 1056 0 8 4294903801 459270 ##(Smalltalk.Message)  8 #displayString 98 0 0 1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 0 202 208 98 1 920646 5 ##(Smalltalk.ListViewColumn)  8 '' 313 8 #left 1298 1328 1344 8 ##(Smalltalk.SortedCollection)  0 0 1056 0 3 0 787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  3 1 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 8 '[:each | ScintillaStylesCollectionPresenter customDraw: each]' 8 #[31 105 45 17 177 106] 721414 ##(Smalltalk.Association)  8 #ScintillaStylesCollectionPresenter 8 ##(Smalltalk.ScintillaStylesCollectionPresenter)  8 #customDraw: 1536 7 257 0 8 #report 1184 0 133217 0 0 738 202 208 98 1 802 832 98 2 866 1 39 866 321 685 1056 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 19 0 0 0 160 0 0 0 105 1 0 0] 98 0 1040 0 27 234 256 98 2 1056 8 'list' 0 738 202 208 98 1 802 832 98 2 866 1 1 866 321 723 528 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 160 0 0 0 105 1 0 0] 98 2 608 1056 1040 0 27 410 8 ##(Smalltalk.ReferenceView)  98 14 0 416 98 2 8 1140850688 131073 2112 0 0 0 5 0 0 0 2112 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.ScintillaTextStylePresenter)  8 #resource_Default_view 0 738 202 208 98 1 802 832 98 2 866 337 1 866 501 723 2112 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 168 0 0 0 0 0 0 0 162 1 0 0 105 1 0 0] 1184 1040 0 27 234 256 98 4 2112 8 'detail' 528 8 'styles' 590342 ##(Smalltalk.Rectangle)  866 1 1 866 1 1 738 202 208 98 2 802 832 98 2 866 2799 21 866 837 723 416 802 928 98 1 8 'Text Styles' 416 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 119 5 0 0 10 0 0 0 25 7 0 0 115 1 0 0] 98 2 528 2112 1040 0 27 )!

resource_Developer_view
	"Answer the literal data from which the 'Developer view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Developer_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ContainerView)  98 15 0 0 98 2 8 1409286144 131073 416 0 0 0 5 0 0 0 416 788230 ##(Smalltalk.BorderLayout)  17 11 0 0 0 410 432 98 15 0 416 98 2 8 1140850688 131073 528 0 0 0 5 0 0 0 528 498 1 1 410 8 ##(Smalltalk.ReferenceView)  98 14 0 528 98 2 8 1140850688 131073 608 0 0 0 5 0 0 0 608 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.Toolbar)  8 #resource_List_tools 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  1 1 882 321 51 608 818 8 #text: 98 1 8 'toolbar' 608 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 160 0 0 0 25 0 0 0] 98 0 882 193 193 0 27 0 0 0 410 8 ##(Smalltalk.ListView)  98 30 0 528 98 2 8 1140936781 1025 1072 590662 2 ##(Smalltalk.ListModel)  202 208 1040 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  524550 ##(Smalltalk.ColorRef)  8 4278190080 0 5 265030 4 ##(Smalltalk.Menu)  0 16 98 4 984134 2 ##(Smalltalk.CommandMenuItem)  1 1180998 4 ##(Smalltalk.CommandDescription)  8 #moveFirst 8 'Move to &First' 1 1 0 0 0 1330 1 1362 8 #moveUp 8 'Move to &Previous' 1 1 0 0 0 1330 1 1362 8 #moveDown 8 'Move to &Next' 1 1 0 0 0 1330 1 1362 8 #moveLast 8 'Move to &Last' 1 1 0 0 0 8 '' 0 1 0 0 0 0 0 0 0 1072 0 8 4294903801 459270 ##(Smalltalk.Message)  8 #displayString 98 0 0 1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 0 202 208 98 1 920646 5 ##(Smalltalk.ListViewColumn)  8 '' 313 8 #left 1650 1680 1696 8 ##(Smalltalk.SortedCollection)  0 0 1072 0 3 0 787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  3 1 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 8 '[:each | ScintillaStylesCollectionPresenter customDraw: each]' 8 #[31 105 45 17 177 106] 721414 ##(Smalltalk.Association)  8 #ScintillaStylesCollectionPresenter 8 ##(Smalltalk.ScintillaStylesCollectionPresenter)  8 #customDraw: 1888 7 257 0 8 #report 1040 0 133217 0 0 754 202 208 98 2 818 848 98 2 882 1 51 882 321 783 1072 818 8 #contextMenu: 98 1 1296 1072 994 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 160 0 0 0 160 1 0 0] 98 0 1056 0 27 234 256 98 2 1072 8 'list' 0 754 202 208 98 1 818 848 98 2 882 1 1 882 321 833 528 994 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 160 0 0 0 160 1 0 0] 98 2 608 1072 1056 0 27 410 624 98 14 0 416 98 2 8 1140850688 131073 2512 0 0 0 5 0 0 0 2512 690 8 ##(Smalltalk.ScintillaTextStylePresenter)  8 #resource_Developer_view 0 754 202 208 98 1 818 848 98 2 882 337 1 882 501 833 2512 994 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 168 0 0 0 0 0 0 0 162 1 0 0 160 1 0 0] 1040 1056 0 27 234 256 98 4 528 8 'styles' 2512 8 'detail' 590342 ##(Smalltalk.Rectangle)  882 1 1 882 1 1 754 202 208 98 2 818 848 98 2 882 2799 21 882 837 833 416 818 944 98 1 8 'Text Styles' 416 994 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 119 5 0 0 10 0 0 0 25 7 0 0 170 1 0 0] 98 2 528 2512 1056 0 27 )! !
!ScintillaStylesCollectionPresenter class categoriesFor: #customDraw:!helpers!public! !
!ScintillaStylesCollectionPresenter class categoriesFor: #defaultModel!models!public! !
!ScintillaStylesCollectionPresenter class categoriesFor: #resource_Default_view!public!resources-views! !
!ScintillaStylesCollectionPresenter class categoriesFor: #resource_Developer_view!public!resources-views! !

ScintillaTextStylePresenter guid: (GUID fromString: '{E352DFCB-7063-4B89-9958-074E8500392F}')!
ScintillaTextStylePresenter comment: 'ScintillaTextStylePresenter provides a detail presentation of a <ScintillaTextStyle>, and can be used to view and modify the settings of that style such as the font, foreground and background colours, etc.

Instance Variables:
	facePresenter			<ChoicePresenter>. Font face name.
	pointSizePresenter		<NumberPresenter>. Font point size.
	forecolorPresenter		<ColorPresenter>. Text foreground colour.
	backcolorPresenter		<ColorPresenter>. Text background colour.
	isItalicPresenter			<BooleanPresenter>. Whether font is italic, non-italic, or unspecified.
	isBoldPresenter			<BooleanPresenter>. Whether font is bold, normal weight, or unspecified.
	isUnderlinedPresenter	<BooleanPresenter>. Whether font is underlined, normal, or unspecified.
	casePresenter			<ChoicePresenter>. Whether font is mixed, all-upper or all-lower case.
	characterSetPresenter		<ChoicePresenter>. The character set specified for the font.
	previewPresenter			<ScintillaView> used to preview the style.
	fillToEndOfLinePresenter	<BooleanPresenter>. Whether the backcolour is applied to the end of the window line if after the end of text.
	normalStyle				<ScintillaTextStyle>. The #normal style. Significant because its settings are inherited by other styles.
	defaultStyle				<ScintillaTextStyle>. The default style expected to be inherited from the view at runtime.
	isInvisiblePresenter		<BooleanPresenter>. Whether the text is visible, invisible, or unspecified.
	isHotspotPresenter		<BooleanPresenter>. Whether the text acts like a hotspot when the mouse moves over it, or not, or unspecified.
	isReadOnlyPresenter		<BooleanPresenter>. Whether the text is read-only, read-write, or unspecified.
	namePresenter			<TextPresenter>. The symbolic name of the style, as used by the application.
	idPresenter				<TextPresenter>. The numeric id associated with the style, for identifying the style to Scintilla and by lexers.'!
!ScintillaTextStylePresenter categoriesForClass!MVP-Presenters!MVP-Resources-IDE Tools! !
!ScintillaTextStylePresenter methodsFor!

applyDefaultStyle
	| style preview |
	(previewPresenter notNil and: [previewPresenter isOpen]) ifFalse: [^self].
	preview := previewPresenter view.
	style := self defaultStyle.
	preview
		font: style font;
		forecolor: style forecolor;
		backcolor: style backcolor!

buildDefaultStyle
	| style desktop |
	style := ScintillaTextStyle new.
	desktop := View desktop.
	style
		font: desktop actualFont;
		forecolor: desktop forecolor;
		backcolor: Color window.
	^style!

chooseBackcolor
	(ColorDialog on: backcolorPresenter model)
		caption: 'Background Color';
		showModal!

chooseForecolor
	(ColorDialog on: forecolorPresenter model)
		caption: 'Foreground Color';
		showModal!

createComponents
	"Create the presenters contained by the receiver"

	super createComponents.
	namePresenter := self add: TextPresenter new name: 'name'.
	idPresenter := self add: TextPresenter new name: 'id'.
	facePresenter := self add: ((ChoicePresenter new)
						choices: ((Canvas forDesktop fontNames asSortedCollection asOrderedCollection)
									addFirst: nil;
									yourself);
						yourself)
				name: 'faceName'.
	pointSizePresenter := self add: NumberPresenter new name: 'pointSize'.
	casePresenter := self add: ChoicePresenter new name: 'case'.
	casePresenter choices: #(nil) , ScintillaTextStyle caseNames.
	characterSetPresenter := self add: ChoicePresenter new name: 'charSet'.
	forecolorPresenter := self add: ColorPresenter new name: 'forecolor'.
	backcolorPresenter := self add: ColorPresenter new name: 'backcolor'.
	isItalicPresenter := self add: BooleanPresenter new name: 'isItalic'.
	isBoldPresenter := self add: BooleanPresenter new name: 'isBold'.
	isUnderlinedPresenter := self add: BooleanPresenter new name: 'isUnderlined'.
	fillToEndOfLinePresenter := self add: BooleanPresenter new name: 'isBackcolorExtendedToEndOfLine'.
	previewPresenter := self add: TextPresenter new name: 'preview'.
	isInvisiblePresenter := self add: BooleanPresenter new name: 'isInvisible'.
	isReadOnlyPresenter := self add: BooleanPresenter new name: 'isReadOnly'.
	isHotspotPresenter := self add: BooleanPresenter new name: 'isHotspot'!

defaultStyle
	^defaultStyle ifNil: [self buildDefaultStyle]!

defaultStyle: aScintillaTextStyle 
	defaultStyle := aScintillaTextStyle.
	self applyDefaultStyle!

isEnabled: aBoolean
	self view isEnabledDeeply: aBoolean!

model: aValueHolder 
	"Set the model associated with the receiver."

	| value |
	self model removeEventsTriggeredFor: self.
	value := aValueHolder ifNil: [self normalStyle asValue].
	super model: value.
	namePresenter model: (value aspectValue: #name).
	idPresenter model: (value aspectValue: #id).
	facePresenter model: (value aspectValue: #fontName).
	casePresenter model: (value aspectValue: #caseName).
	characterSetPresenter model: (value aspectValue: #characterSet).
	forecolorPresenter model: (value aspectValue: #forecolor).
	backcolorPresenter model: (value aspectValue: #backcolor).
	isBoldPresenter model: (value aspectValue: #isBold).
	isItalicPresenter model: (value aspectValue: #isItalic).
	isUnderlinedPresenter model: (value aspectValue: #isUnderlined).
	pointSizePresenter model: (value aspectValue: #pointSize).
	(self view viewNamed: 'pointSizeSpinner') model: pointSizePresenter model.
	fillToEndOfLinePresenter model: (value aspectValue: #isBackcolorExtendedToEndOfLine).
	previewPresenter model: (value aspectValue: #description).
	isHotspotPresenter model: (value aspectValue: #isHotspot).
	isReadOnlyPresenter model: (value aspectValue: #isReadOnly).
	isInvisiblePresenter model: (value aspectValue: #isInvisible).
!

normalStyle
	normalStyle isNil ifTrue: [normalStyle := ScintillaTextStyle normal].
	^normalStyle!

normalStyle: aScintillaTextStyle 
	normalStyle := aScintillaTextStyle.
	self onValueChanged!

onValueChanged
	| style preview previewStyle |
	previewPresenter isOpen ifFalse: [^self].
	style := self model value copy.
	namePresenter view isReadOnly: style isPredefined.
	preview := previewPresenter view.
	previewStyle := #preview.
	style name == #lineNumber 
		ifTrue: 
			[previewStyle := #normal.
			preview hasLineNumbers: true]
		ifFalse: 
			[preview hasLineNumbers: false.
			style name: previewStyle].
	self applyDefaultStyle.
	preview styler normalStyleName: previewStyle.
	preview textStyles: (Array with: self normalStyle with: style).
	super onValueChanged!

onViewOpened
	super onViewOpened.
	self onValueChanged! !
!ScintillaTextStylePresenter categoriesFor: #applyDefaultStyle!private!updating! !
!ScintillaTextStylePresenter categoriesFor: #buildDefaultStyle!helpers!private! !
!ScintillaTextStylePresenter categoriesFor: #chooseBackcolor!commands!private! !
!ScintillaTextStylePresenter categoriesFor: #chooseForecolor!commands!private! !
!ScintillaTextStylePresenter categoriesFor: #createComponents!initializing!public! !
!ScintillaTextStylePresenter categoriesFor: #defaultStyle!accessing!public! !
!ScintillaTextStylePresenter categoriesFor: #defaultStyle:!accessing!public! !
!ScintillaTextStylePresenter categoriesFor: #isEnabled:!accessing!public! !
!ScintillaTextStylePresenter categoriesFor: #model:!accessing!private! !
!ScintillaTextStylePresenter categoriesFor: #normalStyle!accessing!public! !
!ScintillaTextStylePresenter categoriesFor: #normalStyle:!accessing!public! !
!ScintillaTextStylePresenter categoriesFor: #onValueChanged!event handling!public! !
!ScintillaTextStylePresenter categoriesFor: #onViewOpened!event handling!public! !

!ScintillaTextStylePresenter class methodsFor!

defaultModel
	"Answer a default model to be assigned to the receiver when it is initialized."

	^ScintillaTextStyle normal asValue!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ContainerView)  98 15 0 0 98 2 8 1409286144 131073 416 0 0 0 517 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[245 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 1 2 1 34 77 83 32 83 97 110 115 32 83 101 114 105 102 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 328198 ##(Smalltalk.Point)  193 193 0 416 656198 1 ##(Smalltalk.FlowLayout)  11 11 17 234 256 98 0 590342 ##(Smalltalk.Rectangle)  578 1 21 578 1 21 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 578 3359 21 578 471 711 416 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 143 6 0 0 10 0 0 0 122 7 0 0 109 1 0 0] 98 4 410 432 98 15 0 416 98 2 8 1140850688 131073 960 0 0 0 5 0 0 0 960 610 11 1 9 234 256 98 4 410 8 ##(Smalltalk.TextEdit)  98 16 0 960 98 2 8 1140916352 1025 1072 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 5 0 0 0 1072 0 8 4294903901 852486 ##(Smalltalk.NullConverter)  0 0 35 738 202 208 98 4 802 832 98 2 578 93 1 578 259 39 1072 802 8 #text: 98 1 8 'Name of style' 1072 802 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 1072 802 8 #isTextModified: 98 1 32 1072 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 46 0 0 0 0 0 0 0 175 0 0 0 19 0 0 0] 98 0 578 193 193 0 27 8 'name' 410 1088 98 16 0 960 98 2 8 1140916352 1025 1632 0 1154 1184 0 5 0 0 0 1632 0 8 4294903901 1218 0 0 35 738 202 208 98 3 802 832 98 2 578 395 1 578 61 39 1632 802 1440 98 1 1474 3 1 3 1632 802 1520 98 1 32 1632 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 197 0 0 0 0 0 0 0 227 0 0 0 19 0 0 0] 98 0 1600 0 27 8 'id' 674 578 21 1 578 1 1 738 202 208 98 1 802 832 98 2 578 1 21 578 471 39 960 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 10 0 0 0 235 0 0 0 29 0 0 0] 98 4 410 8 ##(Smalltalk.StaticText)  98 16 0 960 98 2 8 1140850956 1 2208 0 0 0 5 0 0 0 2208 0 8 4294903875 1218 0 0 0 738 202 208 98 2 802 832 98 2 578 21 7 578 63 27 2208 802 1376 98 1 8 'Name:' 2208 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 3 0 0 0 41 0 0 0 16 0 0 0] 98 0 1600 0 27 1072 410 2224 98 16 0 960 98 2 8 1140850955 1 2528 0 0 0 5 0 0 0 2528 0 8 4294903875 1218 0 0 0 738 202 208 98 2 802 832 98 2 578 361 7 578 25 27 2528 802 1376 98 1 8 'Id:' 2528 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 180 0 0 0 3 0 0 0 192 0 0 0 16 0 0 0] 98 0 1600 0 27 1632 1600 0 27 410 432 98 15 0 416 98 2 8 1140850688 131073 2832 0 0 0 5 0 0 0 2832 852230 ##(Smalltalk.FramingLayout)  234 240 98 24 410 2224 98 16 0 2832 98 2 8 1140916492 1 2960 721990 2 ##(Smalltalk.ValueHolder)  0 32 1310726 ##(Smalltalk.EqualitySearchPolicy)  0 0 0 5 0 0 0 2960 0 8 4294903875 656134 ##(Smalltalk.TimeToText)  0 0 0 0 738 202 208 98 2 802 832 98 2 578 329 41 578 61 27 2960 802 1376 98 1 8 '&Size:' 2960 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 164 0 0 0 20 0 0 0 194 0 0 0 33 0 0 0] 98 0 1600 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedPreviousRight 25 3378 8 #fixedViewLeft 61 3378 8 #fixedPreviousTop -29 3378 8 #fixedViewTop 27 410 8 ##(Smalltalk.PushButton)  98 17 0 2832 98 2 8 1140924416 1 3520 0 1154 1184 0 5 0 0 0 3520 0 8 4294903711 1180998 4 ##(Smalltalk.CommandDescription)  8 #chooseForecolor 8 '...' 1 1 0 0 32 738 202 208 98 2 802 832 98 2 578 121 159 578 41 43 3520 802 1376 98 1 8 '...' 3520 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 60 0 0 0 79 0 0 0 80 0 0 0 100 0 0 0] 98 0 1600 0 27 3346 3392 11 3424 41 3456 1 3378 8 #fixedPreviousBottom 1 410 1088 98 16 0 2832 98 2 8 1140924544 1025 3952 0 721158 ##(Smalltalk.SystemColor)  11 0 5 0 0 0 3952 0 8 4294903901 852742 ##(Smalltalk.IntegerToText)  0 8 '' 0 1 738 202 208 98 4 802 832 98 2 578 329 71 578 1 43 3952 802 1376 98 1 8 '0' 3952 802 1440 98 1 1474 3 1 3 3952 802 1520 98 1 32 3952 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 164 0 0 0 35 0 0 0 164 0 0 0 56 0 0 0] 98 0 1600 0 27 3346 3378 8 #fixedPreviousLeft 1 3424 65 3920 5 3488 43 410 2224 98 16 0 2832 98 2 8 1140850956 1 4448 0 0 0 5 0 0 0 4448 0 8 4294903875 1218 0 0 0 738 202 208 98 2 802 832 98 2 578 21 41 578 61 27 4448 802 1376 98 1 8 '&Font:' 4448 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 20 0 0 0 40 0 0 0 33 0 0 0] 98 0 1600 0 27 3346 3378 8 #fixedParentLeft 1 3424 61 3378 8 #fixedParentTop 41 3488 27 410 8 ##(Smalltalk.ComboBox)  98 17 0 2832 98 2 8 1152583170 1 4832 590662 2 ##(Smalltalk.ListModel)  202 208 656 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  1154 1184 0 5 0 0 0 4832 0 8 4294903891 459270 ##(Smalltalk.Message)  8 #displayString 98 0 656 401 738 202 208 98 1 802 832 98 2 578 21 71 578 285 43 4832 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 35 0 0 0 152 0 0 0 56 0 0 0] 98 0 1600 0 27 3346 4416 1 3424 285 3920 5 3488 43 410 8 ##(Smalltalk.ColorView)  98 16 0 2832 98 2 8 1140850944 262145 5264 3026 0 32 1114118 ##(Smalltalk.NeverSearchPolicy)  0 0 0 5 0 0 0 5264 0 8 4294903875 1218 0 0 0 738 202 208 98 1 802 832 98 2 578 21 159 578 91 43 5264 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 79 0 0 0 55 0 0 0 100 0 0 0] 98 0 1600 0 27 3346 4416 1 3424 91 3920 5 3488 43 410 4848 98 17 0 2832 98 2 8 1144063491 1025 5600 4914 202 208 656 0 4976 1154 1184 0 5 0 0 0 5600 0 8 4294903891 5026 5056 98 0 656 401 738 202 208 98 1 802 832 98 2 578 183 159 578 185 43 5600 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 91 0 0 0 79 0 0 0 183 0 0 0 100 0 0 0] 98 0 1600 0 27 3346 4416 1 3424 185 3920 5 3488 43 410 432 98 15 0 2832 98 2 8 1140850688 131073 5936 0 0 0 5 0 0 0 5936 610 1 1 1 234 256 98 6 410 8 ##(Smalltalk.CheckBox)  98 16 0 5936 98 2 8 1140924422 1 6048 3026 0 0 5376 32 0 0 5 0 498 0 16 530 8 #[245 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 0 0 0 0 1 2 1 34 77 83 32 83 97 110 115 32 83 101 114 105 102 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 578 193 193 0 6048 0 8 4294903711 1218 0 0 0 738 202 208 98 2 802 832 98 2 578 1 1 578 135 29 6048 802 1376 98 1 8 '&Bold' 6048 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 67 0 0 0 14 0 0 0] 98 0 1600 0 27 8 'isBold' 410 6064 98 16 0 5936 98 2 8 1140924422 1 6464 3026 0 0 5376 32 0 0 5 0 498 0 16 530 8 #[245 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 1 0 0 1 2 1 34 77 83 32 83 97 110 115 32 83 101 114 105 102 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 578 193 193 0 6464 0 8 4294903711 1218 0 0 0 738 202 208 98 2 802 832 98 2 578 255 1 578 147 29 6464 802 1376 98 1 8 '&Underlined' 6464 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 0 0 0 0 0 0 0 200 0 0 0 14 0 0 0] 98 0 1600 0 27 8 'isUnderlined' 410 6064 98 16 0 5936 98 2 8 1140924422 1 6864 3026 0 0 5376 32 0 0 5 0 498 0 16 530 8 #[245 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 1 0 0 0 1 2 1 34 77 83 32 83 97 110 115 32 83 101 114 105 102 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 578 193 193 0 6864 0 8 4294903711 1218 0 0 0 738 202 208 98 2 802 832 98 2 578 135 1 578 121 29 6864 802 1376 98 1 8 '&Italic' 6864 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 67 0 0 0 0 0 0 0 127 0 0 0 14 0 0 0] 98 0 1600 0 27 8 'isItalic' 674 578 1 1 578 11 1 738 202 208 98 1 802 832 98 2 578 21 225 578 431 29 5936 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 112 0 0 0 225 0 0 0 126 0 0 0] 98 3 6048 6864 6464 1600 0 27 3346 4768 1 3378 8 #fixedParentRight 1 3920 25 3378 8 #fixedParentBottom -25 410 2224 98 16 0 2832 98 2 8 1140916492 1 7552 3026 0 32 3072 0 0 0 5 0 0 0 7552 0 8 4294903875 3106 0 0 0 0 738 202 208 98 2 802 832 98 2 578 21 129 578 61 27 7552 802 1376 98 1 8 '&Color:' 7552 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 64 0 0 0 40 0 0 0 77 0 0 0] 98 0 1600 0 27 3346 4768 1 3424 61 3920 17 3488 27 410 8 ##(Smalltalk.GroupBox)  98 14 0 2832 98 2 8 1140850695 65 7888 0 0 0 5 0 0 0 7888 0 8 4294903711 738 202 208 98 2 802 832 98 2 578 1 1 578 471 279 7888 802 1376 98 1 8 'Text' 7888 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 235 0 0 0 139 0 0 0] 98 0 1600 0 27 3346 4768 -19 7488 21 4800 1 7520 1 410 8 ##(Smalltalk.SpinButton)  98 15 0 2832 98 2 8 1140916276 1 8208 3026 0 0 1376774 ##(Smalltalk.PluggableSearchPolicy)  5026 8 #= 98 0 5026 8 #hash 98 0 1 1154 1184 0 5 0 0 0 8208 0 8 4294903591 1218 0 0 738 202 208 98 3 802 832 98 2 578 393 71 578 31 43 8208 802 8 #setRange: 98 1 1474 3 201 3 8208 802 8 #udmSetAccel: 98 1 918854 1 ##(Smalltalk.StructureArray)  8 #[0 0 0 0 1 0 0 0 2 0 0 0 5 0 0 0 5 0 0 0 20 0 0 0] 7 8 ##(Smalltalk.UDACCEL)  0 17 8208 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 196 0 0 0 35 0 0 0 211 0 0 0 56 0 0 0] 98 0 1600 0 27 3346 3392 1 3424 31 3456 1 3920 1 410 2224 98 16 0 2832 98 2 8 1140916492 1 8832 3026 0 32 3072 0 0 0 5 0 0 0 8832 0 8 4294903875 3106 0 0 0 0 738 202 208 98 2 802 832 98 2 578 183 129 578 61 27 8832 802 1376 98 1 8 'C&ase:' 8832 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 91 0 0 0 64 0 0 0 121 0 0 0 77 0 0 0] 98 0 1600 0 27 3346 3392 23 3424 61 3456 -29 3488 27 234 256 98 10 5600 8 'case' 8208 8 'pointSizeSpinner' 5264 8 'forecolor' 4832 8 'faceName' 3952 8 'pointSize' 674 578 21 1 578 21 1 738 202 208 98 1 802 832 98 2 578 1 69 578 471 279 2832 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 34 0 0 0 235 0 0 0 173 0 0 0] 98 12 7888 4448 4832 2960 3952 8208 7552 5264 3520 8832 5600 5936 1600 0 27 410 432 98 15 0 416 98 2 8 1140850688 131073 9488 0 0 0 5 0 0 0 9488 2898 234 240 98 10 410 5280 98 16 0 9488 98 2 8 1140850944 262145 9600 3026 0 32 5376 0 0 0 5 0 0 0 9600 0 8 4294903875 1218 0 0 0 738 202 208 98 1 802 832 98 2 578 21 69 578 91 43 9600 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 34 0 0 0 55 0 0 0 55 0 0 0] 98 0 1600 0 27 3346 4416 1 3424 91 3920 5 3488 43 410 3536 98 17 0 9488 98 2 8 1141055488 1 9888 0 1154 1184 0 5 0 0 0 9888 0 8 4294903711 3634 8 #chooseBackcolor 8 '...' 1 1 0 0 32 738 202 208 98 2 802 832 98 2 578 121 69 578 41 43 9888 802 1376 98 1 8 '...' 9888 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 60 0 0 0 34 0 0 0 80 0 0 0 55 0 0 0] 98 0 1600 0 27 3346 3392 11 3424 41 3456 1 3920 1 410 7904 98 14 0 9488 98 2 8 1140850695 65 10256 0 0 0 5 0 0 0 10256 0 8 4294903711 738 202 208 98 2 802 832 98 2 578 1 1 578 471 137 10256 802 1376 98 1 8 'Background' 10256 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 235 0 0 0 68 0 0 0] 98 0 1600 0 27 3346 4768 -19 7488 21 4800 1 7520 1 410 6064 98 16 0 9488 98 2 8 1140924422 1 10560 3026 0 0 5376 32 0 0 5 0 0 0 10560 0 8 4294903711 1218 0 0 0 738 202 208 98 2 802 832 98 2 578 191 71 578 211 31 10560 802 1376 98 1 8 'Fill to &end of line' 10560 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 95 0 0 0 35 0 0 0 200 0 0 0 50 0 0 0] 98 0 1600 0 27 3346 3392 31 3424 211 3456 3 3488 31 410 2224 98 16 0 9488 98 2 8 1140850956 1 10896 0 0 0 5 0 0 0 10896 0 8 4294903875 1218 0 0 0 738 202 208 98 2 802 832 98 2 578 21 39 578 61 27 10896 802 1376 98 1 8 'C&olor:
' 10896 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 19 0 0 0 40 0 0 0 32 0 0 0] 98 0 1600 0 27 3346 4768 1 3424 61 4800 39 3488 27 234 256 98 4 10560 8 'isBackcolorExtendedToEndOfLine' 9600 8 'backcolor' 674 578 21 1 578 21 1 738 202 208 98 1 802 832 98 2 578 1 357 578 471 137 9488 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 178 0 0 0 235 0 0 0 246 0 0 0] 98 5 10256 10896 9600 9888 10560 1600 0 27 410 432 98 15 0 416 98 2 8 1140850688 131073 11488 0 0 0 5 0 0 0 11488 788230 ##(Smalltalk.BorderLayout)  1 11 410 2224 98 16 0 11488 98 2 8 1140850956 1 11584 3026 0 32 3072 0 0 0 5 0 0 0 11584 0 8 4294903875 1218 0 0 0 738 202 208 98 2 802 832 98 2 578 21 5 578 435 27 11584 802 1376 98 1 8 'Preview:' 11584 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 2 0 0 0 227 0 0 0 15 0 0 0] 98 0 1600 0 27 0 0 0 410 8 ##(Smalltalk.ScintillaView)  98 46 0 11488 98 2 8 1174409476 262145 11904 3026 0 32 3072 0 1154 1184 0 5 0 0 0 11904 0 8 4294903673 1218 0 8 '' 43 0 234 256 98 4 8 #normal 1182726 ##(Smalltalk.ScintillaTextStyle)  1 0 0 1 0 0 0 0 12096 0 0 0 8 #indentGuide 12114 75 0 0 1 0 0 0 0 12144 0 0 0 234 256 98 4 1 12128 75 12160 1245510 1 ##(Smalltalk.NullScintillaStyler)  8 #preview 234 256 656 202 208 656 0 63 9215 0 0 0 0 786694 ##(Smalltalk.IndexedColor)  33554447 0 0 0 0 0 0 8 '' 3 234 256 98 2 8 #container 12064 0 0 0 0 1 0 234 256 656 738 202 208 98 10 802 832 98 2 578 21 41 578 435 151 11904 802 1376 98 1 8 '-- Abcdefghijklm ...
	... nopqrstuvwxyz --' 11904 802 1440 98 1 1474 3 1 3 11904 802 1520 98 1 32 11904 802 8 #setMarginWidths: 98 1 98 2 11 11 11904 802 8 #modificationEventMask: 98 1 9215 11904 802 8 #wordWrap: 98 1 16 11904 802 8 #margins: 98 1 98 3 984582 ##(Smalltalk.ScintillaMargin)  1 11904 1 3 32 1 12866 3 11904 1 1 32 67108863 12866 5 11904 1 1 32 1 11904 802 8 #indentationGuides: 98 1 8 #real 11904 802 8 #tabIndents: 98 1 16 11904 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 20 0 0 0 227 0 0 0 95 0 0 0] 98 0 1600 0 27 234 256 98 2 11904 8 'preview' 674 578 21 5 578 17 1 738 202 208 98 1 802 832 98 2 578 1 503 578 471 191 11488 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 251 0 0 0 235 0 0 0 90 1 0 0] 98 2 11584 11904 1600 0 27 1600 0 27 )!

resource_Developer_view
	"Answer the literal data from which the 'Developer view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Developer_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ContainerView)  98 15 0 0 98 2 8 1409286144 131073 416 0 0 0 517 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[245 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 1 2 1 34 77 83 32 83 97 110 115 32 83 101 114 105 102 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 328198 ##(Smalltalk.Point)  193 193 0 416 656198 1 ##(Smalltalk.FlowLayout)  11 21 17 234 256 98 0 590342 ##(Smalltalk.Rectangle)  578 1 21 578 1 21 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 578 3359 21 578 485 791 416 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 143 6 0 0 10 0 0 0 129 7 0 0 149 1 0 0] 98 4 410 432 98 15 0 416 98 2 8 1140850688 131073 960 0 0 0 5 0 0 0 960 610 11 1 9 234 256 98 4 410 8 ##(Smalltalk.TextEdit)  98 16 0 960 98 2 8 1140916352 1025 1072 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 5 0 0 0 1072 0 8 4294903901 852486 ##(Smalltalk.NullConverter)  0 0 35 738 202 208 98 3 802 832 98 2 578 391 1 578 69 39 1072 802 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 1072 802 8 #isTextModified: 98 1 32 1072 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 0 0 0 0 229 0 0 0 19 0 0 0] 98 0 578 193 193 0 27 8 'id' 410 1088 98 16 0 960 98 2 8 1140916352 1025 1568 0 1154 1184 0 5 0 0 0 1568 0 8 4294903901 1218 0 0 1 738 202 208 98 4 802 832 98 2 578 93 1 578 255 39 1568 802 8 #text: 98 1 8 'Name of style' 1568 802 1376 98 1 1410 3 1 3 1568 802 1456 98 1 32 1568 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 46 0 0 0 0 0 0 0 173 0 0 0 19 0 0 0] 98 0 1536 0 27 8 'name' 674 578 21 1 578 1 1 738 202 208 98 1 802 832 98 2 578 1 21 578 481 39 960 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 10 0 0 0 240 0 0 0 29 0 0 0] 98 4 410 8 ##(Smalltalk.StaticText)  98 16 0 960 98 2 8 1140850956 1 2208 0 0 0 5 0 0 0 2208 0 8 4294903875 1218 0 0 0 738 202 208 98 2 802 832 98 2 578 21 7 578 63 27 2208 802 1808 98 1 8 '&Name:' 2208 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 3 0 0 0 41 0 0 0 16 0 0 0] 98 0 1536 0 27 1568 410 2224 98 16 0 960 98 2 8 1140850955 1 2528 0 0 0 5 0 0 0 2528 0 8 4294903875 1218 0 0 0 738 202 208 98 2 802 832 98 2 578 357 7 578 25 27 2528 802 1808 98 1 8 'Id:' 2528 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 178 0 0 0 3 0 0 0 190 0 0 0 16 0 0 0] 98 0 1536 0 27 1072 1536 0 27 410 432 98 15 0 416 98 2 8 1140850688 131073 2832 0 0 0 5 0 0 0 2832 852230 ##(Smalltalk.FramingLayout)  234 240 98 24 410 8 ##(Smalltalk.ComboBox)  98 17 0 2832 98 2 8 1144063491 1025 2960 590662 2 ##(Smalltalk.ListModel)  202 208 656 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  1154 1184 0 5 0 0 0 2960 0 8 4294903891 459270 ##(Smalltalk.Message)  8 #displayString 98 0 656 401 738 202 208 98 1 802 832 98 2 578 191 151 578 171 43 2960 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 95 0 0 0 75 0 0 0 180 0 0 0 96 0 0 0] 98 0 1536 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedPreviousLeft 1 3410 8 #fixedViewLeft 171 3410 8 #fixedPreviousBottom 1 3410 8 #fixedViewTop 43 410 8 ##(Smalltalk.PushButton)  98 17 0 2832 98 2 8 1140924416 1 3552 0 1154 1184 0 5 0 0 0 3552 0 8 4294903711 1180998 4 ##(Smalltalk.CommandDescription)  8 #chooseForecolor 8 '...' 1 1 0 0 32 738 202 208 98 2 802 832 98 2 578 123 153 578 41 43 3552 802 1808 98 1 8 '...' 3552 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 61 0 0 0 76 0 0 0 81 0 0 0 97 0 0 0] 98 0 1536 0 27 3378 3410 8 #fixedPreviousRight 11 3456 41 3410 8 #fixedPreviousTop 1 3488 1 410 8 ##(Smalltalk.GroupBox)  98 14 0 2832 98 2 8 1140850695 65 4016 0 0 0 5 0 0 0 4016 0 8 4294903711 738 202 208 98 2 802 832 98 2 578 1 1 578 485 317 4016 802 1808 98 1 8 'Text' 4016 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 242 0 0 0 158 0 0 0] 98 0 1536 0 27 3378 3410 8 #fixedParentLeft -19 3410 8 #fixedParentRight 21 3410 8 #fixedParentTop 1 3410 8 #fixedParentBottom 1 410 8 ##(Smalltalk.ColorView)  98 16 0 2832 98 2 8 1140850944 262145 4464 721990 2 ##(Smalltalk.ValueHolder)  0 32 1114118 ##(Smalltalk.NeverSearchPolicy)  0 0 0 5 0 0 0 4464 0 8 4294903875 1218 0 0 0 738 202 208 98 1 802 832 98 2 578 23 153 578 91 43 4464 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 11 0 0 0 76 0 0 0 56 0 0 0 97 0 0 0] 98 0 1536 0 27 3378 3424 3 3456 91 3488 3 3520 43 410 2224 98 16 0 2832 98 2 8 1140850956 1 4816 0 0 0 5 0 0 0 4816 0 8 4294903875 1218 0 0 0 738 202 208 98 2 802 832 98 2 578 21 37 578 61 31 4816 802 1808 98 1 8 '&Font:' 4816 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 18 0 0 0 40 0 0 0 33 0 0 0] 98 0 1536 0 27 3378 4336 1 3456 61 4400 37 3520 31 410 1088 98 16 0 2832 98 2 8 1140924544 1025 5136 0 721158 ##(Smalltalk.SystemColor)  11 0 5 0 0 0 5136 0 8 4294903901 852742 ##(Smalltalk.IntegerToText)  0 8 '' 0 1 738 202 208 98 4 802 832 98 2 578 327 67 578 1 43 5136 802 1808 98 1 8 '0' 5136 802 1376 98 1 1410 3 1 3 5136 802 1456 98 1 32 5136 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 163 0 0 0 33 0 0 0 163 0 0 0 54 0 0 0] 98 0 1536 0 27 3378 3424 1 3456 61 3488 1 3520 43 410 2224 98 16 0 2832 98 2 8 1140916492 1 5600 4546 0 32 1310726 ##(Smalltalk.EqualitySearchPolicy)  0 0 0 5 0 0 0 5600 0 8 4294903875 656134 ##(Smalltalk.TimeToText)  0 0 0 0 738 202 208 98 2 802 832 98 2 578 327 37 578 61 31 5600 802 1808 98 1 8 '&Size:' 5600 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 163 0 0 0 18 0 0 0 193 0 0 0 33 0 0 0] 98 0 1536 0 27 3378 3952 27 3456 61 3984 -29 3520 31 410 8 ##(Smalltalk.SpinButton)  98 15 0 2832 98 2 8 1140916276 1 5984 4546 0 0 1376774 ##(Smalltalk.PluggableSearchPolicy)  3154 8 #= 98 0 3154 8 #hash 98 0 1 1154 1184 0 5 0 0 0 5984 0 8 4294903591 1218 0 0 738 202 208 98 3 802 832 98 2 578 387 67 578 31 43 5984 802 8 #setRange: 98 1 1410 3 201 3 5984 802 8 #udmSetAccel: 98 1 918854 1 ##(Smalltalk.StructureArray)  8 #[0 0 0 0 1 0 0 0 2 0 0 0 5 0 0 0 5 0 0 0 20 0 0 0] 7 8 ##(Smalltalk.UDACCEL)  0 17 5984 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 193 0 0 0 33 0 0 0 208 0 0 0 54 0 0 0] 98 0 1536 0 27 3378 3952 1 3456 31 3984 1 3488 1 410 432 98 15 0 2832 98 2 8 1140850688 131073 6608 0 0 0 5 0 0 0 6608 656390 ##(Smalltalk.GridLayout)  1 7 11 7 234 256 98 12 410 8 ##(Smalltalk.CheckBox)  98 16 0 6608 98 2 8 1140924422 1 6736 4546 0 0 4592 32 0 0 5 0 498 0 16 530 8 #[245 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 1 0 0 0 1 2 1 34 77 83 32 83 97 110 115 32 83 101 114 105 102 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 578 193 193 0 6736 0 8 4294903711 1218 0 0 0 738 202 208 98 2 802 832 98 2 578 155 1 578 141 43 6736 802 1808 98 1 8 '&Italic' 6736 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 77 0 0 0 0 0 0 0 147 0 0 0 21 0 0 0] 98 0 1536 0 27 8 'isItalic' 410 6752 98 16 0 6608 98 2 8 1140927750 1 7152 4546 0 0 4592 32 0 0 5 0 498 0 16 530 8 #[245 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 0 0 0 0 1 2 1 34 77 83 32 83 97 110 115 32 83 101 114 105 102 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 578 193 193 0 7152 0 8 4294903711 1218 0 0 0 738 202 208 98 2 802 832 98 2 578 3 1 578 143 43 7152 802 1808 98 1 8 '&Bold' 7152 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 0 0 0 0 72 0 0 0 21 0 0 0] 98 0 1536 0 27 8 'isBold' 410 6752 98 16 0 6608 98 2 8 1140924422 1 7552 4546 0 0 4592 32 0 0 5 0 0 0 7552 0 8 4294903711 1218 0 0 0 738 202 208 98 2 802 832 98 2 578 155 49 578 141 41 7552 802 1808 98 1 8 '&Hotspot' 7552 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 77 0 0 0 24 0 0 0 147 0 0 0 44 0 0 0] 98 0 1536 0 27 8 'isHotspot' 410 6752 98 16 0 6608 98 2 8 1140924422 1 7888 4546 0 0 4592 32 0 0 5 0 0 0 7888 0 8 4294903711 1218 0 0 0 738 202 208 98 2 802 832 98 2 578 305 49 578 143 41 7888 802 1808 98 1 8 'In&visible' 7888 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 152 0 0 0 24 0 0 0 223 0 0 0 44 0 0 0] 98 0 1536 0 27 8 'isInvisible' 410 6752 98 16 0 6608 98 2 8 1140924422 1 8224 4546 0 0 4592 32 0 0 5 0 0 0 8224 0 8 4294903711 1218 0 0 0 738 202 208 98 2 802 832 98 2 578 3 49 578 143 41 8224 802 1808 98 1 8 '&Read Only' 8224 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 24 0 0 0 72 0 0 0 44 0 0 0] 98 0 1536 0 27 8 'isReadOnly' 410 6752 98 16 0 6608 98 2 8 1140924422 1 8560 4546 0 0 4592 32 0 0 5 0 498 0 16 530 8 #[245 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 1 0 0 1 2 1 34 77 83 32 83 97 110 115 32 83 101 114 105 102 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 578 193 193 0 8560 0 8 4294903711 1218 0 0 0 738 202 208 98 2 802 832 98 2 578 305 1 578 143 43 8560 802 1808 98 1 8 '&Underlined' 8560 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 152 0 0 0 0 0 0 0 223 0 0 0 21 0 0 0] 98 0 1536 0 27 8 'isUnderlined' 674 578 3 1 578 11 1 738 202 208 98 1 802 832 98 2 578 21 207 578 457 89 6608 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 103 0 0 0 238 0 0 0 147 0 0 0] 98 6 7152 6736 8560 8224 7552 7888 1536 0 27 3378 4336 1 4368 13 3488 15 4432 -21 410 2224 98 16 0 2832 98 2 8 1140916492 1 9184 4546 0 32 5696 0 0 0 5 0 0 0 9184 0 8 4294903875 5730 0 0 0 0 738 202 208 98 2 802 832 98 2 578 21 121 578 61 31 9184 802 1808 98 1 8 '&Color:' 9184 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 60 0 0 0 40 0 0 0 75 0 0 0] 98 0 1536 0 27 3378 4336 1 3456 61 3488 13 3520 31 410 2976 98 17 0 2832 98 2 8 1152583170 1 9520 3042 202 208 656 0 3104 1154 1184 0 5 0 0 0 9520 0 8 4294903891 3154 3184 98 0 656 401 738 202 208 98 1 802 832 98 2 578 21 67 578 281 43 9520 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 33 0 0 0 150 0 0 0 54 0 0 0] 98 0 1536 0 27 3378 3424 1 3456 281 3488 1 3520 43 410 2224 98 16 0 2832 98 2 8 1140916492 1 9856 4546 0 32 5696 0 0 0 5 0 0 0 9856 0 8 4294903875 5730 0 0 0 0 738 202 208 98 2 802 832 98 2 578 191 121 578 61 31 9856 802 1808 98 1 8 'C&ase:' 9856 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 95 0 0 0 60 0 0 0 125 0 0 0 75 0 0 0] 98 0 1536 0 27 3378 3952 29 3456 61 3984 -31 3520 31 234 256 98 10 4464 8 'forecolor' 5136 8 'pointSize' 5984 8 'pointSizeSpinner' 2960 8 'case' 9520 8 'faceName' 674 578 21 1 578 21 1 738 202 208 98 1 802 832 98 2 578 1 79 578 485 317 2832 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 39 0 0 0 242 0 0 0 197 0 0 0] 98 12 4016 4816 9520 5600 5136 5984 9184 4464 3552 9856 2960 6608 1536 0 27 410 432 98 15 0 416 98 2 8 1140850688 131073 10512 0 0 0 5 0 0 0 10512 2898 234 240 98 10 410 4032 98 14 0 10512 98 2 8 1140850695 65 10624 0 0 0 5 0 0 0 10624 0 8 4294903711 738 202 208 98 2 802 832 98 2 578 1 1 578 485 145 10624 802 1808 98 1 8 'Background' 10624 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 242 0 0 0 72 0 0 0] 98 0 1536 0 27 3378 4336 -19 4368 21 4400 1 4432 11 410 2224 98 16 0 10512 98 2 8 1140850956 1 10928 0 0 0 5 0 0 0 10928 0 8 4294903875 1218 0 0 0 738 202 208 98 2 802 832 98 2 578 21 41 578 61 31 10928 802 1808 98 1 8 'C&olor:
' 10928 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 20 0 0 0 40 0 0 0 35 0 0 0] 98 0 1536 0 27 3378 4336 1 3456 61 4400 41 3520 31 410 4480 98 16 0 10512 98 2 8 1140850944 262145 11248 4546 0 32 4592 0 0 0 5 0 0 0 11248 0 8 4294903875 1218 0 0 0 738 202 208 98 1 802 832 98 2 578 23 73 578 91 43 11248 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 11 0 0 0 36 0 0 0 56 0 0 0 57 0 0 0] 98 0 1536 0 27 3378 3424 3 3456 91 3488 3 3520 43 410 6752 98 16 0 10512 98 2 8 1140924422 1 11536 4546 0 0 4592 32 0 0 5 0 0 0 11536 0 8 4294903711 1218 0 0 0 738 202 208 98 2 802 832 98 2 578 197 79 578 205 31 11536 802 1808 98 1 8 'Fill to &end of line' 11536 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 98 0 0 0 39 0 0 0 200 0 0 0 54 0 0 0] 98 0 1536 0 27 3378 3952 35 3456 205 3984 7 3520 31 410 3568 98 17 0 10512 98 2 8 1141055488 1 11872 0 1154 1184 0 5 0 0 0 11872 0 8 4294903711 3666 8 #chooseBackcolor 8 '...' 1 1 0 0 32 738 202 208 98 2 802 832 98 2 578 123 73 578 41 43 11872 802 1808 98 1 8 '...' 11872 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 61 0 0 0 36 0 0 0 81 0 0 0 57 0 0 0] 98 0 1536 0 27 3378 3952 11 3456 41 3984 1 3488 1 234 256 98 4 11248 8 'backcolor' 11536 8 'isBackcolorExtendedToEndOfLine' 674 578 21 1 578 21 11 738 202 208 98 1 802 832 98 2 578 1 415 578 485 145 10512 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 207 0 0 0 242 0 0 0 23 1 0 0] 98 5 10624 10928 11248 11872 11536 1536 0 27 410 432 98 15 0 416 98 2 8 1140850688 131073 12512 0 0 0 5 0 0 0 12512 788230 ##(Smalltalk.BorderLayout)  1 11 410 2224 98 16 0 12512 98 2 8 1140850956 1 12608 4546 0 32 5696 0 0 0 517 0 0 0 12608 0 8 4294903875 1218 0 0 0 738 202 208 98 2 802 832 98 2 578 21 1 578 449 27 12608 802 1808 98 1 8 '&Description:' 12608 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 0 0 0 0 234 0 0 0 13 0 0 0] 98 0 1536 0 27 0 0 0 410 8 ##(Smalltalk.ScintillaView)  98 46 0 12512 98 2 8 1174409476 1025 12928 4546 0 32 5696 0 1154 1184 0 5 0 0 0 12928 0 8 4294903673 1218 0 8 '' 9 0 234 256 98 4 8 #normal 1182726 ##(Smalltalk.ScintillaTextStyle)  1 0 0 1 0 0 0 0 13120 0 0 0 8 #indentGuide 13138 75 0 0 1 0 0 0 0 13168 0 0 0 98 40 13152 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 13184 0 0 1245510 1 ##(Smalltalk.NullScintillaStyler)  8 #preview 234 256 656 202 208 656 0 63 9215 0 0 0 0 786694 ##(Smalltalk.IndexedColor)  33554447 0 0 0 0 0 0 8 '' 3 234 256 98 2 8 #container 13088 0 0 0 0 1 0 234 256 656 738 202 208 98 10 802 832 98 2 578 21 37 578 449 163 12928 802 1808 98 1 8 '-- Abcdefghijklm ...
	... nopqrstuvwxyz --' 12928 802 1376 98 1 1410 3 1 3 12928 802 1456 98 1 32 12928 802 8 #setMarginWidths: 98 1 98 2 11 11 12928 802 8 #modificationEventMask: 98 1 9215 12928 802 8 #wordWrap: 98 1 16 12928 802 8 #margins: 98 1 98 3 984582 ##(Smalltalk.ScintillaMargin)  1 12928 1 3 32 1 13874 3 12928 1 1 32 67108863 13874 5 12928 1 1 32 1 12928 802 8 #indentationGuides: 98 1 8 #real 12928 802 8 #tabIndents: 98 1 16 12928 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 18 0 0 0 234 0 0 0 99 0 0 0] 98 0 1536 0 27 234 256 98 2 12928 8 'preview' 674 578 21 1 578 17 1 738 202 208 98 1 802 832 98 2 578 1 579 578 485 199 12512 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 33 1 0 0 242 0 0 0 132 1 0 0] 98 2 12608 12928 1536 0 27 1536 0 27 )! !
!ScintillaTextStylePresenter class categoriesFor: #defaultModel!models!public! !
!ScintillaTextStylePresenter class categoriesFor: #resource_Default_view!public!resources-views! !
!ScintillaTextStylePresenter class categoriesFor: #resource_Developer_view!public!resources-views! !

QueryableScintillaAttribute guid: (GUID fromString: '{DD653349-5F8C-41B5-A302-FCC645247163}')!
QueryableScintillaAttribute comment: 'QueryableScintillaAttribute is the abstract class of <ScintillaAtttribute>s that can be retrieved from the underlying control. It provides the basic mechanism for populating the attribute from the view settings.

Instance Variables:
	view		<ScintillaView> with which this attribute is associated.

Class Instance Variables:
	getMessages	<IdentityDictionary>'!
!QueryableScintillaAttribute categoriesForClass!MVP-Views-Support! !
!QueryableScintillaAttribute methodsFor!

populateFromView: aScintillaView at: anInteger 
	id := anInteger.
	view := nil.
	self class getMessages 
		ifNotNil: 
			[:map | 
			map keysAndValuesDo: 
					[:eachSetter :eachGetMessage | 
					self perform: eachSetter with: (aScintillaView sendMessage: eachGetMessage wParam: id)]].
	view := aScintillaView!

view
	^view!

view: aScintillaView 
	view := aScintillaView! !
!QueryableScintillaAttribute categoriesFor: #populateFromView:at:!initializing!private! !
!QueryableScintillaAttribute categoriesFor: #view!accessing!private! !
!QueryableScintillaAttribute categoriesFor: #view:!accessing!private! !

!QueryableScintillaAttribute class methodsFor!

getMessages
	^getMessages!

view: aScintillaView index: anInteger 
	^(self new)
		populateFromView: aScintillaView at: anInteger;
		yourself! !
!QueryableScintillaAttribute class categoriesFor: #getMessages!constants!private! !
!QueryableScintillaAttribute class categoriesFor: #view:index:!instance creation!public! !

ScintillaMarkerDefinition guid: (GUID fromString: '{6191C503-F964-4AFB-9973-C2106886CCF9}')!
ScintillaMarkerDefinition comment: 'ScintillaMarkerDefinition instances represent the configured types of <ScintillaMarker>s for a particular <ScintillaView>.

Markers can be displayed in any margins configured for the view to mark particular lines.

Instance Variables:
	code		<integer>. Marker glyph code.
	forecolor	<Color>. Foreground colour of the marker glyph.
	backcolor	<Color>. Background colour of the marker glyph.
	view		<ScintillaView>. View in which this marker type is defined.
	name		<Symbol>. Symbolic name of the marker type.

'!
!ScintillaMarkerDefinition categoriesForClass!MVP-Views-Support! !
!ScintillaMarkerDefinition methodsFor!

backcolor
	^backcolor!

backcolor: aColor 
	backcolor := aColor.
	self updateViewAttribute: #backcolor!

code
	^code!

code: aSymbol 
	code := aSymbol.
	self updateViewAttribute: #code!

forecolor
	^forecolor!

forecolor: aColor 
	forecolor := aColor.
	self updateViewAttribute: #forecolor!

glyphName
	^code < SC_MARK_CHARACTER 
		ifTrue: [self class glyphNames at: code + 1]
		ifFalse: [(Character codePoint: code - SC_MARK_CHARACTER) asSymbol]!

glyphName: aSymbol 
	self code: (self class glyphNames indexOf: aSymbol
				ifAbsent: 
					[(aSymbol size = 1 and: [aSymbol first isPrintable and: [aSymbol first isWhitespace not]]) 
						ifTrue: [aSymbol first codePoint + SC_MARK_CHARACTER + 1]
						ifFalse: [^self error: 'Invalid glyph  ' , aSymbol printString]]) 
					- 1!

id
	(id isNil and: [name notNil]) ifTrue: [id := FolderNames at: name ifAbsent: []].
	^id!

name
	^name ifNil: [self id]!

name: anObject 
	name = anObject ifTrue: [^self].
	name := anObject.
	id := name isInteger ifTrue: [name]!

newForLine: anInteger 
	"Answer a new <ScintillaMarker> of the type described by the receiver, for insertion
	at the specified line."

	^ScintillaMarker definition: self line: anInteger!

printableAttributes
	^(super printableAttributes)
		remove: #code;
		add: #glyphName;
		yourself!

setCode: anInteger forecolor: foreColor backcolor: backColor 
	code := anInteger.
	forecolor := foreColor.
	backcolor := backColor!

storeableAttributes
	^(Array with: #name) , super storeableAttributes!

view
	^view!

view: aScintillaView 
	view := aScintillaView! !
!ScintillaMarkerDefinition categoriesFor: #backcolor!accessing!public! !
!ScintillaMarkerDefinition categoriesFor: #backcolor:!accessing!public! !
!ScintillaMarkerDefinition categoriesFor: #code!accessing!private! !
!ScintillaMarkerDefinition categoriesFor: #code:!accessing!private! !
!ScintillaMarkerDefinition categoriesFor: #forecolor!accessing!public! !
!ScintillaMarkerDefinition categoriesFor: #forecolor:!accessing!public! !
!ScintillaMarkerDefinition categoriesFor: #glyphName!accessing!public! !
!ScintillaMarkerDefinition categoriesFor: #glyphName:!accessing!public! !
!ScintillaMarkerDefinition categoriesFor: #id!accessing!public! !
!ScintillaMarkerDefinition categoriesFor: #name!accessing!public! !
!ScintillaMarkerDefinition categoriesFor: #name:!accessing!public! !
!ScintillaMarkerDefinition categoriesFor: #newForLine:!adding!public! !
!ScintillaMarkerDefinition categoriesFor: #printableAttributes!development!helpers!printing!private! !
!ScintillaMarkerDefinition categoriesFor: #setCode:forecolor:backcolor:!initializing!private! !
!ScintillaMarkerDefinition categoriesFor: #storeableAttributes!development!printing!private! !
!ScintillaMarkerDefinition categoriesFor: #view!accessing!private! !
!ScintillaMarkerDefinition categoriesFor: #view:!accessing!private! !

!ScintillaMarkerDefinition class methodsFor!

arrows
	"Answer a set of marker definitions for fold margin markers in the Macintosh 'Arrow' style.
	This is a minimalist set with only a right arrow for collapsed fold points, and down arrow
	for expanded."

	| white black |
	white := Color white.
	black := Color black.
	^(OrderedCollection new)
		add: ((self new)
					name: #folderOpen;
					forecolor: black;
					backcolor: black;
					glyphName: #arrowDown;
					yourself);
		add: ((self new)
					name: #folder;
					forecolor: black;
					backcolor: black;
					glyphName: #arrow;
					yourself);
		add: ((self new)
					name: #folderSub;
					forecolor: black;
					backcolor: black;
					glyphName: #empty;
					yourself);
		add: ((self new)
					name: #folderTail;
					forecolor: black;
					backcolor: black;
					glyphName: #empty;
					yourself);
		add: ((self new)
					name: #folderEnd;
					forecolor: white;
					backcolor: black;
					glyphName: #empty;
					yourself);
		add: ((self new)
					name: #folderOpenMid;
					forecolor: white;
					backcolor: black;
					glyphName: #empty;
					yourself);
		add: ((self new)
					name: #folderMidTail;
					forecolor: white;
					backcolor: black;
					glyphName: #empty;
					yourself);
		yourself!

boxTree
	"Answer a set of marker definitions for fold margin markers in the 'Box Tree' style. This is
	like a flattened tree control using square headers and right angle joins"

	| forecolor backcolor |
	forecolor := Color white.
	backcolor := RGB 
				red: 16r80
				green: 16r80
				blue: 16r80.
	^(OrderedCollection new)
		add: ((self new)
					name: #folderOpen;
					forecolor: forecolor;
					backcolor: backcolor;
					glyphName: #boxMinus;
					yourself);
		add: ((self new)
					name: #folder;
					forecolor: forecolor;
					backcolor: backcolor;
					glyphName: #boxPlus;
					yourself);
		add: ((self new)
					name: #folderSub;
					forecolor: forecolor;
					backcolor: backcolor;
					glyphName: #verticalLine;
					yourself);
		add: ((self new)
					name: #folderTail;
					forecolor: forecolor;
					backcolor: backcolor;
					glyphName: #leftCorner;
					yourself);
		add: ((self new)
					name: #folderEnd;
					forecolor: forecolor;
					backcolor: backcolor;
					glyphName: #boxPlusConnected;
					yourself);
		add: ((self new)
					name: #folderOpenMid;
					forecolor: forecolor;
					backcolor: backcolor;
					glyphName: #boxMinusConnected;
					yourself);
		add: ((self new)
					name: #folderMidTail;
					forecolor: forecolor;
					backcolor: backcolor;
					glyphName: #teeJunction;
					yourself);
		yourself!

circleTree
	"Answer a set of marker definitions for fold margin markers in the 'Circle Tree' style. This
	is like a flattened tree control using circular headers and curved joins."

	| forecolor backcolor |
	forecolor := Color white.
	backcolor := RGB 
				red: 16r40
				green: 16r40
				blue: 16r40.
	^(OrderedCollection new)
		add: ((self new)
					name: #folderOpen;
					forecolor: forecolor;
					backcolor: backcolor;
					glyphName: #circleMinus;
					yourself);
		add: ((self new)
					name: #folder;
					forecolor: forecolor;
					backcolor: backcolor;
					glyphName: #circlePlus;
					yourself);
		add: ((self new)
					name: #folderSub;
					forecolor: forecolor;
					backcolor: backcolor;
					glyphName: #verticalLine;
					yourself);
		add: ((self new)
					name: #folderTail;
					forecolor: forecolor;
					backcolor: backcolor;
					glyphName: #leftCornerCurve;
					yourself);
		add: ((self new)
					name: #folderEnd;
					forecolor: forecolor;
					backcolor: backcolor;
					glyphName: #circlePlusConnected;
					yourself);
		add: ((self new)
					name: #folderOpenMid;
					forecolor: forecolor;
					backcolor: backcolor;
					glyphName: #circleMinusConnected;
					yourself);
		add: ((self new)
					name: #folderMidTail;
					forecolor: forecolor;
					backcolor: backcolor;
					glyphName: #teeJunctionCurve;
					yourself);
		yourself!

code: anInteger forecolor: foreColor backcolor: backColor 
	^(self basicNew)
		setCode: anInteger
			forecolor: foreColor
			backcolor: backColor;
		yourself!

folderIds
	^FolderNames values!

glyphNames
	"Answer the symbolic names of all predefined marker shapes. These correspond with the codes in the SC_MARK_XXX enumeration.
	Note that any visible character in the font can also be used as a marker glyph."

	^##(| names |
	names := #(#circle #roundRect #arrow #smallRect #shortArrow #empty #arrowDown #minus #plus).
	"Shapes for fold margin"
	names := names 
				, #(#verticalLine #leftCorner #teeJunction #boxPlus #boxPlusConnected #boxMinus #boxMinusConnected #leftCornerCurve #teeJunctionCurve #circlePlus #circlePlusConnected #circleMinus #circleMinusConnected).
	"Invisible marker that sets the line background colour to match that defined for the marker"
	names := names , #(#background).
	names := names , #(#dotDotDot #arrows #pixmap).
	"Markers for outlining column (not visible)"
	names := names 
				, #(#folderEnd #folderOpenMid #folderMidTail #folderTail #folderSub #folder #folderOpen).
	names)
!

initialize
	"
	self initialize
	"

	attributes := (IdentityDictionary new)
				at: #backcolor put: SCI_MARKERSETBACK;
				at: #forecolor put: SCI_MARKERSETFORE;
				at: #code put: SCI_MARKERDEFINE;
				shrink;
				isImmutable: true;
				yourself.
	FolderNames := (IdentityDictionary new)
				at: #folderEnd put: SC_MARKNUM_FOLDEREND;
				at: #folderOpenMid put: SC_MARKNUM_FOLDEROPENMID;
				at: #folderMidTail put: SC_MARKNUM_FOLDERMIDTAIL;
				at: #folderTail put: SC_MARKNUM_FOLDERTAIL;
				at: #folderSub put: SC_MARKNUM_FOLDERSUB;
				at: #folder put: SC_MARKNUM_FOLDER;
				at: #folderOpen put: SC_MARKNUM_FOLDEROPEN;
				shrink;
				isImmutable: true;
				yourself!

new
	^self 
		code: 0
		forecolor: Color black
		backcolor: Color white!

plusMinus
	"Answer a set of marker definitions for fold margin markers in the 'Plus/Minus' style. This
	is a minimalist set with only a plus sign for collapsed fold points, and minus sign for
	expanded."

	"If you want basically this style but a heavier effect, either invert the foreground and
	background colours, or set both to black"

	| forecolor backcolor |
	forecolor := Color white.
	backcolor := Color black.
	^(OrderedCollection new)
		add: ((self new)
					name: #folderOpen;
					forecolor: forecolor;
					backcolor: backcolor;
					glyphName: #minus;
					yourself);
		add: ((self new)
					name: #folder;
					forecolor: forecolor;
					backcolor: backcolor;
					glyphName: #plus;
					yourself);
		add: ((self new)
					name: #folderSub;
					forecolor: forecolor;
					backcolor: backcolor;
					glyphName: #empty;
					yourself);
		add: ((self new)
					name: #folderTail;
					forecolor: forecolor;
					backcolor: backcolor;
					glyphName: #empty;
					yourself);
		add: ((self new)
					name: #folderEnd;
					forecolor: forecolor;
					backcolor: backcolor;
					glyphName: #empty;
					yourself);
		add: ((self new)
					name: #folderOpenMid;
					forecolor: forecolor;
					backcolor: backcolor;
					glyphName: #empty;
					yourself);
		add: ((self new)
					name: #folderMidTail;
					forecolor: forecolor;
					backcolor: backcolor;
					glyphName: #empty;
					yourself);
		yourself!

visibleCharacterGlyphs
	^(Character byteCharacterSet 
		select: [:each | each isPrintable and: [each isWhitespace not]]) asArray 
		collect: [:each | each asSymbol]! !
!ScintillaMarkerDefinition class categoriesFor: #arrows!constants!public! !
!ScintillaMarkerDefinition class categoriesFor: #boxTree!constants!public! !
!ScintillaMarkerDefinition class categoriesFor: #circleTree!constants!public! !
!ScintillaMarkerDefinition class categoriesFor: #code:forecolor:backcolor:!instance creation!public! !
!ScintillaMarkerDefinition class categoriesFor: #folderIds!constants!public! !
!ScintillaMarkerDefinition class categoriesFor: #glyphNames!constants!public! !
!ScintillaMarkerDefinition class categoriesFor: #initialize!initializing!public! !
!ScintillaMarkerDefinition class categoriesFor: #new!instance creation!public! !
!ScintillaMarkerDefinition class categoriesFor: #plusMinus!constants!public! !
!ScintillaMarkerDefinition class categoriesFor: #visibleCharacterGlyphs!constants!development!public! !

ScintillaTextStyle guid: (GUID fromString: '{441D5422-DB7D-4872-A16D-D6CA9D88E0B8}')!
ScintillaTextStyle comment: 'ScintillaStyle instances represent the visual styles that can be applied to colour text in a particular <ScintillaView>.

There are a number of predefined styles:
	#normal - the default style used to draw all unstyled text, and from which user-defined styles inherit.
	#braceHighlight 	- style used to highlight matching braces (where the brace highlighting feature is implemented)
	#mismatchedBrace - style used to highlight mismatched braces (where the brace highlighting feature is implemented)
	#controlChar - style used to draw control characters such as Form Feed when these are present in the text.
	#lineNumber - style used to draw text in the line number margin, and to control the background colour of all margins except the fold margin.
	#indentGuide - style used to draw indentation guides
	
With the exception of #normal, these styles are present to allow control over the presentation of particular features, but they cannot themselves be set against regions of text. Note that some styling, such as that used for visible whitespace, is controlled independently of the normal text style definitions  by directly setting properties of the ScintillaView.

Up to 32 other styles* can be defined per view. These have symbolic names for use in Smalltalk code, and have id''s allocated automatically. The predefined styles listed above have ids which Scintilla recognises specifically, and these are not included in the limit of 32 styles. All other styles are user-defined, can have any symbolic name, and are allocated an id in the range 1..31. 

The instance variables of all styles specify the differences from the default style, so any that are nil imply inheritance of that aspect from the special #normal style (which has style id 0). In turn the #normal style "inherits" any unspecified font and color settings from those aspects of the view itself. 

*It is possible to define more than 32 styles by changing the number of #styleBits configured for the view, but this reduces the number of indicators that can be used, and is not recommended.

Instance Variables:
	forecolor	<Color>. Text foreground colour.
	backcolor	<Color>. Background colour.
	flags		<integer> Various flags controlling font appearance, etc.
	faceName	<String>. Font face name.
	pointSize	<integer>. Font size in points.
	characterSet	<integer>. Font character set.
	case		<integer>. Font case type
	name		<Symbol>. Symbolic name of the visual style.
	description	<String>. Optional description of the style, useful for presentation in user preferences dialogs.

Class Variables:
	ItalicMask			<integer> bitmask used to specify an italic font in ''flags'' inst. var.
	PredefinedStyleNames	<IdentityDictionary> mapping <Symbol> to <integer> style code for all fixed/predefined styles.
	BoldMask			<integer> bitmask used to specify a bold font in ''flags'' inst. var.
	UnderlinedMask		<integer> bitmask used to specify an underlined font in ''flags'' inst. var.
	ChangeableMask		<integer> bitmask used to control whether the marked text is protected from editing.
	EolFilledMask		<integer> bitmask used to specify whether the backcolor is extended to the end-of-line.
	HotspotMask		<integer> bitmask used to specify whether the marked text should be treated as a hotspot.'!
!ScintillaTextStyle categoriesForClass!MVP-Views-Support! !
!ScintillaTextStyle methodsFor!

= anObject 
	"Answer whether the receiver and the <Object> argument are considered equivalent. A pair of
	<ScintillaTextStyle>s are only considered equivalent if all attributes are equal."

	^anObject class == self species and: 
			[self name = anObject name and: 
					[self flags = anObject flags and: 
							[self forecolor = anObject forecolor and: 
									[self backcolor = anObject backcolor and: 
											[self fontName = anObject fontName and: 
													[self pointSize = anObject pointSize 
														and: [self characterSet = anObject characterSet and: [self case = anObject case]]]]]]]]!

backcolor
	^backcolor!

backcolor: aColorOrNil 
	backcolor = aColorOrNil ifTrue: [^self].
	backcolor := aColorOrNil!

case
	^case!

case: anIntegerOrNil 
	case = anIntegerOrNil ifTrue: [^self].
	case := anIntegerOrNil!

caseName
	^self case ifNotNil: [:enum | self class caseNames at: enum+1]!

caseName: aSymbolOrNil 
	self 
		case: (aSymbolOrNil isNil ifFalse: [(self class caseNames indexOf: aSymbolOrNil) - 1])!

characterSet
	^characterSet!

characterSet: anIntegerOrNil 
	characterSet = anIntegerOrNil ifTrue: [^self].
	characterSet := anIntegerOrNil!

clearFont
	"Clear all font settings so that these are inherited."

	faceName := pointSize := characterSet := nil.
	flags := flags maskClear: ItalicMask | BoldMask | UnderlinedMask!

description
	^description ifNil: [self name ifNotNil: [:string | string asPhrase]]!

description: aString 
	"Set the user-defined description of this text style to the specified <String>."

	description := (aString notNil and: [aString notEmpty]) ifTrue: [aString]!

displayOn: aStream 
	aStream nextPutAll: self name displayString asPhrase!

flags
	^flags!

font
	"Answer a font configured with the receiver's settings."

	| answer |
	answer := self fontName ifNil: [Font default] ifNotNil: [:face | Font name: face].
	self restyleFont: answer.
	^answer!

font: aFont 
	self clearFont.
	self mergeFont: aFont!

fontName
	^faceName!

fontName: aStringOrNil 
	faceName = aStringOrNil ifTrue: [^self].
	faceName := aStringOrNil!

forecolor
	^forecolor!

forecolor: aColorOrNil 
	forecolor := aColorOrNil!

getThreeStateFlag: anInteger
	^#(nil true false) at: ((flags bitAnd: anInteger) >> (anInteger lowBit - 1)) + 1!

id
	(id isNil and: [name notNil]) ifTrue: [id := PredefinedStyleNames at: name ifAbsent: []].
	^id!

initialize
	super initialize.
	flags := 0.
!

isBackcolorExtendedToEndOfLine
	"Answer whether the background colour of this style will be extended to the end of the line if the last 
	visible character on the line has this style. This corresponds to SCI_STYLESETEOLFILLED, see
	the Scintilla documentation for further details."

	^self getThreeStateFlag: EolFilledMask!

isBackcolorExtendedToEndOfLine: aBooleanOrNil 
	"Set whether the background colour of this style will be extended to the end of the line if the last 
	visible character on the line has this style. This corresponds to SCI_STYLESETEOLFILLED, see
	the Scintilla documentation for further details."

	self setThreeStateFlag: EolFilledMask value: aBooleanOrNil!

isBold
	"Answer true if the receiver's font is bold, false if regular weight, or nil if not specified."

	^self getThreeStateFlag: BoldMask!

isBold: aBooleanOrNil 
	"Set the receiver's three state flag that specifies whether the associated font
	is bold. If the argument is nil, then this style has no effect on the default
	font."

	self setThreeStateFlag: BoldMask value: aBooleanOrNil!

isChangeable
	^self isReadOnly ifNotNil: [:readOnly | readOnly not]!

isHotspot
	"Answer true if the text marked with this style is a hotspot that can detect
	mouse clicks, e.g. for hypertext linking.."

	^self getThreeStateFlag: HotspotMask!

isHotspot: aBooleanOrNil 
	"Set whether the text marked with this style is a hotspot that can detect
	mouse clicks, e.g. for hypertext linking.."

	self setThreeStateFlag: HotspotMask value: aBooleanOrNil!

isInvisible
	"Answer whether text marked with this style is hidden."

	^self getThreeStateFlag: InvisibleMask!

isInvisible: aBooleanOrNil 
	"Set  whether text marked with this style is hidden."

	self setThreeStateFlag: InvisibleMask value: aBooleanOrNil!

isItalic
	"Answer true if the receiver's font is italic, false if regular, or nil if not specified."

	^self getThreeStateFlag: ItalicMask!

isItalic: aBooleanOrNil 
	"Set the receiver's three state flag that specifies whether the associated font
	is italic. If the argument is nil, then this  style has no effect on the default
	font."

	self setThreeStateFlag: ItalicMask value: aBooleanOrNil!

isPredefined
	"Answer whether the receiver represents one of the styles pre-defined by Scintilla."

	| n |
	n := self name.
	^n isInteger not and: [PredefinedStyleNames includesKey: n]!

isReadOnly
	"Answer true if the text marked with this style is protected against editing."

	^(self getThreeStateFlag: ReadOnlyMask)!

isReadOnly: aBooleanOrNil 
	"Set whether the text marked with this style will be protected against editing."

	self setThreeStateFlag: ReadOnlyMask value: aBooleanOrNil!

isUnderlined
	"Answer true if the receiver's font is underlined, false if regular weight, or nil if not specified."

	^self getThreeStateFlag: UnderlinedMask!

isUnderlined: aBooleanOrNil 
	"Set the receiver's three state flag that specifies whether the associated font
	is underlined. If the argument is nil, then this style has no effect on the default
	font."

	self setThreeStateFlag: UnderlinedMask value: aBooleanOrNil!

isVisible
	"Private - Answer whether text marked with this style is visible."

	^(self getThreeStateFlag: InvisibleMask) ifNotNil: [:invisible | invisible not]!

mergeFont: aFont 
	"Initialize any font settings that are currently unspecified from the <Font> argument."

	faceName ifNil: [faceName := aFont name].
	pointSize ifNil: [pointSize := aFont pointSize].
	self isItalic ifNil: [self setThreeStateFlag: ItalicMask value: aFont isItalic].
	self isBold ifNil: [self setThreeStateFlag: BoldMask value: aFont isBold].
	self isUnderlined ifNil: [self setThreeStateFlag: UnderlinedMask value: aFont isUnderlined].
	characterSet ifNil: [characterSet := aFont characterSet]!

name
	^name ifNil: ['style', self id displayString]!

name: anObject 
	name = anObject ifTrue: [^self].
	name := anObject isInteger 
				ifTrue: [id := anObject]
				ifFalse: [anObject  isEmpty ifFalse: [anObject asSymbol]]!

pointSize
	^pointSize!

pointSize: anIntegerOrNil 
	pointSize := anIntegerOrNil!

restyleFont: aFont 
	self pointSize ifNotNil: [:points | aFont pointSize: points].
	self isItalic ifNotNil: [:isItalic | aFont isItalic: isItalic].
	self isBold ifNotNil: [:isBold | aFont isBold: isBold].
	self isUnderlined ifNotNil: [:isUnderlined | aFont isUnderlined: isUnderlined].
	self characterSet ifNotNil: [:set | aFont characterSet: set]!

setThreeStateFlag: anInteger value: aBooleanOrNil
	| state |
	state := aBooleanOrNil isNil 
				ifTrue: [0]
				ifFalse: 
					[aBooleanOrNil 
						ifTrue: [(anInteger bitShift: -1) bitAnd: anInteger]
						ifFalse: [(anInteger bitShift: 1) bitAnd: anInteger]].
	flags := (flags maskClear: anInteger) maskSet: state!

storeableAttributes
	| attribs |
	attribs := super storeableAttributes.
	name ifNotNil: [attribs := (Array with: #name) , attribs].
	^attribs! !
!ScintillaTextStyle categoriesFor: #=!comparing!public! !
!ScintillaTextStyle categoriesFor: #backcolor!accessing!public! !
!ScintillaTextStyle categoriesFor: #backcolor:!accessing!public! !
!ScintillaTextStyle categoriesFor: #case!accessing!public! !
!ScintillaTextStyle categoriesFor: #case:!accessing!public! !
!ScintillaTextStyle categoriesFor: #caseName!accessing!public! !
!ScintillaTextStyle categoriesFor: #caseName:!accessing!public! !
!ScintillaTextStyle categoriesFor: #characterSet!accessing!public! !
!ScintillaTextStyle categoriesFor: #characterSet:!accessing!public! !
!ScintillaTextStyle categoriesFor: #clearFont!accessing!public! !
!ScintillaTextStyle categoriesFor: #description!accessing!public! !
!ScintillaTextStyle categoriesFor: #description:!accessing!public! !
!ScintillaTextStyle categoriesFor: #displayOn:!displaying!public! !
!ScintillaTextStyle categoriesFor: #flags!accessing!public! !
!ScintillaTextStyle categoriesFor: #font!accessing!public! !
!ScintillaTextStyle categoriesFor: #font:!accessing!public! !
!ScintillaTextStyle categoriesFor: #fontName!accessing!public! !
!ScintillaTextStyle categoriesFor: #fontName:!accessing!public! !
!ScintillaTextStyle categoriesFor: #forecolor!accessing!public! !
!ScintillaTextStyle categoriesFor: #forecolor:!accessing!public! !
!ScintillaTextStyle categoriesFor: #getThreeStateFlag:!helpers!private! !
!ScintillaTextStyle categoriesFor: #id!accessing!public! !
!ScintillaTextStyle categoriesFor: #initialize!initializing!private! !
!ScintillaTextStyle categoriesFor: #isBackcolorExtendedToEndOfLine!public!testing! !
!ScintillaTextStyle categoriesFor: #isBackcolorExtendedToEndOfLine:!accessing!public! !
!ScintillaTextStyle categoriesFor: #isBold!public!testing! !
!ScintillaTextStyle categoriesFor: #isBold:!accessing!public! !
!ScintillaTextStyle categoriesFor: #isChangeable!private!testing! !
!ScintillaTextStyle categoriesFor: #isHotspot!public!testing! !
!ScintillaTextStyle categoriesFor: #isHotspot:!accessing!public! !
!ScintillaTextStyle categoriesFor: #isInvisible!public!testing! !
!ScintillaTextStyle categoriesFor: #isInvisible:!accessing!public! !
!ScintillaTextStyle categoriesFor: #isItalic!public!testing! !
!ScintillaTextStyle categoriesFor: #isItalic:!accessing!public! !
!ScintillaTextStyle categoriesFor: #isPredefined!public!testing! !
!ScintillaTextStyle categoriesFor: #isReadOnly!public!testing! !
!ScintillaTextStyle categoriesFor: #isReadOnly:!accessing!public! !
!ScintillaTextStyle categoriesFor: #isUnderlined!public!testing! !
!ScintillaTextStyle categoriesFor: #isUnderlined:!accessing!public! !
!ScintillaTextStyle categoriesFor: #isVisible!private!testing! !
!ScintillaTextStyle categoriesFor: #mergeFont:!accessing!public! !
!ScintillaTextStyle categoriesFor: #name!accessing!public! !
!ScintillaTextStyle categoriesFor: #name:!accessing!public! !
!ScintillaTextStyle categoriesFor: #pointSize!accessing!public! !
!ScintillaTextStyle categoriesFor: #pointSize:!accessing!public! !
!ScintillaTextStyle categoriesFor: #restyleFont:!accessing!public! !
!ScintillaTextStyle categoriesFor: #setThreeStateFlag:value:!helpers!private! !
!ScintillaTextStyle categoriesFor: #storeableAttributes!development!printing!private! !

!ScintillaTextStyle class methodsFor!

caseNames
	^#(#mixed #upper #lower)!

icon
	^Font icon!

initialize
	"
		self initialize
	"

	BoldMask := 2r11.
	ItalicMask := 2r1100.
	UnderlinedMask := 2r110000.
	EolFilledMask := 2r11000000.
	ReadOnlyMask := 2r1100000000.
	HotspotMask := 2r110000000000.
	InvisibleMask := 2r11000000000000.
	attributes := (IdentityDictionary new)
				at: #forecolor put: SCI_STYLESETFORE;
				at: #backcolor put: SCI_STYLESETBACK;
				at: #fontName put: SCI_STYLESETFONT;
				at: #isBold put: SCI_STYLESETBOLD;
				at: #isItalic put: SCI_STYLESETITALIC;
				at: #isUnderlined put: SCI_STYLESETUNDERLINE;
				at: #case put: SCI_STYLESETCASE;
				at: #characterSet put: SCI_STYLESETCHARACTERSET;
				at: #pointSize put: SCI_STYLESETSIZE;
				at: #isBackcolorExtendedToEndOfLine put: SCI_STYLESETEOLFILLED;
				at: #isChangeable put: SCI_STYLESETCHANGEABLE;
				at: #isHotspot put: SCI_STYLESETHOTSPOT;
				at: #isVisible put: SCI_STYLESETVISIBLE;
				shrink;
				isImmutable: true;
				yourself.
	PredefinedStyleNames := IdentityDictionary new: STYLE_LASTPREDEFINED + 1.
	(STYLE_NORMAL + 1 to: STYLE_LASTPREDEFINED) do: [:each | PredefinedStyleNames at: each put: each].
	"These are not really Scintilla pre-defined styles, but some additional ones we add for convenience in the wrapping"
	PredefinedStyleNames
		at: #normal put: STYLE_NORMAL;
		yourself.
	PredefinedStyleNames
		removeKey: STYLE_BRACELIGHT;
		at: #braceHighlight put: STYLE_BRACELIGHT;
		removeKey: STYLE_BRACEBAD;
		at: #braceMismatch put: STYLE_BRACEBAD;
		removeKey: STYLE_CONTROLCHAR;
		at: #controlChar put: STYLE_CONTROLCHAR;
		removeKey: STYLE_LINENUMBER;
		at: #lineNumber put: STYLE_LINENUMBER;
		removeKey: STYLE_INDENTGUIDE;
		at: #indentGuide put: STYLE_INDENTGUIDE;
		removeKey: STYLE_CALLTIP;
		at: #callTip put: STYLE_CALLTIP;
		removeKey: STYLE_DEFAULT.
	PredefinedStyleNames
		isImmutable: true;
		shrink!

name: aSymbol 
	^(self new)
		name: aSymbol;
		yourself!

new
	^super new initialize!

normal
	^(self name: #normal)
		description: 'Default text style';
		yourself! !
!ScintillaTextStyle class categoriesFor: #caseNames!constants!public! !
!ScintillaTextStyle class categoriesFor: #icon!constants!public! !
!ScintillaTextStyle class categoriesFor: #initialize!initializing!public! !
!ScintillaTextStyle class categoriesFor: #name:!instance creation!public! !
!ScintillaTextStyle class categoriesFor: #new!instance creation!public! !
!ScintillaTextStyle class categoriesFor: #normal!instance creation!public! !

ScintillaIndicatorStyle guid: (GUID fromString: '{2DE64FD7-5B9F-4AC7-81C6-912B56B41A4F}')!
ScintillaIndicatorStyle comment: 'ScintillaIndicatorStyle instances represent the indicator configuration for a particular <ScintillaView>.

Indicators are orthogonal to visual styles, and can be used to highlight areas of text regardless of styling. A common example usage would be highlight erroneous text by underlining it with a squiggly line.

Older versions of Scintilla supported only 3 indicator types. Indicators shared the styling byte associated with each character, with 5 bits of that byte being allocated to the styles in the normal configuration, although this could be changed (see ScintillaView>>styleBits:). Each character position may only have one visual style, so the 5 bits provided for 32 possible visual styles. Unlike the visual styles, however, each character may have any combination of indicators set, so the 3-bits available for indicators equated to only 3 indicator types. In other words each indicator type required that a bit be reserved.

As of Scintilla 1.75 indicators have become more useful as they can be independent of styles. This means all style bits are available for styles (now up to 255), but also that indicators can be applied completely independently and that there are now up to 32 different types of indicator available. In our original Dolphin wrapping we didn''t name indicator styles because of the implementation. Our wrapper now supports the more powerful and numerous "modern indicators", and indicators are now named symbolically. 

Indicator styles are separated into two ranges:
	0..INDIC_CONTAINER-1			(0..7)		Reserved for use by lexers
	INDIC_CONTAINER..INDIC_MAX		(8..31)		Available for use by the container

Named indicators are automatically allocated an id in the container range. There is a limit of 24 of these. Any that are unconfigured are hidden; they can be set, but will have no visual effect.

The style of lexer indicators can be configured by adding a <ScintillaIndicatorStyle> to the #indicatorStyles collection and explicitly setting the Id to the desired integer value. This can be useful when debugging a <ScintillaStyler> that is using indicators for mark text regions for internal purposes. For example the <SmalltalkStyler> uses an indicator to mark text regions that are covered by literal arrays. Normally indicators used for such purposes would be configured as invisible.

For historical reasons the first 3 indicator styles are preconfigured by Scintilla. If you don''t configure these in the indicatorStyles collection then they will remain with default settings, as described in the Scintilla documentation.

A fixed set of named styles is supported:
	#underline		Underlines the text with a straight line
	#squiggle		Underlines the text with a squiggly line
	#tt			Underlines the text with a dashed line made up of little T''s
	#hatch		Underlines the text with a line of diagonal dashes
	#strikeOut		Strikes through the text with a straight line
	#hidden		An invisible indicator (can be used to temporarily hide a particular indicator type without removing it from the text)
	#box			Surrounds the text with a box
	#roundBox	Surrounds the text with a translucent round cornered box drawn with alpha blending so that the interior is more tranlucent than the edges.

Instance Variables:
	forecolor	<Color>. Foreground colour used to draw the indicator.
	style		<integer> from the INDIC_xxxx enumeration. These relate to the named styles listed above.
	under	<boolean>. Determines whether the indicator is drawn under or over the text when two phase drawing is enabled.
	name	<symbol> uniquely naming a container indicator, or <integer> id of the lexer indicator
'!
!ScintillaIndicatorStyle categoriesForClass!MVP-Views-Support! !
!ScintillaIndicatorStyle methodsFor!

forecolor
	^forecolor ifNotNil: [:color | color isInteger ifTrue: [Color fromInteger: color] ifFalse: [color]]!

forecolor: aColor 
	forecolor := aColor asParameter."isInteger ifTrue: [Color fromInteger: aColor] ifFalse: [aColor]."
	self updateViewAttribute: #forecolor!

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

name
	^name ifNil: ['indicator' , self id displayString]!

name: anObject 
	name = anObject ifTrue: [^self].
	name := anObject isInteger 
				ifTrue: [id := anObject]
				ifFalse: [anObject isEmpty ifFalse: [anObject asSymbol]]!

printableAttributes
	^(super printableAttributes)
		remove: #style;
		add: #styleName;
		yourself!

style
	^style!

style: anInteger 
	(anInteger between: INDIC_PLAIN and: INDIC_ROUNDBOX) 
		ifFalse: [^self error: 'Invalid indicator style'].
	style := anInteger.
	self updateViewAttribute: #style!

styleName
	^StyleNames at: self style + 1!

styleName: aSymbol 
	self 
		style: (StyleNames indexOf: aSymbol
				ifAbsent: [^self error: 'Unrecognised style name: ' , aSymbol]) - 1! !
!ScintillaIndicatorStyle categoriesFor: #forecolor!accessing!public! !
!ScintillaIndicatorStyle categoriesFor: #forecolor:!accessing!public! !
!ScintillaIndicatorStyle categoriesFor: #id!accessing!private! !
!ScintillaIndicatorStyle categoriesFor: #initialize!private! !
!ScintillaIndicatorStyle categoriesFor: #isUnderText!public!testing! !
!ScintillaIndicatorStyle categoriesFor: #isUnderText:!accessing!public! !
!ScintillaIndicatorStyle categoriesFor: #name!accessing!public! !
!ScintillaIndicatorStyle categoriesFor: #name:!accessing!public! !
!ScintillaIndicatorStyle categoriesFor: #printableAttributes!development!helpers!printing!private! !
!ScintillaIndicatorStyle categoriesFor: #style!accessing!public! !
!ScintillaIndicatorStyle categoriesFor: #style:!accessing!public! !
!ScintillaIndicatorStyle categoriesFor: #styleName!accessing!public! !
!ScintillaIndicatorStyle categoriesFor: #styleName:!accessing!public! !

!ScintillaIndicatorStyle class methodsFor!

initialize
	"
		self initialize
	"

	StyleNames := #(#underline #squiggle #tt #hatch #strikeOut #hidden #box #roundBox).
	getMessages := (IdentityDictionary new)
				at: #forecolor: put: SCI_INDICGETFORE;
				at: #style: put: SCI_INDICGETSTYLE;
				at: #isUnderText: put: SCI_INDICGETUNDER;
				shrink;
				isImmutable: true;
				yourself.
	attributes := (IdentityDictionary new)
				at: #forecolor put: SCI_INDICSETFORE;
				at: #style put: SCI_INDICSETSTYLE;
				at: #isUnderText put: SCI_INDICSETUNDER;
				shrink;
				isImmutable: true;
				yourself!

new
	^(super new)
		initialize;
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
!ScintillaIndicatorStyle class categoriesFor: #initialize!initializing!public! !
!ScintillaIndicatorStyle class categoriesFor: #new!public! !
!ScintillaIndicatorStyle class categoriesFor: #stbConvertFrom:!binary filing!private! !
!ScintillaIndicatorStyle class categoriesFor: #stbVersion!binary filing!public! !

ScintillaMargin guid: (GUID fromString: '{FD6F73FE-70D8-4E4A-8332-EC147E5BA899}')!
ScintillaMargin comment: 'ScintillaMargin instances represent the ''margin'' attributes of a <ScintillaView>.

From the Scintilla Documentation:

"There may be up to three margins to the left of the text display, plus a gap either side of the text. Each margin can be set to display either symbols or line numbers [using the #isNumbers aspect]. The markers that can be displayed in each margin are set with [#mask]. Any markers not associated with a visible margin will be displayed as changes in background colour in the text. A width in pixels can be set for each margin. Margins with a zero width are ignored completely. You can choose if a mouse click in a margin [triggers a #marginClicked event] or selects a line of text [by using the #isSensitive aspect].

The margins are numbered 0 to 2. Using a margin number outside the valid range has no effect. By default, margin 0 is set to display line numbers, but is given a width of 0, so it is hidden. Margin 1 is set to display non-folding symbols and is given a width of 16 pixels, so it is visible. Margin 2 is set to display the folding symbols, but is given a width of 0, so it is hidden. Of course, you can set the margins to be whatever you wish."

Instance Variables:
	width		<integer>. Pixel width of the margin (0 if invisible)
	type		<integer>. Type code (numbers, symbols, or numbers and symbols)
	isSensitive	<boolean>. Whether responds to mouse clicks by sending SCN_MARGINCLICK notifications.
	mask		<integer> flags controlling which markers can be displayed. See #mask for further details.

'!
!ScintillaMargin categoriesForClass!MVP-Views-Support! !
!ScintillaMargin methodsFor!

isFolders
	^mask allMask: SC_MASK_FOLDERS!

isFolders: aBoolean 
	self mask: (self mask mask: SC_MASK_FOLDERS set: aBoolean)!

isNumbers
	"Answer whether this margin will display line numbers (i.e. its type is SC_MARGIN_NUMBER)."

	^type allMask: SC_MARGIN_NUMBER!

isNumbers: aBoolean 
	self type: (self type mask: SC_MARGIN_NUMBER set: aBoolean)!

isSensitive
	"Answer whether this margin is sensitive to mouse clicks."

	^isSensitive!

isSensitive: aBoolean 
	isSensitive := aBoolean asBoolean.
	self updateViewAttribute: #isSensitive!

mask
	"Answe the <integer> mask which controls which shapes will be visible in the margin.

	From the Scintilla documentation:

	'The mask is a 32-bit value. Each bit corresponds to one of 32 logical symbols that can be
	displayed in a margin that is enabled for symbols. There is a useful constant,
	SC_MASK_FOLDERS (0xFE000000 or -33554432), that is a mask for the 7 logical symbols used to
	denote folding. You can assign a wide range of symbols and colours to each of the 32 logical
	symbols, see Markers for more information. If (mask & SC_MASK_FOLDERS)==0, the margin
	background colour is controlled by style 33 (STYLE_LINENUMBER).

	... If a line has an associated marker that does not appear in the mask of any margin with a
	non-zero width, the marker changes the background colour of the line. For example, suppose
	you decide to use logical marker 10 to mark lines with a syntax error and you want to show
	such lines by changing the background colour. The mask for this marker is 1 shifted left 10
	times (1<<10) which is 0x400. If you make sure that no symbol margin includes 0x400 in its
	mask, any line with the marker gets the background colour changed.

	To set a non-folding margin 1 use SCI_SETMARGINMASKN(1, ~SC_MASK_FOLDERS); to set a folding
	margin 2 use SCI_SETMARGINMASKN(2, SC_MASK_FOLDERS). This is the default set by Scintilla.
	~SC_MASK_FOLDERS is 0x1FFFFFF in hexadecimal or 33554431 decimal. Of course, you may need to
	display all 32 symbols in a margin, in which case use SCI_SETMARGINMASKN(margin, -1).'"

	^mask!

mask: anInteger 
	mask := anInteger.
	self updateViewAttribute: #mask!

type
	"Private - The Scintilla margin type. From the Scintilla documentation: '...the type of a
	margin.[one of values].0, 1 or 2. You can use the predefined constants SC_MARGIN_SYMBOL (0)
	and SC_MARGIN_NUMBER (1) to set a margin as either a line number or a symbol margin. By
	convention, margin 0 is used for line numbers and the other two are used for symbols'"      

	^type!

type: anInteger 
	type := anInteger bitAnd: 3.
	self updateViewAttribute: #type!

width
	"Answer the <integer> pixel width of this margin."

	^width!

width: anInteger 
	"Answer the <integer> pixel width of this margin. Set to zero to make the margin invisible."

	width := anInteger.
	self updateViewAttribute: #width! !
!ScintillaMargin categoriesFor: #isFolders!accessing!public! !
!ScintillaMargin categoriesFor: #isFolders:!accessing!public! !
!ScintillaMargin categoriesFor: #isNumbers!accessing!public! !
!ScintillaMargin categoriesFor: #isNumbers:!accessing!public! !
!ScintillaMargin categoriesFor: #isSensitive!public!testing! !
!ScintillaMargin categoriesFor: #isSensitive:!accessing!public! !
!ScintillaMargin categoriesFor: #mask!accessing!public! !
!ScintillaMargin categoriesFor: #mask:!accessing!public! !
!ScintillaMargin categoriesFor: #type!accessing!private! !
!ScintillaMargin categoriesFor: #type:!accessing!private! !
!ScintillaMargin categoriesFor: #width!accessing!public! !
!ScintillaMargin categoriesFor: #width:!accessing!public! !

!ScintillaMargin class methodsFor!

initialize
	"
		self initialize
	"

	getMessages := (IdentityDictionary new)
				at: #isSensitive: put: SCI_GETMARGINSENSITIVEN;
				at: #mask: put: SCI_GETMARGINMASKN;
				at: #type: put: SCI_GETMARGINTYPEN;
				at: #width: put: SCI_GETMARGINWIDTHN;
				shrink;
				isImmutable: true;
				yourself.
	attributes := (IdentityDictionary new)
				at: #isSensitive put: SCI_SETMARGINSENSITIVEN;
				at: #mask put: SCI_SETMARGINMASKN;
				at: #type put: SCI_SETMARGINTYPEN;
				at: #width put: SCI_SETMARGINWIDTHN;
				shrink;
				isImmutable: true;
				yourself! !
!ScintillaMargin class categoriesFor: #initialize!initializing!public! !

NullScintillaStyler guid: (GUID fromString: '{930680AF-646B-4660-A349-1F5E5A8E0FEC}')!
NullScintillaStyler comment: 'NullScintillaStyler is a <ScintillaStyler> that leaves text in plain unformatted form, i.e. all text assumes the configured ''normal'' style, which is normally the style named #normal.

This styler is useful either when one wants to use Scintilla, but with plain text, or to temporarily disable styling.

Instance Variables:
	normalStyleName		<Symbol>'!
!NullScintillaStyler categoriesForClass!Kernel-Objects! !
!NullScintillaStyler methodsFor!

initialize
	normalStyleName := #normal!

normalStyleName
	^normalStyleName!

normalStyleName: anObject
	normalStyleName := anObject ifNotNil: [anObject asSymbol]!

onStyleNeeded: aScintillaView from: startInteger to: stopInteger 
	"Callback from Scintilla requesting that the specified range of text be coloured.
	In this case we just set to the default style, regardless."

	aScintillaView applyStyle: self normalStyleName toNext: stopInteger - startInteger + 1! !
!NullScintillaStyler categoriesFor: #initialize!initializing!public! !
!NullScintillaStyler categoriesFor: #normalStyleName!accessing!public! !
!NullScintillaStyler categoriesFor: #normalStyleName:!accessing!public! !
!NullScintillaStyler categoriesFor: #onStyleNeeded:from:to:!event handling!public! !

!NullScintillaStyler class methodsFor!

stbConvertFrom: anSTBVersion 
	^[:instVars | self new]!

stbVersion
	^1! !
!NullScintillaStyler class categoriesFor: #stbConvertFrom:!binary filing!public! !
!NullScintillaStyler class categoriesFor: #stbVersion!binary filing!public! !

ScintillaView guid: (GUID fromString: '{47A3310B-B7E4-4AA9-B6E6-640AD9FE7A3A}')!
ScintillaView comment: 'ScintillaView is a <valueView> class that wraps the "Scintilla" programmers'' edit control. 

Scintilla is a very powerful control with an extensive range of features. It is inevitably somewhat complex, and you are urged to read the documentation at http://www.scintilla.org/ScintillaDoc.html if you want to use this control in your own applications. In order to expose the full capabilities of Scintilla, this class and its supporting classes are also somewhat complex, but as far as reasonably possible this complexity is hidden from common use cases.

This class provides at least a basic interface to all of Scintilla''s extensive functionality; almost every SCI_XXX message listed in the documentation has a corresponding wrapper method in this class. The wrapper methods have been auto-generated from Scintilla''s interface definition file (Scintilla.iface), and are either of the form #sciXXX:etc, or have had a more readable symbol allocated. These can all be found in the ''scintilla interface'' method category. Likewise each of the SCN_XXX notification messages has a corresponding #scnXXX event handler that is (or can be) used to hook the event. These can all be found in the ''event handling-scintilla'' method category. In addition ScintillaView implements higher-level functionality to allow it to be used as a drop-in replacement for a <MultilineTextEdit>, and which also simplifies the use of most of its additional capabilities. 

ScintillaView provides high-level object-oriented wrappings for the following Scintilla features:
	Text retrieval and modification	(though not get/set of styled text, and based around the standard <textView> protocol)
	Searching and replacing 		(TextEdit implementation is inherited, but does not include RegExp replace)
	Overtype
	Cut, copy and paste			(implements standard MVP protocols)
	Undo and Redo
	Mouse capture				(supported as #willCaptureMouse aspect, but not really required - use MVP''s MouseTracker instead)
	Line endings
	Styling					(an extensive Smalltalk framework is provided - stylers can be implemented in Smalltalk e.g. ScintillaSmalltalkStyler based on SmalltalkScanner)
	Style definition				(full control is possible by altering aspects of a collection of ScintillaTextStyle objects, UI editors are provided for this too).
	Caret, selection, 
		and hotspot styles		(not hotspot styles)
	Margins
	Other Settings				(mostly)
	Brace highlighting
	Tab and Indentation Guides
	Markers
	Indicators					Named "modern" indicators are now supported. Indicator values are unused at present and probably not needed. The old style-byte indicators are deprecated.
	Autocompletion				(note that the container needs to invoke auto-completion when appropriate)
	User lists
	Keyboard commands			(some are bound to existing commands)
	Key bindings
	Line wrapping				(currently no public interface for visual line wrap flags)
	Zooming
	Long lines
	Direct access				(i.e. direct calls to the control, rather than through the message queue, faster but not thread safe)
	Folding					(requires lexer support)
	
Features of Scintilla that are not currently exposed at any level higher than the basic message interface include:
	Error handling				(not currently used by Scintilla itself)
	Cursor						(superfluous given MVP''s cursor management framework)
	Call tips
	Popup edit menu				(no special means to disable control context menus is needed in MVP)
	Macro-recording
	Printing						(partial support for some properties is provided)
	Multiple views				(alternatives exist using the MVP framework, though this could be useful in some applications)
	
N.B. Scintilla is an excellent editor control, but does not do much parameter validation. Thus if using the low-level API be careful not to pass in invalid parameters as these may cause unexpected behaviour. For example if length passed to SCI_SETSTYLING is -1, then the control goes into a loop (at the time of writing).

Instance Variables:
	this				<ExternalHandle>. C++ ''this'' pointer for direct function invocation.
	currentTextStyles	<IdentityDictionary> mapping <Symbol> to <ScintillaTextStyle> for the current lexer.
	styleIdMap		<IdentityDictionary> 
	styler			<ScintillaStyler>. Responsible for dynamically ''colouring'' text in the view.
	markerDefinitions	<IdentityDictionary> mapping <Symbol> to <ScintillaMarkerDefinition>.
	markers			<IdentitySet> of <ScintillaMarker>s. All markers currently set in the view.
	wordChars			<String> of characters considered to be word delimiters.
	maxStyle			<SmallInteger> maximum style index. Depends on number of bits used for text styles. Default 31, maximum 127.
	modificationEventMask <integer> bit mask controlling SCN_MODIFIED notifications (see Scintilla docs).
	autoCStops		<String> or nil. The characters which cancel an auto-completion list when typed..
	autoCFillups		<String> or nil. The characters which accept the selection in an auto-completion list when typed.
	whitespaceBackcolor	<Color> or nil. The colour of the background drawn behind whitespace.
	whitespaceForecolor	<Color> or nil. The colour of the glyphs used to display visible whitespace.
	selectionBackcolor 	<Color>. The background colour used to highlight the selection (default is grey).
	selectionForecolor	<Color> or nil. The foreground colour used to highlight the selection.
	indicators			<Array> of <ScintillaIndicator>s
	callTipBackcolor	<Color> or nil. The background colour for calltips.
	callTipForecolor		<Color> or nil. The foreground colour for calltips.
	callTipHighlightColor	<Color> or nil. The hilight colour for calltips.
	braceChars		<String> of characters considered to be brace characters, e.g. ''()[]{}''.
	whitespaceChars	<String> of characters considered to be whitespace.
	scFlags			<integer>. Various flags.
	allTextStyles		<IdentityDictionary> mapping <Symbol>ic lexer name to dictionary of text styles for that lexer.
	foldMarginColor 		<Color> or nil. The fold margin background colour (if visible)
	foldMarginHiColor 	<Color> or nil. The fold margin hilight colour (if visible).
	foldMarkerStyle		<Symbol>. Name of the fold (outlining) marker style employed in the fold margin.
	foldFlags			<integer>. Lexer fold flags.
	extraStyleBits		<integer>
	keyBindings		<IdentityDictionary> mapping Dolphin accelerator key codes to Scintilla messages. nil by default (default bindings are used).
	indicatorStyles		<IdentityDictionary> mapping <Symbol> or <integer> indicator style names to <ScintillaIndicatorStyle>s.

Class Variables:
	BackgroundDwellEvents	<integer>
	BraceHighlightMask	<integer> bit mask for accessing brace highlighting flag.
	CodePages			<IdentityDictionary> mapping <integer> to <Symbol>
	Commands		<IdentityDictionary> mapping Dolphin accelerator key codes to command symbols.
	DefaultTextStyles	<IdentityDictionary> of default text style maps by lexer name.
	FoldingMask		<integer> bit mask for access the folding (outlining) enabled flag.
	FoldMarkerStyles	<Array> of <Symbol>s being the names of the standard fold marker styles.
	KeyBindings		<IdentityDictionary> mapping Dolphin accelerator key codes to Scintilla messages for the default key bindings.
	Lexers			<Array> of <Symbol>. Lexer language names.
	ScnMap			<Array> of <Symbol>s being the selectors of the notification event handlers.
	Whitespaces		<String>
	IndentationGuideStyles	<Array> of <Symbol>s being the names of the indentation guide styles.
'!
!ScintillaView categoriesForClass!MVP-Views! !
!ScintillaView methodsFor!

acceptAutoCompletion
	"User has selected an item so remove the list and insert the selection."

	self 
		sendMessage: SCI_AUTOCCOMPLETE
		wParam: 0
		lParam: 0!

activeHotspotBackcolor
	"Get the back colour for active hotspots."

	^RGB fromInteger: (self 
				sendMessage: SCI_GETHOTSPOTACTIVEBACK
				wParam: 0
				lParam: 0)!

activeHotspotForecolor
	"Get the fore colour for active hotspots."

	^RGB fromInteger: (self 
				sendMessage: SCI_GETHOTSPOTACTIVEFORE
				wParam: 0
				lParam: 0)!

addKeyBinding: aScintillaKeyBinding 
	| bindings |
	bindings := self keyBindings.
	bindings at: aScintillaKeyBinding acceleratorKey put: aScintillaKeyBinding.
	self sciAssignCmdKey: aScintillaKeyBinding scintillaKeyCode msg: aScintillaKeyBinding message.
	keyBindings := bindings!

addMarker: aScintillaMarker 
	"Add the specified <ScintillaMarker> to this view. Depending on the marker and margin
	configuration this may cause a symbol to appear in a margin or affect the visual style (e.g.
	background colour) of the marked line."

	aScintillaMarker addToView: self.
	^markers add: aScintillaMarker!

addMarkerType: aSymbol at: anInteger 
	"Add the named marker to the symbols margin at the specified one-based line index. If the named
	marker is not configured for this view, then use the default marker, initially configured as
	a black-on-white circle. Answer the new <ScintillaMarker>."

	"Note that the name is typically one that is meaningful in the application domain rather
	than the name of a shape. For example if implementing a debugger one might have markers
	named #breakpoint, #currentLine, etc. The styles of these markers (and therefore the shape
	actually used to display the marker) can then be configured by altering the marker
	definition. "

	| markerType |
	markerType := markerDefinitions at: aSymbol ifAbsent: [ScintillaMarkerDefinition new].
	^self addMarker: (markerType newForLine: anInteger)!

anchorPosition
	"Answer the position of the selection anchor."

	^self sciGetAnchor + 1!

anchorPosition: anInteger 
	"Move the selection anchor to be at the specified character position (which is one based,
	i.e. 1 is home)."

	^self sciSetAnchor: anInteger - 1!

appendText: aString 
	"Append the <String> argument to the end of the document without changing the selection."

	self modifyText: [self sciAppendText: aString size text: aString]!

applyAttributes: aSequenceableCollection 
	aSequenceableCollection keysAndValuesDo: [:i :each | each applyToView: self at: i - 1]!

applyStyle: aSymbol toNext: anInteger 
	"Apply the named style to the next anInteger characters (from the current styling position)
	to the style named by the <Symbol> argument, advancing the current styling position
	appropriately. If the style name is not recognised, then the default style is used."

	anInteger > 0 
		ifTrue: [self styleNext: anInteger mask: (currentTextStyles at: aSymbol ifAbsent: [0]) asParameter]!

applyStyleId: idInteger toNext: countInteger 
	"Apply the style with the <integer> id, idInteger, to the next countInteger characters (from
	the current styling position), advancing the current styling position appropriately."

	countInteger > 0 ifTrue: [self styleNext: countInteger mask: idInteger]!

applyTextStylesForLexer: aSymbol 
	aSymbol isNil 
		ifTrue: 
			[self
				setCurrentTextStyles: self class defaultTextStyles;
				removeAllStyling]
		ifFalse: 
			[self
				setCurrentTextStyles: (allTextStyles at: aSymbol ifAbsent: [self defaultTextStylesFor: aSymbol]);
				invalidateStyling]!

areHotspotsSingleLine
	"Get the HotspotSingleLine property"

	^(self 
		sendMessage: SCI_GETHOTSPOTSINGLELINE
		wParam: 0
		lParam: 0) asBoolean!

areHotspotsSingleLine: singleLineBoolean 
	"Limit hotspots to single line so hotspots on two lines don't merge."

	self 
		sendMessage: SCI_SETHOTSPOTSINGLELINE
		wParam: singleLineBoolean asParameter
		lParam: 0!

autoCompletionAcceptChars
	"Answer a <String> of characters that, when typed, will accept the current selection in an
	auto-completion list."

	^autoCFillups ifNil: ['']!

autoCompletionAcceptChars: aString 
	"Set the <String> of characters that, when typed, will cause the auto-completion list to
	choose the currently selected item."

	autoCFillups := aString isEmpty ifFalse: [aString].
	self sciAutoCSetFillUps: aString!

autoCompletionCancelChars
	"Answer the <String> of character that, when typed, will cancel an auto-completion list."

	^autoCStops ifNil: ['']!

autoCompletionCancelChars: aString 
	"Set the <String> of characters that, when typed, will cancel an auto-completion list."

	autoCStops := aString isEmpty ifFalse: [aString].
	self sciAutoCStops: aString!

autoCompletionImageIdSeparator
	"Answer the <Character> used as the separator between entry text and image identifiers in an
	auto-completion list <String>."

	^Character value: self sciAutoCGetTypeSeparator !

autoCompletionImageIdSeparator: aCharacter 
	"Set the <Character> used as the separator between entry text and image identifiers in an
	auto-completion list <String>. The default is '?' but this should be be changed if entries
	may contain '?'."

	self sciAutoCSetTypeSeparator: aCharacter asInteger!

autoCompletionListPosition
	"Retrieve the position of the caret when the auto-completion list was displayed."

	^self sciAutoCPosStart + 1!

autoCompletionSeparator
	"Answer the <Character> used as the separator between entries in an auto-completion list <String>."

	^Character value: self sciAutoCGetSeparator!

autoCompletionSeparator: aCharacter 
	"Set the <Character> used as the separator between entries in an auto-completion list
	string. The default is a space but this should be be changed if entries may contain such."

	self sciAutoCSetSeparator: aCharacter asInteger!

backcolorChanged
	"Private - Note we don't supersend, because we don't need to invalidate on changing colours
	- Scintilla takes care of that"

	self updateTextStyles!

backspace
	"Delete the selection or if no selection, the character before the caret."

	self 
		sendMessage: SCI_DELETEBACK
		wParam: 0
		lParam: 0!

backspaceNoLine
	"Delete the selection or if no selection, the character before the caret. Will not delete
	the character before at the start of a line."

	self 
		sendMessage: SCI_DELETEBACKNOTLINE
		wParam: 0
		lParam: 0!

backspaceUnindents
	"Does a backspace pressed when caret is within indentation unindent?"

	^(self 
		sendMessage: SCI_GETBACKSPACEUNINDENTS
		wParam: 0
		lParam: 0) asBoolean!

backspaceUnindents: bsUnIndentsBoolean 
	"Sets whether a backspace pressed when caret is within indentation unindents."

	self 
		sendMessage: SCI_SETBACKSPACEUNINDENTS
		wParam: bsUnIndentsBoolean asParameter
		lParam: 0!

basicClearContainerIndicators
	"Private - Clear all the containers indicators (those with Id's starting with
	INDIC_CONTAINER) from the receiver Indicators reserved for use by lexers (those with id's in
	the range 0..INDIC_CONTAINER-1) are unaffected."

	| length |
	length := self textLength.
	INDIC_CONTAINER to: INDIC_MAX
		do: 
			[:each | 
			self currentIndicatorId: each.
			self sciIndicatorClearRange: 0 clearLength: length]!

basicClearSelection
	"Clear the selection."

	self 
		sendMessage: SCI_CLEAR
		wParam: 0
		lParam: 0!

basicLineFromPosition: posInteger 
	"Retrieve the line containing a position."

	^self 
		sendMessage: SCI_LINEFROMPOSITION
		wParam: posInteger
		lParam: 0!

basicPositionAtLine: lineInteger 
	"Retrieve the position at the start of a line."

	^self 
		sendMessage: SCI_POSITIONFROMLINE
		wParam: lineInteger
		lParam: 0!

basicSelectAll
	"Select all the text in the document."

	self 
		sendMessage: SCI_SELECTALL
		wParam: 0
		lParam: 0!

basicSelectionRange
	"Private - Answer an <interval> identifying the selected range of text as reported by the underlying 
	Windows control. Note that this range includes the starting character, but the end of the range
	is the first unselected character. Note further that this characteristic means that the range should 
	NEVER be empty."

	^self sciGetSelectionStart to: self sciGetSelectionEnd!

basicSelectionStart: startInteger end: endInteger 
	"Select a range of text."

	self 
		sendMessage: SCI_SETSEL
		wParam: startInteger
		lParam: endInteger!

basicUndo
	"Undo one action in the undo history."

	self 
		sendMessage: SCI_UNDO
		wParam: 0
		lParam: 0!

beginUndoGroup
	"Start a sequence of actions that is undone and redone as a unit. May be nested."

	self 
		sendMessage: SCI_BEGINUNDOACTION
		wParam: 0
		lParam: 0!

braceChars
	"Answer a <LookupTable> the keys of which are <Symbol>ic style names, and the associated
	values are the <String>s containing the the set of <Character>s that should be considered as
	brace characters for that style."

	^braceChars ifNil: [self defaultBraceChars]!

braceChars: aLookupTable 
	"Set the map between <Symbol>ic style names and the brace characters in that style to be the
	<LookupTable> argument. Note that Scintilla recognises a hard-coded set of brace characters,
	it is not possible to use other characters for any style, but the set can be reduced on a
	per-style basis."

	aLookupTable do: 
			[:each | 
			| diffs |
			diffs := each difference: '[]{}()<>'.
			diffs notEmpty ifTrue: [self error: 'invalid brace characters: ' , diffs]].
	braceChars := aLookupTable!

braceHighlight
	| len pos found1 found2 |
	len := self textLength.
	len < 1 ifTrue: [^self].
	pos := self caretPosition.
	found1 := 0.
	(pos > 1 and: [self isBraceAt: pos - 1]) ifTrue: [found1 := pos - 1].
	(found1 == 0 and: [pos <= len and: [self isBraceAt: pos]]) ifTrue: [found1 := pos].
	(found1 == 0 or: [(found2 := self findMatchingBrace: found1) == 0]) 
		ifTrue: [self highlightMismatchedBrace: found1]
		ifFalse: [self highlightBracesAt: found1 and: found2]!

buildDefaultStyle
	| defaultStyle |
	defaultStyle := (currentTextStyles at: #normal) copy.
	defaultStyle mergeFont: self actualFont.
	defaultStyle forecolor ifNil: [defaultStyle forecolor: (self forecolor ifNil: [Color windowText])].
	defaultStyle backcolor ifNil: [defaultStyle backcolor: self actualBackcolor].
	defaultStyle case ifNil: [defaultStyle case: SC_CASE_MIXED].
	defaultStyle characterSet ifNil: [defaultStyle characterSet: SC_CHARSET_DEFAULT].
	^defaultStyle!

buildItemList: aCollection 
	| itemList sep typesep |
	itemList := String writeStream.
	sep := self autoCompletionSeparator.
	typesep := self autoCompletionImageIdSeparator.
	aCollection do: 
			[:each | 
			itemList
				display: each;
				"nextPut: typesep;
				display: each icon imageIndex;"
				nextPut: sep].
	^itemList
		pop;
		contents!

buildViewStyle
	^(ScintillaTextStyle new)
		font: self actualFont;
		backcolor: (self backcolor ifNil: [Color window]);
		forecolor: (self forecolor ifNil: [Color windowText]);
		yourself!

callTipBackcolor
	"Answer the background <Color> of the call tips box. By default this is white."

	^callTipBackcolor ifNil: [##(Color white)]!

callTipBackcolor: aColorOrNil 
	"Set the background <Color> of the call tips box. If the argument is nil then the default
	colour (white) is set."

	(callTipBackcolor := aColorOrNil) isNil 
		ifTrue: [self sciCallTipSetBack: nil]
		ifFalse: [self sciCallTipSetBack: aColorOrNil asRGB]!

callTipForecolor
	"Answer the foreground <Color> of the unhighlighted text in call tips. By default this is
	dark grey."

	^callTipForecolor ifNil: [##(Color darkGray)]!

callTipForecolor: aColorOrNil 
	"Set the foreground <Color> of the unhighlighted text in call tips. If the argument is nil
	then the default colour (dark grey) is set."

	(callTipForecolor := aColorOrNil) isNil 
		ifTrue: [self sciCallTipSetFore: nil]
		ifFalse: [self sciCallTipSetFore: aColorOrNil asRGB]!

callTipHighlightColor
	"Answer the foreground <Color> for the highlighted part of the call tip. By default this is
	dark blue."

	^callTipHighlightColor ifNil: [##(Color darkBlue)]!

callTipHighlightColor: aColorOrNil 
	"Set the foreground <Color> for the highlighted part of the call tip. If the argument is nil
	then the default colour (dark blue) is set."

	(callTipHighlightColor := aColorOrNil) isNil 
		ifTrue: [self sciCallTipSetForeHlt: nil]
		ifFalse: [self sciCallTipSetForeHlt: aColorOrNil asRGB]!

callTipPosition
	"Answer the <integer> character position of the caret immediately before the call tip was
	displayed."

	^self sciCallTipPosStart + 1!

cancelAutoCompletion
	"Remove the auto-completion list from the screen."

	self 
		sendMessage: SCI_AUTOCCANCEL
		wParam: 0
		lParam: 0!

cancelCallTip
	"Remove the call tip from the screen."

	self 
		sendMessage: SCI_CALLTIPCANCEL
		wParam: 0
		lParam: 0!

cancelModes
	"Cancel any modes such as call tip or auto-completion list display."

	self 
		sendMessage: SCI_CANCEL
		wParam: 0
		lParam: 0!

canHScroll
	"Is the horizontal scroll bar visible?"

	^(self 
		sendMessage: SCI_GETHSCROLLBAR
		wParam: 0
		lParam: 0) asBoolean!

canHScroll: aBoolean 
	"Sets the receiver into horizontal scrolling mode the <Boolean> argument is true."

	self sciSetHScrollBar: aBoolean.
	self invalidateCalculatedExtent!

canonicalizeLineEndings: aSymbol 
	"Replace all non-standard line-endings in the text so that all are as named by the <Symbol>
	argument, one of #crlf, #cr, or #lf."

	self sciConvertEOLs: (self class lineEndings indexOf: aSymbol) - 1!

canPaste
	"Answer whether the window can paste from the current contents of the clipboard."

	^self sciCanPaste or: [super canPaste]!

canRedo
	"Are there any redoable actions in the undo history?"

	^(self 
		sendMessage: SCI_CANREDO
		wParam: 0
		lParam: 0) asBoolean!

canScrollPastEnd
	"Answer whether the window can be scrolled up to one-page past the end of the text. If false
	then the window can only be scrolled to the last line."

	^self sciGetEndAtLastLine asBoolean!

canScrollPastEnd: endAtLastLineBoolean 
	"Sets the scroll range so that maximum scroll position has the last line at the bottom of
	the view (default). Setting this to false allows scrolling one page below the last line."

	self 
		sendMessage: SCI_SETENDATLASTLINE
		wParam: endAtLastLineBoolean asParameter
		lParam: 0!

canUndo
	"Are there any undoable actions in the undo history?"

	^(self 
		sendMessage: SCI_CANUNDO
		wParam: 0
		lParam: 0) asBoolean!

canVScroll
	"Is the vertical scroll bar visible?"

	^(self 
		sendMessage: SCI_GETVSCROLLBAR
		wParam: 0
		lParam: 0) asBoolean!

caretForecolor
	"Get the foreground colour of the caret."

	^RGB fromInteger: (self 
				sendMessage: SCI_GETCARETFORE
				wParam: 0
				lParam: 0)!

caretForecolor: foreRGB 
	"Set the foreground colour of the caret."

	self sciSetCaretFore: (foreRGB ifNil: [Color black]) asRGB!

caretPeriod
	"Get the time in milliseconds that the caret is on and off."

	^self 
		sendMessage: SCI_GETCARETPERIOD
		wParam: 0
		lParam: 0!

caretPeriod: periodMillisecondsInteger 
	"Get the time in milliseconds that the caret is on and off. 0 = steady on."

	self 
		sendMessage: SCI_SETCARETPERIOD
		wParam: periodMillisecondsInteger
		lParam: 0!

caretPosition
	"Answer the insertion cursor position, as the number of characters from the start of the
	receivers text."

	^self sciGetCurrentPos + 1!

caretPosition: anInteger
	"Sets the insertion cursor to be at the specified character position (which is one based,
	i.e. caretPosition 1 is home)."

	"Implementation Note: SCI_SETCURRENTPOS moves the current position, but not the anchor.
	We want this to reset the selection to empty, so use superclass implementation."

	"self sendMessage: SCI_SETCURRENTPOS wParam: anInteger - 1"

	super caretPosition: anInteger!

caretStyle
	"Answer the style of caret displayed in the receiver; one of #invisible (no caret), #line or
	#block"

	^CaretStyles at: self sciGetCaretStyle + 1 ifAbsent: [#line]!

caretStyle: aSymbol 
	"Set the style of caret displayed in the receiver; one of #invisible (no caret), #line or
	#block"

	^self sciSetCaretStyle: (CaretStyles indexOf: aSymbol ifAbsent: [CARETSTYLE_LINE + 1]) - 1!

caretWidth
	"Returns the width of the insert mode caret."

	^self 
		sendMessage: SCI_GETCARETWIDTH
		wParam: 0
		lParam: 0!

caretWidth: anInteger 
	"Set the width of the caret to the number of pels specified by the <integer> argument."

	(anInteger between: 1 and: 3) 
		ifFalse: [^self error: 'Caret width must be between 1 and 3, not ' , anInteger printString].
	self sciSetCaretWidth: anInteger!

characterAt: anInteger 
	"Answer the <Character> at the specified one-based <integer> index in the receiver's text."

	^Character 
		value: ((self 
				sendMessage: SCI_GETCHARAT
				wParam: anInteger - 1
				lParam: 0) bitAnd: 16rFF)!

charCloseToPosition: aPoint 
	"Answer the one-based index of the character closest to the specified <Point> within the
	receiver, or 0 if the co-ordinate is outside the window or not 'close' to any character."

	^(self sciPositionFromPointClose: aPoint x y: aPoint y) + 1!

charNearestPosition: aPoint 
	"Answer the one-based index of the character nearest the specified <Point> within the
	receiver."

	^(self sciPositionFromPoint: aPoint x y: aPoint y) + 1!

clearAll
	"Delete all text in the document."

	self 
		sendMessage: SCI_CLEARALL
		wParam: 0
		lParam: 0!

clearContainerIndicators
	"Clear all the indicators (e.g. squiggly underlines) from the receiver."

	self basicClearContainerIndicators.
	indicators := nil!

clearIndicator: anIntegerOrSymbol from: startInteger to: stopInteger 
	"Clear the indicator identified by <integer> id from the range of text between two one-based
	<integer> positions."

	self currentIndicatorId: (self indicatorIdFromName: anIntegerOrSymbol).
	self sciIndicatorClearRange: startInteger - 1 clearLength: stopInteger - startInteger + 1!

codePage
	"Answer a <Symbol> naming the code page currently in use, either #utf8 or #dbcs."

	^CodePages at: self sciGetCodePage ifAbsent: [#dbcs]!

codePage: aSymbol 
	"Set the code page currently in use to that named by the <Symbol> argument (one of #utf8 or
	#dbcs)."

	self sciSetCodePage: (CodePages keyAtValue: aSymbol)!

columnFromPosition: anInteger 
	"Answer the column number of the character at the specified position."

	"From the Scintilla documentation:' This message returns the column number of a position pos
	within the document taking the width of tabs into account. This returns the column number of
	the last tab on the line before pos, plus the number of characters between the last tab and
	pos. If there are no tab characters on the line, the return value is the number of
	characters up to the position on the line. In both cases, double byte characters count as a
	single character. This is probably only useful with monospaced fonts.'"

	^(self sciGetColumn: anInteger - 1) + 1!

controlCharacter
	"Answer the character used to display control characters in the document, or nil if the
	control characters are drawn (the default)."

	| code |
	code := self sciGetControlCharSymbol.
	^code < 32 ifFalse: [Character codePoint: code]!

controlCharacter: aCharacter
	"Set the way control characters are displayed: If the argument is nil (or has a code point <
	32) then draw the characters, otherwise use the given <Character>."

	self sciSetControlCharSymbol: aCharacter codePoint!

convertToLowercase
	"Transform the selection to lower case."

	self 
		sendMessage: SCI_LOWERCASE
		wParam: 0
		lParam: 0!

convertToUppercase
	"Transform the selection to upper case."

	self 
		sendMessage: SCI_UPPERCASE
		wParam: 0
		lParam: 0!

copyLine
	"Copy the line containing the caret."

	self 
		sendMessage: SCI_LINECOPY
		wParam: 0
		lParam: 0!

copySelection
	"Copy the selection to the clipboard."

	self 
		sendMessage: SCI_COPY
		wParam: 0
		lParam: 0!

currentIndicatorId
	"Private - Answer an <integer> identifying the currently set indicator."

	^self 
		sendMessage: SCI_GETINDICATORCURRENT
		wParam: 0
		lParam: 0!

currentIndicatorId: indicatorInteger 
	self 
		sendMessage: SCI_SETINDICATORCURRENT
		wParam: indicatorInteger
		lParam: 0.
!

currentIndicatorValue
	"Private - Get the current indicator vaue"

	^self 
		sendMessage: SCI_GETINDICATORVALUE
		wParam: 0
		lParam: 0!

currentIndicatorValue: valueInteger 
	"Private - Set the value used for IndicatorFillRange"

	self 
		sendMessage: SCI_SETINDICATORVALUE
		wParam: valueInteger
		lParam: 0!

currentLineBackcolor
	"Get the colour of the background of the line containing the caret."

	^RGB fromInteger: (self 
				sendMessage: SCI_GETCARETLINEBACK
				wParam: 0
				lParam: 0)!

currentLineBackcolor: backRGB 
	"Set the colour of the background of the line containing the caret."

	self sciSetCaretLineBack: (backRGB ifNil: [Color yellow]) asRGB!

cutLine
	"Cut the line containing the caret."

	self 
		sendMessage: SCI_LINECUT
		wParam: 0
		lParam: 0!

cutSelection
	"Cut the selection to the clipboard."

	self 
		sendMessage: SCI_CUT
		wParam: 0
		lParam: 0!

decodeStyledText: aByteArray 
	| tokens pair lastStyle buffer wsStyle stream ws |
	tokens := OrderedCollection new.
	pair := nil -> nil.
	lastStyle := nil.
	buffer := ByteArray writeStream.
	"Note that whitespace might have a specific style, or may just use the normal style in which
	case we must check for whitespace characters."
	wsStyle := (self styleNamed: #whitespace) ifNil: [0] ifNotNil: [:style | style id].
	ws := self whitespaces asByteArray.
	stream := aByteArray readStream.
	[stream atEnd] whileFalse: 
			[| char style |
			char := stream next.
			style := stream next bitAnd: self maxStyle.
			(style == wsStyle and: [wsStyle ~~ 0 or: [ws identityIncludes: char]]) 
				ifTrue: [lastStyle := nil]
				ifFalse: 
					[lastStyle == style 
						ifFalse: 
							[lastStyle := style.
							pair value: buffer contents asString.
							buffer reset.
							pair := (self styleWithId: lastStyle) name -> nil.
							tokens addLast: pair].
					buffer nextPut: char]].
	pair value: buffer contents asString.
	^tokens!

defaultBraceChars
	"Private - Answer a <String> containing the set of <Character>s that Scintilla considers to be brace
	characters by default."

	^##((LookupTable new)
		at: #normal put: '()[]{}<>';
		yourself)!

defaultKeyBindings
	^DefaultKeyBindings collect: [:each | each copy]!

defaultMarkerDefinitions
	"Private - Answer an <IdentityDictionary> that associates marker names that are meaningful in the
	application domain to the definition for that marker, where the definition specifies
	attributes such as the glyph used, and foreground and background colours."

	^IdentityDictionary 
		with: #default -> ((ScintillaMarkerDefinition new)
						name: #circle;
						yourself)!

defaultModEventMask
	"Private - Answer the default modification event mask. We're not interested in marker
	changes, or the 'before' notifications of deletions and insertions. These can be enabled on
	a per-instance basis though."

	^##(SC_MODEVENTMASKALL - SC_MOD_CHANGEMARKER - SC_MOD_BEFOREDELETE - SC_MOD_BEFOREINSERT)!

defaultTextStylesFor: aSymbol 
	"Answer the default text style settings to be used for newly configured lexers."

	^(DefaultTextStyles at: aSymbol ifAbsent: [DefaultTextStyles at: #container]) 
		collect: [:each | each copy]!

defaultWhitespaceChars
	"Answer a <String> containing the <Character>s that Scintilla considers to be whitespace by
	default."

	^Character byteCharacterSet select: [:each | each codePoint < 16r20 or: [each == $ ]]!

defaultWindowStyle
	"Private - Answer a default style to use when creating a ScintillaView."

	"Implementation Note: Scintilla occassionally creates child windows, e.g. for
	autocompletion, and since it does background painting it may occassionally paint over these.
	Therefore it needs the WS_CLIPCHILDREN style. Unfortunately the documentation does not
	mention this, but Scite does set this style when creating the window."

	^super defaultWindowStyle bitOr: WS_CLIPCHILDREN!

defaultWordChars
	"Answer a <collection> of the characters that, by default, Scintilla considers to be 'word'
	characters, as opposed to word delimiters such as whitespace or punctuation."

	"Implementation Note: This property of the control is not queryable, so this implementation
	is based on examination of the source code."

	^Character byteCharacterSet 
		select: [:each | each == $_ or: [each codePoint >= 16r80 or: [each isAlphaNumeric]]]!

deleteLine
	"Delete the line containing the caret."

	self 
		sendMessage: SCI_LINEDELETE
		wParam: 0
		lParam: 0!

deleteMarkers: markerNumberInteger 
	"Delete all markers with a particular number from all lines."

	self 
		sendMessage: SCI_MARKERDELETEALL
		wParam: markerNumberInteger
		lParam: 0!

deleteToEndOfLine
	"Delete forwards from the current position to the end of the line."

	self 
		sendMessage: SCI_DELLINERIGHT
		wParam: 0
		lParam: 0!

deleteToEndOfWord
	"Delete the word to the right of the caret, but not the trailing non-word characters."

	self 
		sendMessage: SCI_DELWORDRIGHTEND
		wParam: 0
		lParam: 0!

deleteToNextWord
	"Delete the word to the right of the caret."

	self 
		sendMessage: SCI_DELWORDRIGHT
		wParam: 0
		lParam: 0!

deleteToStartOfLine
	"Delete back from the current position to the start of the line."

	self 
		sendMessage: SCI_DELLINELEFT
		wParam: 0
		lParam: 0!

deleteToStartOfWord
	"Delete the word to the left of the caret."

	self 
		sendMessage: SCI_DELWORDLEFT
		wParam: 0
		lParam: 0!

destroyAutoCompletionListImages
	"Clear all the registered XPM images."

	self 
		sendMessage: SCI_CLEARREGISTEREDIMAGES
		wParam: 0
		lParam: 0!

duplicateLine
	"Duplicate the current line."

	self 
		sendMessage: SCI_LINEDUPLICATE
		wParam: 0
		lParam: 0!

duplicateSelection
	"Duplicate the selection. If selection empty duplicate the line containing the caret."

	self 
		sendMessage: SCI_SELECTIONDUPLICATE
		wParam: 0
		lParam: 0!

edgeColor
	"Retrieve the colour used in edge indication."

	^RGB fromInteger: (self 
				sendMessage: SCI_GETEDGECOLOUR
				wParam: 0
				lParam: 0)!

edgeColor: edgeColourRGB 
	"Change the colour used in edge indication."

	self 
		sendMessage: SCI_SETEDGECOLOUR
		wParam: edgeColourRGB asParameter
		lParam: 0!

edgeColumn
	"Answer the one-based <integer> index of the column considered to be the rightmost edge of
	the view. This is only relevant when long-line marking is enabled by setting the edge mode
	to something other than #none."

	^self sciGetEdgeColumn + 1!

edgeColumn: anInteger
	"Set the one-based <integer> index of the column considered to be the rightmost edge of
	the view. This is only relevant when long-line marking is enabled by setting the edge mode
	to something other than #none."

	self sciSetEdgeColumn: anInteger - 1!

edgeMode
	"Answer the <Symbol>ic name of the current edge marking mode used to indicate long lines.
	See #edgeMode: for a description of the modes."

	^self class edgeModes at: self sciGetEdgeMode + 1!

edgeMode: aSymbol 
	"Set the edge marking mode used to indicate long lines. The <Symbol> argument can be one of:
		#none - long lines are not marked 
		#line - a vertical line is drawn at the edge column
		#background - the #edgeColor is used for the background past the #edgeColumn 
	N.B. #background mode should be used in views with proportional fonts in preference to #line."

	^self sciSetEdgeMode: (self class edgeModes keyAtValue: aSymbol) - 1!

editStyles
	(ScintillaTextStylesDialog createOn: (self aspectValue: #textStyles))
		defaultStyle: self buildViewStyle;
		showModal!

emptyUndoBuffer
	"Delete the undo history."

	self 
		sendMessage: SCI_EMPTYUNDOBUFFER
		wParam: 0
		lParam: 0!

enChange
	"Private - The receiver's text (not styles or other visuals) has been updated, and the change has 
	been displayed."

	"Implementation Note: Ignore EN_CHANGE from Scintilla since it sends it for all sorts of
	events that haven't changed the text. We detect text changes in #scnModified: instead.
	Return 0 to suppress propagation of WM_COMMAND to the parent view's default window
	procedure."

	^0!

endOfLineMode
	"Answer a <Symbol> naming the receiver's current end-of-line mode, one of #cr, #lf, #crlf.
	This controls the character, or characters in the case of #crlf, inserted into the text when
	the carriage return key is pressed."

	^self class lineEndings at: self sciGetEOLMode + 1!

endOfLineMode: aSymbol 
	"Set the End of Line mode of the receiver. The <Symbol> argument must be one of #crlf, #cr,
	or #lf. This controls the character, or characters in the case of #crlf, inserted into the
	text when the carriage return key is pressed. Any pre-existing text is unaffected by changes
	to the end-of-line mode, but see also #canonicalizeLineEndings:."

	self sciSetEOLMode: (self class lineEndings indexOf: aSymbol) - 1!

endUndoGroup
	"End a sequence of actions that is undone and redone as a unit."

	self 
		sendMessage: SCI_ENDUNDOACTION
		wParam: 0
		lParam: 0!

enKillFocus
	"Private - The receiver is actually gaining focus (the defintions of
	SCEN_KILLFOCUS and SCEN_SETFOCUS are inverted with 
	respect to EN_KILLFOCUS and EN_SETFOCUS)."

	^super enSetFocus!

enSetFocus
	"Private - The receiver is actually losing focus (the defintions of
	SCEN_KILLFOCUS and SCEN_SETFOCUS are inverted with 
	respect to EN_KILLFOCUS and EN_SETFOCUS)."

	^super enKillFocus!

ensureCaretVisible
	"Ensure the caret is visible. Note that this will show the caret even if currently hidden inside a fold."

	self ensureVisible: self caretPosition.
	self 
		sendMessage: SCI_SCROLLCARET
		wParam: 0
		lParam: 0!

ensureLineVisible: anInteger 
	self sciEnsureVisible: anInteger - 1!

ensureVisible: anInteger 
	"Ensure that the specified character position is visible."

	self ensureLineVisible: (self lineFromPosition: anInteger)!

enUpdate
	"Private - The receiver's text has been updated, and the change has 
	been displayed."

	"Implementation Note: SCN_UPDATEUI seems better notification to hook.
	Just return 0 to suppress propagation of WM_COMMAND to parent window procedure."

	^0!

errorStatus
	"Get error status."

	^self 
		sendMessage: SCI_GETSTATUS
		wParam: 0
		lParam: 0!

errorStatus: statusCodeInteger 
	"Change error status - 0 = OK."

	self 
		sendMessage: SCI_SETSTATUS
		wParam: statusCodeInteger
		lParam: 0!

extendDown
	"Move caret down one line extending selection to new caret position."

	self 
		sendMessage: SCI_LINEDOWNEXTEND
		wParam: 0
		lParam: 0!

extendLeft
	"Move caret left one character extending selection to new caret position."

	self 
		sendMessage: SCI_CHARLEFTEXTEND
		wParam: 0
		lParam: 0!

extendPageDown
	"Move caret one page down extending selection to new caret position."

	self 
		sendMessage: SCI_PAGEDOWNEXTEND
		wParam: 0
		lParam: 0!

extendPageUp
	"Move caret one page up extending selection to new caret position."

	self 
		sendMessage: SCI_PAGEUPEXTEND
		wParam: 0
		lParam: 0!

extendParaDown
	self 
		sendMessage: SCI_PARADOWNEXTEND
		wParam: 0
		lParam: 0!

extendParaUp
	self 
		sendMessage: SCI_PARAUPEXTEND
		wParam: 0
		lParam: 0!

extendRectangleDown
	"Move caret down one line, extending rectangular selection to new caret position."

	self 
		sendMessage: SCI_LINEDOWNRECTEXTEND
		wParam: 0
		lParam: 0!

extendRectangleLeft
	"Move caret left one character, extending rectangular selection to new caret position."

	self 
		sendMessage: SCI_CHARLEFTRECTEXTEND
		wParam: 0
		lParam: 0!

extendRectanglePageDown
	"Move caret one page down, extending rectangular selection to new caret position."

	self 
		sendMessage: SCI_PAGEDOWNRECTEXTEND
		wParam: 0
		lParam: 0!

extendRectanglePageUp
	"Move caret one page up, extending rectangular selection to new caret position."

	self 
		sendMessage: SCI_PAGEUPRECTEXTEND
		wParam: 0
		lParam: 0!

extendRectangleRight
	"Move caret right one character, extending rectangular selection to new caret position."

	self 
		sendMessage: SCI_CHARRIGHTRECTEXTEND
		wParam: 0
		lParam: 0!

extendRectangleToEndOfLine
	"Move caret to last position on line, extending rectangular selection to new caret
	position."

	self 
		sendMessage: SCI_LINEENDRECTEXTEND
		wParam: 0
		lParam: 0!

extendRectangleToStartOfLine
	"Move caret to first position on line, extending rectangular selection to new caret
	position."

	self 
		sendMessage: SCI_HOMERECTEXTEND
		wParam: 0
		lParam: 0!

extendRectangleToVcHome
	"Move caret to before first visible character on line. If already there move to first
	character on line. In either case, extend rectangular selection to new caret position."

	self 
		sendMessage: SCI_VCHOMERECTEXTEND
		wParam: 0
		lParam: 0!

extendRectangleUp
	"Move caret up one line, extending rectangular selection to new caret position."

	self 
		sendMessage: SCI_LINEUPRECTEXTEND
		wParam: 0
		lParam: 0!

extendRight
	"Move caret right one character extending selection to new caret position."

	self 
		sendMessage: SCI_CHARRIGHTEXTEND
		wParam: 0
		lParam: 0!

extendStutteredPageDown
	"Move caret to bottom of page, or one page down if already at bottom of page, extending
	selection to new caret position."

	self 
		sendMessage: SCI_STUTTEREDPAGEDOWNEXTEND
		wParam: 0
		lParam: 0!

extendStutteredPageUp
	"Move caret to top of page, or one page up if already at top of page, extending selection to
	new caret position."

	self 
		sendMessage: SCI_STUTTEREDPAGEUPEXTEND
		wParam: 0
		lParam: 0!

extendToEndOfDisplayLine
	"Move caret to last position on display line extending selection to new caret position."

	self 
		sendMessage: SCI_LINEENDDISPLAYEXTEND
		wParam: 0
		lParam: 0!

extendToEndOfDocument
	"Move caret to last position in document extending selection to new caret position."

	self 
		sendMessage: SCI_DOCUMENTENDEXTEND
		wParam: 0
		lParam: 0!

extendToEndOfLine
	"Move caret to last position on line extending selection to new caret position."

	self 
		sendMessage: SCI_LINEENDEXTEND
		wParam: 0
		lParam: 0!

extendToEndOfNextWord
	"Move caret right one word, position cursor at end of word, extending selection to new caret
	position."

	self 
		sendMessage: SCI_WORDRIGHTENDEXTEND
		wParam: 0
		lParam: 0!

extendToEndOfPreviousWord
	"Move caret left one word, position cursor at end of word, extending selection to new caret
	position."

	self 
		sendMessage: SCI_WORDLEFTENDEXTEND
		wParam: 0
		lParam: 0!

extendToEndOfWord
	"Move caret right one word extending selection to new caret position."

	self 
		sendMessage: SCI_WORDRIGHTEXTEND
		wParam: 0
		lParam: 0!

extendToEndOfWordPart
	"Move to the next change in capitalisation extending selection to new caret position."

	self 
		sendMessage: SCI_WORDPARTRIGHTEXTEND
		wParam: 0
		lParam: 0!

extendToEndOfWrappedLine
	self 
		sendMessage: SCI_LINEENDWRAPEXTEND
		wParam: 0
		lParam: 0!

extendToStartOfDisplayLine
	"Move caret to first position on display line extending selection to new caret position."

	self 
		sendMessage: SCI_HOMEDISPLAYEXTEND
		wParam: 0
		lParam: 0!

extendToStartOfDocument
	"Move caret to first position in document extending selection to new caret position."

	self 
		sendMessage: SCI_DOCUMENTSTARTEXTEND
		wParam: 0
		lParam: 0!

extendToStartOfLine
	"Move caret to first position on line extending selection to new caret position."

	self 
		sendMessage: SCI_HOMEEXTEND
		wParam: 0
		lParam: 0!

extendToStartOfWord
	"Move caret left one word extending selection to new caret position."

	self 
		sendMessage: SCI_WORDLEFTEXTEND
		wParam: 0
		lParam: 0!

extendToStartOfWordPart
	"Move to the previous change in capitalisation extending selection to new caret position."

	self 
		sendMessage: SCI_WORDPARTLEFTEXTEND
		wParam: 0
		lParam: 0!

extendToStartOfWrappedLine
	self 
		sendMessage: SCI_HOMEWRAPEXTEND
		wParam: 0
		lParam: 0!

extendToVcHome
	"Like VCHome but extending selection to new caret position."

	self 
		sendMessage: SCI_VCHOMEEXTEND
		wParam: 0
		lParam: 0!

extendToWrappedVcHome
	self 
		sendMessage: SCI_VCHOMEWRAPEXTEND
		wParam: 0
		lParam: 0!

extendUp
	"Move caret up one line extending selection to new caret position."

	self 
		sendMessage: SCI_LINEUPEXTEND
		wParam: 0
		lParam: 0!

extraStyleBits
	"Style-byte indicators should no longer be used."

	#deprecated.
	^extraStyleBits!

extraStyleBits: anInteger 
	"Style-byte indicators should no longer be used. Lexers/stylers can use indicators from the
	reserved range, 0..7, for marking regions of text independently of other styling
	information."

	#deprecated.
	(anInteger between: 0 and: 8 - self styleBits) 
		ifFalse: [self error: 'Invalid number of extra style bits'].
	extraStyleBits := anInteger.
	self hideExtraIndicators!

extraStyleMask
	^16r700 >> extraStyleBits bitAnd: 16rFF!

find: aFindDetails range: anInterval 
	"Private - Find the first occurrence of aString in the text of the receiver within the range rangeInterval."

	"Scintilla's regexp support is a bit limited"
	| findFlags range |
	aFindDetails isRegularExpression ifTrue: [^super find: aFindDetails range: anInterval].
	findFlags := 0.
	aFindDetails isWholeWord ifTrue: [findFlags := findFlags maskSet: SCFIND_WHOLEWORD].
	aFindDetails isCaseSensitive ifTrue: [findFlags := findFlags maskSet: SCFIND_MATCHCASE].
	aFindDetails isRegularExpression ifTrue: [findFlags := findFlags maskSet: SCFIND_REGEXP].
	range := aFindDetails isForwards 
				ifTrue: [anInterval]
				ifFalse: [anInterval stop to: anInterval start].
	^self 
		find: aFindDetails pattern
		range: range
		flags: findFlags!

find: aString range: anInterval flags: anInteger 
	"Attempt to find the text findString in the receivers text within findRange. flags are from
	the SCFIND enumeration. Answer the (1-based) position of the found string, or 0 if nothing matched."

	| findText start stop range |
	start := anInterval start - 1.
	stop := anInterval stop.
	stop < start ifTrue: [stop := stop - 1].
	findText := FINDTEXTEX 
				from: start
				to: stop
				text: aString.
	self sciFindText: anInteger ft: findText.
	range := findText chrgText.
	^range cpMin + 1 to: range cpMax!

findAutoCompletionEntry: textString 
	"Select the item in the auto-completion list that starts with a string."

	self 
		sendMessage: SCI_AUTOCSELECT
		wParam: 0
		lpParam: textString!

findMatchingBrace: anInteger 
	"Answer the one-based <integer> index of the character in the receiver which is the brace
	matching that at the one-based <integer> index argument. If there is no matching brace, or
	the character at the specified position is not itself a brace character, then answer zero.
	Note that for two brace characters to be considered a match they must have the same style."

	^(self sciBraceMatch: anInteger - 1) + 1!

findStyleStart: aSymbol before: anInteger 
	"Answer the one-based <integer> index of the first character in a block of the style named
	by the <Symbol> argument, searching back from the specified <integer> character position. If
	the style is not found before the start of the text is reached, answer zero."

	| id start |
	id := (self styleNamed: aSymbol) ifNil: [^0] ifNotNil: [:style | style id].
	start := anInteger.
	[(self styleIdAt: start) == id] whileFalse: [(start := start - 1) == 0 ifTrue: [^0]].
	[start > 0 and: [(self styleIdAt: start) == id]] whileTrue: [start := start - 1].
	^start + 1!

foldFlags
	^foldFlags!

foldFlags: anInteger 
	"Set the fold flags used to configure the visual appearance of folding (document outlining) in Scintilla:
	The <integer> argument should be some combination of the following values:
		SC_FOLDFLAG_BOX -> 1 
		SC_FOLDFLAG_LINEBEFORE_EXPANDED -> 2 
		SC_FOLDFLAG_LINEBEFORE_CONTRACTED -> 4 
		SC_FOLDFLAG_LINEAFTER_EXPANDED -> 8 
		SC_FOLDFLAG_LINEAFTER_CONTRACTED -> 16 
		SC_FOLDFLAG_LEVELNUMBERS -> 64 
	"

	foldFlags := anInteger.
	self sciSetFoldFlags: anInteger!

foldLine: lineInteger level: levelInteger 
	"Set the fold 'level' of the specified line. The levelInteger argument encodes an integer
	level along with flags indicating whether the line is a header and whether it is effectively
	white space."

	self sciSetFoldLevel: lineInteger - 1 level: levelInteger!

foldMargin
	"Answer the <ScintillaMargin> used to display fold markers for outlining."

	"N.B. It is assumed there is only one fold margin in the view, so the first encountered is
	used."

	^self margins detect: [:each | each isFolders] ifNone: []!

foldMarginColor
	"Answer the background colour of the fold margin."

	^foldMarginColor ifNil: [Color face3d]!

foldMarginColor: aColorOrNil 
	"Set the background colour of the fold margin."

	(foldMarginColor := aColorOrNil) isNil 
		ifTrue: [self sciSetFoldMarginColour: false back: nil]
		ifFalse: [self sciSetFoldMarginColour: true back: aColorOrNil asRGB]!

foldMarginHiColor
	"Answer the background colour of the fold margin."

	^foldMarginHiColor ifNil: [Color highlight3d]!

foldMarginHiColor: aColorOrNil 
	"Set the background colour of the fold margin."

	(foldMarginHiColor := aColorOrNil) isNil 
		ifTrue: [self sciSetFoldMarginHiColour: false fore: nil]
		ifFalse: [self sciSetFoldMarginHiColour: true fore: aColorOrNil asRGB]!

foldMarkerStyle
	"Answer the symbolic name of the fold markers (outliner glyphs) in use in the receiver if
	the fold margin is visible, and the current lexer supports (and is enabled for) folding. If
	the fold marker style is nil, then all markers are available to be defined as desired. This
	allows for custom fold marker styles on a per-instance basis, or you can add your own styles
	to the FoldMarkerStyles collection."

	^foldMarkerStyle!

foldMarkerStyle: aSymbol 
	| folders |
	folders := aSymbol ifNil: [#()] ifNotNil: [ScintillaMarkerDefinition perform: aSymbol].
	markerDefinitions := markerDefinitions 
				reject: [:each | each id between: SC_MARKNUM_FOLDEREND and: SC_MARKNUM_FOLDEROPEN].
	folders do: 
			[:each | 
			each applyToView: self.
			markerDefinitions at: each name put: each].
	foldMarkerStyle := aSymbol!

forecolor: aColorOrNil 
	"Sets the background colour of the receiver to aColorOrNil. If aColorOrNil is nil then
	inherit the foreground colour of the parent window."

	"Note we don't supersend, because we don't need to invalidate on changing colours - Scintilla takes care of that"

	forecolor := aColorOrNil.
	self updateTextStyles!

formatRectangle
	"Answers the <Rectangle> used to format the text."

	"Implementation Note: Scintilla does not support this functionality of the standard edit control"

	^self shouldNotImplement!

formFeed
	"Insert a Form Feed character."

	self 
		sendMessage: SCI_FORMFEED
		wParam: 0
		lParam: 0!

getDirectPointer
	this := (super sendMessage: SCI_GETDIRECTPOINTER) asExternalHandle!

goto: anInteger 
	"Move the caret to the specified position and ensure it is visible."

	self sciGotoPos: anInteger - 1!

gotoLine: lineInteger 
	"Set caret to start of the line with the specified one-based <integer> index, ensure that
	the line is visible."

	self sciGotoLine: lineInteger - 1!

handle: aHandleOrNil 
	this := nil.
	super handle: aHandleOrNil!

hangingIndent
	"Retrive the start indent for wrapped lines."

	^self 
		sendMessage: SCI_GETWRAPSTARTINDENT
		wParam: 0
		lParam: 0!

hangingIndent: indentInteger 
	"Set the start indent for wrapped lines."

	self 
		sendMessage: SCI_SETWRAPSTARTINDENT
		wParam: indentInteger
		lParam: 0!

hasFoldMargin
	^self foldMargin ifNotNil: [:margin | margin width ~~ 0] ifNil: [false]!

hasFoldMargin: aBoolean 
	self foldMargin ifNotNil: [:margin | margin width: (aBoolean ifTrue: [16] ifFalse: [0])]!

hasIndentationGuides
	"Are the indentation guides visible?"

	^self indentationGuides ~= nil!

hasIndentationGuides: aBoolean 
	"Show or hide indentation guides."

	self indentationGuides: (aBoolean ifTrue: [#real])
		!

hasLineNumbers
	^self lineNumberMargin ifNotNil: [:margin | margin width ~~ 0] ifNil: [false]!

hasLineNumbers: aBoolean 
	self lineNumberMargin 
		ifNotNil: [:margin | margin width: (aBoolean ifTrue: [self requiredLineMarginWidth] ifFalse: [0])]!

hasVisibleLineEndings
	"Are the end of line characters visible?"

	^(self 
		sendMessage: SCI_GETVIEWEOL
		wParam: 0
		lParam: 0) asBoolean!

hasVisibleLineEndings: visibleBoolean 
	"Make the end of line characters visible or invisible."

	self 
		sendMessage: SCI_SETVIEWEOL
		wParam: visibleBoolean asParameter
		lParam: 0!

hideExtraIndicators
	| indics |
	indics := 8 - self styleBits.	"Make any indicators coinciding with the extra style bits invisible."
	indics - extraStyleBits to: indics - 1
		do: 
			[:each | 
			self 
				sendMessage: SCI_INDICSETSTYLE
				wParam: each
				lParam: INDIC_HIDDEN]!

hideIndicators
	"Private - Reconfigure all indicator styles to hidden apart from the default 3 pre-configured styles."

	3 to: INDIC_MAX
		do: 
			[:each | 
			self 
				sendMessage: SCI_INDICSETSTYLE
				wParam: each
				lParam: INDIC_HIDDEN]!

hideSelection: normalBoolean 
	"Draw the selection in normal style or with selection highlighted."

	self 
		sendMessage: SCI_HIDESELECTION
		wParam: normalBoolean asParameter
		lParam: 0!

highlightBracesAt: leftInteger and: rightInteger 
	"Highlight the pair of characters (presumably braces) at the specified one-based <integer>
	indices by applying the #braceHighlight text style to them."

	self sciBraceHighlight: leftInteger - 1 pos2: rightInteger - 1!

highlightCallTipFrom: startInteger to: stopInteger 
	"Highlight a range of the text in the call tip."

	self sciCallTipSetHlt: startInteger - 1 end: stopInteger - 1!

highlightFindMatch: anInterval 
	"self ensureVisible: anInterval start.
	self ensureVisible: anInterval stop."

	self selectionRange: anInterval.
	self ensureCaretVisible!

highlightGuide
	"Get the highlighted indentation guide column."

	^self 
		sendMessage: SCI_GETHIGHLIGHTGUIDE
		wParam: 0
		lParam: 0!

highlightGuide: columnInteger 
	"Set the highlighted indentation guide column. 0 = no highlighted guide."

	self 
		sendMessage: SCI_SETHIGHLIGHTGUIDE
		wParam: columnInteger
		lParam: 0!

highlightMismatchedBrace: anInteger 
	"Highlight the character (presumably a brace) at the specified one-based <integer> index
	with the #mismatchedBrace text style. If the argument is zero, any brace highlighting is removed."

	self sciBraceBadLight: anInteger - 1!

highlightRange: anInterval withIndicator: idInteger 
	"Apply the indicator identified by the <integer> argument to the range of text
	specified by the <Interval> argument."

	#deprecated.
	self setIndicator: idInteger range: anInterval!

hoverTime
	"Retrieve the time the mouse must sit still to generate a mouse dwell event."

	^self 
		sendMessage: SCI_GETMOUSEDWELLTIME
		wParam: 0
		lParam: 0!

hoverTime: periodMillisecondsInteger 
	"Sets the time the mouse must sit still to generate a mouse dwell event."

	self 
		sendMessage: SCI_SETMOUSEDWELLTIME
		wParam: periodMillisecondsInteger
		lParam: 0!

idOfStyleNamed: aSymbol 
	^(self styleNamed: aSymbol) ifNil: [0] ifNotNil: [:style | style id]!

indent
	"If selection is empty or all on one line replace the selection with a tab character. If
	more than one line selected, indent the lines."

	self 
		sendMessage: SCI_TAB
		wParam: 0
		lParam: 0!

indentation
	"Retrieve indentation size."

	^self 
		sendMessage: SCI_GETINDENT
		wParam: 0
		lParam: 0!

indentation: indentSizeInteger 
	"Set the number of spaces used for one level of indentation."

	self 
		sendMessage: SCI_SETINDENT
		wParam: indentSizeInteger
		lParam: 0!

indentationGuides
	"Answer the symbolic name of the style of indentation guides visible in the receiver, or nil if none."

	^IndentationGuideStyles at: self sciGetIndentationGuides ifAbsent: []!

indentationGuides: aSymbolOrNil 
	"Set the style of indication guides to be displayed in the receiver, one of nil (no
	guides), #real, #lookForward, #lookBoth."

	self sciSetIndentationGuides: (IndentationGuideStyles indexOf: aSymbolOrNil ifAbsent: [0])!

indentationOfLine: anInteger 
	"Retrieve the number of columns that a line is indented."

	^self sciGetLineIndentation: anInteger - 1!

indicatorCount
	"Answer the number of indicators configured for the receiver. With the advent of 'modern'
	indicators, this is now fixed at 32 and not related to the number of style bits used."

	^INDIC_MAX + 1!

indicatorDefinitions
	#deprecated.
	^self indicatorStyles!

indicatorDefinitions: aSequenceableCollection 
	#deprecated.
	self indicatorStyles: aSequenceableCollection!

indicatorIdFromName: anIntegerOrSymbol 
	^anIntegerOrSymbol isInteger 
		ifTrue: 
			[(anIntegerOrSymbol between: 0 and: INDIC_MAX) 
				ifFalse: [self error: 'Indicator id is out of range: ' , anIntegerOrSymbol printString].
			anIntegerOrSymbol]
		ifFalse: 
			[(indicatorStyles ifNotNil: [:indics | indics at: anIntegerOrSymbol ifAbsent: []]) 
				ifNil: [-1	"Indicator is not configured in the view, not necessarily an error"]
				ifNotNil: [:indicStyle | indicStyle id]]!

indicatorMaskAt: anInteger 
	"Answer an <integer> which is a bit mask describing the indicators applied to the character
	at the specified one-based <integer> index."

	^self sciIndicatorAllOnFor: anInteger - 1!

indicators
	"Answer a <collection> of <ScintillaIndicator>s, being the indicators (e.g. squiggly
	underlines) associated with ranges of text in the receiver."

	^indicators ifNil: [#()]!

indicators: aCollectionOfScintillaIndicator 
	"Set the indicators (e.g. squiggly underlines) associated with ranges of text in the
	receiver to the <Collection> of <ScintillaIndicator>s argument."

	self setIndicators: aCollectionOfScintillaIndicator!

indicatorsAt: anInteger 
	"Answer a <collection> of <ScintillaIndicator>s, being the indicators applied to the
	character position with the specified one-based <integer> index."

	(indicators isNil or: [(self indicatorMaskAt: anInteger) == 0]) ifTrue: [^#()].
	^indicators select: [:each | each range includes: anInteger]!

indicatorStyles
	"Answer a <sequencedReadableCollection> containing all the receiver's indicator definitions."

	^(indicatorStyles ifNil: [#()] ifNotNil: [:value | value values]) 
		asSortedCollection: ScintillaAttribute sortByIdBlock!

indicatorStyles: aCollection 
	"Set the receiver's collection of indicators to the <collection> of
	<ScintillaIndicatorStyle>s argument."

	self setIndicatorStyles: (aCollection ifNil: [#()])!

indicatorsUnder: aPoint 
	^self indicatorsAt: (self charCloseToPosition: aPoint)!

initialize
	| styles |
	super initialize.
	markerDefinitions := self defaultMarkerDefinitions.
	markers := IdentitySet new.
	styleMask := 31.
	allTextStyles := IdentityDictionary new.
	styles := self class defaultTextStyles.
	self setCurrentTextStyles: styles.
	selectionForecolor := nil.
	selectionBackcolor := Color gray.
	whitespaces := Whitespaces.
	scFlags := 0.
	extraStyleBits := 0!

initializeControl
	"Private - Prime the Scintilla control with any settings stored in instance variables that
	differ from the defaults."

	self sciSetModEventMask: self modificationEventMask.
	self sciGetStyleBits = self styleBits ifFalse: [self sciSetStyleBits: self styleBits].
	"Not necessary.
		self sciUsePopup: false asParameter."
	self
		updateIndicatorStyles;
		updateTextStyles;
		updateMarkerDefinitions.
	styler isNil ifFalse: [self invalidateStyling].
	self setWordChars.
	autoCFillups isNil ifFalse: [self sciAutoCSetFillUps: autoCFillups].
	autoCStops isNil ifFalse: [self sciAutoCStops: autoCStops].
	whitespaceForecolor isNil 
		ifFalse: [self sciSetWhitespaceFore: true fore: whitespaceForecolor asRGB].
	whitespaceBackcolor isNil 
		ifFalse: [self sciSetWhitespaceBack: true back: whitespaceBackcolor asRGB].
	selectionForecolor isNil ifFalse: [self sciSetSelFore: true fore: selectionForecolor asRGB].
	selectionBackcolor = Color gray ifFalse: [self sciSetSelBack: true back: selectionBackcolor asRGB].
	foldFlags ifNotNil: [self sciSetFoldFlags: foldFlags].
	self isFoldingEnabled ifTrue: [self setFoldProperty: true].
	keyBindings ifNotNil: [self updateKeyBindings]!

insertText: aString at: anInteger 
	"Insert text at the specified position without moving the caret."

	self sciInsertText: anInteger - 1 text: aString!

insertText: lengthInteger from: textString 
	"Add text to the document at current position."

	self 
		sendMessage: SCI_ADDTEXT
		wParam: lengthInteger
		lParam: textString yourAddress!

invalidateStyling
	self startStylingFrom: 1!

isActiveHotspotUnderlined
	"Get whether underlining for active hotspots."

	^(self 
		sendMessage: SCI_GETHOTSPOTACTIVEUNDERLINE
		wParam: 0
		lParam: 0) asBoolean!

isActiveHotspotUnderlined: underlineBoolean 
	"Enable / Disable underlining active hotspots."

	self 
		sendMessage: SCI_SETHOTSPOTACTIVEUNDERLINE
		wParam: underlineBoolean asParameter
		lParam: 0!

isAutoCompletionActive
	"Is there an auto-completion list visible?"

	^(self 
		sendMessage: SCI_AUTOCACTIVE
		wParam: 0
		lParam: 0) asBoolean!

isAutoCompletionCancelledAtStart
	"Retrieve whether auto-completion cancelled by backspacing before start."

	^(self 
		sendMessage: SCI_AUTOCGETCANCELATSTART
		wParam: 0
		lParam: 0) asBoolean!

isAutoCompletionCancelledAtStart: cancelBoolean 
	"Should the auto-completion list be cancelled if the user backspaces to a position before
	where the box was created."

	self 
		sendMessage: SCI_AUTOCSETCANCELATSTART
		wParam: cancelBoolean asParameter
		lParam: 0!

isAutoCompletionCancelledWhenNoMatch
	"Retrieve whether or not autocompletion is hidden automatically when nothing matches."

	^(self 
		sendMessage: SCI_AUTOCGETAUTOHIDE
		wParam: 0
		lParam: 0) asBoolean!

isAutoCompletionCancelledWhenNoMatch: autoHideBoolean 
	"Set whether or not autocompletion is hidden automatically when nothing matches."

	self 
		sendMessage: SCI_AUTOCSETAUTOHIDE
		wParam: autoHideBoolean asParameter
		lParam: 0!

isAutoCompletionCaseInsensitive
	"Retrieve state of ignore case flag."

	^(self 
		sendMessage: SCI_AUTOCGETIGNORECASE
		wParam: 0
		lParam: 0) asBoolean!

isAutoCompletionCaseInsensitive: ignoreCaseBoolean 
	"Set whether case is significant when performing auto-completion searches."

	self 
		sendMessage: SCI_AUTOCSETIGNORECASE
		wParam: ignoreCaseBoolean asParameter
		lParam: 0!

isAutoCompletionSingleMatchChosen
	"Retrieve whether a single item auto-completion list automatically choose the item."

	^(self 
		sendMessage: SCI_AUTOCGETCHOOSESINGLE
		wParam: 0
		lParam: 0) asBoolean!

isAutoCompletionSingleMatchChosen: chooseSingleBoolean 
	"Should a single item auto-completion list automatically choose the item."

	self 
		sendMessage: SCI_AUTOCSETCHOOSESINGLE
		wParam: chooseSingleBoolean asParameter
		lParam: 0!

isAutoCompletionTruncating
	"Retrieve whether or not autocompletion deletes any word characters after the inserted text
	upon completion."

	^(self 
		sendMessage: SCI_AUTOCGETDROPRESTOFWORD
		wParam: 0
		lParam: 0) asBoolean!

isAutoCompletionTruncating: dropRestOfWordBoolean 
	"Set whether or not autocompletion deletes any word characters after the inserted text upon
	completion."

	self 
		sendMessage: SCI_AUTOCSETDROPRESTOFWORD
		wParam: dropRestOfWordBoolean asParameter
		lParam: 0!

isBackgroundDwellEnabled
	"Answer whether dwell (hover) events are generated even when the control does not have focus.
	There is some overhead in supporting this, so it is disabled by default."

	^scFlags allMask: BackgroundDwellEvents!

isBackgroundDwellEnabled: aBoolean 
	"Set whether dwell (hover) events are generated even when the control does not have focus."

	scFlags := scFlags mask: BackgroundDwellEvents set: aBoolean.
	(aBoolean and: [self isOpen]) ifTrue: [self startDwellTimer]!

isBraceAt: anInteger 
	| style |
	style := (self styleAt: anInteger) name.
	^(self braceChars at: style ifAbsent: []) 
		ifNil: [false]
		ifNotNil: [:chars | chars identityIncludes: (self characterAt: anInteger)]!

isBraceHighlightingEnabled
	"Answer whether automatic brace highlighting is enabled in the receiver view."

	^scFlags allMask: BraceHilightingMask!

isBraceHighlightingEnabled: aBoolean 
	"Set  whether automatic brace highlighting is enabled in the receiver view.
	See also: #braceChars:"

	scFlags := scFlags mask: BraceHilightingMask set: aBoolean!

isCallTipActive
	"Is there an active call tip?"

	^(self 
		sendMessage: SCI_CALLTIPACTIVE
		wParam: 0
		lParam: 0) asBoolean!

isCaretSticky
	"Can the caret preferred x position only be changed by explicit movement commands?"

	^(self 
		sendMessage: SCI_GETCARETSTICKY
		wParam: 0
		lParam: 0) asBoolean!

isCaretSticky: useCaretStickyBehaviourBoolean 
	"Stop the caret preferred x position changing when the user types."

	self 
		sendMessage: SCI_SETCARETSTICKY
		wParam: useCaretStickyBehaviourBoolean asParameter
		lParam: 0!

isCurrentLineHighlighted
	"Is the background of the line containing the caret in a different colour?"

	^(self 
		sendMessage: SCI_GETCARETLINEVISIBLE
		wParam: 0
		lParam: 0) asBoolean!

isCurrentLineHighlighted: showBoolean 
	"Display the background of the line containing the caret in a different colour."

	self 
		sendMessage: SCI_SETCARETLINEVISIBLE
		wParam: showBoolean asParameter
		lParam: 0!

isCurrentLineMarkedWith: aSymbol 
	"Answer whether the current line (i.e. the line with the caret) has the named marker."

	^self isLine: self currentLine markedWith: aSymbol!

isDrawingBuffered
	"Is drawing done first into a buffer or direct to the screen?"

	^(self 
		sendMessage: SCI_GETBUFFEREDDRAW
		wParam: 0
		lParam: 0) asBoolean!

isDrawingBuffered: bufferedBoolean 
	"If drawing is buffered then each line of text is drawn into a bitmap buffer before drawing
	it to the screen to avoid flicker."

	self 
		sendMessage: SCI_SETBUFFEREDDRAW
		wParam: bufferedBoolean asParameter
		lParam: 0!

isDrawingTwoPhase
	"Is drawing done in two phases with backgrounds drawn before faoregrounds?"

	^(self 
		sendMessage: SCI_GETTWOPHASEDRAW
		wParam: 0
		lParam: 0) asBoolean!

isDrawingTwoPhase: twoPhaseBoolean 
	"In twoPhaseDraw mode, drawing is performed in two phases, first the background and then the
	foreground. This avoids chopping off characters that overlap the next run."

	self 
		sendMessage: SCI_SETTWOPHASEDRAW
		wParam: twoPhaseBoolean asParameter
		lParam: 0!

isFoldingEnabled
	^scFlags allMask: FoldingMask!

isFoldingEnabled: aBoolean 
	"Enable or disable folding (or outlining). Enabling folding may have no effect if not supported by the lexer."

	"Implementation Note: Changing the fold enablement is rather long winded as there seem to be some bugs in Scintilla
	in this respect - also bearing in mind the importance of the fold flag it shouldn't really be a passive property."

	self isFoldingEnabled = aBoolean ifTrue: [^self].
	scFlags := scFlags mask: FoldingMask set: aBoolean.
	self setFoldProperty: aBoolean.
	aBoolean 
		ifFalse: 
			["If disabling folding it is necessary to call #removeAllStyling
			 (SCI_CLEARDOCUMENTSTYLE) as this is the only way to remove the existing
			 fold information."
			self removeAllStyling.
			"Scintilla Bug: Sometimes redraws incorrectly where word wrap is involved,
			so we need to force a call to the line wrapping code, this being one way to do that."
			self sciSetMarginLeft: self sciGetMarginLeft].
	self invalidateStyling!

isIndicator: anIntegerOrSymbol setAt: positionInteger 
	"Answer whether the indicator identified by the <integer> id or <symbol> name,
	anIntegerOrSymbol, is set at the at the one-based <integer> character position,
	positionInteger."

	^(self indicatorMaskAt: positionInteger) 
		anyMask: (1 bitShift: (self indicatorIdFromName: anIntegerOrSymbol))!

isLine: lineInteger folded: expandedBoolean 
	"Fold/unfold the specified header line, depending on the value of the <boolean> argument."

	self sciSetFoldExpanded: lineInteger - 1 expanded: expandedBoolean!

isLine: anInteger markedWith: aSymbol 
	"Answer whether the line in the receiver with the specified one-based <integer>
	index has the named marker."

	| def |
	def := markerDefinitions at: aSymbol ifAbsent: [].
	^def notNil and: [(self sciMarkerGet: anInteger - 1) allMask: (1 bitShift: def id)]!

isLineVisible: anInteger 
	"Answer whether the line with the specified one-based <integer> index is visible"

	^self sciGetLineVisible: anInteger - 1!

isOvertypeEnabled
	"Returns true if overtype mode is active otherwise false is returned."

	^(self 
		sendMessage: SCI_GETOVERTYPE
		wParam: 0
		lParam: 0) asBoolean!

isOvertypeEnabled: overtypeBoolean 
	"Set to overtype (true) or insert mode."

	self 
		sendMessage: SCI_SETOVERTYPE
		wParam: overtypeBoolean asParameter
		lParam: 0!

isScrollWidthTracking
	"Retrieve whether the scroll width tracks wide lines."

	^(self 
		sendMessage: SCI_GETSCROLLWIDTHTRACKING
		wParam: 0
		lParam: 0) asBoolean!

isScrollWidthTracking: trackingBoolean 
	"Sets whether the maximum width line displayed is used to set scroll width."

	self 
		sendMessage: SCI_SETSCROLLWIDTHTRACKING
		wParam: trackingBoolean asParameter
		lParam: 0!

isSelectionBackcolorExtendedToEndOfLine
	"Is the selection end of line filled?"

	^(self 
		sendMessage: SCI_GETSELEOLFILLED
		wParam: 0
		lParam: 0) asBoolean!

isSelectionBackcolorExtendedToEndOfLine: filledBoolean 
	"Set the selection to have its end of line filled or not."

	self 
		sendMessage: SCI_SETSELEOLFILLED
		wParam: filledBoolean asParameter
		lParam: 0!

isSelectionKept
	"Answer true if the receiver is set to maintain selection even after losing focus."

	"N.B. Scintilla always displays the selection - this is not configurable."

	^true!

isSelectionRectangular
	"Is the selection rectangular? The alternative is the more common stream selection."

	^(self 
		sendMessage: SCI_SELECTIONISRECTANGLE
		wParam: 0
		lParam: 0) asBoolean!

isStylingEnabled
	"Answer whether dynamic text styling using the receiver's configured <ScintillaStyler> is enabled."

	^self sciGetLexer ~~ SCLEX_NULL!

isStylingEnabled: aBoolean 
	"Enable or disable dynamic text styling using the receiver's configured <ScintillaStyler>."

	self isStylingEnabled == aBoolean ifTrue: [^self].
	aBoolean 
		ifTrue: 
			[self sciSetLexer: SCLEX_CONTAINER.
			self invalidateStyling]
		ifFalse: 
			[self sciSetLexer: SCLEX_NULL.
			self removeAllStyling]!

isTextModified
	"Is the document different from when it was last saved?"

	^(self 
		sendMessage: SCI_GETMODIFY
		wParam: 0
		lParam: 0) asBoolean!

isTextModified: aBoolean 
	"Private - Set/reset the receiver's text modification flag."

	#todo.	"Supersend here sends EM_SETMODIFY which is deprecated from Scintilla's point of view"
	aBoolean ifTrue: [super isTextModified: aBoolean] ifFalse: [self sciSetSavePoint]!

isUndoEnabled
	"Is undo history being collected?"

	^(self 
		sendMessage: SCI_GETUNDOCOLLECTION
		wParam: 0
		lParam: 0) asBoolean!

isUndoEnabled: collectUndoBoolean 
	"Choose between collecting actions into the undo history and discarding them."

	self 
		sendMessage: SCI_SETUNDOCOLLECTION
		wParam: collectUndoBoolean asParameter
		lParam: 0!

isUsingPalette
	"In palette mode?"

	^(self 
		sendMessage: SCI_GETUSEPALETTE
		wParam: 0
		lParam: 0) asBoolean!

isUsingPalette: usePaletteBoolean 
	"In palette mode, Scintilla uses the environment's palette calls to display more colours.
	This may lead to ugly displays."

	self 
		sendMessage: SCI_SETUSEPALETTE
		wParam: usePaletteBoolean asParameter
		lParam: 0!

isUsingTabs
	"Retrieve whether tabs will be used in indentation."

	^(self 
		sendMessage: SCI_GETUSETABS
		wParam: 0
		lParam: 0) asBoolean!

isUsingTabs: useTabsBoolean 
	"Indentation will only use space characters if useTabs is false, otherwise it will use a
	combination of tabs and spaces."

	self 
		sendMessage: SCI_SETUSETABS
		wParam: useTabsBoolean asParameter
		lParam: 0!

joinTarget
	"Join the lines in the target."

	self 
		sendMessage: SCI_LINESJOIN
		wParam: 0
		lParam: 0!

keyBindings
	"Answer the collection of key bindings currently assigned in the receiver."

	^(keyBindings ifNil: [self defaultKeyBindings]) 
		asSortedCollection: [:a :b | a commandSymbol <= b commandSymbol]
	" asArray"!

keyBindings: aCollectionOfScintillaKeyBindings 
	aCollectionOfScintillaKeyBindings 
		ifNil: [keyBindings := nil]
		ifNotNil: 
			[| set |
			set := aCollectionOfScintillaKeyBindings asSet.
			(set noDifference: DefaultKeyBindings values) 
				ifFalse: 
					[keyBindings := LookupTable new.
					set do: [:each | keyBindings at: each acceleratorKey put: each]]].
	self updateKeyBindings!

keyboardCommands
	^self keyBindings collect: 
			[:each | 
			(CommandDescription command: each commandSymbol)
				acceleratorKey: each acceleratorKey;
				yourself]!

layoutCachingMode
	"Answer a <Symbol> naming the current layout caching mode. This will be one of: 
		#none			No lines are cached
		#caret			The line containing the text caret. This is the default.
		#page			Visible lines plus the line containing the caret.
		#document		All lines in the document.
	These correspond to the Scintilla constants SC_CACHE_NONE, SC_CACHE_CARET, 
	SC_CACHE_PAGE, SC_CACHE_DOCUMENT respectively."

	"Selection of the this mode is a trade-off between line wrap performance and memory usage:
	From the Scintilla Documentation: 'Much of the time used by Scintilla is spent on laying out
	and drawing text. The same text layout calculations may be performed many times even when
	the data used in these calculations does not change. To avoid these unnecessary calculations
	in some circumstances, the line layout cache can store the results of the calculations. The
	cache is invalidated whenever the underlying data, such as the contents or styling of the
	document changes. Caching the layout of the whole document [#document mode in Dolphin] has
	the most effect, making dynamic line wrap as much as 20 times faster but this requires 7
	times the memory required by the document contents plus around 80 bytes per line.'"    

	^self class layoutCachingModes at: self sciGetLayoutCache+1!

layoutCachingMode: aSymbol
	"Set the layout caching mode - see #layoutCachingMode for further details."

	^self sciSetLayoutCache: (self class layoutCachingModes keyAtValue: aSymbol)-1!

lexer
	"Answer the symbolic name of the lexer currently configured for the receiver.
	This is normally #container, meaning the lexing is implemented by the <ScintillaStyler>
	held in the 'styler' instance variable."

	| id |
	id := self sciGetLexer.
	^id == SCLEX_AUTOMATIC ifTrue: [#automatic] ifFalse: [Lexers at: id + 1 ifAbsent: []]!

lexer: aString 
	"Set the lexer used in the receiver to be that named by the <Symbol> argument. #container is
	a special case, meaning that lexing is performed by the <ScintillaStyler> object held in the
	'styler' instance variable, rather than by some C++ module linked into SciLexer.dll."

	"Note that switching to a particular lexer does not necessarily mean that you will get
	the same visual results as in Scite (for example) because the visual styles may not be
	configured correctly, if at all."

	| sym |
	sym := aString asSymbol.
	self lexer == sym ifTrue: [^self].
	self setLexerLanguage: sym.
	self applyTextStylesForLexer: sym!

lineCount
	"Returns the number of lines in the document. There is always at least one."

	^self 
		sendMessage: SCI_GETLINECOUNT
		wParam: 0
		lParam: 0!

lineHeight: lineInteger 
	"Retrieve the height of a particular line of text in pixels."

	^self sciTextHeight: lineInteger - 1!

lineLength: anInteger 
	"Answers the length of the line at anInteger."

	"Implementation Note: SCI_LINELENGTH includes line-terminators, which we don't want.
	See the Scintilla documentation."

	^(self lineRange: anInteger) size!

lineLengthFromPosition: anInteger 
	"Private - Answer the <integer> length of the line containing the specified one-based
	<integer> character position, charPos. Raise a <BoundsError> if the character position is
	out of bounds."

	^self lineLength: (self lineFromPosition: anInteger)!

lineNumberMargin
	"Answer the <ScintillaMargin> used to display line numbers."

	"N.B. It is assumed there is only one line-number margin in the view, so the first
	encountered is used."

	^self margins detect: [:each | each isNumbers] ifNone: []!

lineRange: anInteger 
	"Answer an <Interval> specifying the range of character positions in the receiver occuppied
	by the line with the specified <integer> index, not including the end-of-line terminators
	(if any)."

	"Implementation Note: Override to exploit specific functionality available from Scintilla"

	| start end pos |
	pos := anInteger - 1.
	start := self basicPositionAtLine: pos.
	end := self sciGetLineEndPosition: pos.
	(start < 0 or: [end < start]) ifTrue: [^self errorSubscriptBounds: anInteger].
	^start + 1 to: end!

lineScroll
	"Answers the number of the first line displayed in the receiver."

	^self sciGetFirstVisibleLine + 1!

lineScrollBy: anInteger 
	"Scrolls the text in the receiver by anInteger lines."

	self sciLineScroll: 0 lines: anInteger!

linesOnScreen
	"Retrieves the number of lines completely visible."

	^self 
		sendMessage: SCI_LINESONSCREEN
		wParam: 0
		lParam: 0!

marginCount
	"Private - Answer the maximum number of margins that can be displayed. This is currently fixed at
	three."

	^3!

margins
	"Answer a <sequencedReadableCollection> of <ScintillaMargin> containing all the receiver's
	margins."

	^(1 to: self marginCount) collect: [:index | ScintillaMargin view: self index: index - 1]!

margins: aSequenceableCollection 
	"Set the receiver's collection of margins to the <sequencedReadableCollection> of <ScintillaMargin> 
	argument."

	self applyAttributes: aSequenceableCollection!

marginWidths
	"Private - Answer a two element <Array> containing the <integer> widths of left and right
	page margins (not the same as Scintilla 'margins')."

	^Array with: self sciGetMarginLeft with: self sciGetMarginRight!

markerDefinitions
	"Answer an <OrderedCollection> of <ScintillaMarkerDefinition>s, being the margin markers
	currently defined for this view, in ascending order of id. Each marker definition can be
	configured to use a particular glyph, such as an arrow or circle, as well as its foreground
	and background colours. The application refers to the markers it wants to use by name so
	that the visual appearance of those markers can be configured in the view. A maximum of 32
	different markers can be defined, this a limit set by Scintilla itself, however we consider
	7 of these to be predefined for use as 'folding' (outlining) markers."

	| defns |
	defns := markerDefinitions values.
	foldMarkerStyle 
		ifNotNil: 
			[defns := defns 
						reject: [:each | each id notNil and: [each id between: SC_MARKNUM_FOLDEREND and: MARKER_MAX]]].
	^defns asSortedCollection: ScintillaAttribute sortByIdBlock!

markerDefinitions: aCollection 
	"Set the margin markers currently defined for this view to be those in the <Collection> of
	<ScintillaMarkerDefinition>s argument. See #markerDefinitions for further information."

	| allocated available count userIds |
	userIds := 0 to: (foldMarkerStyle ifNil: [MARKER_MAX] ifNotNil: [SC_MARKNUM_FOLDEREND - 1]).
	aCollection size > userIds size ifTrue: [^self error: 'Too many marker definitions'].
	count := aCollection size.
	allocated := OrderedCollection new: count.
	aCollection do: [:each | each id ifNotNil: [:id | allocated add: id]].
	available := (userIds difference: allocated) readStream.
	markerDefinitions := markerDefinitions reject: [:each | userIds includes: each id].
	aCollection do: 
			[:each | 
			each id isNil ifTrue: [each id: available next].
			each applyToView: self.
			markerDefinitions at: each name put: each]!

markers
	"Answer a <collection> of the <ScintillaMarker>s currently set in the receiver.
	These display as minature graphics in the margin alongside the marked lines."

	^markers!

markers: aCollection 
	"Set the markers to be displayed in the receiver's margin(s) to be those in the <collection>
	of the <ScintillaMarker>s argument."

	markers := aCollection asOrderedCollection.
	self updateMarkers!

markerTypesOnLine: anInteger 
	"Answer a <collection> of <Symbol>s, being the names of the marker types currently
	set on the line with the specified one-based <integer> index."

	| mask types |
	mask := (self sciMarkerGet: anInteger - 1) asDword.
	types := IdentitySet new.
	markerDefinitions 
		do: [:each | (mask allMask: (1 bitShift: each id)) ifTrue: [types add: each name]].
	^types!

maxCompletionListHeight
	"Set the maximum height, in rows, of auto-completion and user lists."

	^self 
		sendMessage: SCI_AUTOCGETMAXHEIGHT
		wParam: 0
		lParam: 0!

maxCompletionListHeight: rowCountInteger 
	"Set the maximum height, in rows, of auto-completion and user lists. The default is 5 rows."

	self 
		sendMessage: SCI_AUTOCSETMAXHEIGHT
		wParam: rowCountInteger
		lParam: 0!

maxCompletionListWidth
	"Get the maximum width, in characters, of auto-completion and user lists."

	^self 
		sendMessage: SCI_AUTOCGETMAXWIDTH
		wParam: 0
		lParam: 0!

maxCompletionListWidth: characterCountInteger 
	"Set the maximum width, in characters, of auto-completion and user lists. Set to 0 to
	autosize to fit longest item, which is the default."

	self 
		sendMessage: SCI_AUTOCSETMAXWIDTH
		wParam: characterCountInteger
		lParam: 0!

maxStyle
	"Answer the maximum usable style index. From Scintilla 1.75 all 8-bits can now be used for
	style bits with 0 for indicators, so this is normally 255. Styles are numbered from zero, so
	the limit on the number of styles is maxStyle+1"

	^styleMask!

modificationEventMask
	"Answer the <integer> notification event mask that controls the SCN_MODIFIED notifications that 
	the control will send. This will be a combination of the bit flags mainly in the SC_MOD_ enumeration,
	but see the Scintilla documentation for a full list and further details."

	^modificationEventMask ifNil: [self defaultModEventMask]!

modificationEventMask: anInteger 
	"Set the notification event mask that controls the SCN_MODIFIED notifications that will be sent.
	The <integer> argument is a combination of the bit flags mainly in the SC_MOD_ enumeration,
	but see the Scintilla documentation for a full list and further details."

	modificationEventMask = anInteger ifTrue: [^self].
	modificationEventMask := anInteger = self defaultModEventMask ifFalse: [anInteger].
	self sciSetModEventMask: self modificationEventMask!

modifyText: niladicBlock 
	"Evaluate the <niladicBlock> argument, which is assumed to modify the receiver's text
	content."

	"Implementation Note: Scintilla has a bug (well I consider it one) such that it refuses to
	set text if in read-only mode. This is not consistent with the normal behaviour of Windows
	text controls, which permit programmatic modifications in read-only mode. To workaround we
	must temporarily disable read-only mode when performing any operation which updates text in
	the view."

	| readOnly |
	readOnly := self isReadOnly.
	self isReadOnly: false.
	niladicBlock ensure: [self isReadOnly: readOnly]!

moveCaretInsideView
	"Move the caret inside current view if it's not there already."

	self 
		sendMessage: SCI_MOVECARETINSIDEVIEW
		wParam: 0
		lParam: 0!

moveDown
	"Move caret down one line."

	self 
		sendMessage: SCI_LINEDOWN
		wParam: 0
		lParam: 0!

moveLeft
	"Move caret left one character."

	self 
		sendMessage: SCI_CHARLEFT
		wParam: 0
		lParam: 0!

movePageDown
	"Move caret one page down."

	self 
		sendMessage: SCI_PAGEDOWN
		wParam: 0
		lParam: 0!

movePageUp
	"Move caret one page up."

	self 
		sendMessage: SCI_PAGEUP
		wParam: 0
		lParam: 0!

moveParaDown
	"Move caret between paragraphs (delimited by empty lines)."

	self 
		sendMessage: SCI_PARADOWN
		wParam: 0
		lParam: 0!

moveParaUp
	self 
		sendMessage: SCI_PARAUP
		wParam: 0
		lParam: 0!

moveRight
	"Move caret right one character."

	self 
		sendMessage: SCI_CHARRIGHT
		wParam: 0
		lParam: 0!

moveStutteredPageDown
	"Move caret to bottom of page, or one page down if already at bottom of page."

	self 
		sendMessage: SCI_STUTTEREDPAGEDOWN
		wParam: 0
		lParam: 0!

moveStutteredPageUp
	"Move caret to top of page, or one page up if already at top of page."

	self 
		sendMessage: SCI_STUTTEREDPAGEUP
		wParam: 0
		lParam: 0!

moveToEndOfDisplayLine
	"Move caret to last position on display line."

	self 
		sendMessage: SCI_LINEENDDISPLAY
		wParam: 0
		lParam: 0!

moveToEndOfDocument
	"Move caret to last position in document."

	self 
		sendMessage: SCI_DOCUMENTEND
		wParam: 0
		lParam: 0!

moveToEndOfLine
	"Move caret to last position on line."

	self 
		sendMessage: SCI_LINEEND
		wParam: 0
		lParam: 0!

moveToEndOfNextWord
	"Move caret right one word, position cursor at end of word."

	self 
		sendMessage: SCI_WORDRIGHTEND
		wParam: 0
		lParam: 0!

moveToEndOfPreviousWord
	"Move caret left one word, position cursor at end of word."

	self 
		sendMessage: SCI_WORDLEFTEND
		wParam: 0
		lParam: 0!

moveToEndOfWord
	"Move caret right one word."

	self 
		sendMessage: SCI_WORDRIGHT
		wParam: 0
		lParam: 0!

moveToEndOfWordPart
	"Move to the change next in capitalisation."

	self 
		sendMessage: SCI_WORDPARTRIGHT
		wParam: 0
		lParam: 0!

moveToEndOfWrappedLine
	self 
		sendMessage: SCI_LINEENDWRAP
		wParam: 0
		lParam: 0!

moveToStartOfDisplayLine
	"Move caret to first position on display line."

	self 
		sendMessage: SCI_HOMEDISPLAY
		wParam: 0
		lParam: 0!

moveToStartOfDocument
	"Move caret to first position in document."

	self 
		sendMessage: SCI_DOCUMENTSTART
		wParam: 0
		lParam: 0!

moveToStartOfLine
	"Move caret to first position on line."

	self 
		sendMessage: SCI_HOME
		wParam: 0
		lParam: 0!

moveToStartOfWord
	"Move caret left one word."

	self 
		sendMessage: SCI_WORDLEFT
		wParam: 0
		lParam: 0!

moveToStartOfWordPart
	"Move to the previous change in capitalisation."

	self 
		sendMessage: SCI_WORDPARTLEFT
		wParam: 0
		lParam: 0!

moveToStartOfWrappedLine
	"These are like their namesakes Home(Extend)?, LineEnd(Extend)?, VCHome(Extend)? except they
	behave differently when word-wrap is enabled: They go first to the start / end of the
	display line, like (Home|LineEnd)Display The difference is that, the cursor is already at
	the point, it goes on to the start or end of the document line, as appropriate for
	(Home|LineEnd|VCHome)(Extend)?."

	self 
		sendMessage: SCI_HOMEWRAP
		wParam: 0
		lParam: 0!

moveToVcHome
	"Move caret to before first visible character on line. If already there move to first
	character on line."

	self 
		sendMessage: SCI_VCHOME
		wParam: 0
		lParam: 0!

moveToWrappedVcHome
	self 
		sendMessage: SCI_VCHOMEWRAP
		wParam: 0
		lParam: 0!

moveUp
	"Move caret up one line."

	self 
		sendMessage: SCI_LINEUP
		wParam: 0
		lParam: 0!

newLine
	"Insert a new line, may use a CRLF, CR or LF depending on EOL mode."

	self 
		sendMessage: SCI_NEWLINE
		wParam: 0
		lParam: 0!

nmNotify: pNMHDR 
	"Private - Handler for a redirected generic WM_NOTIFY message."

	^(ScnMap at: (pNMHDR sdwordAtOffset: 8) - 1999 ifAbsent: []) 
		ifNotNil: [:action | self perform: action with: pNMHDR]!

onEraseRequired: aColorEvent 
	"Handler for erase background event - allow the control to take care of this, unless
	transparent backcolor is set in which case the erase is suppressed.."

	"Scintilla deals with all this itself"

	^nil!

onKillFocus
	"Handler for loss of focus"

	"Implementation Note: Scintilla does not stop its caret/dwell timer when it loses focus,
	causing needless consumption of CPU (and network bandwidth if using RDC) when in the
	background. The timer is needed, however, if background dwell events are wanted."

	self isBackgroundDwellEnabled ifFalse: [self stopDwellTimer].
	^super onKillFocus!

onSetFocus
	"Handler for set focus event"

	"See #onKillFocus"

	self isBackgroundDwellEnabled ifFalse: [self startDwellTimer].
	^super onSetFocus!

onViewCreated
	"The receiver window has just been created. Populate the control with any non-default state
	preserved in instance variables."

	super onViewCreated.
	self initializeControl!

passwordCharacter
	^self shouldNotImplement!

passwordCharacter: aCharacter 
	^self shouldNotImplement!

pasteClipboard
	"Paste the contents of the clipboard into the document replacing the selection."

	self 
		sendMessage: SCI_PASTE
		wParam: 0
		lParam: 0!

performUndoableAction: aNiladicBlock 
	"Evaluate the <niladicBlock> argument as a composite update within an undo group
	such that it may be undone as a single undo operation."

	self beginUndoGroup.
	aNiladicBlock ensure: [self endUndoGroup]!

plainText
	"Answers the plain, unformatted, text from the receiver."

	| buf |
	buf := String newFixed: self textLength.
	self sciGetText: buf byteSize text: buf.
	^buf!

plainText: aString 
	"Private - Set the text contents of the receiver to the plain text aString.
	Part of the RichText double dispatching protocol"

	self modifyText: [self setText: aString]!

plainTextFrom: startInteger to: stopInteger 
	"Answer a string containing the plain text contents of the receiver in the specified
	one-based, end-inclusive, range."

	| range |
	startInteger < 1 ifTrue: [^self errorSubscriptBounds: startInteger].
	stopInteger < startInteger ifTrue: [^String new].
	stopInteger > self textLength ifTrue: [^self errorSubscriptBounds: stopInteger].
	range := TEXTRANGE from: startInteger - 1 to: stopInteger.
	self sciGetTextRange: range.
	^range text!

positionCacheSize
	"How many entries are allocated to the position cache?"

	^self 
		sendMessage: SCI_GETPOSITIONCACHE
		wParam: 0
		lParam: 0!

positionCacheSize: sizeInteger 
	"Set number of entries in position cache"

	self 
		sendMessage: SCI_SETPOSITIONCACHE
		wParam: sizeInteger
		lParam: 0!

positionOfChar: anInteger 
	"Map the one-based index of a character in the receiver to its client co-ordinates within
	the receiver."

	| pos |
	pos := anInteger - 1.
	^(self sciPointXFromPosition: pos) @ (self sciPointYFromPosition: pos)!

preservingStylingPositionDo: aBlock 
	"Private - Evaluate the <niladicBlock> argument without disturbing the last valid styling position."

	"ensure: [self sciSetLayoutCache: mode]"

	| save |
	save := self stylingPosition.
	aBlock ensure: [self startStylingFrom: save]!

printColourMode
	"Returns the print colour mode."

	^self 
		sendMessage: SCI_GETPRINTCOLOURMODE
		wParam: 0
		lParam: 0!

printColourMode: modeInteger 
	"Modify colours when printing for clearer printed text."

	self 
		sendMessage: SCI_SETPRINTCOLOURMODE
		wParam: modeInteger
		lParam: 0!

printMagnification
	"Returns the print magnification."

	^self 
		sendMessage: SCI_GETPRINTMAGNIFICATION
		wParam: 0
		lParam: 0!

printMagnification: magnificationInteger 
	"Sets the print magnification added to the point size of each style for printing."

	self 
		sendMessage: SCI_SETPRINTMAGNIFICATION
		wParam: magnificationInteger
		lParam: 0!

queryCommand: aCommandQuery 
	"Private - Enters details about a potential command for the receiver into the 
	<CommandQuery>."

	| command |
	command := aCommandQuery commandSymbol.
	command == #toggleStyling 
		ifTrue: 
			[aCommandQuery
				isEnabled: true;
				isChecked: self isStylingEnabled.
			^true].
	command == #toggleLineNumbers 
		ifTrue: 
			[self lineNumberMargin 
				ifNil: [aCommandQuery isEnabled: false]
				ifNotNil: 
					[:margin | 
					aCommandQuery
						isEnabled: true;
						isChecked: margin width ~= 0].
			^true].
	command == #toggleLineEndings 
		ifTrue: 
			[aCommandQuery
				isEnabled: true;
				isChecked: self hasVisibleLineEndings.
			^true].
	command == #toggleIndentationGuides 
		ifTrue: 
			[aCommandQuery
				isEnabled: true;
				isChecked: self hasIndentationGuides.
			^true].
	command == #toggleWhitespace 
		ifTrue: 
			[aCommandQuery
				isEnabled: true;
				isChecked: self whitespaceVisibility ~~ #invisible.
			^true].
	^super queryCommand: aCommandQuery!

rangeOfIndicator: anIntegerOrSymbol at: positionInteger 
	"Answer an <Interval> representing the range of one-based character positions of the
	indicator whose style is identified by the <integer> id or <symbol> name, anIntegerOrSymbol,
	that intersects with the one-based <integer> character position, positionIndicator. If the
	indicator is not set at the specified position then the interval will be empty."

	| pos |
	pos := positionInteger - 1.
	^(positionInteger > 0 and: [self isIndicator: anIntegerOrSymbol setAt: positionInteger]) 
		ifTrue: 
			[| id |
			id := self indicatorIdFromName: anIntegerOrSymbol.
			(self sciIndicatorStart: id position: pos) + 1 to: (self sciIndicatorEnd: id position: pos) + 1]
		ifFalse: 
			["The interval occuppied by the indicator that intersects the position is empty"
			positionInteger to: pos]!

redo
	"Redoes the next action on the undo history."

	self 
		sendMessage: SCI_REDO
		wParam: 0
		lParam: 0!

rememberCaretX
	"Set the last x chosen value to be the caret x position."

	self 
		sendMessage: SCI_CHOOSECARETX
		wParam: 0
		lParam: 0!

removeAllMarkers
	"Remove all markers on all lines from the view."

	self resetMarkers.
	self deleteMarkers: 0!

removeAllStyling
	"Set all style bytes to 0, remove all folding information."

	self 
		sendMessage: SCI_CLEARDOCUMENTSTYLE
		wParam: 0
		lParam: 0!

removeBraceHighlight
	"Remove any current brace highlighting."

	self sciBraceHighlight: -1 pos2: -1!

removeKeyBinding: aScintillaKeyBinding 
	| bindings |
	bindings := self keyBindings.
	(bindings removeKey: aScintillaKeyBinding ifAbsent: []) 
		ifNotNil: 
			[:removed | 
			self sciClearCmdKey: removed scintillaKeyCode.
			keyBindings := bindings]!

removeMarker: aScintillaMarker 
	"Remove the specified <ScintillaMarker> from the view."

	markers remove: aScintillaMarker.
	self sciMarkerDeleteHandle: aScintillaMarker handle.
	aScintillaMarker removedFromView!

removeMarkersOfType: aSymbol 
	"Remove markers of the type named by the <Symbol> argument from the view."

	| markerDef |
	markerDef := markerDefinitions at: aSymbol.
	markers := markers reject: [:each | each definition == markerDef].
	self deleteMarkers: markerDef id!

removeStylingFrom: startInteger to: stopInteger 
	"Remove any styling from the specified range of text."

	"ensure: [self sciSetLayoutCache: mode]"

	self sciStartStyling: startInteger - 1 mask: self restyleMask.
	self styleNext: (stopInteger - startInteger) + 1 mask: 0!

replaceTarget: aString 
	"Replace the receiver's current target range with the plain text represented by the <String>
	argument."

	self modifyText: [self sciReplaceTarget: -1 text: aString]!

requiredLineMarginWidth
	^self widthOfText: '_' , (self lineCount max: 999) displayString inStyle: #lineNumber!

resetMarkers
	markers do: [:each | each removedFromView].
	markers := IdentitySet new!

resetZoom
	"Reset the zoom level so the text is displayed at standard size."

	self 
		sendMessage: SCI_SETZOOM
		wParam: 0
		lParam: 0!

restyleAll
	"Re-colour the entire contents of the receiver."

	self restyleFrom: 1 to: 0!

restyleFrom: startInteger to: stopInteger 
	"Restyle the specified range of text. If stopInteger is zero, then text is styled up to the
	end. Note that this is only a request. The text may be restyled asynchronously in blocks if
	it is large. In particular if lexing is performed by a <SmalltalkStyler> (i.e. container
	based lexer with Dolphin as the container), then the maximum amount of text that will be
	synchronously styled in response to this request is controlled by the #blockSize method on
	that styler."

	"Prior to 1.63 SCI_COLOURISE had no effect for container based lexing (a bug)."

	true ifTrue: [self startStylingFrom: startInteger].
	self sciColourise: startInteger - 1 end: stopInteger - 1!

restyleMask
	^styleMask bitOr: self extraStyleMask!

sciAddRefDocument: docInteger 
	"Private - Extend life of document."

	self 
		sendMessage: SCI_ADDREFDOCUMENT
		wParam: 0
		lParam: docInteger!

sciAddStyledText: lengthInteger c: cByteArray 
	"Private - Add array of cells to document."

	self 
		sendMessage: SCI_ADDSTYLEDTEXT
		wParam: lengthInteger
		lParam: cByteArray yourAddress!

sciAllocate: bytesInteger 
	"Private - Enlarge the document to a particular size of text bytes."

	self 
		sendMessage: SCI_ALLOCATE
		wParam: bytesInteger
		lParam: 0!

sciAppendText: lengthInteger text: textString 
	"Private - Append a string to the end of the document without changing the selection."

	self 
		sendMessage: SCI_APPENDTEXT
		wParam: lengthInteger
		lParam: textString yourAddress!

sciAssignCmdKey: kmInteger msg: msgInteger 
	"Private - When key+modifier combination km is pressed perform msg."

	self 
		sendMessage: SCI_ASSIGNCMDKEY
		wParam: kmInteger
		lParam: msgInteger!

sciAutoCGetCurrent
	"Private - Get currently selected item position in the auto-completion list"

	^self 
		sendMessage: SCI_AUTOCGETCURRENT
		wParam: 0
		lParam: 0!

sciAutoCGetSeparator
	"Private - Retrieve the auto-completion list separator character."

	^self 
		sendMessage: SCI_AUTOCGETSEPARATOR
		wParam: 0
		lParam: 0!

sciAutoCGetTypeSeparator
	"Private - Retrieve the auto-completion list type-separator character."

	^self 
		sendMessage: SCI_AUTOCGETTYPESEPARATOR
		wParam: 0
		lParam: 0!

sciAutoCPosStart
	"Private - Retrieve the position of the caret when the auto-completion list was displayed."

	^self 
		sendMessage: SCI_AUTOCPOSSTART
		wParam: 0
		lParam: 0!

sciAutoCSetFillUps: characterSetString 
	"Private - Define a set of characters that when typed will cause the autocompletion to
	choose the selected item."

	self 
		sendMessage: SCI_AUTOCSETFILLUPS
		wParam: 0
		lpParam: characterSetString!

sciAutoCSetSeparator: separatorCharacterInteger 
	"Private - Change the separator character in the string setting up an auto-completion list.
	Default is space but can be changed if items contain space."

	self 
		sendMessage: SCI_AUTOCSETSEPARATOR
		wParam: separatorCharacterInteger
		lParam: 0!

sciAutoCSetTypeSeparator: separatorCharacterInteger 
	"Private - Change the type-separator character in the string setting up an auto-completion
	list. Default is '?' but can be changed if items contain '?'."

	self 
		sendMessage: SCI_AUTOCSETTYPESEPARATOR
		wParam: separatorCharacterInteger
		lParam: 0!

sciAutoCShow: lenEnteredInteger itemList: itemListString 
	"Private - Display a auto-completion list. The lenEntered parameter indicates how many
	characters before the caret should be used to provide context."

	self 
		sendMessage: SCI_AUTOCSHOW
		wParam: lenEnteredInteger
		lParam: itemListString yourAddress!

sciAutoCStops: characterSetString 
	"Private - Define a set of character that when typed cancel the auto-completion list."

	self 
		sendMessage: SCI_AUTOCSTOPS
		wParam: 0
		lpParam: characterSetString!

sciBraceBadLight: posInteger 
	"Private - Highlight the character at a position indicating there is no matching brace."

	self 
		sendMessage: SCI_BRACEBADLIGHT
		wParam: posInteger
		lParam: 0!

sciBraceHighlight: pos1Integer pos2: pos2Integer 
	"Private - Highlight the characters at two positions."

	self 
		sendMessage: SCI_BRACEHIGHLIGHT
		wParam: pos1Integer
		lParam: pos2Integer!

sciBraceMatch: posInteger 
	"Private - Find the position of a matching brace or INVALID_POSITION if no match."

	^self 
		sendMessage: SCI_BRACEMATCH
		wParam: posInteger
		lParam: 0!

sciCallTipPosStart
	"Private - Retrieve the position where the caret was before displaying the call tip."

	^self 
		sendMessage: SCI_CALLTIPPOSSTART
		wParam: 0
		lParam: 0!

sciCallTipSetBack: backRGB 
	"Private - Set the background colour for the call tip."

	self 
		sendMessage: SCI_CALLTIPSETBACK
		wParam: backRGB asParameter
		lParam: 0!

sciCallTipSetFore: foreRGB 
	"Private - Set the foreground colour for the call tip."

	self 
		sendMessage: SCI_CALLTIPSETFORE
		wParam: foreRGB asParameter
		lParam: 0!

sciCallTipSetForeHlt: foreRGB 
	"Private - Set the foreground colour for the highlighted part of the call tip."

	self 
		sendMessage: SCI_CALLTIPSETFOREHLT
		wParam: foreRGB asParameter
		lParam: 0!

sciCallTipSetHlt: startInteger end: endInteger 
	"Private - Highlight a segment of the definition."

	self 
		sendMessage: SCI_CALLTIPSETHLT
		wParam: startInteger
		lParam: endInteger!

sciCallTipShow: posInteger definition: definitionString 
	"Private - Show a call tip containing a definition near position pos."

	self 
		sendMessage: SCI_CALLTIPSHOW
		wParam: posInteger
		lParam: definitionString yourAddress!

sciCallTipUseStyle: tabSizeInteger 
	"Private - Enable use of STYLE_CALLTIP and set call tip tab size in pixels."

	self 
		sendMessage: SCI_CALLTIPUSESTYLE
		wParam: tabSizeInteger
		lParam: 0!

sciCanPaste
	"Private - Will a paste succeed?"

	^(self 
		sendMessage: SCI_CANPASTE
		wParam: 0
		lParam: 0) asBoolean!

sciClearAllCmdKeys
	"Private - Drop all key mappings."

	self 
		sendMessage: SCI_CLEARALLCMDKEYS
		wParam: 0
		lParam: 0!

sciClearCmdKey: kmInteger 
	"Private - When key+modifier combination km is pressed do nothing."

	self 
		sendMessage: SCI_CLEARCMDKEY
		wParam: kmInteger
		lParam: 0!

sciColourise: startInteger end: endInteger 
	"Private - Colourise a segment of the document using the current lexing language."

	self 
		sendMessage: SCI_COLOURISE
		wParam: startInteger
		lParam: endInteger!

sciConvertEOLs: eolModeInteger 
	"Private - Convert all line endings in the document to one mode."

	self 
		sendMessage: SCI_CONVERTEOLS
		wParam: eolModeInteger
		lParam: 0!

sciCopyRange: startInteger end: endInteger 
	"Private - Copy a range of text to the clipboard. Positions are clipped into the document."

	self 
		sendMessage: SCI_COPYRANGE
		wParam: startInteger
		lParam: endInteger!

sciCreateDocument
	"Private - Create a new document object. Starts with reference count of 1 and not selected
	into editor."

	^self 
		sendMessage: SCI_CREATEDOCUMENT
		wParam: 0
		lParam: 0!

sciDocLineFromVisible: lineDisplayInteger 
	"Private - Find the document line of a display line taking hidden lines into account."

	^self 
		sendMessage: SCI_DOCLINEFROMVISIBLE
		wParam: lineDisplayInteger
		lParam: 0!

sciEncodedFromUTF8: utf8String encoded: encodedString 
	"Private - Translates a UTF8 string into the document encoding. Return the length of the
	result in bytes. On error return 0."

	^self 
		sendMessage: SCI_ENCODEDFROMUTF8
		wParam: utf8String yourAddress
		lParam: encodedString yourAddress!

sciEnsureVisible: lineInteger 
	"Private - Ensure a particular line is visible by expanding any header line hiding it."

	self 
		sendMessage: SCI_ENSUREVISIBLE
		wParam: lineInteger
		lParam: 0!

sciEnsureVisibleEnforcePolicy: lineInteger 
	"Private - Ensure a particular line is visible by expanding any header line hiding it. Use
	the currently set visibility policy to determine which range to display."

	self 
		sendMessage: SCI_ENSUREVISIBLEENFORCEPOLICY
		wParam: lineInteger
		lParam: 0!

sciFindColumn: lineInteger column: columnInteger 
	"Private - Find the position of a column on a line taking into account tabs and multi-byte
	characters. If beyond end of line, return line end position."

	^self 
		sendMessage: SCI_FINDCOLUMN
		wParam: lineInteger
		lParam: columnInteger!

sciFindText: flagsInteger ft: ftFINDTEXT 
	"Private - Find some text in the document."

	^self 
		sendMessage: SCI_FINDTEXT
		wParam: flagsInteger
		lParam: ftFINDTEXT yourAddress!

sciFormatRange: drawBoolean fr: frFORMATRANGE 
	"Private - On Windows, will draw the document into a display context such as a printer."

	^self 
		sendMessage: SCI_FORMATRANGE
		wParam: drawBoolean asParameter
		lParam: frFORMATRANGE yourAddress!

sciGetAnchor
	"Private - Returns the position of the opposite end of the selection to the caret."

	^self 
		sendMessage: SCI_GETANCHOR
		wParam: 0
		lParam: 0!

sciGetCaretLineBackAlpha
	"Private - Get the background alpha of the caret line."

	^self 
		sendMessage: SCI_GETCARETLINEBACKALPHA
		wParam: 0
		lParam: 0!

sciGetCaretStyle
	"Private - Returns the current style of the caret."

	^self 
		sendMessage: SCI_GETCARETSTYLE
		wParam: 0
		lParam: 0!

sciGetCharAt: posInteger 
	"Private - Returns the character byte at the position."

	^self 
		sendMessage: SCI_GETCHARAT
		wParam: posInteger
		lParam: 0!

sciGetCodePage
	"Private - Get the code page used to interpret the bytes of the document as characters."

	^self 
		sendMessage: SCI_GETCODEPAGE
		wParam: 0
		lParam: 0!

sciGetColumn: posInteger 
	"Private - Retrieve the column number of a position, taking tab width into account."

	^self 
		sendMessage: SCI_GETCOLUMN
		wParam: posInteger
		lParam: 0!

sciGetControlCharSymbol
	"Private - Get the way control characters are displayed."

	^self 
		sendMessage: SCI_GETCONTROLCHARSYMBOL
		wParam: 0
		lParam: 0!

sciGetCurLine: lengthInteger text: textString 
	"Private - Retrieve the text of the line containing the caret. Returns the index of the
	caret on the line."

	^self 
		sendMessage: SCI_GETCURLINE
		wParam: lengthInteger
		lParam: textString yourAddress!

sciGetCurrentPos
	"Private - Returns the position of the caret."

	^self 
		sendMessage: SCI_GETCURRENTPOS
		wParam: 0
		lParam: 0!

sciGetDocPointer
	"Private - Retrieve a pointer to the document object."

	^self 
		sendMessage: SCI_GETDOCPOINTER
		wParam: 0
		lParam: 0!

sciGetEdgeColumn
	"Private - Retrieve the column number which text should be kept within."

	^self 
		sendMessage: SCI_GETEDGECOLUMN
		wParam: 0
		lParam: 0!

sciGetEdgeMode
	"Private - Retrieve the edge highlight mode."

	^self 
		sendMessage: SCI_GETEDGEMODE
		wParam: 0
		lParam: 0!

sciGetEndAtLastLine
	"Private - Retrieve whether the maximum scroll position has the last line at the bottom of
	the view."

	^(self 
		sendMessage: SCI_GETENDATLASTLINE
		wParam: 0
		lParam: 0) asBoolean!

sciGetEndStyled
	"Private - Retrieve the position of the last correctly styled character."

	^self 
		sendMessage: SCI_GETENDSTYLED
		wParam: 0
		lParam: 0!

sciGetEOLMode
	"Private - Retrieve the current end of line mode - one of CRLF, CR, or LF."

	^self 
		sendMessage: SCI_GETEOLMODE
		wParam: 0
		lParam: 0!

sciGetFirstVisibleLine
	"Private - Retrieve the display line at the top of the display."

	^self 
		sendMessage: SCI_GETFIRSTVISIBLELINE
		wParam: 0
		lParam: 0!

sciGetFoldExpanded: lineInteger 
	"Private - Is a header line expanded?"

	^(self 
		sendMessage: SCI_GETFOLDEXPANDED
		wParam: lineInteger
		lParam: 0) asBoolean!

sciGetFoldLevel: lineInteger 
	"Private - Retrieve the fold level of a line."

	^self 
		sendMessage: SCI_GETFOLDLEVEL
		wParam: lineInteger
		lParam: 0!

sciGetFoldParent: lineInteger 
	"Private - Find the parent line of a child line."

	^self 
		sendMessage: SCI_GETFOLDPARENT
		wParam: lineInteger
		lParam: 0!

sciGetIndentationGuides
	"Private - Are the indentation guides visible?"

	^self 
		sendMessage: SCI_GETINDENTATIONGUIDES
		wParam: 0
		lParam: 0!

sciGetLastChild: lineInteger level: levelInteger 
	"Private - Find the last child line of a header line."

	^self 
		sendMessage: SCI_GETLASTCHILD
		wParam: lineInteger
		lParam: levelInteger!

sciGetLayoutCache
	"Private - Retrieve the degree of caching of layout information."

	^self 
		sendMessage: SCI_GETLAYOUTCACHE
		wParam: 0
		lParam: 0!

sciGetLexer
	"Private - Retrieve the lexing language of the document."

	^self 
		sendMessage: SCI_GETLEXER
		wParam: 0
		lParam: 0!

sciGetLine: lineInteger text: textString 
	"Private - Retrieve the contents of a line. Returns the length of the line."

	^self 
		sendMessage: SCI_GETLINE
		wParam: lineInteger
		lParam: textString yourAddress!

sciGetLineEndPosition: lineInteger 
	"Private - Get the position after the last visible characters on a line."

	^self 
		sendMessage: SCI_GETLINEENDPOSITION
		wParam: lineInteger
		lParam: 0!

sciGetLineIndentation: lineInteger 
	"Private - Retrieve the number of columns that a line is indented."

	^self 
		sendMessage: SCI_GETLINEINDENTATION
		wParam: lineInteger
		lParam: 0!

sciGetLineIndentPosition: lineInteger 
	"Private - Retrieve the position before the first non indentation character on a line."

	^self 
		sendMessage: SCI_GETLINEINDENTPOSITION
		wParam: lineInteger
		lParam: 0!

sciGetLineSelEndPosition: lineInteger 
	"Private - Retrieve the position of the end of the selection at the given line
	(INVALID_POSITION if no selection on this line)."

	^self 
		sendMessage: SCI_GETLINESELENDPOSITION
		wParam: lineInteger
		lParam: 0!

sciGetLineSelStartPosition: lineInteger 
	"Private - Retrieve the position of the start of the selection at the given line
	(INVALID_POSITION if no selection on this line)."

	^self 
		sendMessage: SCI_GETLINESELSTARTPOSITION
		wParam: lineInteger
		lParam: 0!

sciGetLineState: lineInteger 
	"Private - Retrieve the extra styling information for a line."

	^self 
		sendMessage: SCI_GETLINESTATE
		wParam: lineInteger
		lParam: 0!

sciGetLineVisible: lineInteger 
	"Private - Is a line visible?"

	^(self 
		sendMessage: SCI_GETLINEVISIBLE
		wParam: lineInteger
		lParam: 0) asBoolean!

sciGetMarginLeft
	"Private - Returns the size in pixels of the left margin."

	^self 
		sendMessage: SCI_GETMARGINLEFT
		wParam: 0
		lParam: 0!

sciGetMarginRight
	"Private - Returns the size in pixels of the right margin."

	^self 
		sendMessage: SCI_GETMARGINRIGHT
		wParam: 0
		lParam: 0!

sciGetMaxLineState
	"Private - Retrieve the last line number that has line state."

	^self 
		sendMessage: SCI_GETMAXLINESTATE
		wParam: 0
		lParam: 0!

sciGetPasteConvertEndings
	"Private - Get convert-on-paste setting"

	^(self 
		sendMessage: SCI_GETPASTECONVERTENDINGS
		wParam: 0
		lParam: 0) asBoolean!

sciGetPrintWrapMode
	"Private - Is printing line wrapped?"

	^self 
		sendMessage: SCI_GETPRINTWRAPMODE
		wParam: 0
		lParam: 0!

sciGetProperty: keyString buf: bufString 
	"Private - Retrieve a 'property' value previously set with SetProperty."

	^self 
		sendMessage: SCI_GETPROPERTY
		wParam: keyString yourAddress
		lParam: bufString yourAddress!

sciGetPropertyExpanded: keyString buf: bufString 
	"Private - Retrieve a 'property' value previously set with SetProperty, with '$()' variable
	replacement on returned buffer."

	^self 
		sendMessage: SCI_GETPROPERTYEXPANDED
		wParam: keyString yourAddress
		lParam: bufString yourAddress!

sciGetPropertyInt: keyString 
	"Private - Retrieve a 'property' value previously set with SetProperty, interpreted as an
	int AFTER any '$()' variable replacement."

	^self 
		sendMessage: SCI_GETPROPERTYINT
		wParam: keyString yourAddress
		lParam: 0!

sciGetSearchFlags
	"Private - Get the search flags used by SearchInTarget."

	^self 
		sendMessage: SCI_GETSEARCHFLAGS
		wParam: 0
		lParam: 0!

sciGetSelectionEnd
	"Private - Returns the position at the end of the selection."

	^self 
		sendMessage: SCI_GETSELECTIONEND
		wParam: 0
		lParam: 0!

sciGetSelectionMode
	"Private - Get the mode of the current selection."

	^self 
		sendMessage: SCI_GETSELECTIONMODE
		wParam: 0
		lParam: 0!

sciGetSelectionStart
	"Private - Returns the position at the start of the selection."

	^self 
		sendMessage: SCI_GETSELECTIONSTART
		wParam: 0
		lParam: 0!

sciGetSelText: textString 
	"Private - Retrieve the selected text. Return the length of the text."

	^self 
		sendMessage: SCI_GETSELTEXT
		wParam: 0
		lpParam: textString!

sciGetStyleBits
	^self 
		sendMessage: SCI_GETSTYLEBITS
		wParam: 0
		lParam: 0!

sciGetStyleBitsNeeded
	"Private - Retrieve the number of bits the current lexer needs for styling."

	^self 
		sendMessage: SCI_GETSTYLEBITSNEEDED
		wParam: 0
		lParam: 0!

sciGetStyledText: trTEXTRANGE 
	"Private - Retrieve a buffer of cells. Returns the number of bytes in the buffer not
	including terminating NULs."

	^self 
		sendMessage: SCI_GETSTYLEDTEXT
		wParam: 0
		lpParam: trTEXTRANGE!

sciGetTargetEnd
	"Private - Get the position that ends the target."

	^self 
		sendMessage: SCI_GETTARGETEND
		wParam: 0
		lParam: 0!

sciGetTargetStart
	"Private - Get the position that starts the target."

	^self 
		sendMessage: SCI_GETTARGETSTART
		wParam: 0
		lParam: 0!

sciGetText: lengthInteger text: textString 
	"Private - Retrieve all the text in the document. Returns number of characters retrieved."

	^self 
		sendMessage: SCI_GETTEXT
		wParam: lengthInteger
		lParam: textString yourAddress!

sciGetTextRange: trTEXTRANGE 
	"Private - Retrieve a range of text. Return the length of the text."

	^self 
		sendMessage: SCI_GETTEXTRANGE
		wParam: 0
		lpParam: trTEXTRANGE!

sciGetWrapMode
	"Private - Retrieve whether text is word wrapped."

	^self 
		sendMessage: SCI_GETWRAPMODE
		wParam: 0
		lParam: 0!

sciGetWrapVisualFlags
	"Private - Retrive the display mode of visual flags for wrapped lines."

	^self 
		sendMessage: SCI_GETWRAPVISUALFLAGS
		wParam: 0
		lParam: 0!

sciGetWrapVisualFlagsLocation
	"Private - Retrive the location of visual flags for wrapped lines."

	^self 
		sendMessage: SCI_GETWRAPVISUALFLAGSLOCATION
		wParam: 0
		lParam: 0!

sciGotoLine: lineInteger 
	"Private - Set caret to start of a line and ensure it is visible."

	self 
		sendMessage: SCI_GOTOLINE
		wParam: lineInteger
		lParam: 0!

sciGotoPos: posInteger 
	"Private - Set caret to a position and ensure it is visible."

	self 
		sendMessage: SCI_GOTOPOS
		wParam: posInteger
		lParam: 0!

sciHideLines: lineStartInteger lineEnd: lineEndInteger 
	"Private - Make a range of lines invisible."

	self 
		sendMessage: SCI_HIDELINES
		wParam: lineStartInteger
		lParam: lineEndInteger!

sciIndicatorAllOnFor: positionInteger 
	"Private - Are any indicators present at position?"

	^self 
		sendMessage: SCI_INDICATORALLONFOR
		wParam: positionInteger
		lParam: 0!

sciIndicatorClearRange: positionInteger clearLength: clearLengthInteger 
	"Private - Turn a indicator off over a range."

	self 
		sendMessage: SCI_INDICATORCLEARRANGE
		wParam: positionInteger
		lParam: clearLengthInteger!

sciIndicatorEnd: indicatorInteger position: positionInteger 
	"Private - Where does a particular indicator end?"

	^self 
		sendMessage: SCI_INDICATOREND
		wParam: indicatorInteger
		lParam: positionInteger!

sciIndicatorFillRange: positionInteger fillLength: fillLengthInteger 
	"Private - Turn a indicator on over a range."

	self 
		sendMessage: SCI_INDICATORFILLRANGE
		wParam: positionInteger
		lParam: fillLengthInteger!

sciIndicatorStart: indicatorInteger position: positionInteger 
	"Private - Where does a particular indicator start?"

	^self 
		sendMessage: SCI_INDICATORSTART
		wParam: indicatorInteger
		lParam: positionInteger!

sciIndicatorValueAt: indicatorInteger position: positionInteger 
	"Private - What value does a particular indicator have at at a position?"

	^self 
		sendMessage: SCI_INDICATORVALUEAT
		wParam: indicatorInteger
		lParam: positionInteger!

sciInsertText: posInteger text: textString 
	"Private - Insert string at a position."

	self 
		sendMessage: SCI_INSERTTEXT
		wParam: posInteger
		lParam: textString yourAddress!

sciLineScroll: columnsInteger lines: linesInteger 
	"Private - Scroll horizontally and vertically."

	self 
		sendMessage: SCI_LINESCROLL
		wParam: columnsInteger
		lParam: linesInteger!

sciLoadLexerLibrary: pathString 
	"Private - Load a lexer library (dll / so)."

	self 
		sendMessage: SCI_LOADLEXERLIBRARY
		wParam: 0
		lpParam: pathString!

sciMarkerAddSet: lineInteger set: setInteger 
	"Private - Add a set of markers to a line."

	self 
		sendMessage: SCI_MARKERADDSET
		wParam: lineInteger
		lParam: setInteger!

sciMarkerDefinePixmap: markerNumberInteger pixmap: pixmapString 
	"Private - Define a marker from a pixmap."

	self 
		sendMessage: SCI_MARKERDEFINEPIXMAP
		wParam: markerNumberInteger
		lParam: pixmapString yourAddress!

sciMarkerDeleteHandle: handleInteger 
	"Private - Delete a marker."

	self 
		sendMessage: SCI_MARKERDELETEHANDLE
		wParam: handleInteger
		lParam: 0!

sciMarkerGet: lineInteger 
	"Private - Get a bit mask of all the markers set on a line."

	^self 
		sendMessage: SCI_MARKERGET
		wParam: lineInteger
		lParam: 0!

sciMarkerLineFromHandle: handleInteger 
	"Private - Retrieve the line number at which a particular marker is located."

	^self 
		sendMessage: SCI_MARKERLINEFROMHANDLE
		wParam: handleInteger
		lParam: 0!

sciMarkerNext: lineStartInteger markerMask: markerMaskInteger 
	"Private - Find the next line after lineStart that includes a marker in mask."

	^self 
		sendMessage: SCI_MARKERNEXT
		wParam: lineStartInteger
		lParam: markerMaskInteger!

sciMarkerPrevious: lineStartInteger markerMask: markerMaskInteger 
	"Private - Find the previous line before lineStart that includes a marker in mask."

	^self 
		sendMessage: SCI_MARKERPREVIOUS
		wParam: lineStartInteger
		lParam: markerMaskInteger!

sciMarkerSetAlpha: markerNumberInteger alpha: alphaInteger 
	"Private - Set the alpha used for a marker that is drawn in the text area, not the margin."

	self 
		sendMessage: SCI_MARKERSETALPHA
		wParam: markerNumberInteger
		lParam: alphaInteger!

sciPointXFromPosition: posInteger 
	"Private - Retrieve the x value of the point in the window where a position is displayed."

	^self 
		sendMessage: SCI_POINTXFROMPOSITION
		wParam: 0
		lParam: posInteger!

sciPointYFromPosition: posInteger 
	"Private - Retrieve the y value of the point in the window where a position is displayed."

	^self 
		sendMessage: SCI_POINTYFROMPOSITION
		wParam: 0
		lParam: posInteger!

sciPositionAfter: posInteger 
	"Private - Given a valid document position, return the next position taking code page into
	account. Maximum value returned is the last position in the document."

	^self 
		sendMessage: SCI_POSITIONAFTER
		wParam: posInteger
		lParam: 0!

sciPositionBefore: posInteger 
	"Private - Given a valid document position, return the previous position taking code page
	into account. Returns 0 if passed 0."

	^self 
		sendMessage: SCI_POSITIONBEFORE
		wParam: posInteger
		lParam: 0!

sciPositionFromPoint: xInteger y: yInteger 
	"Private - Find the position from a point within the window."

	^self 
		sendMessage: SCI_POSITIONFROMPOINT
		wParam: xInteger
		lParam: yInteger!

sciPositionFromPointClose: xInteger y: yInteger 
	"Private - Find the position from a point within the window but return INVALID_POSITION if
	not close to text."

	^self 
		sendMessage: SCI_POSITIONFROMPOINTCLOSE
		wParam: xInteger
		lParam: yInteger!

sciRegisterImage: typeInteger xpmData: xpmDataString 
	"Private - Register an XPM image for use in autocompletion lists."

	self 
		sendMessage: SCI_REGISTERIMAGE
		wParam: typeInteger
		lParam: xpmDataString yourAddress!

sciReleaseDocument: docInteger 
	"Private - Release a reference to the document, deleting document if it fades to black."

	self 
		sendMessage: SCI_RELEASEDOCUMENT
		wParam: 0
		lParam: docInteger!

sciReplaceSel: textString 
	"Private - Replace the selected text with the argument text."

	self 
		sendMessage: SCI_REPLACESEL
		wParam: 0
		lpParam: textString!

sciReplaceTarget: lengthInteger text: textString 
	"Private - Replace the target text with the argument text. Text is counted so it can contain
	NULs. Returns the length of the replacement text."

	^self 
		sendMessage: SCI_REPLACETARGET
		wParam: lengthInteger
		lParam: textString yourAddress!

sciReplaceTargetRE: lengthInteger text: textString 
	"Private - Replace the target text with the argument text after \d processing. Text is
	counted so it can contain NULs. Looks for \d where d is between 1 and 9 and replaces these
	with the strings matched in the last search operation which were surrounded by \( and \).
	Returns the length of the replacement text including any change caused by processing the \d
	patterns."

	^self 
		sendMessage: SCI_REPLACETARGETRE
		wParam: lengthInteger
		lParam: textString yourAddress!

sciSearchAnchor
	"Private - Sets the current caret position to be the search anchor."

	self 
		sendMessage: SCI_SEARCHANCHOR
		wParam: 0
		lParam: 0!

sciSearchInTarget: lengthInteger text: textString 
	"Private - Search for a counted string in the target and set the target to the found range.
	Text is counted so it can contain NULs. Returns length of range or -1 for failure in which
	case target is not moved."

	^self 
		sendMessage: SCI_SEARCHINTARGET
		wParam: lengthInteger
		lParam: textString yourAddress!

sciSearchNext: flagsInteger text: textString 
	"Private - Find some text starting at the search anchor. Does not ensure the selection is
	visible."

	^self 
		sendMessage: SCI_SEARCHNEXT
		wParam: flagsInteger
		lParam: textString yourAddress!

sciSearchPrev: flagsInteger text: textString 
	"Private - Find some text starting at the search anchor and moving backwards. Does not
	ensure the selection is visible."

	^self 
		sendMessage: SCI_SEARCHPREV
		wParam: flagsInteger
		lParam: textString yourAddress!

sciSetAnchor: posAnchorInteger 
	"Private - Set the selection anchor to a position. The anchor is the opposite end of the
	selection from the caret."

	self 
		sendMessage: SCI_SETANCHOR
		wParam: posAnchorInteger
		lParam: 0!

sciSetCaretFore: foreRGB 
	"Private - Set the foreground colour of the caret."

	self 
		sendMessage: SCI_SETCARETFORE
		wParam: foreRGB asParameter
		lParam: 0!

sciSetCaretLineBack: backRGB 
	"Private - Set the colour of the background of the line containing the caret."

	self 
		sendMessage: SCI_SETCARETLINEBACK
		wParam: backRGB asParameter
		lParam: 0!

sciSetCaretLineBackAlpha: alphaInteger 
	"Private - Set background alpha of the caret line."

	self 
		sendMessage: SCI_SETCARETLINEBACKALPHA
		wParam: alphaInteger
		lParam: 0!

sciSetCaretPolicy: caretPolicyInteger caretSlop: caretSlopInteger 
	"Private - CARET_POLICY changed in 1.47"

	#deprecated.
	self 
		sendMessage: SCI_SETCARETPOLICY
		wParam: caretPolicyInteger
		lParam: caretSlopInteger!

sciSetCaretStyle: caretStyleInteger 
	"Private - Set the style of the caret to be drawn."

	self 
		sendMessage: SCI_SETCARETSTYLE
		wParam: caretStyleInteger
		lParam: 0!

sciSetCaretWidth: pixelWidthInteger 
	"Private - Set the width of the insert mode caret."

	self 
		sendMessage: SCI_SETCARETWIDTH
		wParam: pixelWidthInteger
		lParam: 0!

sciSetCharsDefault
	"Private - Reset the set of characters for whitespace and word characters to the defaults."

	self 
		sendMessage: SCI_SETCHARSDEFAULT
		wParam: 0
		lParam: 0!

sciSetCodePage: codePageInteger 
	"Private - Set the code page used to interpret the bytes of the document as characters. The
	SC_CP_UTF8 value can be used to enter Unicode mode."

	self 
		sendMessage: SCI_SETCODEPAGE
		wParam: codePageInteger
		lParam: 0!

sciSetControlCharSymbol: symbolInteger 
	"Private - Change the way control characters are displayed: If symbol is < 32, keep the
	drawn way, else, use the given character."

	self 
		sendMessage: SCI_SETCONTROLCHARSYMBOL
		wParam: symbolInteger
		lParam: 0!

sciSetDocPointer: pointerInteger 
	"Private - Change the document object used."

	self 
		sendMessage: SCI_SETDOCPOINTER
		wParam: 0
		lParam: pointerInteger!

sciSetEdgeColumn: columnInteger 
	"Private - Set the column number of the edge. If text goes past the edge then it is
	highlighted."

	self 
		sendMessage: SCI_SETEDGECOLUMN
		wParam: columnInteger
		lParam: 0!

sciSetEdgeMode: modeInteger 
	"Private - The edge may be displayed by a line (EDGE_LINE) or by highlighting text that goes
	beyond it (EDGE_BACKGROUND) or not displayed at all (EDGE_NONE)."

	self 
		sendMessage: SCI_SETEDGEMODE
		wParam: modeInteger
		lParam: 0!

sciSetEOLMode: eolModeInteger 
	"Private - Set the current end of line mode."

	self 
		sendMessage: SCI_SETEOLMODE
		wParam: eolModeInteger
		lParam: 0!

sciSetFoldExpanded: lineInteger expanded: expandedBoolean 
	"Private - Show the children of a header line."

	self 
		sendMessage: SCI_SETFOLDEXPANDED
		wParam: lineInteger
		lParam: expandedBoolean asParameter!

sciSetFoldFlags: flagsInteger 
	"Private - Set some style options for folding."

	self 
		sendMessage: SCI_SETFOLDFLAGS
		wParam: flagsInteger
		lParam: 0!

sciSetFoldLevel: lineInteger level: levelInteger 
	"Private - Set the fold level of a line. This encodes an integer level along with flags
	indicating whether the line is a header and whether it is effectively white space."

	self 
		sendMessage: SCI_SETFOLDLEVEL
		wParam: lineInteger
		lParam: levelInteger!

sciSetFoldMarginColour: useSettingBoolean back: backRGB 
	"Private - Set the colours used as a chequerboard pattern in the fold margin"

	self 
		sendMessage: SCI_SETFOLDMARGINCOLOUR
		wParam: useSettingBoolean asParameter
		lParam: backRGB asParameter!

sciSetFoldMarginHiColour: useSettingBoolean fore: foreRGB 
	self 
		sendMessage: SCI_SETFOLDMARGINHICOLOUR
		wParam: useSettingBoolean asParameter
		lParam: foreRGB asParameter!

sciSetHotspotActiveBack: useSettingBoolean back: backRGB 
	"Private - Set a back colour for active hotspots."

	self 
		sendMessage: SCI_SETHOTSPOTACTIVEBACK
		wParam: useSettingBoolean asParameter
		lParam: backRGB asParameter!

sciSetHotspotActiveFore: useSettingBoolean fore: foreRGB 
	"Private - Set a fore colour for active hotspots."

	self 
		sendMessage: SCI_SETHOTSPOTACTIVEFORE
		wParam: useSettingBoolean asParameter
		lParam: foreRGB asParameter!

sciSetHScrollBar: showBoolean 
	"Private - Show or hide the horizontal scroll bar."

	self 
		sendMessage: SCI_SETHSCROLLBAR
		wParam: showBoolean asParameter
		lParam: 0!

sciSetIndentationGuides: indentViewInteger 
	"Private - Show or hide indentation guides."

	self 
		sendMessage: SCI_SETINDENTATIONGUIDES
		wParam: indentViewInteger
		lParam: 0!

sciSetKeyWords: keywordSetInteger keyWords: keyWordsString 
	"Private - Set up the key words used by the lexer."

	self 
		sendMessage: SCI_SETKEYWORDS
		wParam: keywordSetInteger
		lParam: keyWordsString yourAddress!

sciSetLayoutCache: modeInteger 
	"Private - Sets the degree of caching of layout information."

	self 
		sendMessage: SCI_SETLAYOUTCACHE
		wParam: modeInteger
		lParam: 0!

sciSetLengthForEncode: bytesInteger 
	"Private - Set the length of the utf8 argument for calling EncodedFromUTF8. Set to -1 and
	the string will be measured to the first nul."

	self 
		sendMessage: SCI_SETLENGTHFORENCODE
		wParam: bytesInteger
		lParam: 0!

sciSetLexer: lexerInteger 
	"Private - Set the lexing language of the document."

	self 
		sendMessage: SCI_SETLEXER
		wParam: lexerInteger
		lParam: 0!

sciSetLexerLanguage: languageString 
	"Private - Set the lexing language of the document based on string name."

	self 
		sendMessage: SCI_SETLEXERLANGUAGE
		wParam: 0
		lpParam: languageString!

sciSetLineIndentation: lineInteger indentSize: indentSizeInteger 
	"Private - Change the indentation of a line to a number of columns."

	self 
		sendMessage: SCI_SETLINEINDENTATION
		wParam: lineInteger
		lParam: indentSizeInteger!

sciSetLineState: lineInteger state: stateInteger 
	"Private - Used to hold extra styling information for each line."

	self 
		sendMessage: SCI_SETLINESTATE
		wParam: lineInteger
		lParam: stateInteger!

sciSetMarginLeft: pixelWidthInteger 
	"Private - Sets the size in pixels of the left margin."

	self 
		sendMessage: SCI_SETMARGINLEFT
		wParam: 0
		lParam: pixelWidthInteger!

sciSetMarginRight: pixelWidthInteger 
	"Private - Sets the size in pixels of the right margin."

	self 
		sendMessage: SCI_SETMARGINRIGHT
		wParam: 0
		lParam: pixelWidthInteger!

sciSetModEventMask: maskInteger 
	"Private - Set which document modification events are sent to the container."

	self 
		sendMessage: SCI_SETMODEVENTMASK
		wParam: maskInteger
		lParam: 0!

sciSetPasteConvertEndings: convertBoolean 
	"Private - Enable/Disable convert-on-paste for line endings"

	self 
		sendMessage: SCI_SETPASTECONVERTENDINGS
		wParam: convertBoolean asParameter
		lParam: 0!

sciSetPrintWrapMode: modeInteger 
	"Private - Set printing to line wrapped (SC_WRAP_WORD) or not line wrapped (SC_WRAP_NONE)."

	self 
		sendMessage: SCI_SETPRINTWRAPMODE
		wParam: modeInteger
		lParam: 0!

sciSetProperty: keyString value: valueString 
	"Private - Set up a value that may be used by a lexer for some optional feature."

	self 
		sendMessage: SCI_SETPROPERTY
		wParam: keyString yourAddress
		lParam: valueString yourAddress!

sciSetSavePoint
	"Private - Remember the current position in the undo history as the position at which the
	document was saved."

	self 
		sendMessage: SCI_SETSAVEPOINT
		wParam: 0
		lParam: 0!

sciSetSearchFlags: flagsInteger 
	"Private - Set the search flags used by SearchInTarget."

	self 
		sendMessage: SCI_SETSEARCHFLAGS
		wParam: flagsInteger
		lParam: 0!

sciSetSelBack: useSettingBoolean back: backRGB 
	"Private - Set the background colour of the selection and whether to use this setting."

	self 
		sendMessage: SCI_SETSELBACK
		wParam: useSettingBoolean asParameter
		lParam: backRGB asParameter!

sciSetSelectionMode: modeInteger 
	"Private - Set the selection mode to stream (SC_SEL_STREAM) or rectangular
	(SC_SEL_RECTANGLE) or by lines (SC_SEL_LINES)."

	self 
		sendMessage: SCI_SETSELECTIONMODE
		wParam: modeInteger
		lParam: 0!

sciSetSelFore: useSettingBoolean fore: foreRGB 
	"Private - Set the foreground colour of the selection and whether to use this setting."

	self 
		sendMessage: SCI_SETSELFORE
		wParam: useSettingBoolean asParameter
		lParam: foreRGB asParameter!

sciSetStyleBits: bitsInteger 
	"Private - Divide each styling byte into lexical class bits (default: 5) and indicator bits
	(default: 3). If a lexer requires more than 32 lexical states, then this is used to expand
	the possible states."

	self 
		sendMessage: SCI_SETSTYLEBITS
		wParam: bitsInteger
		lParam: 0!

sciSetStylingEx: lengthInteger styles: stylesString 
	"Private - Set the styles for a segment of the document."

	self 
		sendMessage: SCI_SETSTYLINGEX
		wParam: lengthInteger
		lParam: stylesString yourAddress!

sciSetTargetEnd: posInteger 
	"Private - Sets the position that ends the target which is used for updating the document
	without affecting the scroll position."

	self 
		sendMessage: SCI_SETTARGETEND
		wParam: posInteger
		lParam: 0!

sciSetTargetStart: posInteger 
	"Private - Sets the position that starts the target which is used for updating the document
	without affecting the scroll position."

	self 
		sendMessage: SCI_SETTARGETSTART
		wParam: posInteger
		lParam: 0!

sciSetText: textString 
	"Private - Replace the contents of the document with the argument text."

	self 
		sendMessage: SCI_SETTEXT
		wParam: 0
		lpParam: textString!

sciSetVisiblePolicy: visiblePolicyInteger visibleSlop: visibleSlopInteger 
	"Private - Set the way the display area is determined when a particular line is to be moved
	to by Find, FindNext, GotoLine, etc."

	self 
		sendMessage: SCI_SETVISIBLEPOLICY
		wParam: visiblePolicyInteger
		lParam: visibleSlopInteger!

sciSetWhitespaceBack: useSettingBoolean back: backRGB 
	"Private - Set the background colour of all whitespace and whether to use this setting."

	self 
		sendMessage: SCI_SETWHITESPACEBACK
		wParam: useSettingBoolean asParameter
		lParam: backRGB asParameter!

sciSetWhitespaceChars: charactersString 
	"Private - Set the set of characters making up whitespace for when moving or selecting by
	word. Should be called after SetWordChars."

	self 
		sendMessage: SCI_SETWHITESPACECHARS
		wParam: 0
		lpParam: charactersString!

sciSetWhitespaceFore: useSettingBoolean fore: foreRGB 
	"Private - Set the foreground colour of all whitespace and whether to use this setting."

	self 
		sendMessage: SCI_SETWHITESPACEFORE
		wParam: useSettingBoolean asParameter
		lParam: foreRGB asParameter!

sciSetWordChars: charactersString 
	"Private - Set the set of characters making up words for when moving or selecting by word.
	First sets deaults like SetCharsDefault."

	self 
		sendMessage: SCI_SETWORDCHARS
		wParam: 0
		lpParam: charactersString!

sciSetWrapMode: modeInteger 
	"Private - Sets whether text is word wrapped."

	self 
		sendMessage: SCI_SETWRAPMODE
		wParam: modeInteger
		lParam: 0!

sciSetWrapVisualFlags: wrapVisualFlagsInteger 
	"Private - Set the display mode of visual flags for wrapped lines."

	self 
		sendMessage: SCI_SETWRAPVISUALFLAGS
		wParam: wrapVisualFlagsInteger
		lParam: 0!

sciSetWrapVisualFlagsLocation: wrapVisualFlagsLocationInteger 
	"Private - Set the location of visual flags for wrapped lines."

	self 
		sendMessage: SCI_SETWRAPVISUALFLAGSLOCATION
		wParam: wrapVisualFlagsLocationInteger
		lParam: 0!

sciSetXCaretPolicy: caretPolicyInteger caretSlop: caretSlopInteger 
	"Private - Set the way the caret is kept visible when going sideway. The exclusion zone is
	given in pixels."

	self 
		sendMessage: SCI_SETXCARETPOLICY
		wParam: caretPolicyInteger
		lParam: caretSlopInteger!

sciSetYCaretPolicy: caretPolicyInteger caretSlop: caretSlopInteger 
	"Private - Set the way the line the caret is on is kept visible. The exclusion zone is given
	in lines."

	self 
		sendMessage: SCI_SETYCARETPOLICY
		wParam: caretPolicyInteger
		lParam: caretSlopInteger!

sciShowLines: lineStartInteger lineEnd: lineEndInteger 
	"Private - Make a range of lines visible."

	self 
		sendMessage: SCI_SHOWLINES
		wParam: lineStartInteger
		lParam: lineEndInteger!

sciStartStyling: posInteger mask: maskInteger 
	"Private - Set the current styling position to pos and the styling mask to mask. The styling
	mask can be used to protect some bits in each styling byte from modification."

	self 
		sendMessage: SCI_STARTSTYLING
		wParam: posInteger
		lParam: maskInteger!

sciStyleClearAll
	"Private - Clear all the styles and make equivalent to the global default style."

	self 
		sendMessage: SCI_STYLECLEARALL
		wParam: 0
		lParam: 0!

sciTargetAsUTF8: sString 
	"Private - Returns the target converted to UTF8. Return the length in bytes."

	^self 
		sendMessage: SCI_TARGETASUTF8
		wParam: 0
		lpParam: sString!

sciTextHeight: lineInteger 
	"Private - Retrieve the height of a particular line of text in pixels."

	^self 
		sendMessage: SCI_TEXTHEIGHT
		wParam: lineInteger
		lParam: 0!

sciTextWidth: styleInteger text: textString 
	"Private - Measure the pixel width of some text in a particular style. NUL terminated text
	argument. Does not handle tab or control characters."

	^self 
		sendMessage: SCI_TEXTWIDTH
		wParam: styleInteger
		lParam: textString yourAddress!

sciToggleCaretSticky
	"Private - Switch between sticky and non-sticky: meant to be bound to a key."

	self 
		sendMessage: SCI_TOGGLECARETSTICKY
		wParam: 0
		lParam: 0!

sciToggleFold: lineInteger 
	"Private - Switch a header line between expanded and contracted."

	self 
		sendMessage: SCI_TOGGLEFOLD
		wParam: lineInteger
		lParam: 0!

sciUserListShow: listTypeInteger itemList: itemListString 
	"Private - Display a list of strings and send notification when user chooses one."

	self 
		sendMessage: SCI_USERLISTSHOW
		wParam: listTypeInteger
		lParam: itemListString yourAddress!

sciVisibleFromDocLine: lineInteger 
	"Private - Find the display line of a document line taking hidden lines into account."

	^self 
		sendMessage: SCI_VISIBLEFROMDOCLINE
		wParam: lineInteger
		lParam: 0!

sciWordEndPosition: posInteger onlyWordCharacters: onlyWordCharactersBoolean 
	"Private - Get position of end of word."

	^self 
		sendMessage: SCI_WORDENDPOSITION
		wParam: posInteger
		lParam: onlyWordCharactersBoolean asParameter!

sciWordStartPosition: posInteger onlyWordCharacters: onlyWordCharactersBoolean 
	"Private - Get position of start of word."

	^self 
		sendMessage: SCI_WORDSTARTPOSITION
		wParam: posInteger
		lParam: onlyWordCharactersBoolean asParameter!

sciWrapCount: lineInteger 
	"Private - The number of display lines needed to wrap a document line"

	^self 
		sendMessage: SCI_WRAPCOUNT
		wParam: lineInteger
		lParam: 0!

scnAutoCSelection: pSCNotification 
	| accept struct |
	accept := true asValue.
	struct := SCNotification fromAddress: pSCNotification.
	self presenter trigger: #autoComplete:startingAt:accept:
		withArguments: (Array 
				with: struct text
				with: struct lParam + 1
				with: accept).
	accept value ifFalse: [self cancelAutoCompletion].
	^0!

scnCallTipClick: pSCNotification 
	"Private - Default handler for an SCN_CALLTIPCLICK event.
	The following fields of the notification structure are set:
		position

	From the Scintilla Docs:
		'... generated when the user clicks on a calltip. ...	can be used to display the next 
		function prototype when a function name is overloaded with different arguments.
		The position field is set to 1 if the click is in an up arrow, 2 if in a down arrow, 
		and 0 if elsewhere.'
	"

	self presenter trigger: #callTipClicked:
		with: (#(#upArrow #downArrow) at: (SCNotification fromAddress: pSCNotification) position
				ifAbsent: [#other]).
	^0!

scnCharAdded: pSCNotification 
	"Private - Default handler for an SCN_CHARADDED event.
	The following fields of the notification structure are set:
		ch 	(set to the code point of the new character).

	From the Scintilla docs:
		'Fired when the user types an ordinary text character (as opposed to a command character) 
		which is entered into the text. Can be used by the container to decide to display a call tip or 
		auto completion list.'
	"

	self presenter trigger: #charAdded: with: (SCNotification fromAddress: pSCNotification) character.
	^0!

scnDoubleClick: pSCNotification 
	"Private - Default handler for an SCN_DOUBLECLICK event.
	No fields of the notification structure are set."

	"Implementation Note: This notification is ignored since it is redundant."

	^0!

scnDwellEnd: pSCNotification 
	"Private - Respond to an SCN_DWELLEND notification. The 'position' , 'x' and 'y' fields of
	the notification structure are set.
	From the Scintilla docs:
		' ...generated after a SCN_DWELLSTART and the mouse is moved or other activity 
		such as key press indicates the dwell is over.'
	"

	self presenter trigger: #hoverEnd: with: (SCNotification fromAddress: pSCNotification) point.
	^0!

scnDwellStart: pSCNotification 
	"Private - Respond to an SCN_DWELLSTART notification. The 'position' , 'x' and 'y' fields of
	the notification structure are set.
	From the Scintilla docs:
		'...generated when the user hold the mouse still in one spot for the dwell period.'
	"

	self presenter trigger: #hoverStart: with: (SCNotification fromAddress: pSCNotification) point.
	^0!

scnHotSpotClick: pSCNotification 
	"Private - Default handler for an SCN_HOTSPOTCLICK event.
	The following fields of the notification structure are set:
		modifiers
		position

	From the Scintilla Docs:
		'... generated when the user clicks .. on text that is in a style with the hotspot attribute set. ...
		The position field is set the text position of the click ... and the modifiers field set to the key 
		modifiers held down in a similar manner to SCN_KEY.'
	"

	#todo.	"Send around a MouseEvent here"
	self presenter trigger: #hotSpotClicked: with: (SCNotification fromAddress: pSCNotification).
	^0!

scnHotSpotDoubleClick: pSCNotification 
	"Private - Default handler for an SCN_HOTSPOTDOUBLECLICK event.
	The following fields of the notification structure are set:
		modifiers
		position

	See scnHotSpotClick:
	"

	#todo.	"Send around a MouseEvent here"
	self presenter trigger: #hotSpotDoubleClicked: with: (SCNotification fromAddress: pSCNotification).
	^0!

scnIndicatorClick: pSCNotification 
	"Private - Default handler for an SCN_INDICATORCLICK event.
	The following fields of the notification structure are set:
		modifiers
		position"

	self presenter trigger: #indicatorClicked: with: (SCNotification fromAddress: pSCNotification).
	^0!

scnIndicatorRelease: pSCNotification 
	"Private - Default handler for an SCN_INDICATORRELEASE event.
	The following fields of the notification structure are set:
		modifiers
		position"

	self presenter trigger: #indicatorReleased: with: (SCNotification fromAddress: pSCNotification).
	^0!

scnKey: pSCNotification 
	"Private - Default handler for an SCN_KEY event.
	The following fields of the notification structure are set:
		ch
		modifiers"

	"Implementation Note: Not sent by Scintilla on Windows."

	^0!

scnMacroRecord: pSCNotification 
	"Private - Respond to an SCN_MACRORECORD notification. The 'message', 'wParam' and
	'lParam' fields of the notification structure are set.

	From the Scintilla docs:
		'Tells the container that an operation is being performed so that the container may 
		choose to record the fact if it is in a macro recording mode.'
	"

	self presenter trigger: #macroRecord: with: (SCNotification fromAddress: pSCNotification).
	^0!

scnMarginClick: pSCNotification 
	"Private - Respond to an SCN_MARGINCLICK notification. The 'modifiers', 'position' and
	'margin' fields of the notification structure are set.

	From the Scintilla docs:
		'Tells the container that the mouse was clicked inside a margin marked sensitive. 
		Can be used to perform folding or to place breakpoints.'
	"

	| struct |
	struct := SCNotification fromAddress: pSCNotification.
	self isFoldingEnabled 
		ifTrue: 
			[| margin |
			margin := self margins at: struct margin + 1.
			margin isFolders 
				ifTrue: 
					[| line |
					line := self lineFromPosition: struct position.
					self toggleFold: line]].
	self presenter trigger: #marginClicked: with: struct.
	^0!

scnModified: pSCNotification 
	"Private - Respond to SCN_MODIFIED notification.The 'position', 'modificationType', 'text',
	'length', 'linesAdded', 'line', 'foleLevelNow' and 'foldLevelPrev' fields of the
	notification structure are set.

	See the Scintilla documentation for more information about this important and heavily
	overloaded notification."

	| struct eventMask |
	struct := SCNotification fromAddress: pSCNotification.
	eventMask := struct modificationType.
	(eventMask anyMask: ##(SC_MOD_INSERTTEXT | SC_MOD_DELETETEXT)) 
		ifTrue: 
			["Text changed"
			super enChange]
		ifFalse: 
			[(eventMask anyMask: ##(SC_MOD_CHANGESTYLE | SC_MOD_CHANGEFOLD)) 
				ifTrue: [self invalidateCalculatedExtent]].
	^0!

scnModifyAttemptRO: pSCNotification
	"Private - Respond to an SCN_MODIFYATTEMPTRO notification. 	No fields of the notification 
	structure are set.

	From the Scintilla docs:
		'When in read-only mode, this notification is sent to the container should the user try to 
		edit the document. This can be used to check the document out of a version control system.'
	"

	^0!

scnNeedShown: pSCNotification
	"Private - Respond to an SCN_NEEDSHOWN notification. The 'position' and 'length' fields of the
	notification structure are set.

	From the Scintilla docs:
		'Scintilla has determined that a range of lines that is currently invisible should be made visible. 
		An example of where this may be needed is if the end of line of a contracted fold point is deleted. 
		This message is sent to the container in case it wants to make the line visible in some unusual way 
		such as making the whole document visible. Most containers will just ensure each line in the range 
		is visible by calling SCI_ENSUREVISIBLE.'
	"

	^0!

scnPainted: pSCNotification 
	"Private - Respond to an SCN_PAINTED notification. No fields of the notification 
	structure are set.

	From the Scintilla docs:
		'Painting has just been done. Useful when you want to update some other widgets based 
		on a change in Scintilla, but want to have the paint occur first to appear more responsive. 
	"

	self presenter trigger: #painted.
	^0!

scnSavePointLeft: pSCNotification 
	"Private - Respond to an SCN_SAVEPOINTLEFT notification. No fields of the notification 
	structure are set.

	From the Scintilla docs:
		'Sent to the container when the savepoint is left..., allowing the container to display 
		a dirty indicator and change its menus.'
	"

	self presenter trigger: #savePointLeft.
	^0!

scnSavePointReached: pSCNotification 
	"Private - Respond to an SCN_SAVEPOINTREACHED notification. No fields of the nofication
	structure are set.

	From the Scintilla docs:
		'Sent to the container when the savepoint is entered..., allowing the container to display 
		a dirty indicator and change its menus.'
	"

	self presenter trigger: #savePointReached.
	^0!

scnStyleNeeded: pSCNotification 
	"Private - Respond to SCN_STYLENEEDED notification. Only the 'position' field of the
	notification structure is set.

	From the Scintilla docs: 'Before displaying a page or printing, this message is sent to the
	container. It is a good opportunity for the container to ensure that syntax styling
	information for the visible text.'

	Beware: The Scintilla documentation states (in the description of SCI_GETENDSTYLED) that
	'Scintilla will always ask to style whole lines'. This is NOT the case for container-based
	styling; see the docs for SCN_STYLENEEDED."

	| stop notification |
	notification := SCNotification fromAddress: pSCNotification.
	"SCNotification.position is a zero-based inter-character index in this case, so we can treat
	it as the one-based index of the last character in the range to be styled."
	stop := notification scPosition.
	
	[| last notificationMask |
	last := self stylingPosition.
	notificationMask := self modificationEventMask.
	
	["Temporarily disable style change notifications to speed up the colouring"
	self 
		modificationEventMask: (notificationMask maskClear: ##(SC_MOD_CHANGESTYLE | SC_PERFORMED_USER)).
	self styler 
		onStyleNeeded: self
		from: last
		to: stop] 
			ensure: 
				[self modificationEventMask: notificationMask.
				self invalidateCalculatedExtent]] 
			on: Error
			do: 
				[:ex | 
				"Avoid repeated error by clearing the style needed  state"
				self startStylingFrom: stop + 1.
				ex pass].
	^0!

scnUpdateUI: pSCNotification
	"Private - Respond to SCN_UPDATEUI notification. No fields of the notification structure are set.

	From the Scintilla docs: 
		'Either the text or styling of the document has changed or the selection range has changed.
		Now would be a good time to update any container UI elements that depend on document or view state'
	"

	self invalidateUserInterface.
	^0!

scnURIDropped: pSCNotification 
	"Private - Default handler for an SCN_URIDROPPED event.
	The following fields of the notification structure are set:
		text"

	"Implementation Note: Not sent by Scintilla on Windows."

	^0!

scnUserListSelection: anExternalAddress 
	"Private - Respond to SCN_USERLISTSELECTION notification. The 'message', 'wParam',
	'lParam' and 'text' fields of the notification structure are set (though note that
	the message and lParam fields are always zero).
	From the Scintilla docs: 
		'User has selected an item in a user list. The list type is available in wParam and the text chosen in text.'
	"

	| struct |
	struct := SCNotification fromAddress: anExternalAddress.
	self presenter 
		trigger: #userList:selection:
		with: struct wParam
		with: struct text.
	^0!

scnZoom: anExternalAddress 
	"Private - Respond to an SCN_ZOOM notification. No fields of the notification structure are set.
	From the Scintilla docs:
		' ...generated when the user zooms the display using the keyboard or the SCI_SETZOOM 
		method is called. This notification can be used to recalculate positions, such as the width 
		of the line number margin to maintain sizes in terms of characters rather than pixels.'
	"

	self presenter trigger: #zoomed.
	^0!

scrollDown
	"Scroll the document down, keeping the caret visible."

	self 
		sendMessage: SCI_LINESCROLLDOWN
		wParam: 0
		lParam: 0!

scrollUp
	"Scroll the document up, keeping the caret visible."

	self 
		sendMessage: SCI_LINESCROLLUP
		wParam: 0
		lParam: 0!

scrollWidth
	"Retrieve the document width assumed for scrolling."

	^self 
		sendMessage: SCI_GETSCROLLWIDTH
		wParam: 0
		lParam: 0!

scrollWidth: pixelWidthInteger 
	"Sets the document width assumed for scrolling."

	self 
		sendMessage: SCI_SETSCROLLWIDTH
		wParam: pixelWidthInteger
		lParam: 0!

selection
	"Answer a String containing the receiver's currently selected text"

	"Implementation Note: The superclass implementation will work (although only for streaming
	selections), but this is about 4x faster, and returns the correct text for a rectangular
	selection too."

	| length buf |
	length := self sciGetSelText: nil.
	"The returned length includes the null character."
	buf := String newFixed: length - 1.
	self sciGetSelText: buf.
	^buf!

selectionAlpha
	"Get the alpha of the selection."

	^self 
		sendMessage: SCI_GETSELALPHA
		wParam: 0
		lParam: 0!

selectionAlpha: alphaInteger 
	"Set the alpha of the selection."

	self 
		sendMessage: SCI_SETSELALPHA
		wParam: alphaInteger
		lParam: 0!

selectionBackcolor
	"Answer the background colour of the selection, or nil if selection background colouring is
	disabled."

	^selectionBackcolor!

selectionBackcolor: aColorOrNil 
	"Set the background colour of the selection. The argument can be nil to turn off selection
	highlighting by background colour. If this setting is nil, then the #selectionForecolor
	should be set, as otherwise the selection will not be visible at all."

	(selectionBackcolor := aColorOrNil) isNil 
		ifTrue: [self sciSetSelBack: false back: nil]
		ifFalse: [self sciSetSelBack: true back: aColorOrNil asRGB]!

selectionForecolor
	"Answer the foreground colour of the selection, or nil if the selection foreground colouring
	is disabled (the default)."

	^selectionForecolor!

selectionForecolor: aColorOrNil 
	"Set the foreground colour of the selection. If the argument is nil, then selection
	foreground colouring is enabled and the selected text retains its original foreground
	colour."

	(selectionForecolor := aColorOrNil) isNil 
		ifTrue: [self sciSetSelFore: false fore: nil]
		ifFalse: [self sciSetSelFore: true fore: aColorOrNil asRGB]!

selectionMode
	"Answer a <Symbol> naming the current selection mode."

	"Note that holding down Ctrl+Alt while selecting with the mouse will perform a #rectangular selection,
	the normal selection mode being #stream."

	^self class selectionModes at: self sciGetSelectionMode + 1!

selectionMode: aSymbol 
	"Set the current selection mode to be that named by the <Symbol> argument, one of #stream,
	#rectangle or #lines."

	self sciSetSelectionMode: (self class selectionModes indexOf: aSymbol) - 1!

selectionPlainText: aString 
	"Private - Replace the receiver's current selection with aString.
	SCI_REPLACESEL does not return a useful value."

	self modifyText: [self sciReplaceSel: aString]!

sendMessage: anInteger 
	"Private - Send the Win32 message, anInteger, to the receiver's real window with wParam and
	lParam set to 0."

	"Implementation Note: Override to ensure goes through direct call mechanism."

	^self 
		sendMessage: anInteger
		wParam: 0
		lParam: 0!

sendMessage: msgInteger wParam: wParamInteger 
	"Private - Send the Win32 message msgInteger to the receiver's real window with the
	specified wParam, and lParam set to 0."

	"Implementation Note: Override to ensure goes through direct call mechanism."

	^self 
		sendMessage: msgInteger
		wParam: wParamInteger
		lParam: 0!

sendMessage: anIntegerMessageID wParam: wParam lParam: lParam 
	"Private - Send the specified Win32 <integer> message to the receiver with the specified
	32-bit <integer> wParam and lParam parameters."

	"Implementation Note: Use the direct pointer mechanism provided by Scintilla, which allows
	us to bypass the Windows message queue altogether for best performance. This does mean,
	however, that no callback into Dolphin will result. Also we must be careful not to call
	through a null pointer or we will get a GPF."

	this 
		ifNil: 
			[self getDirectPointer.
			this ifNil: [^0]].
	^ScintillaLibrary default 
		directFunction: this
		msg: anIntegerMessageID
		wParam: wParam
		lParam: lParam!

sendMessage: anIntegerMessageID wParam: wParam lpParam: lParam 
	"Private - Send the Win32 message anIntegerMessageID with parameters wParam and lpParam
	(pointer) to the receiver's Window."

	"Implementation Note: Use the direct pointer mechanism provided by Scintilla, which allows
	us to bypass the Windows message queue altogether for best performance. This does mean,
	however, that no callback into Dolphin will result. Also we must be careful not to call
	through a null pointer or we will get a GPF."

	this isNil 
		ifTrue: 
			[self getDirectPointer.
			this isNil ifTrue: [^0]].
	^ScintillaLibrary default 
		directFunction: this
		msg: anIntegerMessageID
		wParam: wParam
		lParam: lParam yourAddress!

setCurrentTextStyles: aCollection 
	| newStylesByName allocatedStyles newStylesById availableStyles count |
	count := aCollection size.
	allocatedStyles := OrderedCollection new: count.
	aCollection do: [:each | each id ifNotNil: [:id | allocatedStyles add: id]].
	availableStyles := ((1 to: STYLE_DEFAULT - 1) , (STYLE_LASTPREDEFINED + 1 to: self maxStyle) 
				difference: allocatedStyles) readStream.
	newStylesByName := IdentityDictionary new: count.
	newStylesById := Array new: (STYLE_LASTPREDEFINED max: self maxStyle) + 1.
	aCollection do: 
			[:each | 
			each id isNil ifTrue: [each id: availableStyles next].
			newStylesByName at: each name put: each.
			newStylesById at: each id + 1 put: each].
	newStylesByName at: #normal ifAbsentPut: [ScintillaTextStyle normal].
	currentTextStyles := newStylesByName.
	styleIdMap := newStylesById.
	self updateTextStyles!

setDefaultTextStyle
	"Private - Set the default text style by merging settings from the #normal style, and the view with
	the #normal style taking precedence. All styles are then reset to match the default style.
	This should be done as a precursor to setting style attributes, so that all other styles
	inherit unspecified settings from the #normal style (and ultimately the view)."

	| defaultStyle |
	defaultStyle := self buildDefaultStyle.
	defaultStyle applyToView: self at: STYLE_DEFAULT.
	self sciStyleClearAll!

setFoldProperty: aBoolean 
	self isOpen 
		ifTrue: 
			["The HTML lexer uses an additional property"
			#('fold' 'fold.html') 
				do: [:each | self sciSetProperty: each yourAddress value: aBoolean asParameter displayString]]!

setFont: aFont 
	"Private - Apply the specified font to the receiver's associated Windows control"

	super setFont: aFont.
	self updateTextStyles!

setIndicator: anIntegerOrSymbol from: startInteger length: lengthInteger 
	"Apply the indicator identified by the <integer> id or <symbol> name, anIntegerOrSymbol, to
	the range of text in the receiver starting from the one-base integer character position,
	startInteger, for a run length specified by the <integer>, lengthInteger."

	self currentIndicatorId: (self indicatorIdFromName: anIntegerOrSymbol).
	self sciIndicatorFillRange: startInteger - 1 fillLength: lengthInteger!

setIndicator: anIntegerOrSymbol range: anInterval 
	"Apply the indicator identified by the <integer> id or <symbol> name, anIntegerOrSymbol, to
	the range of text specified by the <Interval>, anInterval."

	self 
		setIndicator: anIntegerOrSymbol
		from: anInterval start
		length: anInterval size!

setIndicators: anArray 
	indicators := anArray.
	self updateIndicators!

setIndicatorStyles: aCollection 
	| newStylesByName allocatedStyles availableStyles count |
	count := aCollection size.
	allocatedStyles := OrderedCollection new: count.
	aCollection do: [:each | each id ifNotNil: [:id | allocatedStyles add: id]].
	availableStyles := ((INDIC_CONTAINER to: INDIC_MAX) difference: allocatedStyles) readStream.
	newStylesByName := IdentityDictionary new: count.
	aCollection do: 
			[:each | 
			each id isNil 
				ifTrue: 
					[availableStyles atEnd ifTrue: [^self error: 'Too many styles'].
					each id: availableStyles next].
			newStylesByName at: each name put: each].
	indicatorStyles := newStylesByName.
	self updateIndicatorStyles!

setLexerLanguage: aSymbol 
	aSymbol == #container 
		ifTrue: [self sciSetLexer: SCLEX_CONTAINER]
		ifFalse: 
			[aSymbol == #automatic 
				ifTrue: [self sciSetLexer: SCLEX_AUTOMATIC]
				ifFalse: 
					[self sciSetLexerLanguage: aSymbol.
					self lexer ~~ aSymbol ifTrue: [self error: 'Unrecognised language: ' , aSymbol printString]]]!

setMarginWidths: anArray 
	"Private - Sets the left and right margins of the receiver to anInteger pixels"

	self
		sciSetMarginLeft: anArray first;
		sciSetMarginRight: anArray last!

setReadOnly: readOnlyBoolean 
	"Set to read only or read write."

	self 
		sendMessage: SCI_SETREADONLY
		wParam: readOnlyBoolean asParameter
		lParam: 0!

setTabStops: anInteger 
	"Private - Set the width of the receiver's tab stops to anInteger."

	self tabWidth: anInteger!

setTargetRangeFromSelection
	"Make the target range start and end be the same as the selection range start and end."

	self 
		sendMessage: SCI_TARGETFROMSELECTION
		wParam: 0
		lParam: 0!

setText: aString 
	"Private - Set up the receiver to be displaying the new text in the <String> argument."

	self cancelModes.
	self sciSetText: aString.
	self
		isTextModified: false;
		emptyUndoBuffer.
	"Setting the text removes all current markers - we still tell Scintilla to delete them
	though, since it seems that setting empty text may not remove any markers on line 0."
	self removeAllMarkers.
	"Setting the text also removes the indicators"
	indicators := nil
!

setWhitespaceChars
	self sciSetWhitespaceChars: whitespaces!

setWordChars
	wordChars isNil ifTrue: [^self].
	self sciSetWordChars: wordChars.
	"Setting word chars causes Scintilla to reset the whitespace chars to the defaults"
	self setWhitespaceChars!

showAutoCompletionList: aCollection prefixLength: anInteger 
	"Display a auto-completion list built from the displayStrings of the elements of the
	<collection> argument. The <Integer> parameter indicates how many characters before the
	caret should be used to provide context."

	"N.B. It is important for correct operation of incremental search in the auto-completion
	list that the collection of items be appropriately sorted. In particular if Scintilla is
	configured to perform case-sensitive auto-completion then the list must be sorted in the
	same order as would result from using strcmp() to compare the items. This is because
	Scintilla uses a binary chop to perform an incremental search of the the list as characters
	are entered, and so it may fail to find items if the sort order is not correct."

	aCollection isEmpty 
		ifTrue: [self cancelAutoCompletion]
		ifFalse: 
			[| string |
			string := self buildItemList: aCollection asSortedCollection.
			self sciAutoCShow: anInteger itemList: string]!

showCallTip: aString at: anInteger 
	"Show the specified <String> as a call tip at the specified <integer> character position."

	self sciCallTipShow: anInteger - 1 definition: aString!

showUserList: aCollection id: anInteger 
	"Display a 'user list' built from the displayStrings of the elements of the <collection>
	argument. The <Integer> parameter is passed back with the SCN_USERLISTSELECTION
	notification."

	self sciUserListShow: anInteger itemList: (self buildItemList: aCollection)!

showVerticalScrollBar: showBoolean 
	"Show or hide the vertical scroll bar."

	self 
		sendMessage: SCI_SETVSCROLLBAR
		wParam: showBoolean asParameter
		lParam: 0!

splitTarget: pixelWidthInteger 
	"Split the lines in the target into lines that are less wide than pixelWidth where
	possible."

	self 
		sendMessage: SCI_LINESSPLIT
		wParam: pixelWidthInteger
		lParam: 0!

startDwellTimer
	self setTimer: 1 interval: 100!

startRecording
	"Start notifying the container of all key presses and commands."

	self 
		sendMessage: SCI_STARTRECORD
		wParam: 0
		lParam: 0!

startStylingFrom: anInteger 
	"Set the current styling position to the one-based <integer> index argument."

	self sciStartStyling: anInteger - 1 mask: self restyleMask!

state
	"Private - Answer a <MessageSequence> which, when replayed, will restore the receiver 
	to its current state"

	"Implementation Note: Each attributes has a 3-element array, respectively the set selector,
	get selector, and default value. State is only saved for an attribute if the current value
	differs from the default.

	The markers must be stored as part of the state, even though stored in an instance variable,
	as these are reset when the text is restored."

	| answer |
	answer := super state.
	#(#(#modificationEventMask: #modificationEventMask ##(SC_MODEVENTMASKALL)) #(#isDrawingBuffered: #isDrawingBuffered true) #(#hoverTime: #hoverTime 10000000) #(#setIndicators: #indicators #()) #(#caretStyle: #caretStyle #line) #(#caretForecolor: #caretForecolor ##(Color 
		black asRGB)) #(#caretPeriod: #caretPeriod 500) #(#caretWidth: #caretWidth 1) #(#isCurrentLineHighlighted: #isCurrentLineHighlighted false) #(#currentLineBackcolor: #currentLineBackcolor ##(Color 
		yellow asRGB)) #(#endOfLineMode: #endOfLineMode #crlf) #(#hasVisibleLineEndings: #hasVisibleLineEndings false) #(#wordWrap: #wordWrap false) #(#layoutCachingMode: #layoutCachingMode #caret) #(#margins: #margins #()) #(#markers: #markers ##(OrderedCollection 
		new)) #(#isOvertypeEnabled: #isOvertypeEnabled false) #(#printMagnification: #printMagnification 0) #(#printColourMode: #printColourMode 0) #(#canHScroll: #canHScroll true) #(#canScrollPastEnd #canScrollPastEnd true) #(#scrollWidth: #scrollWidth 2000) #(#xOffset: #xOffset 0) #(#selectionMode: #selectionMode #stream) #(#backspaceUnindents: #backspaceUnindents false) #(#indentationGuides: #indentationGuides #none) #(#indentation: #indentation 0) #(#tabIndents: #tabIndents false) #(#tabWidth: #tabWidth 8) #(#isUsingTabs: #isUsingTabs true) #(#targetRange: #targetRange ##(1 
		to: 0)) #(#whitespaceVisibility: #whitespaceVisibility #invisible) #(#autoCompletionSeparator: #autoCompletionSeparator $ ) #(#autoCompletionImageIdSeparator: #autoCompletionImageIdSeparator $?) #(#isAutoCompletionCancelledAtStart: #isAutoCompletionCancelledAtStart true) #(#isAutoCompletionCaseInsensitive: #isAutoCompletionCaseInsensitive false) #(#isAutoCompletionCancelledWhenNoMatch: #isAutoCompletionCancelledWhenNoMatch true) #(#isAutoCompletionTruncating: #isAutoCompletionTruncating false) #(#maxCompletionListHeight: #maxCompletionListHeight 5) #(#maxCompletionListWidth: #maxCompletionListWidth 0) #(#sciSetCodePage: #sciGetCodePage 0) #(#edgeMode: #edgeMode #none) #(#edgeColor: #edgeColor ##(Color 
		gray asRGB)) #(#zoomLevel: #zoomLevel 0) #(#isUsingPalette: #isUsingPalette false) #(#setLexerLanguage: #lexer #container) #(#controlCharacter: #controlCharacter nil) #(#selectionAlpha: #selectionAlpha 256) #(#positionCacheSize: #positionCacheSize 1024)) 
			do: 
				[:each | 
				| attrib |
				attrib := self perform: each second.
				attrib = each last 
					ifFalse: 
						[| msg |
						msg := MessageSend 
									receiver: self
									selector: each first
									argument: attrib.
						answer add: msg]].
	^answer!

stopDwellTimer
	self killTimer: 1!

stopRecording
	"Stop notifying the container of all key presses and commands."

	self 
		sendMessage: SCI_STOPRECORD
		wParam: 0
		lParam: 0!

stopStyling
	"Cancel any further styling for the specified mask by moving the styling end point to the
	end of the document."

	self startStylingFrom: self textLength + 1!

styleAt: anInteger 
	"Answer a <ScintillaTextStyle> being the the style of the character at the specified character
	position."

	^self styleWithId: (self styleIdAt: anInteger)!

styleBits
	"Answer the number of bits available for text styles. See #styleBits for further
	information."

	^styleMask highBit!

styleBits: anInteger 
	"Set the number of bits available for text styles. Originally this defaulted to 5 giving a
	total of 32 user configurable styles, allowing 3 bits for indicators. With the advent of
	modern indicators (which are stored separately from styles) all 8 bits are available for
	styles."

	(anInteger between: 1 and: 8) ifFalse: [self error: 'Invalid number of style bits'].
	self styleBits = anInteger ifTrue: [^self].
	styleMask := (1 << anInteger) - 1.
	self sciSetStyleBits: anInteger.
	"Changing the number of style bits necessitates recreating the control, else the text pane will just go black"
	self recreate!

styledTextFrom: startInteger to: stopInteger 
	"Private - Answer a <ByteArray> containing pairs of bytes that represent the character and
	style byte for each position in the specified range."

	| bytes range |
	startInteger < 1 ifTrue: [self errorSubscriptBounds: startInteger].
	stopInteger < startInteger ifTrue: [#()].
	stopInteger > self textLength ifTrue: [self errorSubscriptBounds: stopInteger].
	"Note extra two bytes needed for null-terminator"
	bytes := ByteArray newFixed: (stopInteger - startInteger + 1 + 1) * 2.
	range := TEXTRANGE 
				from: startInteger - 1
				to: stopInteger
				text: bytes.
	self sciGetStyledText: range.
	"Drop the redundant null terms"
	bytes resize: bytes size - 2.
	^bytes!

styleIdAt: anInteger 
	^(self styleMaskAt: anInteger) bitAnd: styleMask!

styleMaskAt: anInteger 
	"Inlined to cut overhead when scanning text for the start of a style."

	^self 
		sendMessage: SCI_GETSTYLEAT
		wParam: anInteger - 1
		lParam: 0!

styleNamed: aSymbol 
	"Answer a <ScintillaTextStyle> being the named style, or nil if the name is not recognised."

	^currentTextStyles at: aSymbol ifAbsent: []!

styleNext: lengthInteger mask: styleInteger 
	"Change style from current styling position for length characters to a style and move the
	current styling position to after this newly styled segment."

	self 
		sendMessage: SCI_SETSTYLING
		wParam: lengthInteger
		lParam: styleInteger!

styler
	"Answer the <ScintillaStyler> used to dynamically colour text in the receiver."

	styler isNil ifTrue: [styler := NullScintillaStyler new].
	^styler!

styler: aScintillaStyler 
	"Set the <ScintillaStyler> used to dynamically colour text in the receiver."

	styler := aScintillaStyler.
	styler prepareToStyleView: self.
	self invalidateStyling!

stylerClass
	"Answer the class of the receiver's dynamic styler."

	^self styler class!

stylerClass: aScintillaStylerClass 
	"Set the class of the receiver's dynamic styler, replacing the styler with a new instance of the specified class."

	self styler: (aScintillaStylerClass ifNil: [NullScintillaStyler]) new!

styleUnderCaret
	"Answer a <ScintillaTextStyle> representing the style of the character under the caret."

	^self styleAt: self caretPosition!

styleWithId: anInteger 
	^(styleIdMap at: anInteger + 1 ifAbsent: []) 
		ifNil: 
			[(ScintillaTextStyle new)
				name: anInteger;
				yourself]!

stylingPosition
	"Answer the one-based <integer> index of the next character in the receiver that requires styling."

	^self sciGetEndStyled + 1!

tabIndents
	"Does a tab pressed when caret is within indentation indent?"

	^(self 
		sendMessage: SCI_GETTABINDENTS
		wParam: 0
		lParam: 0) asBoolean!

tabIndents: tabIndentsBoolean 
	"Sets whether a tab pressed when caret is within indentation indents."

	self 
		sendMessage: SCI_SETTABINDENTS
		wParam: tabIndentsBoolean asParameter
		lParam: 0!

tabWidth
	"Retrieve the visible size of a tab."

	^self 
		sendMessage: SCI_GETTABWIDTH
		wParam: 0
		lParam: 0!

tabWidth: tabWidthInteger 
	"Change the visible size of a tab to be a multiple of the width of a space character."

	self 
		sendMessage: SCI_SETTABWIDTH
		wParam: tabWidthInteger
		lParam: 0!

targetAll
	"Target all the text in the receiver, and answer the new selection range."

	| interval |
	interval := 1 to: self textLength.
	self targetRange: interval.
	^interval!

targetRange
	"Answer the range of the current search/replace target. The target is like the selection,
	but not visible."

	^self sciGetTargetStart + 1 to: self sciGetTargetEnd!

targetRange: anInterval 
	"Set the range of the current search/replace target. The target is like the selection,
	but not visible."

	self sciSetTargetStart: anInterval start - 1.
	self sciSetTargetEnd: anInterval stop!

textAtLine: anInteger 
	"Private - Answer the text of a line at the given line index (1-based)."

	"Implementation Note: The result will include any line terminators, which we should probably
	strip off here."

	| length text |
	anInteger <= 0 ifTrue: [^self errorSubscriptBounds: anInteger].
	"As of 1.60, Scintilla reports line length if passed null buffer"
	length := self sciGetLine: anInteger - 1 text: nil.
	text := String newFixed: length.
	self sciGetLine: anInteger - 1 text: text.
	^text!

textLength
	"Retrieve the number of characters in the document."

	^self 
		sendMessage: SCI_GETTEXTLENGTH
		wParam: 0
		lParam: 0!

textLimit
	"Answer the current text limit for the control (i.e. the amount of text it can hold)."

	^SmallInteger maximum

!

textLimit: anInteger 
	"Set the maximum text limit for the receiver."

	"Implementation Note: Ignored as not supported by Scintilla."

	^self!

textStyles
	"Answer the currently configured <collection> of <ScintillaTextStyle>s. 
	This collection should be considered as immutable - any changes to its elements, or the
	addition/removal/replacement of elements, will not result in the view being updated. To
	change text styles the entire collection must be replaced."

	^currentTextStyles values asSortedCollection: ScintillaAttribute sortByIdBlock!

textStyles: aCollection 
	"Set the collection of <ScintillaTextStyle>s configured for the receiver to be those
	specified in the argument. This may involve removing old style definitions, and adding new
	ones."

	"Implementation Note: Like most of Scintilla's attributes, text styles are numbered by
	integer indices .As this is a somewhat inconvenient representation, we allow styles to be
	given symbolic names. Any pre-existing styles (i.e. styles of the same name) maintain their
	index so that existing styled text remains associated with the correct style. Normally a new
	style is allocated the first available style number, however certain style names are
	associated with Scintilla's predefined styles and so these names are always mapped to the
	same style number."

	self setCurrentTextStyles: aCollection.
	allTextStyles at: self lexer put: currentTextStyles!

toggleFold: anInteger 
	self sciToggleFold: anInteger - 1!

toggleFoldMargin
	"Show or hide the first fold margin, inverting the current state."

	self hasFoldMargin: self hasFoldMargin not!

toggleIndentationGuides
	"Show or hide the indentation guides."

	self hasIndentationGuides: self hasIndentationGuides not!

toggleLineEndings
	"Show or hide the end-of-line characters."

	self hasVisibleLineEndings: self hasVisibleLineEndings not!

toggleLineNumbers
	"Show or hide the first line number margin, inverting the current state."

	self hasLineNumbers: self hasLineNumbers not!

toggleOvertype
	"Switch from insert to overtype mode or the reverse."

	self 
		sendMessage: SCI_EDITTOGGLEOVERTYPE
		wParam: 0
		lParam: 0!

toggleStyling
	"Enable/disable dynamic styling of text in the receiver."

	self isStylingEnabled: self isStylingEnabled not!

toggleWhitespace
	"Show or hide the whitespace markers in the view."

	self whitespaceVisibility: (self whitespaceVisibility == #invisible 
				ifTrue: [#visibleAlways]
				ifFalse: [#invisible])!

toggleWordWrap
	"Toggle the receiver into/out-of word wrap mode."

	self wordWrap: self wordWrap not!

tokenEndAt: anInteger 
	"Answer the <integer> end position of the token that includes the specified <integer>
	position. This is dependent on there being valid styling information available, and that the
	styles delimit tokens."

	| stop tokenId max |
	tokenId := self styleIdAt: anInteger.
	stop := anInteger.
	max := self textLength.
	[stop < max and: [(self styleIdAt: stop + 1) == tokenId]] whileTrue: [stop := stop + 1].
	^stop!

tokenRangeAt: anInteger 
	"Answer the <Interval> of text in the receiver occuppied by the token under the specified
	<integer> position. This is dependent on there being valid styling information available,
	and that the styles delimit tokens."

	^(self tokenStartAt: anInteger) to: (self tokenEndAt: anInteger)!

tokensFrom: startInteger to: stopInteger 
	"Answer an <sequencedReadableCollection> of <Associations> representing the tokenised form
	of the text in the specified range, as deduced from the styling information. The key of each
	association is the name of the style, and the value is the token text."

	| bytes |
	bytes := self styledTextFrom: startInteger to: stopInteger.
	^self decodeStyledText: bytes!

tokenStartAt: anInteger 
	"Answer the <integer> start position of the token that includes the specified <integer>
	position. This is dependent on there being valid styling information available, and that the
	styles delimit tokens."

	| start tokenId |
	tokenId := self styleIdAt: anInteger.
	start := anInteger.
	[start > 1 and: [(self styleIdAt: start - 1) == tokenId]] whileTrue: [start := start - 1].
	^start!

twiddleLines
	"Switch the current line with the previous."

	self 
		sendMessage: SCI_LINETRANSPOSE
		wParam: 0
		lParam: 0!

undo
	"Undo one action in the undo history."

	self cancelModes.
	self basicUndo!

unindent
	"Dedent the selected lines."

	self 
		sendMessage: SCI_BACKTAB
		wParam: 0
		lParam: 0!

updateIndicators
	self basicClearContainerIndicators.
	indicators do: [:each | self setIndicator: each styleName range: each range]!

updateIndicatorStyles
	self hideIndicators.
	indicatorStyles ifNotNil: [:indics | indics do: [:each | each applyToView: self at: each id]]!

updateKeyBindings
	"Private - Update the control with the key bindings configured for the receiver, replacing
	its default command key assignments."

	self sciClearAllCmdKeys.
	self keyBindings do: [:each | self sciAssignCmdKey: each scintillaKeyCode msg: each message]!

updateMarkerDefinitions
	markerDefinitions do: [:each | each applyToView: self at: each id]!

updateMarkers
	self deleteMarkers: 0.
	markers do: [:each | each addToView: self]!

updateTextStyles
	"Private - Sync. the control's knowledge of the text styles with those recorded in the receiver."

	self isOpen ifFalse: [^self].
	self setDefaultTextStyle.
	currentTextStyles do: [:each | each applyToView: self]!

validateUserInterface
	"Validates the user interface for the receiver. Usually performed at idle time
	when the UI has been flagged as being invalid"

	self isBraceHighlightingEnabled ifTrue: [self braceHighlight].
	super validateUserInterface!

whitespaceBackcolor
	"Answer the background colour of all whitespace, or nil if none is specified. If no global
	white space background colour, then the default colour is used. This will be that of
	whatever style the styler is setting on blocks of whitespace, otherwise the default
	background colour of the view."

	^whitespaceBackcolor!

whitespaceBackcolor: aColorOrNil 
	"Set the background colour of all whitespace. The argument can be nil to adopt the default
	background colour."

	(whitespaceBackcolor := aColorOrNil) isNil 
		ifTrue: [self sciSetWhitespaceBack: false back: nil]
		ifFalse: [self sciSetWhitespaceBack: true back: aColorOrNil asRGB]!

whitespaceForecolor
	"Answer the foreground colour used to display whitespace markers (when visible). If nil then
	the default whitespace colour is being used. This will be that of whatever style the styler
	is setting on blocks of whitespace (which could be a specially allocated whitespace style,
	or just the normal style), otherwise the default foreground colour of the view."

	^whitespaceForecolor!

whitespaceForecolor: aColorOrNil 
	"Set the foreground colour to be used to display whitespace markers (when visible). If nil
	then the default white space colour will be used."

	(whitespaceForecolor := aColorOrNil) isNil 
		ifTrue: [self sciSetWhitespaceFore: false fore: nil]
		ifFalse: [self sciSetWhitespaceFore: true fore: aColorOrNil asRGB]!

whitespaces
	^whitespaces!

whitespaces: aString 
	whitespaces := aString ifNil: [Whitespaces].
	self setWhitespaceChars!

whitespaceVisibility
	"Answer a <Symbol> naming the level of whitespace marking currently configured in the receiver."

	^self class whitespaceVisibilityLevels 
		at: (self 
				sendMessage: SCI_GETVIEWWS
				wParam: 0
				lParam: 0) + 1!

whitespaceVisibility: aSymbol 
	"Make white space characters invisible, always visible or visible outside indentation
	depending on the <Symbol> argument (one of #invisible, #visibleAlways or
	#visibleAfterIndent, respectively)."

	self 
		sendMessage: SCI_SETVIEWWS
		wParam: (self class whitespaceVisibilityLevels indexOf: aSymbol) - 1
		lParam: 0!

widthOfText: aString inStyle: aSymbol 
	"Answer the <integer> pixel width that would be needed to display the text in the <String>
	argument in the text style named by the <Symbol> argument."

	^self sciTextWidth: ((self styleNamed: aSymbol) ifNil: [0] ifNotNil: [:style | style id])
		text: aString!

willCaptureMouse
	"Get whether mouse gets captured."

	^(self 
		sendMessage: SCI_GETMOUSEDOWNCAPTURES
		wParam: 0
		lParam: 0) asBoolean!

willCaptureMouse: capturesBoolean 
	"Set whether the mouse is captured when its button is pressed."

	self 
		sendMessage: SCI_SETMOUSEDOWNCAPTURES
		wParam: capturesBoolean asParameter
		lParam: 0!

wmContextMenu: message wParam: wParam lParam: lParam 
	"Private - Shows and tracks a context menu for the receiver"

	self cancelModes.
	^super wmContextMenu: message wParam: wParam lParam: lParam!

wmTimer: message wParam: wParam lParam: lParam 
	"Private - A timer event, identified by the <integer> id, wParam, has fired."

	"Implementation Note: Scintilla uses a rapidly firing timer, id 1, that we want to ignore"

	^wParam == 1 
		ifFalse: 
			[super 
				wmTimer: message
				wParam: wParam
				lParam: lParam]!

wordChars
	"Answer a <String> containting the set of characters considered to be those valid as
	characters in a single word. All other characters are considered to be delimiters. This
	setting controls word-oriented cursor movements and selections."

	^wordChars 
		ifNil: 
			[| lib |
			lib := CRTLibrary default.
			"This replicates the Scintilla default (this property of the control is not queryable), 
			although personally I think this includes too many control/graphic characters."
			Character byteCharacterSet 
				select: [:each | each == $_ or: [each codePoint >= 16r80 or: [lib isalnum: each]]]]!

wordChars: aString 
	"Set the set of characters considered to be those valid as characters in a single word. All
	other characters are considered to be delimiters. This setting controls word-oriented cursor
	movements and selections."

	wordChars := aString.
	self setWordChars!

wordWrap
	"Answer whether the receiver is in word-wrap mode."

	"From the Scintilla Documentation: 'By default, Scintilla does not wrap lines of text. If 
	you enable line wrapping, lines wider than the window width are continued on the following 
	lines. Lines are broken after space or tab characters or between runs of different styles. 
	If this is not possible because a word in one style is wider than the window then the break 
	occurs after the last character that completely fits on the line. The horizontal scroll bar 
	does not appear when wrap mode is on.'"

	^self sciGetWrapMode = SC_WRAP_WORD!

wordWrap: aBoolean 
	"Set whether the receiver is in word-wrap mode."

	self sciSetWrapMode: (aBoolean ifTrue: [SC_WRAP_WORD] ifFalse: [SC_WRAP_NONE])!

xOffset
	^self 
		sendMessage: SCI_GETXOFFSET
		wParam: 0
		lParam: 0!

xOffset: newOffsetInteger 
	"Get and Set the xOffset (ie, horizonal scroll position)."

	self 
		sendMessage: SCI_SETXOFFSET
		wParam: newOffsetInteger
		lParam: 0!

zoomIn
	"Magnify the displayed text by increasing the sizes by 1 point."

	self 
		sendMessage: SCI_ZOOMIN
		wParam: 0
		lParam: 0!

zoomLevel
	"Retrieve the zoom level."

	^self 
		sendMessage: SCI_GETZOOM
		wParam: 0
		lParam: 0!

zoomLevel: zoomInteger 
	"Set the zoom level. This number of points is added to the size of all fonts. It may be
	positive to magnify or negative to reduce."

	self 
		sendMessage: SCI_SETZOOM
		wParam: zoomInteger
		lParam: 0!

zoomOut
	"Make the displayed text smaller by decreasing the sizes by 1 point."

	self 
		sendMessage: SCI_ZOOMOUT
		wParam: 0
		lParam: 0! !
!ScintillaView categoriesFor: #acceptAutoCompletion!**auto generated**!autocompletion!public!scintilla interface! !
!ScintillaView categoriesFor: #activeHotspotBackcolor!**auto generated**!caret, selection, and hotspot styles!public!scintilla interface! !
!ScintillaView categoriesFor: #activeHotspotForecolor!**auto generated**!caret, selection, and hotspot styles!public!scintilla interface! !
!ScintillaView categoriesFor: #addKeyBinding:!key bindings!public! !
!ScintillaView categoriesFor: #addMarker:!markers!public! !
!ScintillaView categoriesFor: #addMarkerType:at:!markers!public! !
!ScintillaView categoriesFor: #anchorPosition!accessing!public!selection! !
!ScintillaView categoriesFor: #anchorPosition:!accessing!public!selection! !
!ScintillaView categoriesFor: #appendText:!public!text retrieval & modification! !
!ScintillaView categoriesFor: #applyAttributes:!helpers!private! !
!ScintillaView categoriesFor: #applyStyle:toNext:!public!styling! !
!ScintillaView categoriesFor: #applyStyleId:toNext:!public!styling! !
!ScintillaView categoriesFor: #applyTextStylesForLexer:!helpers!lexer!private! !
!ScintillaView categoriesFor: #areHotspotsSingleLine!**auto generated**!caret, selection, and hotspot styles!public!scintilla interface! !
!ScintillaView categoriesFor: #areHotspotsSingleLine:!**auto generated**!caret, selection, and hotspot styles!public!scintilla interface! !
!ScintillaView categoriesFor: #autoCompletionAcceptChars!accessing!autocompletion!public!scintilla interface! !
!ScintillaView categoriesFor: #autoCompletionAcceptChars:!accessing!autocompletion!public!scintilla interface! !
!ScintillaView categoriesFor: #autoCompletionCancelChars!accessing!autocompletion!public!scintilla interface! !
!ScintillaView categoriesFor: #autoCompletionCancelChars:!accessing!autocompletion!public!scintilla interface! !
!ScintillaView categoriesFor: #autoCompletionImageIdSeparator!accessing!autocompletion!public!scintilla interface! !
!ScintillaView categoriesFor: #autoCompletionImageIdSeparator:!accessing!autocompletion!public!scintilla interface! !
!ScintillaView categoriesFor: #autoCompletionListPosition!accessing!autocompletion!public!scintilla interface! !
!ScintillaView categoriesFor: #autoCompletionSeparator!accessing!autocompletion!public!scintilla interface! !
!ScintillaView categoriesFor: #autoCompletionSeparator:!accessing!autocompletion!public!scintilla interface! !
!ScintillaView categoriesFor: #backcolorChanged!helpers!private! !
!ScintillaView categoriesFor: #backspace!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #backspaceNoLine!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #backspaceUnindents!**auto generated**!public!scintilla interface!tabs & indentation guides!testing! !
!ScintillaView categoriesFor: #backspaceUnindents:!**auto generated**!accessing!public!scintilla interface!tabs & indentation guides! !
!ScintillaView categoriesFor: #basicClearContainerIndicators!indicators!private! !
!ScintillaView categoriesFor: #basicClearSelection!**auto generated**!clipboard operations!helpers!public!scintilla interface! !
!ScintillaView categoriesFor: #basicLineFromPosition:!**auto generated**!enquiries!public!scintilla interface! !
!ScintillaView categoriesFor: #basicPositionAtLine:!**auto generated**!public!scintilla interface!selection! !
!ScintillaView categoriesFor: #basicSelectAll!**auto generated**!public!scintilla interface!selection! !
!ScintillaView categoriesFor: #basicSelectionRange!private!selection! !
!ScintillaView categoriesFor: #basicSelectionStart:end:!**auto generated**!public!scintilla interface!selection! !
!ScintillaView categoriesFor: #basicUndo!**auto generated**!commands!public!scintilla interface!undo & redo! !
!ScintillaView categoriesFor: #beginUndoGroup!**auto generated**!public!scintilla interface!undo & redo! !
!ScintillaView categoriesFor: #braceChars!accessing!brace highlighting!public! !
!ScintillaView categoriesFor: #braceChars:!accessing!brace highlighting!public! !
!ScintillaView categoriesFor: #braceHighlight!brace highlighting!helpers!private! !
!ScintillaView categoriesFor: #buildDefaultStyle!private!style definition! !
!ScintillaView categoriesFor: #buildItemList:!autocompletion!private! !
!ScintillaView categoriesFor: #buildViewStyle!helpers!private! !
!ScintillaView categoriesFor: #callTipBackcolor!accessing!call tips!public! !
!ScintillaView categoriesFor: #callTipBackcolor:!accessing!call tips!public! !
!ScintillaView categoriesFor: #callTipForecolor!accessing!call tips!public! !
!ScintillaView categoriesFor: #callTipForecolor:!accessing!call tips!public! !
!ScintillaView categoriesFor: #callTipHighlightColor!accessing!call tips!public! !
!ScintillaView categoriesFor: #callTipHighlightColor:!accessing!call tips!public! !
!ScintillaView categoriesFor: #callTipPosition!accessing!call tips!public!scintilla interface! !
!ScintillaView categoriesFor: #cancelAutoCompletion!**auto generated**!autocompletion!public!scintilla interface! !
!ScintillaView categoriesFor: #cancelCallTip!**auto generated**!call tips!public!scintilla interface! !
!ScintillaView categoriesFor: #cancelModes!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #canHScroll!**auto generated**!public!scintilla interface!scrolling!testing! !
!ScintillaView categoriesFor: #canHScroll:!accessing!public!scrolling! !
!ScintillaView categoriesFor: #canonicalizeLineEndings:!line endings!public! !
!ScintillaView categoriesFor: #canPaste!clipboard operations!public!testing! !
!ScintillaView categoriesFor: #canRedo!**auto generated**!public!scintilla interface!testing!undo & redo! !
!ScintillaView categoriesFor: #canScrollPastEnd!public!scrolling!testing! !
!ScintillaView categoriesFor: #canScrollPastEnd:!**auto generated**!accessing!public!scintilla interface!scrolling! !
!ScintillaView categoriesFor: #canUndo!**auto generated**!public!scintilla interface!testing!undo & redo! !
!ScintillaView categoriesFor: #canVScroll!**auto generated**!public!scintilla interface!scrolling!testing! !
!ScintillaView categoriesFor: #caretForecolor!**auto generated**!accessing!caret, selection, and hotspot styles!public!scintilla interface! !
!ScintillaView categoriesFor: #caretForecolor:!accessing!caret, selection, and hotspot styles!public!scintilla interface! !
!ScintillaView categoriesFor: #caretPeriod!**auto generated**!accessing!caret, selection, and hotspot styles!public!scintilla interface! !
!ScintillaView categoriesFor: #caretPeriod:!**auto generated**!accessing!caret, selection, and hotspot styles!public!scintilla interface! !
!ScintillaView categoriesFor: #caretPosition!accessing!caret!public!selection! !
!ScintillaView categoriesFor: #caretPosition:!accessing!caret!public! !
!ScintillaView categoriesFor: #caretStyle!caret, selection, and hotspot styles!public! !
!ScintillaView categoriesFor: #caretStyle:!caret, selection, and hotspot styles!public! !
!ScintillaView categoriesFor: #caretWidth!**auto generated**!accessing!caret, selection, and hotspot styles!public!scintilla interface! !
!ScintillaView categoriesFor: #caretWidth:!accessing!caret, selection, and hotspot styles!public!scintilla interface! !
!ScintillaView categoriesFor: #characterAt:!accessing!public!text retrieval & modification! !
!ScintillaView categoriesFor: #charCloseToPosition:!public!selection! !
!ScintillaView categoriesFor: #charNearestPosition:!public!selection! !
!ScintillaView categoriesFor: #clearAll!**auto generated**!commands!public!scintilla interface!text retrieval & modification! !
!ScintillaView categoriesFor: #clearContainerIndicators!indicators!public! !
!ScintillaView categoriesFor: #clearIndicator:from:to:!indicators!public! !
!ScintillaView categoriesFor: #codePage!accessing!public! !
!ScintillaView categoriesFor: #codePage:!accessing!public! !
!ScintillaView categoriesFor: #columnFromPosition:!enquiries!public! !
!ScintillaView categoriesFor: #controlCharacter!caret, selection, and hotspot styles!public! !
!ScintillaView categoriesFor: #controlCharacter:!caret, selection, and hotspot styles!public! !
!ScintillaView categoriesFor: #convertToLowercase!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #convertToUppercase!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #copyLine!**auto generated**!clipboard operations!public!scintilla interface! !
!ScintillaView categoriesFor: #copySelection!**auto generated**!clipboard operations!public!scintilla interface! !
!ScintillaView categoriesFor: #currentIndicatorId!**auto generated**!indicators!private! !
!ScintillaView categoriesFor: #currentIndicatorId:!**auto generated**!indicators!private!scintilla interface! !
!ScintillaView categoriesFor: #currentIndicatorValue!**auto generated**!indicators!private!scintilla interface! !
!ScintillaView categoriesFor: #currentIndicatorValue:!**auto generated**!indicators!private!scintilla interface! !
!ScintillaView categoriesFor: #currentLineBackcolor!**auto generated**!accessing!caret, selection, and hotspot styles!public!scintilla interface! !
!ScintillaView categoriesFor: #currentLineBackcolor:!accessing!caret, selection, and hotspot styles!public!scintilla interface! !
!ScintillaView categoriesFor: #cutLine!**auto generated**!clipboard operations!public!scintilla interface! !
!ScintillaView categoriesFor: #cutSelection!**auto generated**!clipboard operations!public!scintilla interface! !
!ScintillaView categoriesFor: #decodeStyledText:!accessing!public!text retrieval & modification! !
!ScintillaView categoriesFor: #defaultBraceChars!brace highlighting!constants!private! !
!ScintillaView categoriesFor: #defaultKeyBindings!helpers!key bindings!private! !
!ScintillaView categoriesFor: #defaultMarkerDefinitions!constants!markers!private! !
!ScintillaView categoriesFor: #defaultModEventMask!constants!private! !
!ScintillaView categoriesFor: #defaultTextStylesFor:!constants!public!style definition! !
!ScintillaView categoriesFor: #defaultWhitespaceChars!constants!public! !
!ScintillaView categoriesFor: #defaultWindowStyle!constants!private! !
!ScintillaView categoriesFor: #defaultWordChars!constants!public! !
!ScintillaView categoriesFor: #deleteLine!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #deleteMarkers:!**auto generated**!markers!public!scintilla interface! !
!ScintillaView categoriesFor: #deleteToEndOfLine!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #deleteToEndOfWord!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #deleteToNextWord!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #deleteToStartOfLine!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #deleteToStartOfWord!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #destroyAutoCompletionListImages!**auto generated**!autocompletion!public!scintilla interface! !
!ScintillaView categoriesFor: #duplicateLine!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #duplicateSelection!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #edgeColor!**auto generated**!accessing!long lines!public!scintilla interface! !
!ScintillaView categoriesFor: #edgeColor:!**auto generated**!accessing!long lines!public!scintilla interface! !
!ScintillaView categoriesFor: #edgeColumn!accessing!long lines!public! !
!ScintillaView categoriesFor: #edgeColumn:!accessing!long lines!public! !
!ScintillaView categoriesFor: #edgeMode!accessing!long lines!public! !
!ScintillaView categoriesFor: #edgeMode:!accessing!long lines!public! !
!ScintillaView categoriesFor: #editStyles!commands!public! !
!ScintillaView categoriesFor: #emptyUndoBuffer!**auto generated**!public!scintilla interface!undo & redo! !
!ScintillaView categoriesFor: #enChange!event handling-win32!private! !
!ScintillaView categoriesFor: #endOfLineMode!accessing!line endings!public! !
!ScintillaView categoriesFor: #endOfLineMode:!accessing!line endings!public! !
!ScintillaView categoriesFor: #endUndoGroup!**auto generated**!public!scintilla interface!undo & redo! !
!ScintillaView categoriesFor: #enKillFocus!event handling-win32!private! !
!ScintillaView categoriesFor: #enSetFocus!event handling-win32!private! !
!ScintillaView categoriesFor: #ensureCaretVisible!caret!public! !
!ScintillaView categoriesFor: #ensureLineVisible:!public!scrolling! !
!ScintillaView categoriesFor: #ensureVisible:!public!scrolling! !
!ScintillaView categoriesFor: #enUpdate!event handling-win32!private! !
!ScintillaView categoriesFor: #errorStatus!**auto generated**!error handling!public!scintilla interface! !
!ScintillaView categoriesFor: #errorStatus:!**auto generated**!error handling!public!scintilla interface! !
!ScintillaView categoriesFor: #extendDown!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #extendLeft!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #extendPageDown!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #extendPageUp!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #extendParaDown!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #extendParaUp!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #extendRectangleDown!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #extendRectangleLeft!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #extendRectanglePageDown!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #extendRectanglePageUp!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #extendRectangleRight!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #extendRectangleToEndOfLine!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #extendRectangleToStartOfLine!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #extendRectangleToVcHome!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #extendRectangleUp!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #extendRight!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #extendStutteredPageDown!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #extendStutteredPageUp!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #extendToEndOfDisplayLine!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #extendToEndOfDocument!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #extendToEndOfLine!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #extendToEndOfNextWord!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #extendToEndOfPreviousWord!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #extendToEndOfWord!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #extendToEndOfWordPart!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #extendToEndOfWrappedLine!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #extendToStartOfDisplayLine!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #extendToStartOfDocument!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #extendToStartOfLine!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #extendToStartOfWord!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #extendToStartOfWordPart!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #extendToStartOfWrappedLine!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #extendToVcHome!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #extendToWrappedVcHome!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #extendUp!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #extraStyleBits!accessing!public!style definition! !
!ScintillaView categoriesFor: #extraStyleBits:!accessing!public!style definition! !
!ScintillaView categoriesFor: #extraStyleMask!accessing!private! !
!ScintillaView categoriesFor: #find:range:!private!searching & replacing! !
!ScintillaView categoriesFor: #find:range:flags:!public!searching & replacing! !
!ScintillaView categoriesFor: #findAutoCompletionEntry:!**auto generated**!autocompletion!public!scintilla interface! !
!ScintillaView categoriesFor: #findMatchingBrace:!brace highlighting!public! !
!ScintillaView categoriesFor: #findStyleStart:before:!brace highlighting!public! !
!ScintillaView categoriesFor: #foldFlags!accessing!folding!public! !
!ScintillaView categoriesFor: #foldFlags:!accessing!folding!public! !
!ScintillaView categoriesFor: #foldLine:level:!folding!public!scintilla interface! !
!ScintillaView categoriesFor: #foldMargin!accessing!folding!margins!public! !
!ScintillaView categoriesFor: #foldMarginColor!accessing!folding!margins!public! !
!ScintillaView categoriesFor: #foldMarginColor:!accessing!folding!margins!public! !
!ScintillaView categoriesFor: #foldMarginHiColor!accessing!folding!margins!public! !
!ScintillaView categoriesFor: #foldMarginHiColor:!accessing!folding!margins!public! !
!ScintillaView categoriesFor: #foldMarkerStyle!accessing!folding!markers!public! !
!ScintillaView categoriesFor: #foldMarkerStyle:!accessing!folding!markers!public! !
!ScintillaView categoriesFor: #forecolor:!accessing!public! !
!ScintillaView categoriesFor: #formatRectangle!accessing!public! !
!ScintillaView categoriesFor: #formFeed!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #getDirectPointer!accessing!private! !
!ScintillaView categoriesFor: #goto:!caret!public! !
!ScintillaView categoriesFor: #gotoLine:!caret!commands!public! !
!ScintillaView categoriesFor: #handle:!accessing!private! !
!ScintillaView categoriesFor: #hangingIndent!**auto generated**!accessing!public!scintilla interface! !
!ScintillaView categoriesFor: #hangingIndent:!**auto generated**!accessing!public!scintilla interface! !
!ScintillaView categoriesFor: #hasFoldMargin!folding!margins!public!testing! !
!ScintillaView categoriesFor: #hasFoldMargin:!folding!margins!public! !
!ScintillaView categoriesFor: #hasIndentationGuides!public!tabs & indentation guides! !
!ScintillaView categoriesFor: #hasIndentationGuides:!public!tabs & indentation guides! !
!ScintillaView categoriesFor: #hasLineNumbers!public!testing! !
!ScintillaView categoriesFor: #hasLineNumbers:!commands!margins!public! !
!ScintillaView categoriesFor: #hasVisibleLineEndings!**auto generated**!line endings!public!scintilla interface!testing! !
!ScintillaView categoriesFor: #hasVisibleLineEndings:!**auto generated**!line endings!public!scintilla interface!testing! !
!ScintillaView categoriesFor: #hideExtraIndicators!helpers!private! !
!ScintillaView categoriesFor: #hideIndicators!helpers!indicators!private! !
!ScintillaView categoriesFor: #hideSelection:!**auto generated**!public!scintilla interface!selection! !
!ScintillaView categoriesFor: #highlightBracesAt:and:!brace highlighting!public! !
!ScintillaView categoriesFor: #highlightCallTipFrom:to:!call tips!public!scintilla interface! !
!ScintillaView categoriesFor: #highlightFindMatch:!private!searching & replacing! !
!ScintillaView categoriesFor: #highlightGuide!**auto generated**!accessing!public!scintilla interface!tabs & indentation guides! !
!ScintillaView categoriesFor: #highlightGuide:!**auto generated**!accessing!public!scintilla interface!tabs & indentation guides! !
!ScintillaView categoriesFor: #highlightMismatchedBrace:!brace highlighting!public! !
!ScintillaView categoriesFor: #highlightRange:withIndicator:!indicators!public! !
!ScintillaView categoriesFor: #hoverTime!**auto generated**!accessing!public!scintilla interface! !
!ScintillaView categoriesFor: #hoverTime:!**auto generated**!accessing!public!scintilla interface! !
!ScintillaView categoriesFor: #idOfStyleNamed:!helpers!private! !
!ScintillaView categoriesFor: #indent!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #indentation!**auto generated**!accessing!public!scintilla interface!tabs & indentation guides! !
!ScintillaView categoriesFor: #indentation:!**auto generated**!accessing!public!scintilla interface!tabs & indentation guides! !
!ScintillaView categoriesFor: #indentationGuides!public!tabs & indentation guides! !
!ScintillaView categoriesFor: #indentationGuides:!public!tabs & indentation guides! !
!ScintillaView categoriesFor: #indentationOfLine:!public!tabs & indentation guides! !
!ScintillaView categoriesFor: #indicatorCount!constants!indicators!public! !
!ScintillaView categoriesFor: #indicatorDefinitions!indicators!public! !
!ScintillaView categoriesFor: #indicatorDefinitions:!indicators!public! !
!ScintillaView categoriesFor: #indicatorIdFromName:!indicators!private! !
!ScintillaView categoriesFor: #indicatorMaskAt:!indicators!public! !
!ScintillaView categoriesFor: #indicators!indicators!public! !
!ScintillaView categoriesFor: #indicators:!indicators!public! !
!ScintillaView categoriesFor: #indicatorsAt:!accessing!public! !
!ScintillaView categoriesFor: #indicatorStyles!public! !
!ScintillaView categoriesFor: #indicatorStyles:!public! !
!ScintillaView categoriesFor: #indicatorsUnder:!event handling!private! !
!ScintillaView categoriesFor: #initialize!initializing!public! !
!ScintillaView categoriesFor: #initializeControl!initializing!private! !
!ScintillaView categoriesFor: #insertText:at:!public!text retrieval & modification! !
!ScintillaView categoriesFor: #insertText:from:!**auto generated**!public!scintilla interface!text retrieval & modification! !
!ScintillaView categoriesFor: #invalidateStyling!public!styling! !
!ScintillaView categoriesFor: #isActiveHotspotUnderlined!**auto generated**!caret, selection, and hotspot styles!public!scintilla interface! !
!ScintillaView categoriesFor: #isActiveHotspotUnderlined:!**auto generated**!caret, selection, and hotspot styles!public!scintilla interface! !
!ScintillaView categoriesFor: #isAutoCompletionActive!**auto generated**!autocompletion!public!scintilla interface!testing! !
!ScintillaView categoriesFor: #isAutoCompletionCancelledAtStart!**auto generated**!autocompletion!public!scintilla interface!testing! !
!ScintillaView categoriesFor: #isAutoCompletionCancelledAtStart:!**auto generated**!accessing!autocompletion!public!scintilla interface! !
!ScintillaView categoriesFor: #isAutoCompletionCancelledWhenNoMatch!**auto generated**!autocompletion!public!scintilla interface!testing! !
!ScintillaView categoriesFor: #isAutoCompletionCancelledWhenNoMatch:!**auto generated**!accessing!autocompletion!public!scintilla interface! !
!ScintillaView categoriesFor: #isAutoCompletionCaseInsensitive!**auto generated**!autocompletion!public!scintilla interface!testing! !
!ScintillaView categoriesFor: #isAutoCompletionCaseInsensitive:!**auto generated**!accessing!autocompletion!public!scintilla interface! !
!ScintillaView categoriesFor: #isAutoCompletionSingleMatchChosen!**auto generated**!autocompletion!public!scintilla interface!testing! !
!ScintillaView categoriesFor: #isAutoCompletionSingleMatchChosen:!**auto generated**!autocompletion!public!scintilla interface! !
!ScintillaView categoriesFor: #isAutoCompletionTruncating!**auto generated**!autocompletion!public!scintilla interface!testing! !
!ScintillaView categoriesFor: #isAutoCompletionTruncating:!**auto generated**!accessing!autocompletion!public!scintilla interface! !
!ScintillaView categoriesFor: #isBackgroundDwellEnabled!brace highlighting!public! !
!ScintillaView categoriesFor: #isBackgroundDwellEnabled:!brace highlighting!public! !
!ScintillaView categoriesFor: #isBraceAt:!brace highlighting!private!testing! !
!ScintillaView categoriesFor: #isBraceHighlightingEnabled!brace highlighting!public! !
!ScintillaView categoriesFor: #isBraceHighlightingEnabled:!brace highlighting!public! !
!ScintillaView categoriesFor: #isCallTipActive!**auto generated**!call tips!public!scintilla interface!testing! !
!ScintillaView categoriesFor: #isCaretSticky!**auto generated**!caret, selection, and hotspot styles!public!scintilla interface! !
!ScintillaView categoriesFor: #isCaretSticky:!**auto generated**!caret, selection, and hotspot styles!public!scintilla interface! !
!ScintillaView categoriesFor: #isCurrentLineHighlighted!**auto generated**!caret, selection, and hotspot styles!public!scintilla interface!testing! !
!ScintillaView categoriesFor: #isCurrentLineHighlighted:!**auto generated**!accessing!caret, selection, and hotspot styles!public!scintilla interface! !
!ScintillaView categoriesFor: #isCurrentLineMarkedWith:!markers!public!testing! !
!ScintillaView categoriesFor: #isDrawingBuffered!**auto generated**!public!scintilla interface!testing! !
!ScintillaView categoriesFor: #isDrawingBuffered:!**auto generated**!accessing!public!scintilla interface! !
!ScintillaView categoriesFor: #isDrawingTwoPhase!**auto generated**!public!scintilla interface!testing! !
!ScintillaView categoriesFor: #isDrawingTwoPhase:!**auto generated**!accessing!public!scintilla interface! !
!ScintillaView categoriesFor: #isFoldingEnabled!folding!public!testing! !
!ScintillaView categoriesFor: #isFoldingEnabled:!accessing!folding!public! !
!ScintillaView categoriesFor: #isIndicator:setAt:!indicators!public! !
!ScintillaView categoriesFor: #isLine:folded:!folding!public!scintilla interface! !
!ScintillaView categoriesFor: #isLine:markedWith:!markers!public!testing! !
!ScintillaView categoriesFor: #isLineVisible:!folding!public!testing! !
!ScintillaView categoriesFor: #isOvertypeEnabled!**auto generated**!overtype!public!scintilla interface!testing! !
!ScintillaView categoriesFor: #isOvertypeEnabled:!**auto generated**!overtype!public!scintilla interface!testing! !
!ScintillaView categoriesFor: #isScrollWidthTracking!**auto generated**!public!scintilla interface!scrolling! !
!ScintillaView categoriesFor: #isScrollWidthTracking:!**auto generated**!public!scintilla interface!scrolling! !
!ScintillaView categoriesFor: #isSelectionBackcolorExtendedToEndOfLine!**auto generated**!caret, selection, and hotspot styles!public!scintilla interface! !
!ScintillaView categoriesFor: #isSelectionBackcolorExtendedToEndOfLine:!**auto generated**!caret, selection, and hotspot styles!public!scintilla interface! !
!ScintillaView categoriesFor: #isSelectionKept!public!testing! !
!ScintillaView categoriesFor: #isSelectionRectangular!**auto generated**!public!scintilla interface!selection!testing! !
!ScintillaView categoriesFor: #isStylingEnabled!public!styling!testing! !
!ScintillaView categoriesFor: #isStylingEnabled:!accessing!public!styling! !
!ScintillaView categoriesFor: #isTextModified!**auto generated**!public!scintilla interface!testing! !
!ScintillaView categoriesFor: #isTextModified:!modes!private! !
!ScintillaView categoriesFor: #isUndoEnabled!**auto generated**!public!scintilla interface!testing!undo & redo! !
!ScintillaView categoriesFor: #isUndoEnabled:!**auto generated**!accessing!public!scintilla interface!undo & redo! !
!ScintillaView categoriesFor: #isUsingPalette!**auto generated**!other settings!public!scintilla interface!testing! !
!ScintillaView categoriesFor: #isUsingPalette:!**auto generated**!other settings!public!scintilla interface! !
!ScintillaView categoriesFor: #isUsingTabs!**auto generated**!public!scintilla interface!tabs & indentation guides!testing! !
!ScintillaView categoriesFor: #isUsingTabs:!**auto generated**!accessing!public!scintilla interface!tabs & indentation guides! !
!ScintillaView categoriesFor: #joinTarget!**auto generated**!commands!line wrapping!public!scintilla interface! !
!ScintillaView categoriesFor: #keyBindings!accessing!key bindings!public! !
!ScintillaView categoriesFor: #keyBindings:!accessing!key bindings!public! !
!ScintillaView categoriesFor: #keyboardCommands!commands!public! !
!ScintillaView categoriesFor: #layoutCachingMode!accessing!line wrapping!public! !
!ScintillaView categoriesFor: #layoutCachingMode:!accessing!line wrapping!public! !
!ScintillaView categoriesFor: #lexer!accessing!lexer!public! !
!ScintillaView categoriesFor: #lexer:!accessing!lexer!public! !
!ScintillaView categoriesFor: #lineCount!**auto generated**!accessing!public!scintilla interface! !
!ScintillaView categoriesFor: #lineHeight:!enquiries!public! !
!ScintillaView categoriesFor: #lineLength:!accessing!public! !
!ScintillaView categoriesFor: #lineLengthFromPosition:!accessing!private! !
!ScintillaView categoriesFor: #lineNumberMargin!accessing!margins!public! !
!ScintillaView categoriesFor: #lineRange:!accessing!public! !
!ScintillaView categoriesFor: #lineScroll!public!scrolling! !
!ScintillaView categoriesFor: #lineScrollBy:!public!scrolling! !
!ScintillaView categoriesFor: #linesOnScreen!**auto generated**!enquiries!public!scintilla interface! !
!ScintillaView categoriesFor: #marginCount!constants!margins!private! !
!ScintillaView categoriesFor: #margins!accessing!margins!public! !
!ScintillaView categoriesFor: #margins:!accessing!margins!public! !
!ScintillaView categoriesFor: #marginWidths!accessing!private! !
!ScintillaView categoriesFor: #markerDefinitions!accessing!markers!public! !
!ScintillaView categoriesFor: #markerDefinitions:!accessing!markers!public! !
!ScintillaView categoriesFor: #markers!accessing!markers!public! !
!ScintillaView categoriesFor: #markers:!accessing!markers!public! !
!ScintillaView categoriesFor: #markerTypesOnLine:!markers!public! !
!ScintillaView categoriesFor: #maxCompletionListHeight!**auto generated**!accessing!autocompletion!public!scintilla interface! !
!ScintillaView categoriesFor: #maxCompletionListHeight:!**auto generated**!accessing!autocompletion!public!scintilla interface! !
!ScintillaView categoriesFor: #maxCompletionListWidth!**auto generated**!accessing!autocompletion!public!scintilla interface! !
!ScintillaView categoriesFor: #maxCompletionListWidth:!**auto generated**!accessing!autocompletion!public!scintilla interface! !
!ScintillaView categoriesFor: #maxStyle!constants!public!style definition! !
!ScintillaView categoriesFor: #modificationEventMask!accessing!public! !
!ScintillaView categoriesFor: #modificationEventMask:!accessing!public! !
!ScintillaView categoriesFor: #modifyText:!private!text retrieval & modification! !
!ScintillaView categoriesFor: #moveCaretInsideView!**auto generated**!caret!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #moveDown!**auto generated**!caret!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #moveLeft!**auto generated**!caret!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #movePageDown!**auto generated**!caret!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #movePageUp!**auto generated**!caret!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #moveParaDown!**auto generated**!caret!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #moveParaUp!**auto generated**!caret!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #moveRight!**auto generated**!caret!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #moveStutteredPageDown!**auto generated**!caret!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #moveStutteredPageUp!**auto generated**!caret!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #moveToEndOfDisplayLine!**auto generated**!caret!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #moveToEndOfDocument!**auto generated**!caret!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #moveToEndOfLine!**auto generated**!caret!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #moveToEndOfNextWord!**auto generated**!caret!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #moveToEndOfPreviousWord!**auto generated**!caret!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #moveToEndOfWord!**auto generated**!caret!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #moveToEndOfWordPart!**auto generated**!caret!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #moveToEndOfWrappedLine!**auto generated**!caret!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #moveToStartOfDisplayLine!**auto generated**!caret!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #moveToStartOfDocument!**auto generated**!caret!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #moveToStartOfLine!**auto generated**!caret!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #moveToStartOfWord!**auto generated**!caret!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #moveToStartOfWordPart!**auto generated**!caret!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #moveToStartOfWrappedLine!**auto generated**!caret!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #moveToVcHome!**auto generated**!caret!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #moveToWrappedVcHome!**auto generated**!caret!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #moveUp!**auto generated**!caret!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #newLine!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #nmNotify:!event handling-win32!private! !
!ScintillaView categoriesFor: #onEraseRequired:!event handling!public! !
!ScintillaView categoriesFor: #onKillFocus!event handling!public! !
!ScintillaView categoriesFor: #onSetFocus!event handling!public! !
!ScintillaView categoriesFor: #onViewCreated!event handling!public! !
!ScintillaView categoriesFor: #passwordCharacter!accessing!public! !
!ScintillaView categoriesFor: #passwordCharacter:!accessing!public! !
!ScintillaView categoriesFor: #pasteClipboard!**auto generated**!clipboard operations!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #performUndoableAction:!public!undo & redo! !
!ScintillaView categoriesFor: #plainText!accessing!public!text retrieval & modification! !
!ScintillaView categoriesFor: #plainText:!accessing!private!text retrieval & modification! !
!ScintillaView categoriesFor: #plainTextFrom:to:!accessing!public!text retrieval & modification! !
!ScintillaView categoriesFor: #positionCacheSize!**auto generated**!public!scintilla interface! !
!ScintillaView categoriesFor: #positionCacheSize:!**auto generated**!public!scintilla interface! !
!ScintillaView categoriesFor: #positionOfChar:!accessing!public! !
!ScintillaView categoriesFor: #preservingStylingPositionDo:!helpers!private! !
!ScintillaView categoriesFor: #printColourMode!**auto generated**!accessing!printing!public!scintilla interface! !
!ScintillaView categoriesFor: #printColourMode:!**auto generated**!accessing!printing!public!scintilla interface! !
!ScintillaView categoriesFor: #printMagnification!**auto generated**!accessing!printing!public!scintilla interface! !
!ScintillaView categoriesFor: #printMagnification:!**auto generated**!accessing!printing!public!scintilla interface! !
!ScintillaView categoriesFor: #queryCommand:!commands!private! !
!ScintillaView categoriesFor: #rangeOfIndicator:at:!indicators!public! !
!ScintillaView categoriesFor: #redo!**auto generated**!commands!public!scintilla interface!undo & redo! !
!ScintillaView categoriesFor: #rememberCaretX!**auto generated**!caret!public!scintilla interface! !
!ScintillaView categoriesFor: #removeAllMarkers!markers!public! !
!ScintillaView categoriesFor: #removeAllStyling!**auto generated**!public!scintilla interface!styling! !
!ScintillaView categoriesFor: #removeBraceHighlight!brace highlighting!public! !
!ScintillaView categoriesFor: #removeKeyBinding:!key bindings!public! !
!ScintillaView categoriesFor: #removeMarker:!markers!public! !
!ScintillaView categoriesFor: #removeMarkersOfType:!markers!public! !
!ScintillaView categoriesFor: #removeStylingFrom:to:!public!styling! !
!ScintillaView categoriesFor: #replaceTarget:!public!searching & replacing! !
!ScintillaView categoriesFor: #requiredLineMarginWidth!helpers!margins!private! !
!ScintillaView categoriesFor: #resetMarkers!helpers!markers!private! !
!ScintillaView categoriesFor: #resetZoom!commands!public!zooming! !
!ScintillaView categoriesFor: #restyleAll!public!styling! !
!ScintillaView categoriesFor: #restyleFrom:to:!public!styling! !
!ScintillaView categoriesFor: #restyleMask!constants!private!styling! !
!ScintillaView categoriesFor: #sciAddRefDocument:!**auto generated**!multiple views!private!scintilla interface! !
!ScintillaView categoriesFor: #sciAddStyledText:c:!**auto generated**!private!scintilla interface!text retrieval & modification! !
!ScintillaView categoriesFor: #sciAllocate:!**auto generated**!private!scintilla interface!text retrieval & modification! !
!ScintillaView categoriesFor: #sciAppendText:text:!**auto generated**!private!scintilla interface!text retrieval & modification! !
!ScintillaView categoriesFor: #sciAssignCmdKey:msg:!**auto generated**!key bindings!private!scintilla interface! !
!ScintillaView categoriesFor: #sciAutoCGetCurrent!**auto generated**!autocompletion!private!scintilla interface! !
!ScintillaView categoriesFor: #sciAutoCGetSeparator!**auto generated**!autocompletion!private!scintilla interface! !
!ScintillaView categoriesFor: #sciAutoCGetTypeSeparator!**auto generated**!autocompletion!private!scintilla interface! !
!ScintillaView categoriesFor: #sciAutoCPosStart!**auto generated**!autocompletion!private!scintilla interface! !
!ScintillaView categoriesFor: #sciAutoCSetFillUps:!**auto generated**!autocompletion!private!scintilla interface! !
!ScintillaView categoriesFor: #sciAutoCSetSeparator:!**auto generated**!autocompletion!private!scintilla interface! !
!ScintillaView categoriesFor: #sciAutoCSetTypeSeparator:!**auto generated**!autocompletion!private!scintilla interface! !
!ScintillaView categoriesFor: #sciAutoCShow:itemList:!**auto generated**!autocompletion!private!scintilla interface! !
!ScintillaView categoriesFor: #sciAutoCStops:!**auto generated**!autocompletion!private!scintilla interface! !
!ScintillaView categoriesFor: #sciBraceBadLight:!**auto generated**!brace highlighting!private!scintilla interface! !
!ScintillaView categoriesFor: #sciBraceHighlight:pos2:!**auto generated**!brace highlighting!private!scintilla interface! !
!ScintillaView categoriesFor: #sciBraceMatch:!**auto generated**!brace highlighting!private!scintilla interface! !
!ScintillaView categoriesFor: #sciCallTipPosStart!**auto generated**!call tips!private!scintilla interface! !
!ScintillaView categoriesFor: #sciCallTipSetBack:!**auto generated**!call tips!private!scintilla interface! !
!ScintillaView categoriesFor: #sciCallTipSetFore:!**auto generated**!call tips!private!scintilla interface! !
!ScintillaView categoriesFor: #sciCallTipSetForeHlt:!**auto generated**!call tips!private!scintilla interface! !
!ScintillaView categoriesFor: #sciCallTipSetHlt:end:!**auto generated**!call tips!private!scintilla interface! !
!ScintillaView categoriesFor: #sciCallTipShow:definition:!**auto generated**!call tips!private!scintilla interface! !
!ScintillaView categoriesFor: #sciCallTipUseStyle:!**auto generated**!call tips!private!scintilla interface! !
!ScintillaView categoriesFor: #sciCanPaste!**auto generated**!clipboard operations!private!scintilla interface! !
!ScintillaView categoriesFor: #sciClearAllCmdKeys!**auto generated**!key bindings!private!scintilla interface! !
!ScintillaView categoriesFor: #sciClearCmdKey:!**auto generated**!key bindings!private!scintilla interface! !
!ScintillaView categoriesFor: #sciColourise:end:!**auto generated**!lexer!private!scintilla interface! !
!ScintillaView categoriesFor: #sciConvertEOLs:!**auto generated**!line endings!private!scintilla interface! !
!ScintillaView categoriesFor: #sciCopyRange:end:!**auto generated**!clipboard operations!private!scintilla interface! !
!ScintillaView categoriesFor: #sciCreateDocument!**auto generated**!multiple views!private!scintilla interface! !
!ScintillaView categoriesFor: #sciDocLineFromVisible:!**auto generated**!folding!private!scintilla interface! !
!ScintillaView categoriesFor: #sciEncodedFromUTF8:encoded:!**auto generated**!private!scintilla interface!text retrieval & modification! !
!ScintillaView categoriesFor: #sciEnsureVisible:!**auto generated**!folding!private!scintilla interface! !
!ScintillaView categoriesFor: #sciEnsureVisibleEnforcePolicy:!**auto generated**!folding!private!scintilla interface! !
!ScintillaView categoriesFor: #sciFindColumn:column:!**auto generated**!private!scintilla interface!selection! !
!ScintillaView categoriesFor: #sciFindText:ft:!**auto generated**!private!scintilla interface!searching & replacing! !
!ScintillaView categoriesFor: #sciFormatRange:fr:!**auto generated**!printing!private!scintilla interface! !
!ScintillaView categoriesFor: #sciGetAnchor!**auto generated**!private!scintilla interface!selection! !
!ScintillaView categoriesFor: #sciGetCaretLineBackAlpha!**auto generated**!caret, selection, and hotspot styles!private!scintilla interface! !
!ScintillaView categoriesFor: #sciGetCaretStyle!**auto generated**!caret, selection, and hotspot styles!private!scintilla interface! !
!ScintillaView categoriesFor: #sciGetCharAt:!**auto generated**!private!scintilla interface!text retrieval & modification! !
!ScintillaView categoriesFor: #sciGetCodePage!**auto generated**!other settings!private!scintilla interface! !
!ScintillaView categoriesFor: #sciGetColumn:!**auto generated**!enquiries!private!scintilla interface! !
!ScintillaView categoriesFor: #sciGetControlCharSymbol!**auto generated**!caret, selection, and hotspot styles!private!scintilla interface! !
!ScintillaView categoriesFor: #sciGetCurLine:text:!**auto generated**!caret!private!scintilla interface! !
!ScintillaView categoriesFor: #sciGetCurrentPos!**auto generated**!caret!private!scintilla interface! !
!ScintillaView categoriesFor: #sciGetDocPointer!**auto generated**!multiple views!private!scintilla interface! !
!ScintillaView categoriesFor: #sciGetEdgeColumn!**auto generated**!long lines!private!scintilla interface! !
!ScintillaView categoriesFor: #sciGetEdgeMode!**auto generated**!long lines!private!scintilla interface! !
!ScintillaView categoriesFor: #sciGetEndAtLastLine!**auto generated**!private!scintilla interface!scrolling! !
!ScintillaView categoriesFor: #sciGetEndStyled!**auto generated**!private!scintilla interface!styling! !
!ScintillaView categoriesFor: #sciGetEOLMode!**auto generated**!line endings!private!scintilla interface! !
!ScintillaView categoriesFor: #sciGetFirstVisibleLine!**auto generated**!enquiries!private!scintilla interface! !
!ScintillaView categoriesFor: #sciGetFoldExpanded:!**auto generated**!folding!private!scintilla interface! !
!ScintillaView categoriesFor: #sciGetFoldLevel:!**auto generated**!folding!private!scintilla interface! !
!ScintillaView categoriesFor: #sciGetFoldParent:!**auto generated**!folding!private!scintilla interface! !
!ScintillaView categoriesFor: #sciGetIndentationGuides!**auto generated**!private!scintilla interface!tabs & indentation guides! !
!ScintillaView categoriesFor: #sciGetLastChild:level:!**auto generated**!folding!private!scintilla interface! !
!ScintillaView categoriesFor: #sciGetLayoutCache!**auto generated**!line wrapping!private!scintilla interface! !
!ScintillaView categoriesFor: #sciGetLexer!**auto generated**!lexer!private!scintilla interface! !
!ScintillaView categoriesFor: #sciGetLine:text:!**auto generated**!private!scintilla interface!text retrieval & modification! !
!ScintillaView categoriesFor: #sciGetLineEndPosition:!**auto generated**!enquiries!private!scintilla interface! !
!ScintillaView categoriesFor: #sciGetLineIndentation:!**auto generated**!private!scintilla interface!tabs & indentation guides! !
!ScintillaView categoriesFor: #sciGetLineIndentPosition:!**auto generated**!private!scintilla interface!tabs & indentation guides! !
!ScintillaView categoriesFor: #sciGetLineSelEndPosition:!**auto generated**!private!scintilla interface!selection! !
!ScintillaView categoriesFor: #sciGetLineSelStartPosition:!**auto generated**!private!scintilla interface!selection! !
!ScintillaView categoriesFor: #sciGetLineState:!**auto generated**!private!scintilla interface!styling! !
!ScintillaView categoriesFor: #sciGetLineVisible:!**auto generated**!folding!private!scintilla interface! !
!ScintillaView categoriesFor: #sciGetMarginLeft!**auto generated**!margins!private!scintilla interface! !
!ScintillaView categoriesFor: #sciGetMarginRight!**auto generated**!margins!private!scintilla interface! !
!ScintillaView categoriesFor: #sciGetMaxLineState!**auto generated**!private!scintilla interface!styling! !
!ScintillaView categoriesFor: #sciGetPasteConvertEndings!**auto generated**!clipboard operations!private!scintilla interface! !
!ScintillaView categoriesFor: #sciGetPrintWrapMode!**auto generated**!printing!private!scintilla interface! !
!ScintillaView categoriesFor: #sciGetProperty:buf:!**auto generated**!lexer!private!scintilla interface! !
!ScintillaView categoriesFor: #sciGetPropertyExpanded:buf:!**auto generated**!lexer!private!scintilla interface! !
!ScintillaView categoriesFor: #sciGetPropertyInt:!**auto generated**!lexer!private!scintilla interface! !
!ScintillaView categoriesFor: #sciGetSearchFlags!**auto generated**!private!scintilla interface!searching & replacing! !
!ScintillaView categoriesFor: #sciGetSelectionEnd!**auto generated**!private!scintilla interface!selection! !
!ScintillaView categoriesFor: #sciGetSelectionMode!**auto generated**!private!scintilla interface!selection! !
!ScintillaView categoriesFor: #sciGetSelectionStart!**auto generated**!private!scintilla interface!selection! !
!ScintillaView categoriesFor: #sciGetSelText:!**auto generated**!private!scintilla interface!selection! !
!ScintillaView categoriesFor: #sciGetStyleBits!**auto generated**!private!scintilla interface!style definition! !
!ScintillaView categoriesFor: #sciGetStyleBitsNeeded!**auto generated**!lexer!private!scintilla interface! !
!ScintillaView categoriesFor: #sciGetStyledText:!**auto generated**!private!scintilla interface!text retrieval & modification! !
!ScintillaView categoriesFor: #sciGetTargetEnd!**auto generated**!private!scintilla interface!searching & replacing! !
!ScintillaView categoriesFor: #sciGetTargetStart!**auto generated**!private!scintilla interface!searching & replacing! !
!ScintillaView categoriesFor: #sciGetText:text:!**auto generated**!private!scintilla interface!text retrieval & modification! !
!ScintillaView categoriesFor: #sciGetTextRange:!**auto generated**!private!scintilla interface!text retrieval & modification! !
!ScintillaView categoriesFor: #sciGetWrapMode!**auto generated**!line wrapping!private!scintilla interface! !
!ScintillaView categoriesFor: #sciGetWrapVisualFlags!**auto generated**!line wrapping!private!scintilla interface! !
!ScintillaView categoriesFor: #sciGetWrapVisualFlagsLocation!**auto generated**!line wrapping!private!scintilla interface! !
!ScintillaView categoriesFor: #sciGotoLine:!**auto generated**!caret!private!scintilla interface! !
!ScintillaView categoriesFor: #sciGotoPos:!**auto generated**!caret!private!scintilla interface! !
!ScintillaView categoriesFor: #sciHideLines:lineEnd:!**auto generated**!folding!private!scintilla interface! !
!ScintillaView categoriesFor: #sciIndicatorAllOnFor:!**auto generated**!indicators!private!scintilla interface! !
!ScintillaView categoriesFor: #sciIndicatorClearRange:clearLength:!**auto generated**!indicators!private!scintilla interface! !
!ScintillaView categoriesFor: #sciIndicatorEnd:position:!**auto generated**!indicators!private!scintilla interface! !
!ScintillaView categoriesFor: #sciIndicatorFillRange:fillLength:!**auto generated**!indicators!private!scintilla interface! !
!ScintillaView categoriesFor: #sciIndicatorStart:position:!**auto generated**!indicators!private!scintilla interface! !
!ScintillaView categoriesFor: #sciIndicatorValueAt:position:!**auto generated**!indicators!private!scintilla interface! !
!ScintillaView categoriesFor: #sciInsertText:text:!**auto generated**!private!scintilla interface!text retrieval & modification! !
!ScintillaView categoriesFor: #sciLineScroll:lines:!**auto generated**!private!scintilla interface!scrolling! !
!ScintillaView categoriesFor: #sciLoadLexerLibrary:!**auto generated**!lexer!private!scintilla interface! !
!ScintillaView categoriesFor: #sciMarkerAddSet:set:!**auto generated**!markers!private!scintilla interface! !
!ScintillaView categoriesFor: #sciMarkerDefinePixmap:pixmap:!**auto generated**!markers!private!scintilla interface! !
!ScintillaView categoriesFor: #sciMarkerDeleteHandle:!**auto generated**!markers!private!scintilla interface! !
!ScintillaView categoriesFor: #sciMarkerGet:!**auto generated**!markers!private!scintilla interface! !
!ScintillaView categoriesFor: #sciMarkerLineFromHandle:!**auto generated**!markers!private!scintilla interface! !
!ScintillaView categoriesFor: #sciMarkerNext:markerMask:!**auto generated**!markers!private!scintilla interface! !
!ScintillaView categoriesFor: #sciMarkerPrevious:markerMask:!**auto generated**!markers!private!scintilla interface! !
!ScintillaView categoriesFor: #sciMarkerSetAlpha:alpha:!**auto generated**!private!scintilla interface! !
!ScintillaView categoriesFor: #sciPointXFromPosition:!**auto generated**!enquiries!private!scintilla interface! !
!ScintillaView categoriesFor: #sciPointYFromPosition:!**auto generated**!enquiries!private!scintilla interface! !
!ScintillaView categoriesFor: #sciPositionAfter:!**auto generated**!enquiries!private!scintilla interface! !
!ScintillaView categoriesFor: #sciPositionBefore:!**auto generated**!enquiries!private!scintilla interface! !
!ScintillaView categoriesFor: #sciPositionFromPoint:y:!**auto generated**!enquiries!private!scintilla interface! !
!ScintillaView categoriesFor: #sciPositionFromPointClose:y:!**auto generated**!enquiries!private!scintilla interface! !
!ScintillaView categoriesFor: #sciRegisterImage:xpmData:!**auto generated**!autocompletion!private!scintilla interface! !
!ScintillaView categoriesFor: #sciReleaseDocument:!**auto generated**!multiple views!private!scintilla interface! !
!ScintillaView categoriesFor: #sciReplaceSel:!**auto generated**!private!scintilla interface!text retrieval & modification! !
!ScintillaView categoriesFor: #sciReplaceTarget:text:!**auto generated**!private!scintilla interface!searching & replacing! !
!ScintillaView categoriesFor: #sciReplaceTargetRE:text:!**auto generated**!private!scintilla interface!searching & replacing! !
!ScintillaView categoriesFor: #sciSearchAnchor!**auto generated**!private!scintilla interface!searching & replacing! !
!ScintillaView categoriesFor: #sciSearchInTarget:text:!**auto generated**!private!scintilla interface!searching & replacing! !
!ScintillaView categoriesFor: #sciSearchNext:text:!**auto generated**!private!scintilla interface!searching & replacing! !
!ScintillaView categoriesFor: #sciSearchPrev:text:!**auto generated**!private!scintilla interface!searching & replacing! !
!ScintillaView categoriesFor: #sciSetAnchor:!**auto generated**!private!scintilla interface!selection! !
!ScintillaView categoriesFor: #sciSetCaretFore:!**auto generated**!caret, selection, and hotspot styles!private!scintilla interface! !
!ScintillaView categoriesFor: #sciSetCaretLineBack:!**auto generated**!caret, selection, and hotspot styles!private!scintilla interface! !
!ScintillaView categoriesFor: #sciSetCaretLineBackAlpha:!**auto generated**!caret, selection, and hotspot styles!private!scintilla interface! !
!ScintillaView categoriesFor: #sciSetCaretPolicy:caretSlop:!**auto generated**!caret, selection, and hotspot styles!private!scintilla interface! !
!ScintillaView categoriesFor: #sciSetCaretStyle:!**auto generated**!caret, selection, and hotspot styles!private!scintilla interface! !
!ScintillaView categoriesFor: #sciSetCaretWidth:!**auto generated**!caret, selection, and hotspot styles!private!scintilla interface! !
!ScintillaView categoriesFor: #sciSetCharsDefault!**auto generated**!private!scintilla interface!white space! !
!ScintillaView categoriesFor: #sciSetCodePage:!**auto generated**!other settings!private!scintilla interface! !
!ScintillaView categoriesFor: #sciSetControlCharSymbol:!**auto generated**!caret, selection, and hotspot styles!private!scintilla interface! !
!ScintillaView categoriesFor: #sciSetDocPointer:!**auto generated**!multiple views!private!scintilla interface! !
!ScintillaView categoriesFor: #sciSetEdgeColumn:!**auto generated**!long lines!private!scintilla interface! !
!ScintillaView categoriesFor: #sciSetEdgeMode:!**auto generated**!long lines!private!scintilla interface! !
!ScintillaView categoriesFor: #sciSetEOLMode:!**auto generated**!line endings!private!scintilla interface! !
!ScintillaView categoriesFor: #sciSetFoldExpanded:expanded:!**auto generated**!folding!private!scintilla interface! !
!ScintillaView categoriesFor: #sciSetFoldFlags:!**auto generated**!folding!private!scintilla interface! !
!ScintillaView categoriesFor: #sciSetFoldLevel:level:!**auto generated**!folding!private!scintilla interface! !
!ScintillaView categoriesFor: #sciSetFoldMarginColour:back:!**auto generated**!margins!private!scintilla interface! !
!ScintillaView categoriesFor: #sciSetFoldMarginHiColour:fore:!**auto generated**!margins!public!scintilla interface! !
!ScintillaView categoriesFor: #sciSetHotspotActiveBack:back:!**auto generated**!caret, selection, and hotspot styles!private!scintilla interface! !
!ScintillaView categoriesFor: #sciSetHotspotActiveFore:fore:!**auto generated**!caret, selection, and hotspot styles!private!scintilla interface! !
!ScintillaView categoriesFor: #sciSetHScrollBar:!**auto generated**!private!scintilla interface!scrolling! !
!ScintillaView categoriesFor: #sciSetIndentationGuides:!**auto generated**!private!scintilla interface!tabs & indentation guides! !
!ScintillaView categoriesFor: #sciSetKeyWords:keyWords:!**auto generated**!lexer!private!scintilla interface! !
!ScintillaView categoriesFor: #sciSetLayoutCache:!**auto generated**!line wrapping!private!scintilla interface! !
!ScintillaView categoriesFor: #sciSetLengthForEncode:!**auto generated**!private!scintilla interface!text retrieval & modification! !
!ScintillaView categoriesFor: #sciSetLexer:!**auto generated**!lexer!private!scintilla interface! !
!ScintillaView categoriesFor: #sciSetLexerLanguage:!**auto generated**!lexer!private!scintilla interface! !
!ScintillaView categoriesFor: #sciSetLineIndentation:indentSize:!**auto generated**!private!scintilla interface!tabs & indentation guides! !
!ScintillaView categoriesFor: #sciSetLineState:state:!**auto generated**!private!scintilla interface!styling! !
!ScintillaView categoriesFor: #sciSetMarginLeft:!**auto generated**!margins!private!scintilla interface! !
!ScintillaView categoriesFor: #sciSetMarginRight:!**auto generated**!margins!private!scintilla interface! !
!ScintillaView categoriesFor: #sciSetModEventMask:!**auto generated**!notifications!private!scintilla interface! !
!ScintillaView categoriesFor: #sciSetPasteConvertEndings:!**auto generated**!clipboard operations!private!scintilla interface! !
!ScintillaView categoriesFor: #sciSetPrintWrapMode:!**auto generated**!printing!private!scintilla interface! !
!ScintillaView categoriesFor: #sciSetProperty:value:!**auto generated**!lexer!private!scintilla interface! !
!ScintillaView categoriesFor: #sciSetSavePoint!**auto generated**!private!scintilla interface!text retrieval & modification! !
!ScintillaView categoriesFor: #sciSetSearchFlags:!**auto generated**!private!scintilla interface!searching & replacing! !
!ScintillaView categoriesFor: #sciSetSelBack:back:!**auto generated**!caret, selection, and hotspot styles!private!scintilla interface! !
!ScintillaView categoriesFor: #sciSetSelectionMode:!**auto generated**!private!scintilla interface!selection! !
!ScintillaView categoriesFor: #sciSetSelFore:fore:!**auto generated**!caret, selection, and hotspot styles!private!scintilla interface! !
!ScintillaView categoriesFor: #sciSetStyleBits:!**auto generated**!private!scintilla interface!style definition! !
!ScintillaView categoriesFor: #sciSetStylingEx:styles:!**auto generated**!private!scintilla interface!styling! !
!ScintillaView categoriesFor: #sciSetTargetEnd:!**auto generated**!private!scintilla interface!searching & replacing! !
!ScintillaView categoriesFor: #sciSetTargetStart:!**auto generated**!private!scintilla interface!searching & replacing! !
!ScintillaView categoriesFor: #sciSetText:!**auto generated**!private!scintilla interface!text retrieval & modification! !
!ScintillaView categoriesFor: #sciSetVisiblePolicy:visibleSlop:!**auto generated**!private!scintilla interface!scrolling! !
!ScintillaView categoriesFor: #sciSetWhitespaceBack:back:!**auto generated**!private!scintilla interface!white space! !
!ScintillaView categoriesFor: #sciSetWhitespaceChars:!**auto generated**!private!scintilla interface!white space! !
!ScintillaView categoriesFor: #sciSetWhitespaceFore:fore:!**auto generated**!private!scintilla interface!white space! !
!ScintillaView categoriesFor: #sciSetWordChars:!**auto generated**!other settings!private!scintilla interface! !
!ScintillaView categoriesFor: #sciSetWrapMode:!**auto generated**!line wrapping!private!scintilla interface! !
!ScintillaView categoriesFor: #sciSetWrapVisualFlags:!**auto generated**!line wrapping!private!scintilla interface! !
!ScintillaView categoriesFor: #sciSetWrapVisualFlagsLocation:!**auto generated**!line wrapping!private!scintilla interface! !
!ScintillaView categoriesFor: #sciSetXCaretPolicy:caretSlop:!**auto generated**!private!scintilla interface!scrolling! !
!ScintillaView categoriesFor: #sciSetYCaretPolicy:caretSlop:!**auto generated**!private!scintilla interface!scrolling! !
!ScintillaView categoriesFor: #sciShowLines:lineEnd:!**auto generated**!folding!private!scintilla interface! !
!ScintillaView categoriesFor: #sciStartStyling:mask:!**auto generated**!private!scintilla interface!styling! !
!ScintillaView categoriesFor: #sciStyleClearAll!**auto generated**!private!scintilla interface!style definition! !
!ScintillaView categoriesFor: #sciTargetAsUTF8:!**auto generated**!private!scintilla interface!text retrieval & modification! !
!ScintillaView categoriesFor: #sciTextHeight:!**auto generated**!enquiries!private!scintilla interface! !
!ScintillaView categoriesFor: #sciTextWidth:text:!**auto generated**!enquiries!private!scintilla interface! !
!ScintillaView categoriesFor: #sciToggleCaretSticky!**auto generated**!caret, selection, and hotspot styles!private!scintilla interface! !
!ScintillaView categoriesFor: #sciToggleFold:!**auto generated**!folding!private!scintilla interface! !
!ScintillaView categoriesFor: #sciUserListShow:itemList:!**auto generated**!private!scintilla interface!user lists! !
!ScintillaView categoriesFor: #sciVisibleFromDocLine:!**auto generated**!folding!private!scintilla interface! !
!ScintillaView categoriesFor: #sciWordEndPosition:onlyWordCharacters:!**auto generated**!enquiries!private!scintilla interface! !
!ScintillaView categoriesFor: #sciWordStartPosition:onlyWordCharacters:!**auto generated**!enquiries!private!scintilla interface! !
!ScintillaView categoriesFor: #sciWrapCount:!**auto generated**!line wrapping!private!scintilla interface! !
!ScintillaView categoriesFor: #scnAutoCSelection:!autocompletion!event handling-scintilla!private! !
!ScintillaView categoriesFor: #scnCallTipClick:!event handling-scintilla!notifications!private! !
!ScintillaView categoriesFor: #scnCharAdded:!event handling-scintilla!notifications!private! !
!ScintillaView categoriesFor: #scnDoubleClick:!event handling-scintilla!notifications!private! !
!ScintillaView categoriesFor: #scnDwellEnd:!event handling-scintilla!notifications!private! !
!ScintillaView categoriesFor: #scnDwellStart:!event handling-scintilla!notifications!private! !
!ScintillaView categoriesFor: #scnHotSpotClick:!event handling-scintilla!notifications!private! !
!ScintillaView categoriesFor: #scnHotSpotDoubleClick:!event handling-scintilla!notifications!private! !
!ScintillaView categoriesFor: #scnIndicatorClick:!event handling-scintilla!private! !
!ScintillaView categoriesFor: #scnIndicatorRelease:!event handling-scintilla!private! !
!ScintillaView categoriesFor: #scnKey:!event handling-scintilla!notifications!private! !
!ScintillaView categoriesFor: #scnMacroRecord:!event handling-scintilla!notifications!private! !
!ScintillaView categoriesFor: #scnMarginClick:!event handling-scintilla!margins!notifications!private! !
!ScintillaView categoriesFor: #scnModified:!event handling-scintilla!notifications!private! !
!ScintillaView categoriesFor: #scnModifyAttemptRO:!event handling-scintilla!notifications!private! !
!ScintillaView categoriesFor: #scnNeedShown:!event handling-scintilla!notifications!private! !
!ScintillaView categoriesFor: #scnPainted:!event handling-scintilla!notifications!private! !
!ScintillaView categoriesFor: #scnSavePointLeft:!event handling-scintilla!notifications!private! !
!ScintillaView categoriesFor: #scnSavePointReached:!event handling-scintilla!notifications!private! !
!ScintillaView categoriesFor: #scnStyleNeeded:!event handling-scintilla!notifications!private! !
!ScintillaView categoriesFor: #scnUpdateUI:!event handling-scintilla!notifications!private! !
!ScintillaView categoriesFor: #scnURIDropped:!event handling-scintilla!notifications!private! !
!ScintillaView categoriesFor: #scnUserListSelection:!event handling-scintilla!notifications!private! !
!ScintillaView categoriesFor: #scnZoom:!event handling-scintilla!notifications!private! !
!ScintillaView categoriesFor: #scrollDown!**auto generated**!commands!public!scintilla interface!scrolling! !
!ScintillaView categoriesFor: #scrollUp!**auto generated**!commands!public!scintilla interface!scrolling! !
!ScintillaView categoriesFor: #scrollWidth!**auto generated**!accessing!public!scintilla interface!scrolling! !
!ScintillaView categoriesFor: #scrollWidth:!**auto generated**!accessing!public!scintilla interface!scrolling! !
!ScintillaView categoriesFor: #selection!public!selection! !
!ScintillaView categoriesFor: #selectionAlpha!**auto generated**!public!scintilla interface! !
!ScintillaView categoriesFor: #selectionAlpha:!**auto generated**!public!scintilla interface! !
!ScintillaView categoriesFor: #selectionBackcolor!accessing!caret, selection, and hotspot styles!public! !
!ScintillaView categoriesFor: #selectionBackcolor:!accessing!caret, selection, and hotspot styles!public! !
!ScintillaView categoriesFor: #selectionForecolor!accessing!caret, selection, and hotspot styles!public! !
!ScintillaView categoriesFor: #selectionForecolor:!accessing!caret, selection, and hotspot styles!public! !
!ScintillaView categoriesFor: #selectionMode!accessing!public!selection! !
!ScintillaView categoriesFor: #selectionMode:!accessing!public!selection! !
!ScintillaView categoriesFor: #selectionPlainText:!private!selection! !
!ScintillaView categoriesFor: #sendMessage:!operations!private! !
!ScintillaView categoriesFor: #sendMessage:wParam:!operations!private! !
!ScintillaView categoriesFor: #sendMessage:wParam:lParam:!operations!private! !
!ScintillaView categoriesFor: #sendMessage:wParam:lpParam:!operations!private! !
!ScintillaView categoriesFor: #setCurrentTextStyles:!private!style definition! !
!ScintillaView categoriesFor: #setDefaultTextStyle!private!style definition! !
!ScintillaView categoriesFor: #setFoldProperty:!helpers!private! !
!ScintillaView categoriesFor: #setFont:!helpers!private!style definition! !
!ScintillaView categoriesFor: #setIndicator:from:length:!indicators!public! !
!ScintillaView categoriesFor: #setIndicator:range:!indicators!public! !
!ScintillaView categoriesFor: #setIndicators:!indicators!private! !
!ScintillaView categoriesFor: #setIndicatorStyles:!public! !
!ScintillaView categoriesFor: #setLexerLanguage:!accessing!lexer!public! !
!ScintillaView categoriesFor: #setMarginWidths:!helpers!margins!private! !
!ScintillaView categoriesFor: #setReadOnly:!**auto generated**!modes!public!scintilla interface!text retrieval & modification! !
!ScintillaView categoriesFor: #setTabStops:!private!tabs & indentation guides! !
!ScintillaView categoriesFor: #setTargetRangeFromSelection!**auto generated**!public!scintilla interface!searching & replacing! !
!ScintillaView categoriesFor: #setText:!helpers!private!text retrieval & modification! !
!ScintillaView categoriesFor: #setWhitespaceChars!helpers!private!white space! !
!ScintillaView categoriesFor: #setWordChars!helpers!other settings!private! !
!ScintillaView categoriesFor: #showAutoCompletionList:prefixLength:!autocompletion!public!scintilla interface! !
!ScintillaView categoriesFor: #showCallTip:at:!call tips!public!scintilla interface! !
!ScintillaView categoriesFor: #showUserList:id:!autocompletion!public!scintilla interface! !
!ScintillaView categoriesFor: #showVerticalScrollBar:!**auto generated**!public!scintilla interface!scrolling! !
!ScintillaView categoriesFor: #splitTarget:!**auto generated**!commands!line wrapping!public!scintilla interface! !
!ScintillaView categoriesFor: #startDwellTimer!notifications!private! !
!ScintillaView categoriesFor: #startRecording!**auto generated**!macro recording!public!scintilla interface! !
!ScintillaView categoriesFor: #startStylingFrom:!public!styling! !
!ScintillaView categoriesFor: #state!accessing!private! !
!ScintillaView categoriesFor: #stopDwellTimer!notifications!private! !
!ScintillaView categoriesFor: #stopRecording!**auto generated**!macro recording!public!scintilla interface! !
!ScintillaView categoriesFor: #stopStyling!public!styling! !
!ScintillaView categoriesFor: #styleAt:!accessing!public!styling! !
!ScintillaView categoriesFor: #styleBits!constants!public!style definition! !
!ScintillaView categoriesFor: #styleBits:!accessing!public!style definition! !
!ScintillaView categoriesFor: #styledTextFrom:to:!accessing!private!text retrieval & modification! !
!ScintillaView categoriesFor: #styleIdAt:!accessing!private!styling! !
!ScintillaView categoriesFor: #styleMaskAt:!accessing!public!styling! !
!ScintillaView categoriesFor: #styleNamed:!accessing!public!styling! !
!ScintillaView categoriesFor: #styleNext:mask:!**auto generated**!public!scintilla interface!styling! !
!ScintillaView categoriesFor: #styler!accessing!public!styling! !
!ScintillaView categoriesFor: #styler:!accessing!public!styling! !
!ScintillaView categoriesFor: #stylerClass!accessing!public!styling! !
!ScintillaView categoriesFor: #stylerClass:!accessing!public!styling! !
!ScintillaView categoriesFor: #styleUnderCaret!accessing!public!styling! !
!ScintillaView categoriesFor: #styleWithId:!accessing!public!style definition! !
!ScintillaView categoriesFor: #stylingPosition!public!styling! !
!ScintillaView categoriesFor: #tabIndents!**auto generated**!public!scintilla interface!tabs & indentation guides!testing! !
!ScintillaView categoriesFor: #tabIndents:!**auto generated**!accessing!public!scintilla interface!tabs & indentation guides! !
!ScintillaView categoriesFor: #tabWidth!**auto generated**!accessing!public!scintilla interface!tabs & indentation guides! !
!ScintillaView categoriesFor: #tabWidth:!**auto generated**!accessing!public!scintilla interface!tabs & indentation guides! !
!ScintillaView categoriesFor: #targetAll!commands!public!selection! !
!ScintillaView categoriesFor: #targetRange!accessing!public!searching & replacing! !
!ScintillaView categoriesFor: #targetRange:!accessing!public!searching & replacing! !
!ScintillaView categoriesFor: #textAtLine:!accessing!private!text retrieval & modification! !
!ScintillaView categoriesFor: #textLength!**auto generated**!accessing!public!scintilla interface! !
!ScintillaView categoriesFor: #textLimit!accessing!public! !
!ScintillaView categoriesFor: #textLimit:!accessing!public! !
!ScintillaView categoriesFor: #textStyles!accessing!public!style definition! !
!ScintillaView categoriesFor: #textStyles:!accessing!public!style definition! !
!ScintillaView categoriesFor: #toggleFold:!folding!operations!public! !
!ScintillaView categoriesFor: #toggleFoldMargin!commands!margins!public! !
!ScintillaView categoriesFor: #toggleIndentationGuides!commands!public!tabs & indentation guides! !
!ScintillaView categoriesFor: #toggleLineEndings!commands!public!tabs & indentation guides! !
!ScintillaView categoriesFor: #toggleLineNumbers!commands!margins!public! !
!ScintillaView categoriesFor: #toggleOvertype!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #toggleStyling!commands!public!styling! !
!ScintillaView categoriesFor: #toggleWhitespace!commands!margins!public! !
!ScintillaView categoriesFor: #toggleWordWrap!commands!public! !
!ScintillaView categoriesFor: #tokenEndAt:!enquiries!public! !
!ScintillaView categoriesFor: #tokenRangeAt:!enquiries!public! !
!ScintillaView categoriesFor: #tokensFrom:to:!accessing!public!text retrieval & modification! !
!ScintillaView categoriesFor: #tokenStartAt:!enquiries!public! !
!ScintillaView categoriesFor: #twiddleLines!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #undo!commands!public!scintilla interface!undo & redo! !
!ScintillaView categoriesFor: #unindent!**auto generated**!commands!public!scintilla interface! !
!ScintillaView categoriesFor: #updateIndicators!helpers!indicators!private! !
!ScintillaView categoriesFor: #updateIndicatorStyles!helpers!indicators!private! !
!ScintillaView categoriesFor: #updateKeyBindings!helpers!key bindings!private! !
!ScintillaView categoriesFor: #updateMarkerDefinitions!helpers!markers!private! !
!ScintillaView categoriesFor: #updateMarkers!helpers!markers!private! !
!ScintillaView categoriesFor: #updateTextStyles!helpers!private!style definition! !
!ScintillaView categoriesFor: #validateUserInterface!operations!public! !
!ScintillaView categoriesFor: #whitespaceBackcolor!accessing!public!white space! !
!ScintillaView categoriesFor: #whitespaceBackcolor:!accessing!public!white space! !
!ScintillaView categoriesFor: #whitespaceForecolor!accessing!public!white space! !
!ScintillaView categoriesFor: #whitespaceForecolor:!accessing!public!white space! !
!ScintillaView categoriesFor: #whitespaces!accessing!public! !
!ScintillaView categoriesFor: #whitespaces:!accessing!public! !
!ScintillaView categoriesFor: #whitespaceVisibility!accessing!public!white space! !
!ScintillaView categoriesFor: #whitespaceVisibility:!accessing!public!white space! !
!ScintillaView categoriesFor: #widthOfText:inStyle:!enquiries!public! !
!ScintillaView categoriesFor: #willCaptureMouse!**auto generated**!public!scintilla interface!testing! !
!ScintillaView categoriesFor: #willCaptureMouse:!**auto generated**!accessing!public!scintilla interface! !
!ScintillaView categoriesFor: #wmContextMenu:wParam:lParam:!event handling-win32!private! !
!ScintillaView categoriesFor: #wmTimer:wParam:lParam:!event handling-win32!private!timers! !
!ScintillaView categoriesFor: #wordChars!accessing!other settings!public! !
!ScintillaView categoriesFor: #wordChars:!accessing!other settings!public! !
!ScintillaView categoriesFor: #wordWrap!accessing-styles!line wrapping!public! !
!ScintillaView categoriesFor: #wordWrap:!accessing-styles!line wrapping!public! !
!ScintillaView categoriesFor: #xOffset!**auto generated**!accessing!public!scintilla interface!scrolling! !
!ScintillaView categoriesFor: #xOffset:!**auto generated**!accessing!public!scintilla interface!scrolling! !
!ScintillaView categoriesFor: #zoomIn!**auto generated**!commands!public!scintilla interface!zooming! !
!ScintillaView categoriesFor: #zoomLevel!**auto generated**!accessing!public!scintilla interface!zooming! !
!ScintillaView categoriesFor: #zoomLevel:!**auto generated**!accessing!public!scintilla interface!zooming! !
!ScintillaView categoriesFor: #zoomOut!**auto generated**!commands!public!scintilla interface!zooming! !

!ScintillaView class methodsFor!

codePages
	^CodePages!

defaultKeyMap
	"Private - This key map is built (using editor macros) from the source code of the Scintilla control.
	Unfortunately there is no way to query the command key assignments."

	^#(
	    (##(VK_DOWN)		0	##(SCI_LINEDOWN))
	    (##(VK_DOWN)		##(FSHIFT)	##(SCI_LINEDOWNEXTEND))
	    (##(VK_DOWN)		##(FCONTROL)	##(SCI_LINESCROLLDOWN))
	    (##(VK_DOWN)		##(FALT|FSHIFT)	##(SCI_LINEDOWNRECTEXTEND))
	    (##(VK_UP)		0	##(SCI_LINEUP))
	    (##(VK_UP)			##(FSHIFT)	##(SCI_LINEUPEXTEND))
	    (##(VK_UP)			##(FCONTROL)	##(SCI_LINESCROLLUP))
	    (##(VK_UP)		##(FALT|FSHIFT)	##(SCI_LINEUPRECTEXTEND))
	    (##(VK_OEM_4)		##(FCONTROL)		##(SCI_PARAUP))
	    (##(VK_OEM_4)		##(FCONTROL|FSHIFT)	##(SCI_PARAUPEXTEND))
	    (##(VK_OEM_6)		##(FCONTROL)		##(SCI_PARADOWN))
	    (##(VK_OEM_6)		##(FCONTROL|FSHIFT)	##(SCI_PARADOWNEXTEND))
	    (##(VK_LEFT)		0	##(SCI_CHARLEFT))
	    (##(VK_LEFT)		##(FSHIFT)	##(SCI_CHARLEFTEXTEND))
	    (##(VK_LEFT)		##(FCONTROL)	##(SCI_WORDLEFT))
	    (##(VK_LEFT)		##(FCONTROL|FSHIFT)	##(SCI_WORDLEFTEXTEND))
	    (##(VK_LEFT)		##(FALT|FSHIFT)	##(SCI_CHARLEFTRECTEXTEND))
	    (##(VK_RIGHT)		0	##(SCI_CHARRIGHT))
	    (##(VK_RIGHT)		##(FSHIFT)	##(SCI_CHARRIGHTEXTEND))
	    (##(VK_RIGHT)		##(FCONTROL)	##(SCI_WORDRIGHT))
	    (##(VK_RIGHT)		##(FCONTROL|FSHIFT)	##(SCI_WORDRIGHTEXTEND))
	    (##(VK_RIGHT)		##(FALT|FSHIFT)	##(SCI_CHARRIGHTRECTEXTEND))
	    (##(VK_OEM_2)	##(FCONTROL)		##(SCI_WORDPARTLEFT))
	    (##(VK_OEM_2)	##(FCONTROL|FSHIFT)	##(SCI_WORDPARTLEFTEXTEND))
	    (##(VK_OEM_5)	##(FCONTROL)		##(SCI_WORDPARTRIGHT))
	    (##(VK_OEM_5)	##(FCONTROL|FSHIFT)	##(SCI_WORDPARTRIGHTEXTEND))
	    (##(VK_HOME)		0	##(SCI_VCHOME))
	    (##(VK_HOME) 		##(FSHIFT) 	##(SCI_VCHOMEEXTEND))
	    (##(VK_HOME) 		##(FCONTROL) 	##(SCI_DOCUMENTSTART))
	    (##(VK_HOME) 		##(FCONTROL|FSHIFT) 	##(SCI_DOCUMENTSTARTEXTEND))
	    (##(VK_HOME) 		##(FALT) 	##(SCI_HOMEDISPLAY))
	    (##(VK_HOME)		##(FALT|FSHIFT)	##(SCI_VCHOMERECTEXTEND))
	    (##(VK_END)	 	0	##(SCI_LINEEND))
	    (##(VK_END)	 	##(FSHIFT) 	##(SCI_LINEENDEXTEND))
	    (##(VK_END) 		##(FCONTROL) 	##(SCI_DOCUMENTEND))
	    (##(VK_END) 		##(FCONTROL|FSHIFT) 	##(SCI_DOCUMENTENDEXTEND))
	    (##(VK_END) 		##(FALT) 	##(SCI_LINEENDDISPLAY))
	    (##(VK_END)		##(FALT|FSHIFT)	##(SCI_LINEENDRECTEXTEND))
	    (##(VK_PRIOR)		0	##(SCI_PAGEUP))
	    (##(VK_PRIOR)		##(FSHIFT) 	##(SCI_PAGEUPEXTEND))
	    (##(VK_PRIOR)		##(FALT|FSHIFT)	##(SCI_PAGEUPRECTEXTEND))
	    (##(VK_NEXT) 		0 	##(SCI_PAGEDOWN))
	    (##(VK_NEXT) 		##(FSHIFT) 	##(SCI_PAGEDOWNEXTEND))
	    (##(VK_NEXT)		##(FALT|FSHIFT)	##(SCI_PAGEDOWNRECTEXTEND))
	    (##(VK_DELETE) 	0	##(SCI_CLEAR))
	    (##(VK_DELETE) 	##(FSHIFT)	##(SCI_CUT))
	    (##(VK_DELETE) 	##(FCONTROL)	##(SCI_DELWORDRIGHT))
	    (##(VK_DELETE)	##(FCONTROL|FSHIFT)	##(SCI_DELLINERIGHT))
	    (##(VK_INSERT) 		0	##(SCI_EDITTOGGLEOVERTYPE))
	    (##(VK_INSERT) 		##(FSHIFT)	##(SCI_PASTE))
	    (##(VK_INSERT) 		##(FCONTROL)	##(SCI_COPY))
	    (##(VK_ESCAPE)  	0	##(SCI_CANCEL))
	    (##(VK_BACK)		0 	##(SCI_DELETEBACK))
	    (##(VK_BACK)		##(FSHIFT) 	##(SCI_DELETEBACK))
	    (##(VK_BACK)		##(FCONTROL) 	##(SCI_DELWORDLEFT))
	    (##(VK_BACK) 		##(FALT)	##(SCI_UNDO))
	    (##(VK_BACK)		##(FCONTROL|FSHIFT)	##(SCI_DELLINELEFT))
	    ($Z 			##(FCONTROL)	##(SCI_UNDO))
	    ($Y 			##(FCONTROL)	##(SCI_REDO))
	    ($X 			##(FCONTROL)	##(SCI_CUT))
	    ($C 			##(FCONTROL)	##(SCI_COPY))
	    ($V 			##(FCONTROL)	##(SCI_PASTE))
	    ($A 			##(FCONTROL)	##(SCI_SELECTALL))
	    (##(VK_TAB)		0	##(SCI_TAB))
	    (##(VK_TAB)		##(FSHIFT)	##(SCI_BACKTAB))
	    (##(VK_RETURN) 	0	##(SCI_NEWLINE))
	    (##(VK_RETURN) 	##(FSHIFT)	##(SCI_NEWLINE))
	    (##(VK_ADD) 		##(FCONTROL)	##(SCI_ZOOMIN))
	    (##(VK_SUBTRACT)	##(FCONTROL)	##(SCI_ZOOMOUT))
	    (##(VK_DIVIDE)	##(FCONTROL)	##(SCI_SETZOOM)) "Reset zoom level to zero"
	    ($L 			##(FCONTROL)	##(SCI_LINECUT))
	    ($L 			##(FCONTROL|FSHIFT)	##(SCI_LINEDELETE))
	    ($T 			##(FCONTROL|FSHIFT)	##(SCI_LINECOPY))
	    ($T 			##(FCONTROL)	##(SCI_LINETRANSPOSE))
	    ($D 			##(FCONTROL)	##(SCI_LINEDUPLICATE))
	    ($U 			##(FCONTROL)	##(SCI_LOWERCASE))
	    ($U 			##(FCONTROL|FSHIFT)	##(##(SCI_UPPERCASE)))
	)!

defaultTextStyles
	"Answer the default text style settings to be used for new instances of the receiver. By
	default we just set up some of the predefined/special styles where we are likely to want
	change these from those preconfigured into the control.

	Any aspect of a style which is not explicitly specified is inherited from the global
	settings (e.g. font and colours) associated with the view. Unless set-up here, or configured
	in the view resource or otherwise set up by the presenter, all the predefined styles will
	use the default background and foreground colour and font."

	| answer |
	answer := Set new.
	"The #normal style must be present - it has id 0, and is the style from which the others
	inherit there default settings. In turn it inherits its own default settings from the view."
	answer add: (ScintillaTextStyle name: #normal).
	"Some 'predefined' styles"
	answer
		add: (ScintillaTextStyle name: #lineNumber);
		add: (ScintillaTextStyle name: #indentGuide);
		yourself.
	^answer!

defaultWhitespaceChars
	"Answer a <String> containing the <Character>s that Scintilla considers to be whitespace by
	default."

	^Character byteCharacterSet select: [:each | each codePoint < 16r20 or: [each == $ ]]!

edgeModes
	"Answer the symbolic names of the long-line edge marking modes supported by Scintilla."

	^#(#none #line #background)!

icon
	"Answers an Icon that can be used to represent this class"

	^##(self) defaultIcon!

immutableCopyOfCollection: aCollection 
	^(aCollection collect: 
			[:each | 
			(each copy)
				isImmutable: true;
				yourself])
		isImmutable: true;
		yourself!

initialize
	"Private - Initialize the receiver's class variables, etc
		self initialize
	"

	self initializeNotificationMap.
	CodePages := (IdentityDictionary new)
				at: SC_CP_DBCS put: #dbcs;
				at: SC_CP_UTF8 put: #utf8;
				shrink;
				isImmutable: true;
				yourself.
	self initializeLexerLanguages.
	BraceHilightingMask := 1.
	FoldingMask := 2.
	BackgroundDwellEvents := 4.
	DefaultTextStyles := (IdentityDictionary new)
				at: #container put: (self immutableCopyOfCollection: self defaultTextStyles);
				at: #xml put: (self immutableCopyOfCollection: self xmlTextStyles);
				at: #text put: (self immutableCopyOfCollection: self txtTextStyles);
				at: #smalltalk put: (self immutableCopyOfCollection: self smalltalkTextStyles);
				shrink;
				isImmutable: true;
				yourself.
	FoldMarkerStyles := #(#arrows #boxTree #circleTree #plusMinus).
	IndentationGuideStyles := #(#real #lookForward #lookBoth).
	CaretStyles := #(#invisible #line #block).
	Whitespaces := (self defaultWhitespaceChars)
				isImmutable: true;
				yourself.
	DefaultKeyBindings := LookupTable new.
	self defaultKeyMap do: 
			[:each | 
			| keyCode |
			keyCode := AcceleratorTable keyCode: each first asInteger modifiers: each second.
			DefaultKeyBindings at: keyCode
				put: ((ScintillaKeyBinding newAcceleratorKey: keyCode message: each last)
						isImmutable: true;
						yourself)].
	DefaultKeyBindings
		shrink;
		isImmutable: true!

initializeLexerLanguages
	"Private - The set of lexers tends to increase quite rapidly over time, this is from 1.75. Note that a
	Smalltalk lexer was added around 1.63, however we do not use this. Dolphin's Smalltalk lexer
	is implemented in the image and container based lexing is used. This is slower, but much
	more powerful.

	self initializeLexerLanguages

	"

	Lexers := #(#container #text #python #cpp #hypertext #xml #perl #sql #vb #props #errorlist #makefile #batch nil #latex #lua #diff #conf #pascal #ave #ada #lisp #ruby #eiffel #eiffelkw #tcl #nncrontab #bullant #vbscript #asp #php #baan #matlab #scriptol #asm #cppnocase #fortran #f77 #css #pov #lout #escript #ps #nsis #mmixal nil nil #lot #yaml #tex #metapost #powerbasic #forth #erlang #octave #mssql #verilog #kix #gui4cli #specman #au3 #apdl #bash #asn1 #vhdl #caml #blitzbasic #purebasic #haskell #phpscript #tads3 #rebol #smalltalk #flagship #csound #freebasic #inno #opal #spice #d #cmake #gap #'PL/M' nil #abaqus #asy #r #automatic).
	self assert: [Lexers size = (SCLEX_R + 2)]!

initializeNotificationMap
	ScnMap := (Array new: 25)
				at: SCN_STYLENEEDED - 1999 put: #scnStyleNeeded:;
				at: SCN_CHARADDED - 1999 put: #scnCharAdded:;
				at: SCN_SAVEPOINTREACHED - 1999 put: #scnSavePointReached:;
				at: SCN_SAVEPOINTLEFT - 1999 put: #scnSavePointLeft:;
				at: SCN_MODIFYATTEMPTRO - 1999 put: #scnModifyAttemptRO:;
				at: SCN_KEY - 1999 put: #scnKey:;
				at: SCN_DOUBLECLICK - 1999 put: #scnDoubleClick:;
				at: SCN_UPDATEUI - 1999 put: #scnUpdateUI:;
				at: SCN_MODIFIED - 1999 put: #scnModified:;
				at: SCN_MACRORECORD - 1999 put: #scnMacroRecord:;
				at: SCN_MARGINCLICK - 1999 put: #scnMarginClick:;
				at: SCN_NEEDSHOWN - 1999 put: #scnNeedShown:;
				at: SCN_PAINTED - 1999 put: #scnPainted:;
				at: SCN_USERLISTSELECTION - 1999 put: #scnUserListSelection:;
				at: SCN_URIDROPPED - 1999 put: #scnURIDropped:;
				at: SCN_DWELLSTART - 1999 put: #scnDwellStart:;
				at: SCN_DWELLEND - 1999 put: #scnDwellEnd:;
				at: SCN_ZOOM - 1999 put: #scnZoom:;
				at: SCN_HOTSPOTCLICK - 1999 put: #scnHotSpotClick:;
				at: SCN_HOTSPOTDOUBLECLICK - 1999 put: #scnHotSpotDoubleClick:;
				at: SCN_CALLTIPCLICK - 1999 put: #scnCallTipClick:;
				at: SCN_AUTOCSELECTION - 1999 put: #scnAutoCSelection:;
				at: SCN_INDICATORCLICK - 1999 put: #scnIndicatorClick:;
				at: SCN_INDICATORRELEASE - 1999 put: #scnIndicatorRelease:;
				at: SCN_POSCHANGED - 1999 put: nil;
				isImmutable: true;
				yourself!

layoutCachingModes
	^#(#none #caret #page #document)!

lineEndings
	"Answer the symbolic names of the end-of-line modes supported by Scintilla. These correspond
	to the character sequences, with #crlf being the default for Windows."

	^#(#crlf #cr #lf)!

selectionModes
	"Answer the symbolic names of the selection modes supported by Scintilla."

	^#(#stream #rectangle #lines)!

smalltalkTextStyles
	"Answer the default text style settings to be used for new instances of the receiver in
	conjunction with the built in (not Dolphin) smalltalk lexer. Note that these styles are
	those from Scite, and are NOT those used in the Dolphin IDE, which uses container based
	lexing."

	| answer keywordColor |
	answer := self defaultTextStyles.
	keywordColor := Color fromHTMLSpec: '#00007F'.
	answer
		add: ((ScintillaTextStyle name: #string)
					id: 1;
					forecolor: (Color fromHTMLSpec: '#7F007F');
					isBackcolorExtendedToEndOfLine: true;
					yourself);
		add: ((ScintillaTextStyle name: #number)
					id: 2;
					forecolor: (Color fromHTMLSpec: '#007F7F');
					yourself);
		add: ((ScintillaTextStyle name: #comment)
					id: 3;
					forecolor: (Color fromHTMLSpec: '#007F00');
					yourself);
		add: ((ScintillaTextStyle name: #symbol)
					id: 4;
					forecolor: Color darkMagenta;
					yourself);
		add: ((ScintillaTextStyle name: #binary)
					id: 5;
					forecolor: Color black;
					yourself);
		add: ((ScintillaTextStyle name: #boolean)
					id: 6;
					forecolor: keywordColor;
					isBold: true;
					yourself);
		add: ((ScintillaTextStyle name: #self)
					id: 7;
					forecolor: keywordColor;
					isBold: true;
					yourself);
		add: ((ScintillaTextStyle name: #super)
					id: 8;
					forecolor: keywordColor;
					isBold: true;
					yourself);
		add: ((ScintillaTextStyle name: #nil)
					id: 9;
					forecolor: keywordColor;
					isBold: true;
					yourself);
		add: ((ScintillaTextStyle name: #global)
					id: 10;
					isBold: true;
					yourself);
		add: ((ScintillaTextStyle name: #return)
					id: 11;
					forecolor: (Color fromHTMLSpec: '#A00000');
					isBold: true;
					yourself);
		add: ((ScintillaTextStyle name: #special)
					id: 12;
					isBold: true;
					yourself);
		add: ((ScintillaTextStyle name: #keywordSend)
					id: 13;
					forecolor: Color darkGreen;
					isBold: true;
					yourself);
		add: ((ScintillaTextStyle name: #assignment)
					id: 14;
					isBold: true;
					yourself);
		add: ((ScintillaTextStyle name: #character)
					id: 15;
					forecolor: (Color fromHTMLSpec: '#7F007F');
					isBold: true;
					yourself);
		add: ((ScintillaTextStyle name: #specialSelector)
					id: 16;
					forecolor: keywordColor;
					isBold: true;
					yourself);
		yourself.
	"Add some standard styles"
	answer
		add: ((ScintillaTextStyle name: #indentGuide)
					forecolor: Color gray;
					yourself);
		add: ((ScintillaTextStyle name: #braceHighlight)
					forecolor: Color blue;
					isBold: true;
					yourself);
		add: ((ScintillaTextStyle name: #braceMismatch)
					forecolor: Color red;
					isBold: true;
					yourself);
		yourself.
	^answer!

stbConvertFromVersion9: anArray 
	"Private - Perform an STB conversion from a version 9 (or earlier) <ScintillaView> to version 10.
	The single collection of text styles was replaced by a current collection, and a collection of collections
	keyed by lexer language. This allows a single ScintillaView to be switched easily between different languages.
	Also add a few more spare instance variables."

	| table array |
	array := super stbConvertFromVersion9: anArray.
	table := IdentityDictionary new.
	array at: 39 put: table.
	"All older resources only have container based lexing"
	table at: #container put: (array at: 18).
	^array!

txtTextStyles
	"Answer the default text style settings to be used for new instances of the receiver.
	These are the styles used as the defaults for txt files in Scite."

	| styles |
	styles := self defaultTextStyles.
	styles add: ((ScintillaTextStyle name: #normal)
				backcolor: Color window;
				yourself).
	^styles!

whitespaceVisibilityLevels
	"Answer the symbolic names of the whitespace visibility modes supported by Scintilla."

	^#(#invisible #visibleAlways #visibleAfterIndent)!

winClassName
	"Private - Answer the name of the Windows window class to use when realizing the receiver."

	"Ensure the DLL has been loaded"

	ScintillaLibrary realize.
	^'Scintilla'!

xmlTextStyles
	"Answer the default XML text style settings to be used for new instances of the receiver.
	These are the styles used as the defaults for XML files in Scite."

	"Implementation Note: The XML lexer uses a different style for the open tag character (1) vs
	the close tag characters (11). In consequence Scintilla cannot do brace matching in XML
	docs, since it will only match braces where they have the same style - this avoids a brace
	in a comment, for example, wrongly matching a brace in code. Because of this there is no
	point defining the brace highlight and mismatch styles."

	| styles sgmlBackcolour |
	styles := self defaultTextStyles.
	styles add: ((ScintillaTextStyle name: #normal)
				backcolor: Color window;
				yourself).
	styles add: ((ScintillaTextStyle name: #tag)
				id: 1;
				forecolor: Color darkBlue;
				yourself).
	styles add: ((ScintillaTextStyle name: #unknownTag)
				id: 2;
				forecolor: Color darkBlue;
				yourself).
	styles add: ((ScintillaTextStyle name: #attribute)
				id: 3;
				forecolor: Color darkCyan;
				yourself).
	styles add: ((ScintillaTextStyle name: #unknownAttribute)
				id: 4;
				forecolor: Color darkCyan;
				yourself).
	styles add: ((ScintillaTextStyle name: #number)
				id: 5;
				forecolor: (RGB 
							red: 0
							green: 127
							blue: 127);
				yourself).
	styles add: ((ScintillaTextStyle name: #doubleString)
				id: 6;
				forecolor: (RGB 
							red: 127
							green: 0
							blue: 127);
				yourself).
	styles add: ((ScintillaTextStyle name: #singleString)
				id: 7;
				forecolor: (RGB 
							red: 127
							green: 0
							blue: 127);
				yourself).
	styles add: ((ScintillaTextStyle name: #otherInsideTag)
				id: 8;
				forecolor: Color darkMagenta;
				yourself).
	styles add: ((ScintillaTextStyle name: #comment)
				id: 9;
				forecolor: Color brown;
				yourself).
	styles add: ((ScintillaTextStyle name: #entity)
				id: 10;
				forecolor: Color darkMagenta;
				yourself).
	styles add: ((ScintillaTextStyle name: #tagEnd)
				id: 11;
				forecolor: Color darkBlue;
				yourself).
	styles add: ((ScintillaTextStyle name: #identifierEnd)
				id: 12;
				forecolor: Color darkMagenta;
				isBold: true;
				yourself).
	styles add: ((ScintillaTextStyle name: #identifierStart)
				id: 13;
				forecolor: Color darkMagenta;
				isBold: true;
				yourself).
	styles add: ((ScintillaTextStyle name: #CDATA)
				id: 17;
				backcolor: (RGB 
							red: 255
							green: 240
							blue: 240);
				forecolor: Color darkRed;
				isBackcolorExtendedToEndOfLine: true;
				yourself).
	styles add: ((ScintillaTextStyle name: #question)
				id: 18;
				forecolor: Color darkRed;
				yourself).
	styles add: ((ScintillaTextStyle name: #unquotedValue)
				id: 19;
				forecolor: (RGB 
							red: 96
							green: 128
							blue: 96);
				yourself).
	sgmlBackcolour := RGB 
				red: 239
				green: 239
				blue: 255.
	styles add: ((ScintillaTextStyle name: #sgmlTag)
				id: 21;
				backcolor: sgmlBackcolour;
				forecolor: Color darkBlue;
				yourself).
	styles add: ((ScintillaTextStyle name: #sgmlCommand)
				id: 22;
				backcolor: sgmlBackcolour;
				forecolor: Color darkBlue;
				isBold: true;
				yourself).
	styles add: ((ScintillaTextStyle name: #sgmlFirstParam)
				id: 23;
				backcolor: sgmlBackcolour;
				forecolor: (RGB 
							red: 0
							green: 102
							blue: 0);
				yourself).
	styles add: ((ScintillaTextStyle name: #sgmlDoubleString)
				id: 24;
				backcolor: sgmlBackcolour;
				forecolor: Color darkRed;
				yourself).
	styles add: ((ScintillaTextStyle name: #sgmlSingleString)
				id: 25;
				backcolor: sgmlBackcolour;
				forecolor: (RGB 
							red: 153
							green: 51
							blue: 0);
				yourself).
	styles add: ((ScintillaTextStyle name: #sgmlError)
				id: 26;
				backcolor: (RGB 
							red: 255
							green: 102
							blue: 102);
				forecolor: Color darkRed;
				yourself).
	styles add: ((ScintillaTextStyle name: #sgmlSpecial)
				id: 27;
				backcolor: sgmlBackcolour;
				forecolor: (RGB 
							red: 51
							green: 102
							blue: 255);
				yourself).
	styles add: ((ScintillaTextStyle name: #sgmlEntity)
				id: 28;
				backcolor: sgmlBackcolour;
				forecolor: (RGB 
							red: 51
							green: 51
							blue: 51);
				yourself).
	styles add: ((ScintillaTextStyle name: #sgmlComment)
				id: 29;
				backcolor: sgmlBackcolour;
				forecolor: Color brown;
				yourself).
	styles add: ((ScintillaTextStyle name: #sgmlBlock)
				id: 31;
				backcolor: (RGB 
							red: 204
							green: 204
							blue: 224);
				forecolor: (RGB 
							red: 0
							green: 0
							blue: 102);
				yourself).
	^styles! !
!ScintillaView class categoriesFor: #codePages!constants!public! !
!ScintillaView class categoriesFor: #defaultKeyMap!constants!private! !
!ScintillaView class categoriesFor: #defaultTextStyles!constants!public!style definition! !
!ScintillaView class categoriesFor: #defaultWhitespaceChars!accessing!public! !
!ScintillaView class categoriesFor: #edgeModes!constants!public! !
!ScintillaView class categoriesFor: #icon!constants!public! !
!ScintillaView class categoriesFor: #immutableCopyOfCollection:!helpers!private! !
!ScintillaView class categoriesFor: #initialize!development!initializing!private! !
!ScintillaView class categoriesFor: #initializeLexerLanguages!development!initializing!private! !
!ScintillaView class categoriesFor: #initializeNotificationMap!**auto generated**!must not strip!public!scintilla interface! !
!ScintillaView class categoriesFor: #layoutCachingModes!constants!public! !
!ScintillaView class categoriesFor: #lineEndings!constants!public! !
!ScintillaView class categoriesFor: #selectionModes!constants!public! !
!ScintillaView class categoriesFor: #smalltalkTextStyles!constants!public!style definition! !
!ScintillaView class categoriesFor: #stbConvertFromVersion9:!binary filing!private! !
!ScintillaView class categoriesFor: #txtTextStyles!constants!public!style definition! !
!ScintillaView class categoriesFor: #whitespaceVisibilityLevels!constants!public! !
!ScintillaView class categoriesFor: #winClassName!constants!private! !
!ScintillaView class categoriesFor: #xmlTextStyles!constants!public!style definition! !

"Binary Globals"!


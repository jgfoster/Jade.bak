| package |
package := Package name: 'JGF Fading Dialog'.
package paxVersion: 1;
	basicComment: ''.

package basicPackageVersion: '0.006'.


package classNames
	add: #FadingDialog;
	add: #LayeredDialogView;
	yourself.

package methodNames
	add: #UserLibrary -> #setLayeredWindowAttributes:_:_:_:;
	add: #View -> #alpha:;
	add: #View -> #setLayeredWindowColorRef:alpha:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	yourself).

package!

"Class Definitions"!

Dialog subclass: #FadingDialog
	instanceVariableNames: 'fadeProcess'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DialogView subclass: #LayeredDialogView
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!UserLibrary methodsFor!

setLayeredWindowAttributes: hwnd _: crKey _: bAlpha _: dwFlags
	"
BOOL SetLayeredWindowAttributes(
	HWND				hwnd,
	COLORREF		crKey,
	BYTE				bAlpha,
	DWORD			dwFlags);"

	<stdcall: bool SetLayeredWindowAttributes handle dword byte dword>
	#JGF.
	^self invalidCall! !
!UserLibrary categoriesFor: #setLayeredWindowAttributes:_:_:_:!JGF!public! !

!View methodsFor!

alpha: anInteger
	"From zero (transparent) to 255 (fully visible)"

	#JGF.
	self
		setLayeredWindowColorRef: nil
		alpha: anInteger.
!

setLayeredWindowColorRef: crKey alpha: bAlpha

	| dwFlags |
	#JGF.
	dwFlags := 0.
	crKey notNil ifTrue: [dwFlags := dwFlags bitOr: LWA_COLORKEY].
	bAlpha notNil ifTrue: [dwFlags := dwFlags bitOr: LWA_ALPHA].
	^UserLibrary default 
		setLayeredWindowAttributes: self asParameter 
		_: crKey 
		_: (((bAlpha isNil ifTrue: [255] ifFalse: [bAlpha]) min: 255) max: 0)
		_: dwFlags.
! !
!View categoriesFor: #alpha:!JGF!public! !
!View categoriesFor: #setLayeredWindowColorRef:alpha:!JGF!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

FadingDialog guid: (GUID fromString: '{81825E2C-53D9-43B4-8404-860898A3830F}')!
FadingDialog comment: ''!
!FadingDialog categoriesForClass!Unclassified! !
!FadingDialog methodsFor!

beTransparent

	self terminateFadeProcess.
	self view alpha: 0.
!

onViewClosed

	self terminateFadeProcess.
	super onViewClosed.
!

onViewOpened

	super onViewOpened.
	fadeProcess := [
		5 to: 255 by: 10 do: [:i | 
			self view alpha: i.
			(Delay forMilliseconds: 50) wait.
		].
		fadeProcess := nil.
	] forkAt: Processor userBackgroundPriority.
!

terminateFadeProcess

	fadeProcess notNil ifTrue: [
		fadeProcess terminate.
	].
! !
!FadingDialog categoriesFor: #beTransparent!public! !
!FadingDialog categoriesFor: #onViewClosed!public! !
!FadingDialog categoriesFor: #onViewOpened!public! !
!FadingDialog categoriesFor: #terminateFadeProcess!public! !

!FadingDialog class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(STBViewProxy)  8 ##(LayeredDialogView)  98 30 0 0 98 2 26214401 131073 416 0 524550 ##(ColorRef)  8 4278190080 0 133 0 0 0 416 788230 ##(BorderLayout)  1 1 0 410 8 ##(ReferenceView)  98 14 0 416 98 2 8 1140850688 131073 560 0 0 0 5 0 0 0 560 1180166 ##(ResourceIdentifier)  8 ##(Presenter)  8 #resource_OK_Cancel_button_block 0 983302 ##(MessageSequence)  202 208 98 1 721670 ##(MessageSend)  8 #createAt:extent: 98 2 328198 ##(Point)  21 437 834 469 71 560 983302 ##(WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 218 0 0 0 244 0 0 0 253 0 0 0] 98 0 834 193 193 0 27 0 0 0 234 256 928 590342 ##(Rectangle)  834 21 21 834 21 21 0 0 0 0 12301 0 0 0 0 1 0 0 590598 ##(Semaphore)  0 0 1 0 8 2010572111 706 202 208 98 2 770 800 98 2 834 1341 905 834 521 591 416 770 8 #menuBar: 98 1 0 416 882 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 158 2 0 0 196 1 0 0 162 3 0 0 235 2 0 0] 98 1 560 944 0 27 )! !
!FadingDialog class categoriesFor: #resource_Default_view!public! !

LayeredDialogView guid: (GUID fromString: '{5A9072ED-06CC-4DA5-8C06-66054ABB7D70}')!
LayeredDialogView comment: ''!
!LayeredDialogView categoriesForClass!Unclassified! !
!LayeredDialogView methodsFor!

extendedStyle

	^super extendedStyle bitOr: WS_EX_LAYERED.
! !
!LayeredDialogView categoriesFor: #extendedStyle!public! !

"Binary Globals"!


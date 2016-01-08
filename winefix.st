"File in to make Dolphin 6.0x, 6.1x images compatible with Wine"!

"** WF1 ** Label the image as Wine compatible and running under Mac OS/X if applicable"!
!KernelLibrary class methodsFor!

isMacOSX
	"Answer true if this image is running under Mac OS/X under WINE"
	#wineFix.
	^self isWine and: [true]!

isWine
	"Answer true if this image is running under WINE"
	#wineFix.
	^true! !
!KernelLibrary class categoriesFor: #isMacOSX!public!wine fix! !
!KernelLibrary class categoriesFor: #isWine!public!wine fix! !

"** WF2 ** Wine doesn't yet implement EM_SETMARGINS "!
!TextEdit methodsFor!

setMarginWidths: anArray 
	| margins |
	KernelLibrary isWine 
		ifTrue: 
			[ #wineFix.
			^self].
	margins := (DWORD new)
				highWord: anArray first;
				lowWord: anArray last.
	self 
		sendMessage: EM_SETMARGINS
		wParam: (EC_LEFTMARGIN bitOr: EC_RIGHTMARGIN)
		lParam: margins asParameter! !
!TextEdit categoriesFor: #setMarginWidths:!accessing!private!wine fix! !

"** WF3 ** Wine doesn't yet implement GDILibrary>>getPaletteEntries:..."!
!IndexedColor methodsFor!

asRGBUsingNoPalette
	"Answer the receiver as an RGB  when no palette is available ."

	| colorNameMap stdColorName |
	#wineFix.
	colorNameMap := LookupTable new.
	#(#(#black 16r1000000) #(#darkRed 16r1000001) #(#darkGreen 16r1000002) #(#brown 16r1000003) #(#darkBlue 16r1000004) #(#darkMagenta 16r1000005) #(#darkCyan 16r1000006) #(#gray 16r1000007) #(#darkGray 16r100000C) #(#red 16r100000D) #(#green 16r100000E) #(#yellow 16r100000F) #(#blue 16r1000010) #(#magenta 16r1000011) #(#cyan 16r1000012) #(#white 16r1000013)) 
		do: [:each | colorNameMap at: each second put: each first].
	stdColorName := colorNameMap at: index.
	^RGB stdColor: stdColorName! !
!IndexedColor categoriesFor: #asRGBUsingNoPalette!converting!public!wine fix! !

!IndexedColor methodsFor!asRGB	"Answer the receiver as an RGB using the default palette."	#wineFix.
	KernelLibrary isWine ifTrue: [		^self asRGBUsingNoPalette].
	^self asRGBUsingPalette: self defaultPalette! !
!IndexedColor categoriesFor: #asRGBUsingPalette:!converting!public!wine fix! !

"** WF4 ** ListView watermark in system folder causes problems"!
!SmalltalkSystemShell methodsFor!

onViewCreated
	"Private - The receiver's has been created (that is the CreateWindow() API call has returned), but may
	not yet be connected to the receiver (if the view-presenter pair is being instantiated, then
	the connection will not have been made, but if the view is being restored from saved state
	in the image then the presenter will be connected). This is an appropriate point for
	subclasses to apply any dynamic view configuration that must be performed every time the
	view is created. Typically this might involve customising some feature of the view that it
	does not itself save, perhaps because a feature of the underlying control does not have full
	support in the wrapper class. Note that one should normally override #onViewAvailable to
	restore user configuration settings, since the views will normally save and restore all
	relevant settings across an image save."

	KernelLibrary isWine ifFalse: [
		#wineFix.
		[self setListViewWatermark] postToMessageQueue].
	self registerHotKey.
	^super onViewCreated! !
!SmalltalkSystemShell categoriesFor: #onViewCreated!event handling!private!wine fix! !

"** WF5 ** Wine does not support the ComCtrlLibrary>>loadIconWithScaleDown:.. method which is normally availble under Vista
and later. Hence we don't allow this to be called even if the Windows version indicates Vista is available"!
!Icon methodsFor!

valueWithHandle: operation forExtent: aPoint 
	"Private - Evaluate the <monadicValuable>, operation, with a handle for the receiver's icon at the best possible
	rendering for the specified extent. On Vista and later platforms the icon will be scaled down from the next largest
	icon if the requested size is not available."

	| hModule ident |
	self handle = self class question handle 
		ifTrue: 
			["If the icon failed to load, the standard question icon will have been substituted"
			hModule := 0.
			ident := self class question identifier]
		ifFalse: 
			[ident := self identifier.
			hModule := instanceHandle asParameter].
	hModule isNull 
		ifTrue: 
			["If loaded from a file or a standard system icon, then need to reload to ensure has correct extent"
			instanceHandle isNil 
				ifTrue: 
					[(self loadFromFile: self fileSpec extent: aPoint) ifNotNil: [:hIcon | ^operation value: hIcon]]
				ifFalse: 
					[(hModule == 0 
						and: [aPoint ~= SystemMetrics current largeIconExtent and: [#wineFix. 
							OSVERSIONINFO current isWinVista & KernelLibrary isWine not ]]) 
							ifTrue: 
								[(self 
									load: ident
									fromInstance: hModule
									extent: aPoint) ifNotNil: [:hIcon | ^operation value: hIcon]]]].
	^operation value: self asParameter! !
!Icon categoriesFor: #valueWithHandle:forExtent:!helpers!private!wine fix! !

"** WF6 **  Suppressible message boxes under Wine have a bug where they ignore the buttonStyles #yesNo and #yesNoCancel and will
always answer #ok or #cancel instead. Here we map the return button ids to the correct values"!

!MessageBox methodsFor!

result
	"Answer a portable symbolic constant describing the button that was pressed by the user
	to close the receiver."

	KernelLibrary isWine 
		ifTrue: 
			[#wineFix.
			"Suppressible message boxes under Wine have a bug where they ignore the buttonStyles #yesNo and #yesNoCancel and will
			always answer #ok or #cancel instead. Here we map the return button ids to the correct values"
			(self buttonStyle == #yesNo or: [self buttonStyle == #yesNoCancel]) 
				ifTrue: 
					[button = IDOK ifTrue: [button := IDYES].
					button = IDCANCEL ifTrue: [button := IDNO]]].
	^ButtonMap at: button! !
!MessageBox categoriesFor: #result!accessing!public!wine fix! !

"** WF7 ** Helper method to edit view resources - will appear in D6.2"!

!ResourceIdentifier methodsFor!

editViewUsing: aMonadicBlock 
	| view |
	view := self loadWithContext: View desktop.
	aMonadicBlock value: view.
	self assign: view literalStoreArray.
	view destroy! !
!ResourceIdentifier categoriesFor: #editViewUsing:!operations!public! !

"** WF8 ** Wine gives error when turning of theming"!
!View methodsFor!

beNotThemed
	"Turns off any visual theme for the receiver. Only effective on Windows XP and above"

	KernelLibrary isWine ifTrue: [
		"Wine currently errors on this, so ignore"
		#wineFix.
		^self].

	ThemeLibrary default 
		setWindowTheme: self handle
		pszSubAppName: '' asUnicodeString
		pszSubIdList: '' asUnicodeString! !
!View categoriesFor: #beNotThemed!modes!wine fix!public! !

"** WF9 ** Having autoSelectPackages option turned on in System Browser gives unpleasant scrolling effect when many multiple packages
are selected. This also happens in Package Browser. Problem under Wine traced to LVIS_FOCUSED option in ListView>>selectIndex:. Not
sure  what overall effect it will have turning this off."!

!ListView methodsFor!

selectIndex: anInteger set: aBoolean 
	"Private - Set/reset the selection state of the object at the specified one-based <integer>
	index within the receiver according to the <boolean> argument."

	| anLvItem mask |
	anLvItem := LVITEM new.
	mask := ##(LVIS_SELECTED|LVIS_FOCUSED).
	KernelLibrary isWine ifTrue: [
				#wineFix.
				mask := mask maskClear: LVIS_FOCUSED
		].
	anLvItem stateMask: mask.
	aBoolean ifTrue: [anLvItem dwState: mask].
	self lvmSetItem: anInteger - 1 state: anLvItem! !
!ListView categoriesFor: #selectIndex:set:!private!selection! !

"** WF10 ** Wine ListViews seem to expect image indices to always be set in ListView>>onDisplayDetailsRequired:, even if we don't want
images to appear. Normally, we don't set an image if none is required. In Wine we set an image index of 0"!

!ListViewColumn methodsFor!

imageFromRow: item
	"Private - Answer the image for this column extracted from the <Object>, item,
	which is an element of the receiver's parent <ListView>.
	As of Dolphin 3.0 the image block is permitted to be nil for the common case
	where the columns does not have images."

	KernelLibrary isWine ifTrue: [
		#wineFix.
		"WF10: Wine ListViews seem to expect image indices to always be set in ListView>>onDisplayDetailsRequired:, 
		even if we don't want images to appear. Normally, we answer nil here if no image is required. 
		In Wine we must answer an image index of 0"
		getImageBlock ifNil: [^0]].

	^getImageBlock isNil ifFalse: [
		getImageBlock value: (self contentFromRow: item)].
! !
!ListViewColumn categoriesFor: #imageFromRow:!adapters!private!wine fix! !

!ListView methodsFor!

imageFromRow: item 
	"Private - Answer the image for row which represents the <Object>, item.
	As of Dolphin 3.0 the image block is permitted to be nil for the common case
	where the list does not have images."

	self viewMode == #thumbnails ifTrue: [^self getThumbnailOf: item].

	KernelLibrary isWine ifTrue: [
		#wineFix.
		"WF10: Wine ListViews seem to expect image indices to always be set in ListView>>onDisplayDetailsRequired:, 
		even if we don't want images to appear. Normally, we answer nil here if no image is required. 
		In Wine we must answer an image index of 0"
		getImageBlock ifNil: [^0]].
	^self getImageBlock isNil ifFalse: [getImageBlock value: item]! !
!ListView categoriesFor: #imageFromRow:!adapters!private!wine fix! !

"** WF11 ** Wine doesn't appear to support setting drag cursors from image lists. Until available we fall back to using the old (non-specific) drag and drop cursors"!

!InternalDragDropSession methodsFor!

giveFeedback
	"Private - Set the drag cursor to indicate the effect of a drop on the current target."

	| imageKey |
	(imageKey := self imageKey) ~~ lastImageKey 
		ifTrue: 
			[| dragIm |
			KernelLibrary isWine 
				ifTrue: 
					[| cursorSelector |
					#wineFix.
					"WF11: Wine doesn't appear to support setting drag cursors
					from image lists. Until available we fall back to using the
					old (non-specific) drag and drop cursors"
					cursorSelector := ##((LookupTable new)
								at: #none put: #ddNone;
								at: #move put: #ddMove;
								at: #copy put: #ddCopy;
								at: #copyScroll put: #ddCopyScroll;
								at: #moveScroll put: #ddMoveScroll;
								at: #linkScroll put: #ddLinkScroll;
								yourself) at: self imageKey.
					(Cursor perform: cursorSelector) makeCurrent.
					lastImageKey := imageKey].
			dragIm := dragImages at: imageKey.
			imageList 
				setDragCursorImage: dragIm key
				hotSpotX: dragIm value x
				hotSpotY: dragIm value y.
			lastImageKey := imageKey].
	imageList dragMoveTo: self dragPoint! !
!InternalDragDropSession categoriesFor: #giveFeedback!operations!wine fix!private! !

"** WF 12 ** Fixes for Registry Editor sample (which can be quite useful). They are mainly to do
with the pulling or resource items out of the genuine Microsoft RegEdit.exe, which isn't available. These fixes should be migrated back into the main code as they make it less dependent on changes to the underlying Windows OS"!

!RegEdit class methodsFor!

displayStringForValue: anObject 
	"Private - Answer an appropriate registry editor display string for the argument, anObject.
	Not clean, but we don't want to add these as polymorphic operations into the types
	of object in the registry.
	This method also illustrates the use of the C-runtime library function sprintf() for
	fast string formatting."

	| s n max |
	anObject isNil 
		ifTrue: 
			[^'(value not set)'].
	anObject isInteger ifTrue: [^'0x%08x (%u)' sprintfWith: anObject with: anObject].
	(anObject isKindOf: String) ifTrue: [^'"' , anObject , '"'].
	"Assume its a byte array"
	anObject isEmpty 
		ifTrue: 
			[^'(zero-length binary value)'].
	n := anObject size.
	max := 64 min: n.
	s := String writeStream: (max + 1) * 3.
	(1 to: max) do: [:i | s nextPutAll: ('%02x' sprintfWith: (anObject basicAt: i))]
		separatedBy: [s space].
	max < n ifTrue: [s nextPutAll: '...'].
	^s contents! !
!RegEdit class categoriesFor: #displayStringForValue:!displaying!private! !

!RegEdit class methodsFor!

binaryIcon
	"Answers an Icon that can be used to represent binary values."

	^InternalIcon 
		fromBytes: #[137 80 78 71 13 10 26 10 0 0 0 13 73 72 68 82 0 0 0 16 0 0 0 16 8 6 0 0 0 31 243 255 97 0 0 0 1 115 82 71 66 0 174 206 28 233 0 0 0 4 103 65 77 65 0 0 177 143 11 252 97 5 0 0 0 9 112 72 89 115 0 0 14 195 0 0 14 195 1 199 111 168 100 0 0 0 228 73 68 65 84 56 79 157 147 61 14 131 48 12 133 57 81 47 211 51 177 177 48 114 13 174 18 88 122 128 174 84 72 45 205 15 184 121 73 29 153 160 74 161 150 158 244 108 147 207 102 112 53 12 3 157 81 219 182 84 201 24 199 209 82 97 204 243 76 125 223 211 14 226 167 23 3 48 7 0 68 130 0 176 109 27 65 151 235 45 41 175 113 31 0 86 128 48 128 31 75 208 47 191 174 43 61 166 41 110 241 47 0 144 186 174 239 1 128 4 146 191 144 215 100 223 185 248 125 216 64 121 128 115 150 206 136 225 95 128 74 0 222 0 48 233 101 142 186 181 54 40 2 148 242 222 248 130 73 143 74 125 2 24 99 8 226 169 165 94 0 180 7 104 1 216 123 217 195 16 228 80 0 116 93 71 44 220 132 214 186 88 135 187 96 144 214 111 15 137 146 3 224 101 239 0 192 145 97 139 124 43 62 62 238 45 203 139 160 166 105 158 187 203 44 73 242 1 31 131 69 242 138 122 147 62 248 0 0 0 0 73 69 78 68 174 66 96 130]! !
!RegEdit class categoriesFor: #binaryIcon!constants!public! !

!RegEdit class methodsFor!

keyIcon
	"Answers an Icon that can be used to registry keys."

	^InternalIcon fromBytes: #[137 80 78 71 13 10 26 10 0 0 0 13 73 72 68 82 0 0 0 16 0 0 0 16 8 6 0 0 0 31 243 255 97 0 0 0 1 115 82 71 66 0 174 206 28 233 0 0 0 4 103 65 77 65 0 0 177 143 11 252 97 5 0 0 0 9 112 72 89 115 0 0 14 195 0 0 14 195 1 199 111 168 100 0 0 2 87 73 68 65 84 56 79 205 146 191 111 82 81 20 199 49 198 31 131 131 113 49 174 141 255 128 131 78 46 14 198 65 55 83 135 46 117 209 166 134 98 218 52 74 211 2 86 83 36 252 104 161 242 67 232 243 1 22 8 60 124 175 200 143 148 39 5 11 181 208 80 72 33 150 54 148 68 218 80 26 127 196 152 24 141 219 253 122 193 68 83 73 116 113 240 36 159 229 220 239 61 231 220 239 185 18 201 127 17 31 223 239 225 237 238 54 105 238 212 176 93 78 225 247 161 252 150 241 179 49 143 81 187 40 56 74 29 3 123 39 85 93 213 92 24 235 25 158 108 102 67 216 92 158 195 6 165 178 36 180 115 175 211 207 72 57 205 145 242 34 71 74 47 3 164 163 128 201 52 112 164 144 240 33 255 194 67 242 162 7 249 248 44 86 90 204 187 145 141 58 73 54 226 36 203 17 150 188 10 63 33 153 231 51 157 5 90 21 83 188 13 201 160 13 11 65 43 18 156 5 98 192 12 209 63 141 56 37 58 107 192 214 74 4 205 245 20 234 107 9 236 86 87 241 110 247 13 190 125 253 130 220 188 251 83 123 34 129 213 128 103 30 34 56 67 113 168 193 217 39 16 114 234 176 150 244 163 94 140 163 86 16 241 249 67 3 141 90 25 59 213 34 234 149 92 59 7 72 14 180 11 176 134 17 48 122 57 194 46 61 237 22 198 86 219 11 1 27 75 60 42 153 32 154 149 12 26 229 36 90 30 173 45 248 176 42 210 39 198 92 191 204 54 40 111 65 175 232 71 41 233 69 49 225 65 65 116 99 53 238 162 62 56 145 139 50 200 70 102 176 28 114 32 51 103 67 90 176 34 197 91 96 183 223 60 244 211 80 133 172 7 138 129 30 42 176 82 129 5 169 160 25 73 206 132 5 191 17 9 223 20 68 175 30 243 30 29 98 110 45 34 46 13 66 172 122 255 170 101 189 87 240 232 190 140 196 220 26 42 80 35 204 82 15 152 7 16 28 227 132 183 223 35 220 99 37 9 88 20 196 111 30 163 140 66 165 234 61 186 111 157 215 175 94 64 192 50 70 69 163 240 155 71 224 155 150 195 107 186 139 217 169 59 228 233 228 48 113 233 135 137 83 55 68 88 237 16 97 180 131 29 31 77 210 125 233 28 90 2 167 110 16 172 246 54 24 77 11 25 28 234 1 210 194 168 236 35 93 167 142 235 105 215 19 148 131 29 159 233 198 181 139 176 79 72 169 88 74 47 73 33 239 239 222 163 162 203 148 99 148 31 171 250 83 156 63 115 154 163 231 39 41 135 255 166 253 231 231 223 1 239 156 156 182 204 221 139 214 0 0 0 0 73 69 78 68 174 66 96 130]! !
!RegEdit class categoriesFor: #keyIcon!constants!public! !

!RegEdit class methodsFor!

stringIcon
	"Answers an Icon that can be used to string values."

	^InternalIcon 
		fromBytes: #[137 80 78 71 13 10 26 10 0 0 0 13 73 72 68 82 0 0 0 16 0 0 0 16 8 6 0 0 0 31 243 255 97 0 0 0 1 115 82 71 66 0 174 206 28 233 0 0 0 4 103 65 77 65 0 0 177 143 11 252 97 5 0 0 0 9 112 72 89 115 0 0 14 195 0 0 14 195 1 199 111 168 100 0 0 0 220 73 68 65 84 56 79 157 147 219 13 131 48 12 69 153 168 203 116 50 62 89 131 85 18 126 186 4 21 31 133 60 192 205 77 117 145 41 169 20 26 233 40 142 81 142 13 194 141 181 86 174 208 182 173 52 122 13 195 16 164 114 77 211 36 125 223 203 65 146 170 87 11 80 7 2 172 93 2 193 182 109 242 139 199 253 38 128 11 2 146 37 87 5 20 61 199 241 211 133 22 176 26 118 118 164 115 204 175 235 42 96 23 48 193 157 151 112 46 197 49 42 129 73 223 32 198 32 224 187 154 206 233 248 208 129 177 38 11 120 185 38 14 33 8 200 175 96 140 73 177 63 85 135 176 148 71 142 236 2 239 189 252 131 18 184 36 184 78 22 116 93 39 4 51 225 156 171 230 52 23 20 57 183 36 201 146 255 25 93 0 49 159 97 63 9 48 100 232 2 15 8 206 28 62 196 144 204 243 43 83 20 28 70 181 112 160 132 157 189 1 190 52 13 233 65 30 83 35 0 0 0 0 73 69 78 68 174 66 96 130]! !
!RegEdit class categoriesFor: #stringIcon!constants!public! !

!RegEdit methodsFor!

newKey
	"Add a new, empty, sub-key under the currently selected key, 
	and leave the receiver in key renaming mode."

	| i sel newKey |
	i := 1.
	sel := self selectedKey.
	"Generate a unique key name"
	[newKey := 'New Key #', i displayString. sel includesKey: newKey] whileTrue: [i := i + 1].
	newKey := sel createKey: newKey.
	keyPresenter model add: newKey asChildOf: sel.
	keyPresenter
		selection: newKey;
		view editSelectionLabel! !
!RegEdit categoriesFor: #newKey!commands!public! !

!RegEdit methodsFor!

newValueName
	"Private - Answer a new unique value name under the currently selected key."

	| i sel unique |
	i := 1.
	sel := self selectedKey subValues.
	
	[unique := 'New Value #' , i displayString.
	sel includesKey: unique] whileTrue: [i := i + 1].
	^unique! !
!RegEdit categoriesFor: #newValueName!commands!private! !

!RegEdit methodsFor!

deleteSelectedKey
	"Private - Delete the selected registry key."

	| key mb |
	key := self selectedKey.
	key isRoot ifTrue: [^nil].	"Can't delete root key."
	mb := MessageBox new.
	mb text: 'Are you sure you want to permanently delete this key and all of its subkeys?'.
	mb caption: 'Confirm Key Delete'.
	mb icon: Warning icon.
	mb buttonStyle: #yesNo.
	mb open == #yes 
		ifTrue: 
			[| parentKey |
			parentKey := key parentKey.
			
			[parentKey removeKey: key name ifAbsent: [^self].
			keyPresenter model remove: key] 
					on: HRESULTError
					do: [:e | e okCancel]]! !
!RegEdit categoriesFor: #deleteSelectedKey!commands!private! !

!RegEdit methodsFor!

deleteSelectedValue
	"Private - Delete the selected registry value."

	| idx mb |
	idx := valuesPresenter selectionByIndex.
	idx == 0 ifTrue: [^self].
	mb := MessageBox new.
	mb text: 'Deleting certain registry values could cause system instability. Are you sure you want to permanently delete these values?'.
	mb caption: 'Confirm Value Delete'.
	mb icon: Warning icon.
	mb buttonStyle: #yesNo.
	mb open == #yes 
		ifTrue: 
			[
			[| value |
			value := valuesPresenter model at: idx.
			self selectedKey subValues removeKey: value key ifAbsent: [^self].
			value value: nil.
			value key isEmpty 
				ifTrue: [valuesPresenter model refreshAtIndex: idx]
				ifFalse: [valuesPresenter model removeAtIndex: idx]] 
					on: HRESULTError
					do: [:e | e okCancel]]! !
!RegEdit categoriesFor: #deleteSelectedValue!commands!private! !

!RegEdit class methodsFor!

about
	"Private - Pop up a little helpful info. about this sample program."

	(MessageBox new)
		caption: 'About Dolphin Registry Editor';
		text: ('Simple Registry Editor Sample. Copyright (c) Object Arts Ltd, 1998-2011.<n><1s><n>
This is a reduced functionality implementation of the standard system utility REGEDIT.EXE, intended to demonstrate various features of Dolphin''s application framework such as virtual tree models, and in-place label editing.<n> 
DISCLAIMER: This software is freely provided purely as an educational sample and as such it is provided "as is", WITHOUT ANY WARRANTY; without even the implied warranty of  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. Furthermore, as with the real registry editor extreme care and expert knowledge are required to avoid damaging the installation of the operating system or other software on your machine if any modifications to the registry are attempted. Please take care - we accept no responsibility for any damage to your computer''s configuration which may be caused by the use of this sample.' 
					expandMacrosWith: VMLibrary default fullVersion);
		open! !
!RegEdit class categoriesFor: #about!enquiries!private! !

"** WF13 ** Uninstall Simple Web Browser sample since it requires IE component"!
| swb |
swb := (Package manager packageNamed: 'Simple Web Browser' ifNone:[]).
swb notNil ifTrue: [swb uninstall]!

"** WF14 ** Remove CodeMentorPlugin from all browsers since it requires IE component"!
Smalltalk at: #CodeMentorPlugin ifPresent: [:cmp |
	ClassBrowserShell plugins: (ClassBrowserShell plugins reject: [:each | each==cmp]).
	(Smalltalk at: #SystemBrowserShell) plugins: (ClassBrowserShell plugins reject: [:each | each==cmp])]!

"Appearances for Mac OS/X"!
KernelLibrary isMacOSX ifTrue: [
	| macSystemFont |
	macSystemFont := (Font name: 'Lucida Grande' pointSize: 10) .
	SmalltalkWorkspace defaultFont: macSystemFont.
	SmalltalkSystem current defaultFont: macSystemFont.
	SmalltalkSystemShell defaultBackcolor: (RGB red: 0 green: 128 blue: 192).
	(ResourceIdentifier class: SmalltalkSystemShell name: 'Default view') editViewUsing: [:shell |
		(shell viewNamed: 'systemFolder') 
			forecolor: Color white;
			iconSpacing: 96@96].
	]!

"Reset UI"!
SmalltalkSystem current panic !



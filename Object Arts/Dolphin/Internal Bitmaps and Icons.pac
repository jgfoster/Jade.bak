| package |
package := Package name: 'Internal Bitmaps and Icons'.
package paxVersion: 1;
	basicComment: 'One of the most common reasons why Dolphin applications need to use external files is to make use of icon and bitmap resources. These may be required solely to present your Dolphin classes with an icon inside the browsers or for more functional reasons such as toolbar buttons etc. 

This package makes use of GDI+ to load in external image files and hold them as GdiplusBitmap byte array initializers within the image. Hence the appropriate bitmap can be created at anytime without reference to the external file. Typically, the external file will be a PNG file, which can contain transparency information and is therefore capable of replacing most functionality of standard Windows ICO files. Other files, such as BMP and JPG, may also be used if alpha transparency is not required. Note, however, that currently ICO files are not allowed due to an inability to easily load the correct (large) resource from the icon file (anyone care to fix this?)

USAGE:

(1) If you want to install a class side #icon method for one of your classes simply go to any workspace and evaluate:

MyClass createIconMethod.

This will prompt for a (usually PNG) file, and then create a MyClass class>>icon method for it. If you browse this method you''ll see it answers an instance of InternalIcon instantiated from a literal byte array. Note that InternalIcons automatically reformat any image to 48@48 pixels by default.

(2) If you want to add an InternalIcon to a toolbar, in the View Composer first create a ToolbarIconButton in your toolbar and locate the commandDescription/image aspect for this button. Then type and accept:

InternalIcon chooseFile

This will prompt you for a suitable image file (again PNG files work best) and will load the image bytes into a literal byte array from which the InternalIcon can be instantiated when required. 

EFFICIENCY:

Obviously, holding byte array initializers in the Dolphin image is somewhat more inefficient in terms of runtime object memory usage than demand loading external icon files. However, the PNG format in which the image bytes are held is relatively efficient and is significantly smaller than the equivalent ICO file (which often has several different resolution bitmaps within it). So although the object memory load may be greater, the overall application size will generally be smaller. This seems like a worthwhile compromise given the fact that the need to distribute separate ICO/BMP files with your application is eradicated. An additional gain is that, because the InternalIcons are represented entirely within Smalltalk methods, they can be held in an STS repository, which is not possible with external files.

VISUAL RESOLUTION

Unlike ICO files, InternalIcons work by holding a single high (=48@48 by default) resolution image and then rescaling this on demand to any lower resolutions that are required. This generally works rather well, since GDI+ is good at performing anti-aliased scaling. However, this doesn''t have the same flexibility of hand tailoring the lower resolution images that one gets with ICO files. Thus you might have to choose your icon images with more care in order to avoid poor results with smaller icons renditions.

CAUTION:

Both InternalIcons and InternalBitmaps will display their full byte array initializers if you use #displayString or #printString. If you choose to load a huge bitmap into one of these objects (which is not a good idea anyway) this could cause problems if you "Display It" in a workspace. It is for this reason that InternalIcons forcibly reformat any supplied image to a default size of 48@48 pixels. Thus, InternalIcons should never be too large to "Display It". Indeed, this is the only difference between InternalIcons and InternalBitmaps; the latter are not resized on load and so can be arbitrarily large.'.

package basicPackageVersion: '0.010'.


package classNames
	add: #GdiplusBitmapFromBytesInitializer;
	add: #GdiplusFromBytesInitializer;
	add: #GdiplusImageFromBytesInitializer;
	add: #InternalBitmap;
	add: #InternalIcon;
	yourself.

package methodNames
	add: #ClassDescription -> #createIconMethod;
	add: #ClassDescription -> #createIconMethod:OfSize:;
	add: #ClassDescription -> #createIconMethod:ofSize:fromFile:;
	add: #Collection -> #printOn:upTo:;
	add: #Collection -> #printStringUpTo:;
	add: #GdiplusImage -> #addToImageList:mask:;
	add: #GdiplusImage -> #internalize;
	add: 'GdiplusImage class' -> #fromByteArray2:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Dolphin\Base\Dolphin';
	add: '..\..\Dolphin\MVP\Dialogs\Common\Dolphin Common Dialogs';
	add: '..\..\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\Dolphin\MVP\Gdiplus\Gdiplus';
	add: '..\..\Dolphin\ActiveX\Structured Storage\OLE Structured Storage';
	yourself).

package!

"Class Definitions"!

Object subclass: #InternalBitmap
	instanceVariableNames: 'gdiplusBitmap'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GdiplusInitializer subclass: #GdiplusFromBytesInitializer
	instanceVariableNames: 'bytes'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GdiplusFromBytesInitializer subclass: #GdiplusBitmapFromBytesInitializer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GdiplusFromBytesInitializer subclass: #GdiplusImageFromBytesInitializer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
InternalBitmap subclass: #InternalIcon
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!ClassDescription methodsFor!

createIconMethod
	"Prompts the user for an image or icon file and builds and compiles an #icon method from it for this class"

	^self createIconMethod: #icon OfSize: 48!

createIconMethod: methodName OfSize: pixelSize 
	| extent imageFileName |
	extent := pixelSize asPoint.
	imageFileName := FileOpenDialog showModal.
	imageFileName ifNil: [^self].
	^self 
		createIconMethod: methodName
		ofSize: extent
		fromFile: imageFileName!

createIconMethod: methodName ofSize: pixelSize fromFile: imageFileName 
	| extent internalIcon methodSourceStream |
	extent := pixelSize asPoint.
	(File splitExtensionFrom: imageFileName) asLowercase = 'ico' 
		ifTrue: 
			[#todo.	"Anyone care to fix this?"
			MessageBox 
				warning: 'Can''t currently compile from ICO files. 

Why not use: 
  http://www.converticon.com
to convert to PNG format first?'.
			^self].

	"Let GDI+ do the image load"
	internalIcon := InternalIcon fromFile: imageFileName extent: extent.
	methodSourceStream := String writeStream.
	methodSourceStream
		nextPutAll: methodName;
		cr;
		nextPutAll: '
	"Generated from:
	self createIconMethod: ';
		nextPutAll: methodName printString;
		nextPutAll: ' ofSize: ';
		nextPutAll: extent printString;
		nextPutAll: ' fromFile: ';
		nextPutAll: imageFileName printString;
		nextPutAll: '.
	"
	^'.
	internalIcon printOn: methodSourceStream.
	self class compile: methodSourceStream contents
		categories: (Array with: MethodCategory public with: (MethodCategory name: 'constants'))! !
!ClassDescription categoriesFor: #createIconMethod!development!public! !
!ClassDescription categoriesFor: #createIconMethod:OfSize:!public! !
!ClassDescription categoriesFor: #createIconMethod:ofSize:fromFile:!public! !

!Collection methodsFor!

printOn: aStream upTo: maxPrintCharacters 
	"Print a string representation of self on aStream. This method suffices for 
	most collections, and is able to handle cyclic references."

	| printed |
	printed := Processor activeProcess _alreadyPrinted.
	(printed includes: self) ifTrue: [^self printCyclicRefOn: aStream].
	printed add: self.
	
	[| tooMany |
	tooMany := aStream position + maxPrintCharacters.
	self printPrefixOn: aStream.
	self do: 
			[:each | 
			aStream position > tooMany 
				ifTrue: 
					[aStream nextPutAll: '... etc ...'.
					^self].
			each printOn: aStream]
		separatedBy: [aStream space].
	self printSuffixOn: aStream] 
			ensure: [printed remove: self ifAbsent: []]!

printStringUpTo: macPrintCharacters 
	"Answer a <readableString> whose characters are a description of the receiver 
	as a developer would want to see it."

	| stream |
	stream := String writeStream: 32.
	self printOn: stream upTo: macPrintCharacters.
	^stream contents! !
!Collection categoriesFor: #printOn:upTo:!printing!public! !
!Collection categoriesFor: #printStringUpTo:!printing!public! !

!GdiplusImage methodsFor!

addToImageList: aWinImageList mask: aColorOrNil 
	"Private - Add a pictorial representation of the receiver to aWinImageList scaled to the
	extent of the image list. We assume that the receiver contains transparency information so
	the mask color is ignored"

	"The chage to use a display compatible bitmap is for #2280"

	| thumb bitmap |
	thumb := self class fromImage: self extent: aWinImageList extent.
	bitmap := Bitmap displayCompatibleWithExtent: thumb extent.
	thumb drawOn: bitmap canvas.
	^bitmap addToImageList: aWinImageList mask: nil!

internalize
	"Answer a copy of the receiver created from the PNG bytes. This can be held in the image and
	removes the need for an external file reference"

	^self class fromByteArray2: (self asByteArray: 'image/png')! !
!GdiplusImage categoriesFor: #addToImageList:mask:!private! !
!GdiplusImage categoriesFor: #internalize!public! !

!GdiplusImage class methodsFor!

fromByteArray2: aByteArray 
	"Answer an instance of the receiver created from data in aByteArray. The results is an instance with a persistent
	initializer that can be used after an image reload or after passing through STB serialization/deserialization. Eventually,
	this should replace the original #fromByteArray: method."

	#todo.	"Replace #fromByteArray: with this"
	^self fromInitializer: (GdiplusImageFromBytesInitializer bytes: aByteArray)! !
!GdiplusImage class categoriesFor: #fromByteArray2:!instance creation!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

InternalBitmap guid: (GUID fromString: '{B9454399-94C0-4572-BC54-802FF110731F}')!
InternalBitmap comment: 'An InternalBitmap is capable of holding it''s image bytes internally in the Smalltalk image. Thus it is no longer necessary to refer to external files outside the image to make use of bitmap resources. This has several advantages, including the ability to version InternalBitmap resources into STS as part of a Smalltalk method and the fact that fewer external files need to be distributed with an application. 

A word of caution, however. InternalBitmaps will display their full byte array initializers if you use #displayString or #printString. If you choose to load a huge bitmap into one of these objects (which is not a good idea anyway) this could cause problems if you "Display It" in a workspace. It is for this reason that InternalIcons forcibly reformat any supplied image to a default size of 48@48 pixels. Thus, InternalIcons should never be too large to "Display It". Indeed, this is the only difference between InternalIcons and InternalBitmaps; the latter are not resized on load and so can be arbitrarily large.
'!
!InternalBitmap categoriesForClass!Kernel-Objects! !
!InternalBitmap methodsFor!

= comperand 
	"Gdiplus images comapre equal if their initializers are equal. This allows GdiplusImages to be added to ImageManagers"

	^self species = comperand species and: [self bitmap initializer = comperand bitmap initializer]!

addToImageList: aWinImageList mask: aColorOrNil 
	"Private - Add a pictorial representation of the receiver to aWinImageList scaled to the
	extent of the image list. We assume that the receiver contains transparency information so
	the mask color is ignored"

	^self bitmap addToImageList: aWinImageList mask: aColorOrNil!

asBitmap
	| bitmap |
	bitmap := DIBSection 
				width: self extent x
				height: self extent y
				depth: 32.
	gdiplusBitmap drawOn: bitmap canvas.
	^bitmap!

asByteArray
	"Ideally, we don't want to duplicate pixel byte arrays unnecessarily. Hence the ugly type
	test here. If the receiver has been created from a GdiplusFromBytesInitializer then we
	can just answer the original bytes"

	(gdiplusBitmap initializer isKindOf: GdiplusFromBytesInitializer) 
		ifTrue: [^gdiplusBitmap initializer bytes].

	"Otherwise ask GDI+ to yield the PNG bytes for the image"
	^gdiplusBitmap asByteArray: 'image/png'!

asIcon
	"Answer the receiver converted to a normal Windows icon format. We do this using a temporary
	ImageManager but NOT the default IconImageManager. Since the receiver could be of arbitrary size this
	would result in large image list being rendered for all icons. "

	| imageList imageManager index |
	imageManager := ImageManager new.
	index := imageManager addImage: self.
	imageList := imageManager imageListWithExtent: self extent.
	^imageList getIcon: index style: 32!

bitmap
	^gdiplusBitmap!

drawOn: aCanvas 
	^self bitmap drawOn: aCanvas!

drawOn: aCanvas at: aPoint 
	"Draw the receiver on aCanvas at position aPoint (no stretching)."

	self bitmap drawOn: aCanvas at: aPoint!

drawOn: aCanvas at: dstOrigin extent: dstExtent 
	"Draw the receiver on aCanvas at Point dstOrigin, and with size dstExtent. 
	The receiver is stretched to fit the destination rectangle."

	self bitmap 
		drawOn: aCanvas
		at: dstOrigin
		extent: dstExtent!

drawOnGraphics: aGraphics at: dstOrigin extent: dstExtent from: srcOrigin extent: srcExtent unit: aUnit attributes: aImageAtt 
	^self bitmap 
		drawOnGraphics: aGraphics
		at: dstOrigin
		extent: dstExtent
		from: srcOrigin
		extent: srcExtent
		unit: aUnit
		attributes: aImageAtt!

extent
	^self bitmap extent!

handle
	^self bitmap handle!

hash
	^self bitmap initializer hash!

imageType
	^Win32Constants.IMAGE_BITMAP!

printOn: aStream 
	"Append the ASCII representation of the receiver to aStream."

	aStream
		print: self class;
		space;
		nextPutAll: #fromBytes:;
		space.
	self asByteArray printOn: aStream upTo: 100000!

setBitmap: aGdiplusBitmap 
	gdiplusBitmap := aGdiplusBitmap! !
!InternalBitmap categoriesFor: #=!comparing!comparison!public! !
!InternalBitmap categoriesFor: #addToImageList:mask:!double dispatch!private! !
!InternalBitmap categoriesFor: #asBitmap!public! !
!InternalBitmap categoriesFor: #asByteArray!converting!public! !
!InternalBitmap categoriesFor: #asIcon!public! !
!InternalBitmap categoriesFor: #bitmap!accessing!public! !
!InternalBitmap categoriesFor: #drawOn:!drawing-bitmaps!public! !
!InternalBitmap categoriesFor: #drawOn:at:!drawing-bitmaps!public! !
!InternalBitmap categoriesFor: #drawOn:at:extent:!drawing-bitmaps!public! !
!InternalBitmap categoriesFor: #drawOnGraphics:at:extent:from:extent:unit:attributes:!drawing-gdiplus!public! !
!InternalBitmap categoriesFor: #extent!accessing!public! !
!InternalBitmap categoriesFor: #handle!public! !
!InternalBitmap categoriesFor: #hash!comparing!comparison!public! !
!InternalBitmap categoriesFor: #imageType!public! !
!InternalBitmap categoriesFor: #printOn:!printing!public! !
!InternalBitmap categoriesFor: #setBitmap:!initializing!private! !

!InternalBitmap class methodsFor!

chooseFile
	| filename |
	filename := FileOpenDialog showModal.
	(File splitExtensionFrom: filename) asLowercase = 'ico' 
		ifTrue: 
			[#todo.	"Anyone care to fix this?"
			MessageBox 
				warning: 'Can''t currently internalize ICO files. 

Why not use: 
  http://www.converticon.com
to convert to PNG format first?'.
			filename := nil].
	filename ifNil: [self error: 'No file selected'].
	^self fromFile: filename!

fromBytes: aByteArray 
	^(super new)
		setBitmap: (GdiplusBitmap fromByteArray2: aByteArray);
		yourself!

fromFile: aFilename 
	^(super new)
		setBitmap: (GdiplusBitmap fromFile: aFilename) internalize;
		yourself!

imageManager
	"Private - Answer an image manager to use for storing images"

	^ImageManager new! !
!InternalBitmap class categoriesFor: #chooseFile!public! !
!InternalBitmap class categoriesFor: #fromBytes:!public! !
!InternalBitmap class categoriesFor: #fromFile:!public! !
!InternalBitmap class categoriesFor: #imageManager!accessing!private! !

GdiplusFromBytesInitializer guid: (GUID fromString: '{3156F9E7-C46C-48E7-9CCA-E6405BF8A085}')!
GdiplusFromBytesInitializer comment: ''!
!GdiplusFromBytesInitializer categoriesForClass!Unclassified! !
!GdiplusFromBytesInitializer methodsFor!

= comperand 
	^self species = comperand species and: [self bytes = comperand bytes]!

bytes
	^bytes!

hash
	^self bytes hash!

iStream
	| iStream |
	iStream := IStream onHGLOBAL.
	iStream nextPutAll: bytes.
	iStream reset.
	^iStream!

setBytes: aByteArray 
	bytes := aByteArray! !
!GdiplusFromBytesInitializer categoriesFor: #=!comparing!public! !
!GdiplusFromBytesInitializer categoriesFor: #bytes!accessing!public! !
!GdiplusFromBytesInitializer categoriesFor: #hash!comparing!public! !
!GdiplusFromBytesInitializer categoriesFor: #iStream!public!realizing/unrealizing! !
!GdiplusFromBytesInitializer categoriesFor: #setBytes:!initializing!private! !

!GdiplusFromBytesInitializer class methodsFor!

bytes: aByteArray 
	^self new setBytes: aByteArray! !
!GdiplusFromBytesInitializer class categoriesFor: #bytes:!instance creation!public! !

GdiplusBitmapFromBytesInitializer guid: (GUID fromString: '{6E7E335E-9DD7-44FA-8648-96E7D9528F11}')!
GdiplusBitmapFromBytesInitializer comment: ''!
!GdiplusBitmapFromBytesInitializer categoriesForClass!Unclassified! !
!GdiplusBitmapFromBytesInitializer methodsFor!

createHandle
	" Bitmap::Bitmap(
	    IN IStream *stream, 
	    IN BOOL useEmbeddedColorManagement
	    )
	...
	 {
	        lastResult = DllExports::GdipCreateBitmapFromStream(stream, &bitmap);
	    }"

	| gpHandle status |
	gpHandle := ExternalHandle new.
	status := self library gdipCreateBitmapFromStream: self iStream asParameter bitmap: gpHandle.
	self assertStatusOk: status.
	^gpHandle!

gdiplusConstructorErrorDescription
	^'Error creating Bitmap'! !
!GdiplusBitmapFromBytesInitializer categoriesFor: #createHandle!public!realizing/unrealizing! !
!GdiplusBitmapFromBytesInitializer categoriesFor: #gdiplusConstructorErrorDescription!constants!private! !

GdiplusImageFromBytesInitializer guid: (GUID fromString: '{351525B1-07B3-43C9-B7D0-BC92D49DB495}')!
GdiplusImageFromBytesInitializer comment: ''!
!GdiplusImageFromBytesInitializer categoriesForClass!Unclassified! !
!GdiplusImageFromBytesInitializer methodsFor!

createHandle
	" Bitmap::Bitmap(
	    IN IStream *stream, 
	    IN BOOL useEmbeddedColorManagement
	    )
	...
	 {
	        lastResult = DllExports::GdipCreateBitmapFromStream(stream, &bitmap);
	    }"

	| gpHandle status |
	gpHandle := ExternalHandle new.
	status := self library gdipLoadImageFromStream: self iStream asParameter image: gpHandle.
	self assertStatusOk: status.
	^gpHandle!

gdiplusConstructorErrorDescription
	^'Error creating Image'! !
!GdiplusImageFromBytesInitializer categoriesFor: #createHandle!public!realizing/unrealizing! !
!GdiplusImageFromBytesInitializer categoriesFor: #gdiplusConstructorErrorDescription!constants!private! !

InternalIcon guid: (GUID fromString: '{7446A3C1-E3AE-45B8-BCA2-BBD493A5C7D3}')!
InternalIcon comment: 'An InternalIcon is capable of holding it''s image bytes internally in the Smalltalk image. Thus it is no longer necessary to refer to external files outside the image to make use of Icon resources. This has several advantages, including the ability to version InternalIcon resources into STS as part of a Smalltalk method and the fact that fewer external files need to be distributed with an application.

USAGE:

(1) If you want to install a class side #icon method for one of your classes simply go to any workspace and evaluate:

MyClass createIconMethod.

This will prompt for a (usually PNG) file, and then create a MyClass class>>icon method for it. If you browse this method you''ll see answers an instance of InternalIcon instantiated from a literal byte array. Note that InternalIcons automatically reformat any image to 48@48 pixels by default.

(2) If you want to add an InternalIcon to a toolbar, in the View Composer first create a ToolbarIconButton in your toolbar and locate the commandDescription/image aspect for this button. Then type and accept:

InternalIcon chooseFile

This will prompt you for a suitable image file (again PNG files work best) and will load the image bytes into a literal byte array from which the InternalIcon can be instantiated when required. 

'!
!InternalIcon categoriesForClass!Kernel-Objects! !
!InternalIcon methodsFor!

asAlphaBitmap: aPoint 
	"Answer a 32-bpp bitmap (i.e. with Alpha channel) that contains the receiver's
	image rendered at the specified size."

	| bmp |
	bmp := DIBSection 
				width: aPoint x
				height: aPoint y
				depth: 32.
	"Use the services of an ImageList to get the correct sized icon automagically, and to
	provide an alpha channel even if the original icon had no more than a simple mask."
	(self class imageManager imageListWithExtent: aPoint) 
		draw: self imageIndex
		on: bmp canvas
		at: Point zero
		flags: 4096.
	bmp freeDC.
	^bmp!

asMenuBitmap
	"Answer a 32-bpp bitmap (i.e. with alpha channel) containing the receiver's image at system
	menu image extent. Note that the result will be a shared bitmap which is always the same
	instance for the same icon."

	^self class imageManager menuBitmapForIcon: self!

asParameter
	"Create an icon from the image list so we can answer the handle"

	^self asIcon detachHandle!

hotspot
	^self extent / 2!

imageIndex
	^Icon imageManager indexOfImage: self!

imageType
	^Win32Constants.IMAGE_ICON! !
!InternalIcon categoriesFor: #asAlphaBitmap:!converting!public! !
!InternalIcon categoriesFor: #asMenuBitmap!converting!public! !
!InternalIcon categoriesFor: #asParameter!public! !
!InternalIcon categoriesFor: #hotspot!public! !
!InternalIcon categoriesFor: #imageIndex!public! !
!InternalIcon categoriesFor: #imageType!public! !

!InternalIcon class methodsFor!

defaultExtent
	^48 @ 48!

fromFile: aFilename 
	^self fromFile: aFilename extent: self defaultExtent!

fromFile: aFilename extent: aPoint 
	| bitmap |
	bitmap := GdiplusBitmap fromFile: aFilename.
	bitmap := (bitmap thumbnailWithExtent: aPoint asPoint) internalize.
	^(super new)
		setBitmap: bitmap;
		yourself!

imageManager
	"Private - Answer an image manager to use for storing icons"

	^IconImageManager current! !
!InternalIcon class categoriesFor: #defaultExtent!private! !
!InternalIcon class categoriesFor: #fromFile:!public! !
!InternalIcon class categoriesFor: #fromFile:extent:!public! !
!InternalIcon class categoriesFor: #imageManager!accessing!private! !

"Binary Globals"!


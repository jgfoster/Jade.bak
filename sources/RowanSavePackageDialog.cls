"Filed out from Dolphin Smalltalk 7"!

MCVersionDialog subclass: #RowanSavePackageDialog
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanSavePackageDialog guid: (GUID fromString: '{204cf49e-d0c3-4402-ba59-a15cafb7f7d5}')!
RowanSavePackageDialog comment: ''!
!RowanSavePackageDialog categoriesForClass!Unclassified! !
!RowanSavePackageDialog class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.DialogView) 34 30 nil nil 8 #(13369344 65536) 416 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 167 nil nil nil 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 6 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1409286144 131073 576 nil nil nil 7 nil nil nil 576 514 170 176 34 4 410 ##(Smalltalk.PushButton) 34 20 nil 576 34 2 8 1140924416 1 688 nil nil nil 7 nil nil nil 688 nil 8 4294908323 1180998 4 ##(Smalltalk.CommandDescription) #cancel 8 'Cancel' 1 1 nil nil false nil nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 479 25 930 151 47 688 882 #text: 34 1 8 'Cancel' 688 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 239 0 0 0 12 0 0 0 58 1 0 0 35 0 0 0] 8 #() 930 193 193 nil 29 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedPreviousRight 21 1138 #fixedViewLeft 151 1138 #fixedViewBottom -45 1138 #fixedParentBottom 1 410 ##(Smalltalk.PushButton) 34 20 nil 576 34 2 8 1141055488 1 1216 nil nil nil 7 nil nil nil 1216 nil 8 4294908323 770 #ok 8 'Commit' 1 1 nil nil true nil nil nil 818 138 144 34 2 882 #createAt:extent: 34 2 930 309 25 930 151 47 1216 882 #text: 34 1 8 'Commit' 1216 1026 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 154 0 0 0 12 0 0 0 229 0 0 0 35 0 0 0] 8 #() 1088 nil 29 1106 1138 #fixedViewRight -149 1138 #fixedParentRight -169 1184 -45 1200 1 170 192 8 #() 590342 ##(Smalltalk.Rectangle) 930 11 11 930 1 1 818 138 144 34 1 882 #createAt:extent: 34 2 930 21 633 930 629 71 576 1026 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 60 1 0 0 68 1 0 0 95 1 0 0] 34 2 1216 688 1088 nil 27 1106 1138 #fixedParentLeft 1 1568 1 1200 -69 1138 #fixedViewTop 71 410 ##(Smalltalk.MultilineTextEdit) 34 16 nil 416 34 2 8 1143017796 1025 1888 nil 466 8 4278190080 nil 7 nil nil nil 1888 nil 8 4294908341 852486 ##(Smalltalk.NullConverter) nil nil 9 818 138 144 34 1 882 #createAt:extent: 34 2 930 25 79 930 631 541 1888 1026 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 12 0 0 0 39 0 0 0 71 1 0 0 53 1 0 0] 8 #() 1088 nil 27 1106 1856 5 1568 7 1138 #fixedParentTop 59 1200 -83 410 ##(Smalltalk.StaticText) 34 16 nil 416 34 2 8 1140850944 1 2224 nil nil nil 7 nil nil nil 2224 nil 8 4294908027 2002 nil nil nil 818 138 144 34 2 882 #createAt:extent: 34 2 930 21 21 930 241 39 2224 882 #text: 34 1 8 'Commit Message:
Commit Message:' 2224 1026 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 10 0 0 0 130 0 0 0 29 0 0 0] 8 #() 1088 nil 27 1106 1856 1 1168 241 2208 1 1872 39 170 192 34 2 1888 8 'message' 1618 930 21 21 930 21 21 nil nil nil nil 32155 nil nil nil nil 1 nil nil 590598 ##(Smalltalk.Semaphore) nil nil 1 nil 8 2007642112 818 138 144 34 2 882 #createAt:extent: 34 2 930 3839 21 930 701 801 416 882 #updateMenuBar 1600 416 1026 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 221 8 0 0 154 1 0 0] 34 3 2224 1888 576 1088 nil 27 )! !
!RowanSavePackageDialog class categoriesFor: #resource_Default_view!public!resources-views! !


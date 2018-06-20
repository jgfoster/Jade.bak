"Filed out from Dolphin Smalltalk 7"!

JadeValueDialog subclass: #JadeErrorShell
	instanceVariableNames: 'messagePresenter stackPresenter textEditPresenter isResumable'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeErrorShell guid: (GUID fromString: '{3c339b99-4380-48fd-94fa-1b7bfeef120c}')!
JadeErrorShell comment: ''!
!JadeErrorShell categoriesForClass!Unclassified! !
!JadeErrorShell methodsFor!

createComponents

	super createComponents.
	messagePresenter 	:= self add: TextPresenter		new name: 'message'.
	stackPresenter 		:= self add: TextPresenter		new name: 'stack'.
	textEditPresenter	:= self add: TextPresenter		new name: 'textEdit'.
!

createSchematicWiring

	super createSchematicWiring.
	textEditPresenter 		when: #valueChanged send: #textEntry to: self.
!

doCopy

	stackPresenter view 
		selectAll;
		copySelection.
!

doDebug

	self return: #debug.
!

doResume

	self return: #'resume'.
!

doTerminate

	self return: #terminate.
!

onViewOpened

	| gsError gsProcess message detail stack stream |
	gsError := self model value.
	super onViewOpened.
	Sound warningBeep.
	gsProcess := gsError gsProcess.
	message := gsError errorReport message.
	stack := gsProcess stack.
	isResumable := gsError isResumableInGem.
	detail := message.
	message := message notEmpty
		ifTrue: [(message subStrings: Character lf) first]
		ifFalse: [''].
	message := message copyFrom: 1 to: (message size min: 100).
	self caption: message.
	messagePresenter value: detail replaceLfWithCrLf.
	stream := WriteStream on: String new.
	stack do: [:each | stream nextPutAll: each; cr].
	stackPresenter value: stream contents.
	self model: #'terminate'.		"If window is closed without any buttons pressed!!"!

queryCommand: query

	(query commandSymbol = #'doResume') ifTrue: [
		query isEnabled: isResumable == true.	"I've seen a 'receiver should be boolean' error so this is added as insurance"
		^true.
	].
	^super queryCommand: query.
!

return: aSymbol

	self model: aSymbol.
	self ok.
!

textEntry

	MessageBox notify: 'Sorry, we are not yet prepared to handle this feature!!'.
	SessionManager current pause.
! !
!JadeErrorShell categoriesFor: #createComponents!public! !
!JadeErrorShell categoriesFor: #createSchematicWiring!public! !
!JadeErrorShell categoriesFor: #doCopy!public! !
!JadeErrorShell categoriesFor: #doDebug!public! !
!JadeErrorShell categoriesFor: #doResume!public! !
!JadeErrorShell categoriesFor: #doTerminate!public! !
!JadeErrorShell categoriesFor: #onViewOpened!public! !
!JadeErrorShell categoriesFor: #queryCommand:!public! !
!JadeErrorShell categoriesFor: #return:!public! !
!JadeErrorShell categoriesFor: #textEntry!public! !

!JadeErrorShell class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.DialogView)  98 30 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  801 601 551 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 10 410 8 ##(Smalltalk.ContainerView)  98 15 0 416 98 2 8 1140850688 131073 624 0 482 512 0 7 0 0 0 624 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 2 410 8 ##(Smalltalk.MultilineTextEdit)  98 16 0 624 98 2 8 1144066500 1025 784 0 482 512 0 7 0 0 0 784 0 8 4294903099 852486 ##(Smalltalk.NullConverter)  0 0 11 983302 ##(Smalltalk.MessageSequence)  202 208 98 3 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 1 121 530 785 333 784 994 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 784 994 8 #isTextModified: 98 1 32 784 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 60 0 0 0 136 1 0 0 226 0 0 0] 98 0 530 193 193 0 27 7 16 234 256 98 4 410 800 98 16 0 624 98 2 8 1143017796 1025 1328 0 482 512 0 7 0 0 0 1328 0 8 4294903099 898 0 0 11 930 202 208 98 3 994 1024 98 2 530 1 1 530 785 111 1328 994 1104 98 1 1138 3 1 3 1328 994 1184 98 1 32 1328 1218 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 136 1 0 0 55 0 0 0] 98 0 1280 0 27 8 'message' 784 8 'stack' 0 930 202 208 98 1 994 1024 98 2 530 1 1 530 785 453 624 1218 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 136 1 0 0 226 0 0 0] 98 3 1328 410 8 ##(Smalltalk.Splitter)  98 12 0 624 98 2 8 1140850688 1 1872 0 482 512 0 519 0 0 0 1872 930 202 208 98 1 994 1024 98 2 530 1 111 530 785 11 1872 1218 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 55 0 0 0 136 1 0 0 60 0 0 0] 98 0 1280 0 27 784 1280 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 1 2170 2192 8 #fixedParentRight 1 2170 2192 8 #fixedParentTop 1 2170 2192 8 #fixedParentBottom -79 410 8 ##(Smalltalk.PushButton)  98 17 0 416 98 2 8 1140924416 1 2320 0 482 512 0 7 0 0 0 2320 0 8 4294902797 1180998 4 ##(Smalltalk.CommandDescription)  8 #doTerminate 8 '&Terminate' 1193 1 0 0 32 930 202 208 98 3 994 1024 98 2 530 411 463 530 171 61 2320 994 8 #isEnabled: 98 1 32 2320 994 8 #text: 98 1 8 '&Terminate' 2320 1218 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 205 0 0 0 231 0 0 0 34 1 0 0 5 1 0 0] 98 0 1280 0 27 2130 2176 411 2170 2192 8 #fixedViewLeft 171 2288 -69 2170 2192 8 #fixedViewTop 61 410 2336 98 17 0 416 98 2 8 1140924416 1 2848 0 482 512 0 7 0 0 0 2848 0 8 4294902797 2434 8 #doDebug 8 '&Debug' 1161 1 0 0 16 930 202 208 98 3 994 1024 98 2 530 13 463 530 171 61 2848 994 2624 98 1 32 2848 994 2672 98 1 8 '&Debug' 2848 1218 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 6 0 0 0 231 0 0 0 91 0 0 0 5 1 0 0] 98 0 1280 0 27 2130 2176 13 2784 171 2288 -69 2816 61 410 2336 98 17 0 416 98 2 8 1140924416 1 3248 0 482 512 0 7 0 0 0 3248 0 8 4294902797 2434 8 #doResume 8 '&Resume' 1189 1 0 0 32 930 202 208 98 3 994 1024 98 2 530 211 463 530 171 61 3248 994 2624 98 1 32 3248 994 2672 98 1 8 '&Resume' 3248 1218 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 105 0 0 0 231 0 0 0 190 0 0 0 5 1 0 0] 98 0 1280 0 27 2130 2176 211 2784 171 2288 -69 2816 61 410 2336 98 17 0 416 98 2 8 1140924416 1 3648 0 482 512 0 7 0 0 0 3648 0 8 4294902797 2434 8 #doCopy 8 '&Copy' 1159 1 0 0 32 930 202 208 98 3 994 1024 98 2 530 611 463 530 161 61 3648 994 2624 98 1 32 3648 994 2672 98 1 8 '&Copy' 3648 1218 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 49 1 0 0 231 0 0 0 129 1 0 0 5 1 0 0] 98 0 1280 0 27 2130 2176 611 2784 161 2288 -69 2816 61 234 256 98 0 590342 ##(Smalltalk.Rectangle)  530 1 1 530 1 1 0 0 0 0 13627 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 32 8 2010572111 930 202 208 98 2 994 1024 98 2 530 2559 21 530 801 601 416 994 8 #updateMenuBar 4064 416 1218 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 10 0 0 0 143 6 0 0 54 1 0 0] 98 5 2848 3248 2320 3648 624 1280 0 27 )! !
!JadeErrorShell class categoriesFor: #resource_Default_view!public!resources-views! !


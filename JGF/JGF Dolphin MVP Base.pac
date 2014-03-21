| package |
package := Package name: 'JGF Dolphin MVP Base'.
package paxVersion: 1;
	basicComment: ''.

package basicPackageVersion: '0.015'.

package basicScriptAt: #postinstall put: '| viewComposer subView class |

SmalltalkSystem current 
	defaultFont: (Font  name: ''Arial'' pointSize: 9);
	showSplashAtStartup: false;
	yourself.
SmalltalkSystemShell 
	showTipsAtStartup: false;
	playSystemSounds: false;
	yourself.
SmalltalkWorkspace 
	defaultFont:  (Font  name: ''Times New Roman'' pointSize: 11);
	defaultTabWidth: 4;
	yourself.
SmalltalkWorkspaceDocument
	defaultFont:  (Font  name: ''Times New Roman'' pointSize: 11);
	defaultTabWidth: 4;
	yourself.
SUnitBrowser shouldInitializeResources: true.

viewComposer := ViewComposer show: ''Vertical view''. 

viewComposer openOn: (ResourceIdentifier class: ResourceToolboxPresenter name: ''Default view''). 
subView := viewComposer composingView viewNamed: ''categories''. 
subView viewMode: #list. 
viewComposer fileSave. 

viewComposer openOn: (ResourceIdentifier class: AdvancedFindDialog name: ''Default view''). 
subView := viewComposer composingView subViews first subViews third subViews third. 
subView extent: 101@85. 
subView := viewComposer composingView subViews first subViews third subViews first. 
subView extent: 144@85. 
viewComposer fileSave. 

viewComposer openOn: (ResourceIdentifier class: AdvancedFindDialog name: ''Directionless view''). 
subView := viewComposer composingView subViews first subViews third subViews second. 
subView extent: 103@87. 
subView := viewComposer composingView subViews first subViews third subViews first. 
subView extent: 143@87. 
viewComposer fileSave. 

viewComposer openOn: (ResourceIdentifier class: AdvancedFindDialog name: ''Selector view''). 
subView := viewComposer composingView subViews first subViews third subViews second. 
subView extent: 103@87. 
subView := viewComposer composingView subViews first subViews third subViews first. 
subView extent: 143@87. 
viewComposer fileSave. 

(Smalltalk at: #''CodeMentorPlugin'' ifAbsent: [nil]) ifNotNil: [:class | 
	viewComposer openOn: (ResourceIdentifier class: class name: ''Default view''). 
	subView := viewComposer composingView subViews second subViews third. 
	subView preferredExtent: 80@21. 
	viewComposer fileSave. 
].

viewComposer exit. 

(Smalltalk at: #''ApplicationDeploymentWizard'' ifAbsent: [nil]) 
	ifNotNil: [:class | class saveImageOnDeploy: false].


''JGF Dolphin MVP Base'' yourself.'.

package methodNames
	add: #CommandQuery -> #command:enable:;
	add: #FileSaveDialog -> #overwritePrompt;
	add: #KeyEvent -> #upshift;
	add: #MultilineTextEdit -> #wmPaint:wParam:lParam:;
	add: #NumberPresenter -> #onTextOverflow;
	add: #ShellView -> #maximize;
	add: #TextEdit -> #findNextWrappedEx:down:wholeWord:matchCase:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\Object Arts\Dolphin\MVP\Dialogs\Common\Dolphin Common Dialogs';
	add: '..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Number\Dolphin Number Presenter';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Text Presenter';
	yourself).

package!

"Class Definitions"!


"Global Aliases"!


"Loose Methods"!

!CommandQuery methodsFor!

command: aSymbol enable: aBoolean

	#JGF.
	self command = aSymbol ifTrue: [
		self isEnabled: aBoolean.
	].
! !
!CommandQuery categoriesFor: #command:enable:!accessing!JGF!public! !

!FileSaveDialog methodsFor!

overwritePrompt

	#JGF.
	self style: self style | OFN_OVERWRITEPROMPT.
! !
!FileSaveDialog categoriesFor: #overwritePrompt!JGF!public! !

!KeyEvent methodsFor!

upshift

	| char |
	#JGF.
	char := Character value: wParam.
	char isLowerCase ifTrue: [
		wParam := char asUppercase codePoint.
	].
! !
!KeyEvent categoriesFor: #upshift!accessing!JGF!public! !

!MultilineTextEdit methodsFor!

wmPaint: message wParam: wParam lParam: lParam

	"Somehow it seems we can come here while the widget is being destroyed."
	#JGF.
	[
		^super
			wmPaint: message 
			wParam: wParam 
			lParam: lParam.
	] on: Error do: [:ex | 
		handle isNil ifTrue: [^1].
		ex pass.
	].
! !
!MultilineTextEdit categoriesFor: #wmPaint:wParam:lParam:!event handling-win32!JGF!private! !

!NumberPresenter methodsFor!

onTextOverflow
	"Default handler for EN_MAXTEXT notification. Can be overridden, but the
	preferred customization route is to observe the #textOverflow event."

	#JGF.
	^self view onTextOverflow.
! !
!NumberPresenter categoriesFor: #onTextOverflow!error handling!JGF!public! !

!ShellView methodsFor!

maximize

	#JGF.
	self showWithStyle: SW_MAXIMIZE.
! !
!ShellView categoriesFor: #maximize!JGF!operations!public! !

!TextEdit methodsFor!

findNextWrappedEx: aString down: downBoolean wholeWord: wordBoolean matchCase: caseBoolean 

	| answer |
	#JGF.
	answer := self findNextWrapped: ((FindDetails new)
		pattern: aString;
		isForwards: downBoolean;
		isWholeWord: wordBoolean;
		isCaseSensitive: caseBoolean;
		isRegularExpression: false;
		action: #findNext;
		yourself).
	(answer isKindOf: Boolean) ifTrue: [^answer].
	^answer notEmpty.
! !
!TextEdit categoriesFor: #findNextWrappedEx:down:wholeWord:matchCase:!JGF!public!searching & replacing! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!


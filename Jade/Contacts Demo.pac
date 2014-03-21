| package |
package := Package name: 'Contacts Demo'.
package paxVersion: 1;
	basicComment: ''.

package basicPackageVersion: '0.021'.

package imageStripperBytes: (ByteArray fromBase64String: 'IVNUQiAzIEYPFQAEAAAAQ29udGFjdHNJbWFnZVN0cmlwcGVyAAAAAFIAAAANAAAAQ29udGFjdHMg
RGVtb1IAAAAUAAAAUnVudGltZVxDb250YWN0cy5leGWaAAAAUgAAAA0AAABDb250YWN0cyBEZW1v
UgAAABYAAABDb250YWN0c1Nlc3Npb25NYW5hZ2Vy77clAFIAAABDAAAAc2VsZiBsb2FkSmFkZVNl
cnZlclNvdXJjZUNhY2hlOyBjb3B5UnVudGltZUZpbGVzOyBjbG9zZUxvZ2luU2hlbGxzLgAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA==').

package classNames
	add: #Contact;
	add: #ContactEditor;
	add: #ContactsBrowser;
	add: #ContactsImageStripper;
	add: #ContactsLoginShell;
	add: #ContactsSessionManager;
	add: #ContactsShell;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\Object Arts\Dolphin\MVP\Models\List\Dolphin List Models';
	add: '..\Object Arts\Dolphin\MVP\Presenters\List\Dolphin List Presenter';
	add: '..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Text Presenter';
	add: '..\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	add: 'Jade Deployment';
	add: 'Jade Login';
	yourself).

package!

"Class Definitions"!

Model subclass: #Contact
	instanceVariableNames: 'oop name title phone'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeImageStripper subclass: #ContactsImageStripper
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Shell subclass: #ContactsShell
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ContactsShell subclass: #ContactEditor
	instanceVariableNames: 'contact namePresenter titlePresenter phonePresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ContactsShell subclass: #ContactsBrowser
	instanceVariableNames: 'listPresenter commitRecordPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeLoginShell subclass: #ContactsLoginShell
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeSessionManager subclass: #ContactsSessionManager
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

Contact guid: (GUID fromString: '{4F617257-BA88-4FCE-AD0B-5D8918FEA0EF}')!
Contact comment: '
| segment dict newUser |
segment := Segment newInRepository: SystemRepository.
segment group: ''Publishers'' authorization: #''write''.
segment group: ''Subscribers'' authorization: #''read''.
UserGlobals
	at: #''ContactsSecurityPolicy''
	put: segment.
System commitTransaction ifFalse: [nil error: ''commit failed''].

(dict := SymbolDictionary new)
	at: #''ContactsGlobals'' put: dict;
	at: #''ContactsSecurityPolicy'' put: segment;
	assignToSegment: segment;
	yourself.
(AllUsers userWithId: ''DataCurator'') insertDictionary: dict at: 1.
System commitTransaction ifFalse: [nil error: ''commit failed''].

newUser := AllUsers
	addNewUserWithId: ''Manager1''
	password: ''Monkey''. 
newUser
	addGroup: ''Subscribers''; 
	addPrivilege: ''CodeModification''; 
	insertDictionary: dict at: 1;
	yourself.
System commitTransaction ifFalse: [nil error: ''commit failed''].
'!
!Contact categoriesForClass!Unclassified! !
!Contact methodsFor!

attemptLock: aSymbol session: aGciSession

	| string |
	self oop isEmpty ifTrue: [
		MessageBox notify: 'Save object before attempting lock!!'.
		^self.
	].
	string := '
| object |
object := ContactList 
	detect: [:each | each asOop = ' , self oop , ']
	ifNone: [nil error: ''Contact has been deleted!!''].
System ' , aSymbol , 'Lock: object.
System addToCommitOrAbortReleaseLocksSet: object.'.
	[
		aGciSession executeString: string.
		Sound informationBeep.
	] on: Error do: [:ex | 
		MessageBox errorMsg: ex description.
	].
!

initialize

	oop 		:= '0'.
	name 	:= ''.
	title 		:= ''.
	phone 	:= ''.
!

initialize: aString

	| list |
	list := aString , '					' subStrings: Character tab.
	oop 		:= list at: 1.
	name 	:= list at: 2.
	title 		:= list at: 3.
	phone 	:= list at: 4.
!

name
	^name!

name: anObject
	name := anObject!

oop
	^oop!

oop: anObject
	oop := anObject!

phone
	^phone!

phone: anObject
	phone := anObject!

saveUsing: gciSession

	| string result |
	string := ' 
| array |
array := ContactList 
	detect: [:each | each asOop = ' , oop , ']
	ifNone: [ContactList add: (Array new: 3)].
array
	at: 1 put: ' , name printString , ';
	at: 2 put: ' , title printString , ';
	at: 3 put: ' , phone printString , ';
	yourself.
nil'.
	result := gciSession executeString: string.
	result isNil ifTrue: [^self].
	MessageBox warning: result.
!

title
	^title!

title: anObject
	title := anObject! !
!Contact categoriesFor: #attemptLock:session:!public! !
!Contact categoriesFor: #initialize!public! !
!Contact categoriesFor: #initialize:!public! !
!Contact categoriesFor: #name!accessing!public! !
!Contact categoriesFor: #name:!accessing!public! !
!Contact categoriesFor: #oop!accessing!public! !
!Contact categoriesFor: #oop:!accessing!public! !
!Contact categoriesFor: #phone!accessing!public! !
!Contact categoriesFor: #phone:!accessing!public! !
!Contact categoriesFor: #saveUsing:!public! !
!Contact categoriesFor: #title!accessing!public! !
!Contact categoriesFor: #title:!accessing!public! !

!Contact class methodsFor!

fillListString

^'	| i stream |
	i := 0.
	[
		ContactsGlobals includesKey: #ContactList.
	] whileFalse: [
		ContactsGlobals
			at: #ContactList
			put: RcIdentityBag new.
		System
			commitTransaction;
			abortTransaction;
			yourself.
		i := i + 1.
		10 < i ifTrue: [nil error: ''Unable to create ContactList!!''].
	].
	stream := WriteStream on: String new.
	(ContactsGlobals at: #ContactList) do: [:eachContact | 	"really (Array with: nameString with: titleString with: phoneString)" 
		eachContact asOop printOn: stream.
		eachContact do: [:each | 
			stream tab; nextPutAll: each.
		].
		stream nextPut: Character lf.
	].
	stream contents.
'.
!

fromString: aString

	^super new
		initialize: aString;
		yourself.
!

listFromGciSession: aGciSession

	| string |
	string := aGciSession executeString: self fillListString.
	^self listFromString: string.
!

listFromString: aString

	| list |
	list := aString subStrings: Character lf.
	list := list collect: [:each | self fromString: each].
	^list.
!

new

	^super new
		initialize;
		yourself.
! !
!Contact class categoriesFor: #fillListString!public! !
!Contact class categoriesFor: #fromString:!public! !
!Contact class categoriesFor: #listFromGciSession:!public! !
!Contact class categoriesFor: #listFromString:!public! !
!Contact class categoriesFor: #new!public! !

ContactsImageStripper guid: (GUID fromString: '{9B58C50C-364B-4F25-B578-718999CB9C5B}')!
ContactsImageStripper comment: ''!
!ContactsImageStripper categoriesForClass!Unclassified! !
!ContactsImageStripper methodsFor!

closeLoginShells

	ContactsLoginShell allInstances do: [:each | each view close].
	super closeLoginShells.
!

requiredPackageNames

	^super requiredPackageNames
		add: 'Contacts Demo';
		add: 'Jade Login';
		yourself.
! !
!ContactsImageStripper categoriesFor: #closeLoginShells!public! !
!ContactsImageStripper categoriesFor: #requiredPackageNames!public! !

ContactsShell guid: (GUID fromString: '{F11B768D-F9FA-4272-B4FA-17EC49F4D7AC}')!
ContactsShell comment: ''!
!ContactsShell categoriesForClass!Unclassified! !
!ContactsShell class methodsFor!

icon

	^Icon fromFile: 'icons\GS32x32.ico'.
! !
!ContactsShell class categoriesFor: #icon!public! !

ContactEditor guid: (GUID fromString: '{8F931BE6-A07F-4B11-94DC-52527D28596D}')!
ContactEditor comment: ''!
!ContactEditor categoriesForClass!Unclassified! !
!ContactEditor methodsFor!

attemptLock: aSymbol

	contact
		attemptLock: aSymbol 
		session: model.
!

cancel

	self view close.
!

contact: aContact

	contact := aContact.

!

createComponents

	super createComponents.
	namePresenter 	:= self add: TextPresenter new name: 'name'.
	titlePresenter 	:= self add: TextPresenter new name: 'title'.
	phonePresenter := self add: TextPresenter new name: 'phone'.
!

onViewOpened

	self caption: 'Contact Editor for ' , contact name.
	super onViewOpened.
	self updateFields.
!

readLock

	self attemptLock: #read.
!

save

	contact
		name: 			namePresenter value;
		title:				titlePresenter value;
		phone: 			phonePresenter value;
		saveUsing:	self model;
		yourself.
	self view close.
!

updateFields

	namePresenter 	value: contact name.
	titlePresenter 	value: contact title.
	phonePresenter value: contact phone.
	!

writeLock

	self attemptLock: #write.
! !
!ContactEditor categoriesFor: #attemptLock:!public! !
!ContactEditor categoriesFor: #cancel!public! !
!ContactEditor categoriesFor: #contact:!accessing!public! !
!ContactEditor categoriesFor: #createComponents!public! !
!ContactEditor categoriesFor: #onViewOpened!public! !
!ContactEditor categoriesFor: #readLock!public! !
!ContactEditor categoriesFor: #save!public! !
!ContactEditor categoriesFor: #updateFields!public! !
!ContactEditor categoriesFor: #writeLock!public! !

!ContactEditor class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  621 261 551 0 0 0 416 0 234 256 98 6 410 8 ##(Smalltalk.TextEdit)  98 16 0 416 98 2 8 1140916352 1025 592 0 482 8 4278190080 0 7 0 0 0 592 0 8 4294902817 852486 ##(Smalltalk.NullConverter)  0 0 1 983302 ##(Smalltalk.MessageSequence)  202 208 98 3 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 153 81 530 431 41 592 818 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 592 818 8 #isTextModified: 98 1 32 592 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 76 0 0 0 40 0 0 0 35 1 0 0 60 0 0 0] 98 0 530 193 193 0 27 8 'phone' 410 608 98 16 0 416 98 2 8 1140916352 1025 1136 0 482 688 0 7 0 0 0 1136 0 8 4294902817 722 0 0 1 754 202 208 98 3 818 848 98 2 530 153 41 530 431 41 1136 818 928 98 1 962 3 1 3 1136 818 1008 98 1 32 1136 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 76 0 0 0 20 0 0 0 35 1 0 0 40 0 0 0] 98 0 1104 0 27 8 'title' 410 608 98 16 0 416 98 2 8 1140916352 1025 1504 0 482 688 0 7 0 0 0 1504 0 8 4294902817 722 0 0 1 754 202 208 98 3 818 848 98 2 530 153 1 530 431 41 1504 818 928 98 1 962 3 1 3 1504 818 1008 98 1 32 1504 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 76 0 0 0 0 0 0 0 35 1 0 0 20 0 0 0] 98 0 1104 0 27 8 'name' 0 0 0 0 0 1 0 0 0 0 1 0 0 754 202 208 98 3 818 848 98 2 530 2879 21 530 621 261 416 818 8 #text: 98 1 8 'Contact Editor' 416 818 8 #updateMenuBar 98 0 416 1042 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 213 6 0 0 140 0 0 0] 98 10 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 2144 0 0 0 7 0 0 0 2144 0 8 4294902811 722 0 0 0 754 202 208 98 2 818 848 98 2 530 1 1 530 151 39 2144 818 2000 98 1 8 ' Name:' 2144 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 75 0 0 0 19 0 0 0] 98 0 1104 0 27 410 2160 98 16 0 416 98 2 8 1140850944 1 2464 0 0 0 7 0 0 0 2464 0 8 4294902811 722 0 0 0 754 202 208 98 2 818 848 98 2 530 1 41 530 151 39 2464 818 2000 98 1 8 ' Title:' 2464 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 75 0 0 0 39 0 0 0] 98 0 1104 0 27 410 2160 98 16 0 416 98 2 8 1140850944 1 2768 0 0 0 7 0 0 0 2768 0 8 4294902811 722 0 0 0 754 202 208 98 2 818 848 98 2 530 1 81 530 151 39 2768 818 2000 98 1 8 ' Phone:' 2768 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 40 0 0 0 75 0 0 0 59 0 0 0] 98 0 1104 0 27 1504 1136 592 410 8 ##(Smalltalk.PushButton)  98 20 0 416 98 2 8 1140924416 1 3072 0 0 0 7 0 0 0 3072 0 8 4294902801 1180998 4 ##(Smalltalk.CommandDescription)  8 #readLock 8 'Read Lock' 1 1 0 0 32 0 0 0 754 202 208 98 3 818 848 98 2 530 1 131 530 141 51 3072 818 8 #isEnabled: 98 1 32 3072 818 2000 98 1 8 'Read Lock' 3072 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 65 0 0 0 70 0 0 0 90 0 0 0] 98 0 1104 0 29 410 3088 98 20 0 416 98 2 8 1140924416 1 3488 0 0 0 7 0 0 0 3488 0 8 4294902801 3170 8 #writeLock 8 'Write Lock' 1 1 0 0 32 0 0 0 754 202 208 98 3 818 848 98 2 530 141 131 530 141 51 3488 818 3360 98 1 32 3488 818 2000 98 1 8 'Write Lock' 3488 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 70 0 0 0 65 0 0 0 140 0 0 0 90 0 0 0] 98 0 1104 0 29 410 3088 98 20 0 416 98 2 8 1140924416 1 3856 0 0 0 7 0 0 0 3856 0 8 4294902801 3170 8 #cancel 8 '&Cancel' 1 1 0 0 32 0 0 0 754 202 208 98 3 818 848 98 2 530 301 131 530 141 51 3856 818 3360 98 1 32 3856 818 2000 98 1 8 '&Cancel' 3856 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 150 0 0 0 65 0 0 0 220 0 0 0 90 0 0 0] 98 0 1104 0 29 410 3088 98 20 0 416 98 2 8 1140924416 1 4224 0 0 0 7 0 0 0 4224 0 8 4294902801 3170 8 #save 8 'Save' 1 1 0 0 32 0 0 0 754 202 208 98 3 818 848 98 2 530 441 131 530 141 51 4224 818 3360 98 1 32 4224 818 2000 98 1 8 'Save' 4224 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 220 0 0 0 65 0 0 0 34 1 0 0 90 0 0 0] 98 0 1104 0 29 1104 0 27 )!

showOn: aGciSession 
	^self showOn: Contact new inSession: aGciSession!

showOn: aContact inSession: aGciSession

	^(self on: aGciSession)
		contact: aContact;
		createView: self defaultView; 
		showShell;
		yourself.
! !
!ContactEditor class categoriesFor: #resource_Default_view!public!resources-views! !
!ContactEditor class categoriesFor: #showOn:!public! !
!ContactEditor class categoriesFor: #showOn:inSession:!public! !

ContactsBrowser guid: (GUID fromString: '{C082A068-8D8D-4292-8200-6CC42C48D673}')!
ContactsBrowser comment: ''!
!ContactsBrowser categoriesForClass!Unclassified! !
!ContactsBrowser methodsFor!

abort

	self model abort.
	Sound informationBeep.
	self fillList.
!

begin

	self model begin.
	Sound informationBeep.
	self fillList.
!

commit

	| string |
	self model commit ifTrue: [
		Sound informationBeep.
		self fillList.
		^self.
	].
	string := self model executeString: self commitConflictsSource.
	MessageBox notify: string.

!

commitConflictsSource

^'| stream dict string |
dict := System transactionConflicts.
stream := (WriteStream on: String new)
	nextPutAll: ''Commit result: '';
	nextPutAll: (dict at: #''commitResult''); cr;
	yourself.
dict removeKey: #''commitResult''.
dict keys do: [:each | 
	(each includesString: ''Rc'') ifTrue: [
		dict removeKey: each.
	].
].
dict keysAndValuesDo: [:type :conflicts | 
	stream nextPutAll: type; cr.
	conflicts do: [:each | 
		string := (ReadStream on: each printString) nextLine.
		string := string copyFrom: 1 to: (string size min: 40).
		stream
			tab; nextPutAll: each asOop printString;
			tab; nextPutAll: each class name;
			tab; nextPutAll: string;
			cr.
	].
].
stream contents.'.
!

createComponents

	super createComponents.
	listPresenter := self add: ListPresenter new name: 'list'.
	commitRecordPresenter := self add: TextPresenter new name: 'commitRecord'.
!

createSchematicWiring

	super createSchematicWiring.
	listPresenter when: #actionPerformed send: #editRequest to: self.
!

editRequest

	ContactEditor 
		showOn: listPresenter selection 
		inSession: self model.
!

fillList

	| list number |
	list := Contact listFromGciSession: model.
	listPresenter list: list.
	number := [
		self model executeString: 'System commitRecordPageForSessionId: System session'.
	] on: Error do: [:ex | 
		ex return: 'N/A'.
	].
	commitRecordPresenter value: number.
!

newRequest
	ContactEditor showOn: self model!

onViewActivated: anObject

	self fillList.
!

onViewClosed

	super onViewClosed.
	self model notNil ifTrue: [self model logout].
!

onViewOpened

	self caption: (self model titleBarFor: 'Contacts Browser').
	super onViewOpened.
	[
		self fillList.
		self model commit ifFalse: [self error: 'Commit failed!!'].
	] on: Error do: [:ex |
		self model logout.
		MessageBox warning: ex description.
		Processor terminateActive.
	].
!

queryCommand: aCommandQuery

	aCommandQuery commandSymbol = #'editRequest' ifTrue: [
		aCommandQuery isEnabled: listPresenter hasSelection.
		^true.
	].
	^super queryCommand: aCommandQuery.
! !
!ContactsBrowser categoriesFor: #abort!public! !
!ContactsBrowser categoriesFor: #begin!public! !
!ContactsBrowser categoriesFor: #commit!public! !
!ContactsBrowser categoriesFor: #commitConflictsSource!public! !
!ContactsBrowser categoriesFor: #createComponents!public! !
!ContactsBrowser categoriesFor: #createSchematicWiring!public! !
!ContactsBrowser categoriesFor: #editRequest!public! !
!ContactsBrowser categoriesFor: #fillList!public! !
!ContactsBrowser categoriesFor: #newRequest!public! !
!ContactsBrowser categoriesFor: #onViewActivated:!public! !
!ContactsBrowser categoriesFor: #onViewClosed!public! !
!ContactsBrowser categoriesFor: #onViewOpened!public! !
!ContactsBrowser categoriesFor: #queryCommand:!public! !

!ContactsBrowser class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  1201 801 551 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 18 410 8 ##(Smalltalk.PushButton)  98 20 0 416 98 2 8 1140924416 1 624 0 0 0 7 0 0 0 624 0 8 4294902441 1180998 4 ##(Smalltalk.CommandDescription)  8 #commit 8 'Commit' 1 1 0 0 32 0 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 3 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 421 683 530 141 51 624 850 8 #isEnabled: 98 1 32 624 850 8 #text: 98 1 8 'Commit' 624 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 210 0 0 0 85 1 0 0 24 1 0 0 110 1 0 0] 98 0 530 193 193 0 29 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 421 1170 8 #fixedViewLeft 141 1170 8 #fixedParentBottom -49 1170 8 #fixedViewTop 51 410 640 98 20 0 416 98 2 8 1140924416 1 1312 0 0 0 7 0 0 0 1312 0 8 4294902441 722 8 #fillList 8 'Refresh' 1 1 0 0 32 0 0 0 786 202 208 98 3 850 880 98 2 530 701 683 530 141 51 1312 850 960 98 1 32 1312 850 1008 98 1 8 'Refresh' 1312 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 94 1 0 0 85 1 0 0 164 1 0 0 110 1 0 0] 98 0 1120 0 29 1138 1184 701 1216 141 1248 -49 1280 51 410 640 98 20 0 416 98 2 8 1140924416 1 1696 0 0 0 7 0 0 0 1696 0 8 4294902441 722 8 #abort 8 'Abort' 1 1 0 0 32 0 0 0 786 202 208 98 3 850 880 98 2 530 561 683 530 141 51 1696 850 960 98 1 32 1696 850 1008 98 1 8 'Abort' 1696 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 24 1 0 0 85 1 0 0 94 1 0 0 110 1 0 0] 98 0 1120 0 29 1138 1184 561 1216 141 1248 -49 1280 51 410 8 ##(Smalltalk.ListView)  98 30 0 416 98 2 8 1409355853 1025 2080 590662 2 ##(Smalltalk.ListModel)  202 208 98 0 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  482 8 4278190080 0 7 0 0 0 2080 0 8 4294902273 459270 ##(Smalltalk.Message)  8 #displayString 98 0 8 ##(Smalltalk.IconicListAbstract)  1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 0 202 208 98 3 920646 5 ##(Smalltalk.ListViewColumn)  8 'Name' 393 8 #left 2306 2336 2352 8 ##(Smalltalk.SortedCollection)  787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  2 1 2544 8 'doIt' 8 '[:each | each name]' 8 #[30 105 226 0 106] 8 #name 2560 7 257 0 0 2080 0 3 0 0 2450 8 'Title' 393 2496 2306 2336 98 0 2306 8 #<= 2720 2546 0 0 2578 2 1 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 8 '[:each | each title]' 8 #[30 105 226 0 106] 8 #title 2768 7 257 0 0 2080 0 3 0 0 2450 8 'Phone' 393 2496 2306 2336 2720 2306 2752 2720 2546 0 0 2578 2 1 2544 8 'doIt' 8 '[:each | each phone]' 8 #[30 105 226 0 106] 8 #phone 2944 7 257 0 0 2080 0 3 0 0 8 #report 2208 0 131169 0 0 786 202 208 98 2 850 880 98 2 530 1 1 530 1185 683 2080 850 1008 98 1 8 'Name' 2080 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 80 2 0 0 85 1 0 0] 98 0 1120 0 27 1138 1184 1 1170 8 #fixedParentRight 1 1170 8 #fixedParentTop 1 1248 -49 410 640 98 20 0 416 98 2 8 1140924416 1 3344 0 0 0 7 0 0 0 3344 0 8 4294902441 722 8 #begin 8 'Begin' 1 1 0 0 32 0 0 0 786 202 208 98 3 850 880 98 2 530 281 683 530 141 51 3344 850 960 98 1 32 3344 850 1008 98 1 8 'Begin' 3344 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 140 0 0 0 85 1 0 0 210 0 0 0 110 1 0 0] 98 0 1120 0 29 1138 1184 281 1216 141 1248 -49 1280 51 410 8 ##(Smalltalk.TextEdit)  98 16 0 416 98 2 8 1140924546 1025 3728 0 482 8 4278190080 0 7 0 0 0 3728 0 8 4294902123 852486 ##(Smalltalk.NullConverter)  0 0 3 786 202 208 98 3 850 880 98 2 530 1085 687 530 101 41 3728 850 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 3728 850 8 #isTextModified: 98 1 32 3728 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 30 2 0 0 87 1 0 0 80 2 0 0 107 1 0 0] 98 0 1120 0 27 1138 3280 -99 1216 101 1248 -45 1280 41 410 640 98 20 0 416 98 2 8 1140924416 1 4192 0 0 0 7 0 0 0 4192 0 8 4294902441 722 8 #editRequest 8 'Edit' 1 1 0 0 32 0 0 0 786 202 208 98 3 850 880 98 2 530 141 683 530 141 51 4192 850 960 98 1 32 4192 850 1008 98 1 8 'Edit' 4192 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 70 0 0 0 85 1 0 0 140 0 0 0 110 1 0 0] 98 0 1120 0 29 1138 1184 141 1216 141 1248 -49 1280 51 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 4576 0 0 0 7 0 0 0 4576 0 8 4294902305 3858 0 0 0 786 202 208 98 2 850 880 98 2 530 895 691 530 191 31 4576 850 1008 98 1 8 'Commit Record:' 4576 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 191 1 0 0 89 1 0 0 30 2 0 0 104 1 0 0] 98 0 1120 0 27 1138 3280 -289 1216 191 1248 -41 1280 31 410 640 98 20 0 416 98 2 8 1140924416 1 4912 0 0 0 7 0 0 0 4912 0 8 4294902441 722 8 #newRequest 8 'New' 1 1 0 0 32 0 0 0 786 202 208 98 3 850 880 98 2 530 1 683 530 141 51 4912 850 960 98 1 32 4912 850 1008 98 1 8 'New' 4912 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 85 1 0 0 70 0 0 0 110 1 0 0] 98 0 1120 0 29 1138 1184 1 1216 141 1248 -49 1280 51 234 256 98 4 2080 8 'list' 3728 8 'commitRecord' 0 0 0 0 0 1 0 0 0 0 1 0 0 786 202 208 98 3 850 880 98 2 530 3359 21 530 1201 801 416 850 1008 98 1 8 'Contacts Browser' 416 850 8 #updateMenuBar 2208 416 1058 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 143 6 0 0 10 0 0 0 231 8 0 0 154 1 0 0] 98 9 2080 4912 4192 3344 624 1696 1312 4576 3728 1120 0 27 )! !
!ContactsBrowser class categoriesFor: #resource_Default_view!public!resources-views! !

ContactsLoginShell guid: (GUID fromString: '{8D6109E0-73A1-41F4-8AFA-ED30A9A140EB}')!
ContactsLoginShell comment: ''!
!ContactsLoginShell categoriesForClass!Unclassified! !
!ContactsLoginShell methodsFor!

basicCaption

	^'Contacts Demo Login'.
!

initialSetupCode

	^'| segment dict newUser list |
(AllUsers anySatisfy: [:each | each userId = ''Manager1'']) ifTrue: [^self].
segment := Segment newInRepository: SystemRepository.
segment group: ''Publishers'' authorization: #''write''.
segment group: ''Subscribers'' authorization: #''read''.
UserGlobals
	at: #''ContactsSecurityPolicy''
	put: segment.
System commitTransaction ifFalse: [nil error: ''commit failed''].

list := RcIdentityBag new.
list assignToSegment: segment.
list
		add: (Array with: ''James Foster'' 	with: ''Director of Operations'' 	with: ''+1 503-766-4714'');
		add: (Array with: ''Timothy Cook''	with: ''Apple CEO'' 					with: ''+1 408-996-1010'');
		add: (Array with: ''Bill Gates'' 		with: ''Microsoft Chairman'' 		with: ''+1 800-Microsoft'');
		add: (Array with: ''Larry Ellison''	with: ''Oracle CEO''					with: ''+1 650-506-7000'');
		yourself.
(dict := SymbolDictionary new)
	at: #''ContactsGlobals'' put: dict;
	at: #''ContactsSecurityPolicy'' put: segment;
	at: #ContactList put: list;
	assignToSegment: segment;
	yourself.
(AllUsers userWithId: ''DataCurator'') insertDictionary: dict at: 1.
System commitTransaction ifFalse: [nil error: ''commit failed''].

newUser := AllUsers
	addNewUserWithId: ''Manager1''
	password: ''Monkey''. 
newUser addGroup: ''Subscribers''.
((System stoneVersionReport at: #''gsVersion'') first = $6) ifFalse: [
	newUser addPrivilege: ''CodeModification''.
].
newUser insertDictionary: dict at: 1.
System commitTransaction ifFalse: [nil error: ''commit failed''].
'!

postLogin: aGciSession

	Keyboard default isShiftDown ifTrue: [
		super postLogin: aGciSession.
	] ifFalse: [
		aGciSession executeString: self initialSetupCode.
		ContactsBrowser showOn: aGciSession.
	].
! !
!ContactsLoginShell categoriesFor: #basicCaption!public! !
!ContactsLoginShell categoriesFor: #initialSetupCode!public! !
!ContactsLoginShell categoriesFor: #postLogin:!public! !

ContactsSessionManager guid: (GUID fromString: '{639E76E3-B3C9-4DCE-A9EB-F50B3D878AC8}')!
ContactsSessionManager comment: ''!
!ContactsSessionManager categoriesForClass!Unclassified! !
!ContactsSessionManager class methodsFor!

mainShellClass

	^ContactsLoginShell.
! !
!ContactsSessionManager class categoriesFor: #mainShellClass!public! !

"Binary Globals"!


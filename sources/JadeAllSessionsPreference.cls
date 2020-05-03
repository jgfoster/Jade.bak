"Filed out from Dolphin Smalltalk 7"!

JadePreferenceObject subclass: #JadeAllSessionsPreference
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeAllSessionsPreference guid: (GUID fromString: '{546eb412-bc29-405c-b0f7-29f73e946fa4}')!
JadeAllSessionsPreference comment: ''!
!JadeAllSessionsPreference categoriesForClass!Unclassified! !
!JadeAllSessionsPreference methodsFor!

autoCommit

	^self gciSession isAutoCommit!

autoCommit: aBoolean
	self gciSession isAutoCommit: aBoolean!

autoMigrate
	^self gciSession isAutoMigrate!

autoMigrate: aBoolean

	^self gciSession isAutoMigrate: aBoolean!

displayString

	^'All Sessions'!

icon

	^Icon fromId: 'ListView.ico'!

isShowUnimplementedMessages
	^self gciSession isShowUnimplementedMessages!

isShowUnimplementedMessages: aBoolean
	self gciSession isShowUnimplementedMessages: aBoolean!

nativeCode
	^self gciSession isNativeCode!

nativeCode: aBoolean

	^self gciSession isNativeCode: aBoolean!

publishedAspects
	"Answer a <LookupTable> of the <Aspect>s published by the receiver."

	| aspects |
	aspects := super publishedAspects.
	self gciSession isNativeCode ifNotNil: [aspects add: (Aspect boolean: #nativeCode)].
	self gciSession isAutoCommit ifNotNil: [aspects add: (Aspect boolean: #autoCommit)].
	self gciSession isAutoMigrate ifNotNil: [aspects add: (Aspect boolean: #autoMigrate)].
	self gciSession isShowUnimplementedMessages ifNotNil: [aspects add: (Aspect boolean: #isShowUnimplementedMessages)].
	^aspects! !
!JadeAllSessionsPreference categoriesFor: #autoCommit!public! !
!JadeAllSessionsPreference categoriesFor: #autoCommit:!public! !
!JadeAllSessionsPreference categoriesFor: #autoMigrate!public! !
!JadeAllSessionsPreference categoriesFor: #autoMigrate:!public! !
!JadeAllSessionsPreference categoriesFor: #displayString!public! !
!JadeAllSessionsPreference categoriesFor: #icon!public! !
!JadeAllSessionsPreference categoriesFor: #isShowUnimplementedMessages!public! !
!JadeAllSessionsPreference categoriesFor: #isShowUnimplementedMessages:!public! !
!JadeAllSessionsPreference categoriesFor: #nativeCode!public! !
!JadeAllSessionsPreference categoriesFor: #nativeCode:!public! !
!JadeAllSessionsPreference categoriesFor: #publishedAspects!public! !

!JadeAllSessionsPreference class methodsFor!

icon

	^Icon fromId: 'ListView.ico'! !
!JadeAllSessionsPreference class categoriesFor: #icon!public! !


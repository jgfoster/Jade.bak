"Filed out from Dolphin Smalltalk 7"!

JadePreferenceObject subclass: #JadeAllSessionsPreference
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeAllSessionsPreference guid: (GUID fromString: '{546eb412-bc29-405c-b0f7-29f73e946fa4}')!
JadeAllSessionsPreference comment: ''!
!JadeAllSessionsPreference categoriesForClass!Unclassified! !
!JadeAllSessionsPreference class methodsFor!

autoCommit

	^self gciSession isAutoCommit!

autoCommit: aBoolean
	self gciSession isAutoCommit: aBoolean!

autoMigrate
	^self gciSession isAutoMigrate!

autoMigrate: aBoolean

	^self gciSession isAutoMigrate: aBoolean!

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
!JadeAllSessionsPreference class categoriesFor: #autoCommit!public! !
!JadeAllSessionsPreference class categoriesFor: #autoCommit:!public! !
!JadeAllSessionsPreference class categoriesFor: #autoMigrate!public! !
!JadeAllSessionsPreference class categoriesFor: #autoMigrate:!public! !
!JadeAllSessionsPreference class categoriesFor: #icon!public! !
!JadeAllSessionsPreference class categoriesFor: #isShowUnimplementedMessages!public! !
!JadeAllSessionsPreference class categoriesFor: #isShowUnimplementedMessages:!public! !
!JadeAllSessionsPreference class categoriesFor: #nativeCode!public! !
!JadeAllSessionsPreference class categoriesFor: #nativeCode:!public! !
!JadeAllSessionsPreference class categoriesFor: #publishedAspects!public! !


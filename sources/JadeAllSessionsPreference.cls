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

	| aspects autoCommit autoMigrate nativeCode messages |
	nativeCode := Aspect boolean: #nativeCode.
	autoCommit := Aspect boolean: #autoCommit.
	autoMigrate := Aspect boolean: #autoMigrate.
	messages := Aspect boolean: #isShowUnimplementedMessages.
	self gciSession isNativeCode ifNil: [nativeCode beImmutable].
	self gciSession isAutoCommit ifNil: [autoCommit beImmutable].
	self gciSession isAutoMigrate ifNil: [autoMigrate beImmutable].
	self gciSession isShowUnimplementedMessages ifNil: [messages beImmutable].
	aspects := super publishedAspects.
	aspects
		add: nativeCode;
		add: autoCommit;
		add: autoMigrate;
		add: messages.
	^aspects! !
!JadeAllSessionsPreference class categoriesFor: #autoCommit!public! !
!JadeAllSessionsPreference class categoriesFor: #autoCommit:!public! !
!JadeAllSessionsPreference class categoriesFor: #autoMigrate!public! !
!JadeAllSessionsPreference class categoriesFor: #autoMigrate:!public! !
!JadeAllSessionsPreference class categoriesFor: #isShowUnimplementedMessages!public! !
!JadeAllSessionsPreference class categoriesFor: #isShowUnimplementedMessages:!public! !
!JadeAllSessionsPreference class categoriesFor: #nativeCode!public! !
!JadeAllSessionsPreference class categoriesFor: #nativeCode:!public! !
!JadeAllSessionsPreference class categoriesFor: #publishedAspects!public! !


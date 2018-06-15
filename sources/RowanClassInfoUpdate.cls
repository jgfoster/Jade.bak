"Filed out from Dolphin Smalltalk 7"!

RowanCommandResult subclass: #RowanClassInfoUpdate
	instanceVariableNames: 'oop template category comment'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanClassInfoUpdate guid: (GUID fromString: '{e7acaa37-9865-4b18-a3d9-680bccb6db7c}')!
RowanClassInfoUpdate comment: ''!
!RowanClassInfoUpdate categoriesForClass!Unclassified! !
!RowanClassInfoUpdate methodsFor!

category
	^category!

category: anObject
	category := anObject!

classCommentUpdate: presenter
	presenter value: comment!

classDefinitionUpdate: presenter
	(presenter value notNil and: [presenter value notEmpty])
		ifTrue: 
			[presenter view isModified
				ifTrue: 
					[
					template = presenter value ifTrue: [^self].
					(MessageBox confirm: 'Copy changes to clipboard?' caption: 'Class has unsaved changes!!')
						ifTrue: [presenter value copyToClipboard]]].
	presenter value: template.
	presenter view
		backcolor: Color white;
		isModified: false;
		yourself!

comment
	^comment!

comment: anObject
	comment := anObject!

oop
	^oop!

oop: anObject
	oop := anObject!

template
	^template!

template: anObject
	template := anObject!

update: aPresenter
	aPresenter updateFromCommand: self! !
!RowanClassInfoUpdate categoriesFor: #category!accessing!public! !
!RowanClassInfoUpdate categoriesFor: #category:!accessing!public! !
!RowanClassInfoUpdate categoriesFor: #classCommentUpdate:!public!updating! !
!RowanClassInfoUpdate categoriesFor: #classDefinitionUpdate:!public!updating! !
!RowanClassInfoUpdate categoriesFor: #comment!accessing!public! !
!RowanClassInfoUpdate categoriesFor: #comment:!accessing!public! !
!RowanClassInfoUpdate categoriesFor: #oop!accessing!public! !
!RowanClassInfoUpdate categoriesFor: #oop:!accessing!public! !
!RowanClassInfoUpdate categoriesFor: #template!accessing!public! !
!RowanClassInfoUpdate categoriesFor: #template:!accessing!public! !
!RowanClassInfoUpdate categoriesFor: #update:!public! !


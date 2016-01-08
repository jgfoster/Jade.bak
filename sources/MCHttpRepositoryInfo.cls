"Filed out from Dolphin Smalltalk 7"!

Object subclass: #MCHttpRepositoryInfo
	instanceVariableNames: 'location user password'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCHttpRepositoryInfo guid: (GUID fromString: '{9297CE93-5E15-48CE-8BE4-01AA17099F34}')!
MCHttpRepositoryInfo comment: ''!
!MCHttpRepositoryInfo categoriesForClass!Unclassified! !
!MCHttpRepositoryInfo methodsFor!

initialize

	location		:= 'http://seaside.gemtalksystems.com/ss/'.
	user			:= ''.
	password	:= ''.
!

location
	^location!

location: anObject
	location := anObject!

password
	^password!

password: anObject
	password := anObject!

user
	^user!

user: anObject
	user := anObject! !
!MCHttpRepositoryInfo categoriesFor: #initialize!private! !
!MCHttpRepositoryInfo categoriesFor: #location!accessing!private! !
!MCHttpRepositoryInfo categoriesFor: #location:!accessing!private! !
!MCHttpRepositoryInfo categoriesFor: #password!accessing!private! !
!MCHttpRepositoryInfo categoriesFor: #password:!accessing!private! !
!MCHttpRepositoryInfo categoriesFor: #user!accessing!private! !
!MCHttpRepositoryInfo categoriesFor: #user:!accessing!private! !

!MCHttpRepositoryInfo class methodsFor!

new

	^super new
		initialize;
		yourself.
! !
!MCHttpRepositoryInfo class categoriesFor: #new!public! !


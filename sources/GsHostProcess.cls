"Filed out from Dolphin Smalltalk 7"!

Object subclass: #GsHostProcess
	instanceVariableNames: 'name status version owner started pid port options logfile sysConf exeConf execConf gemstone imageIndex'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsHostProcess guid: (GUID fromString: '{317d129c-b9f4-4df1-8142-4d40e64d6102}')!
GsHostProcess comment: ''!
!GsHostProcess categoriesForClass!Unclassified! !
!GsHostProcess methodsFor!

applyToLogin: aJadeLogin

	self subclassResponsibility.
!

exeConf
	^exeConf!

exeConf: anObject
	exeConf := anObject!

gemstone
	^gemstone!

gemstone: anObject
	gemstone := anObject!

imageIndex

	status = 'killed' ifTrue: [^nil icon imageIndex].
	^imageIndex.
!

imageIndex: anInteger

	imageIndex := anInteger.

!

initializeFromStream: aStream

	| dict line |
	dict := Dictionary new.
	[
		aStream peekFor: Character space.
	] whileTrue: [
		line := aStream nextLine subStrings: $=.
		dict
			at: line first trimBlanks
			put: line last trimBlanks.
	].
	version 		:= dict at: 'version'				ifAbsent: [nil].
	owner 		:= dict at: 'owner'				ifAbsent: [nil].
	started 		:= dict at: 'started' 			ifAbsent: [nil].
	pid 			:= dict at: 'pid' 				ifAbsent: [nil].
	port 			:= dict at: 'port' 				ifAbsent: [nil].
	options 		:= dict at: 'options' 			ifAbsent: [nil].
	logfile 		:= dict at: 'logfile' 				ifAbsent: [nil].
	sysConf 		:= dict at: 'sysconf' 			ifAbsent: [nil].
	exeConf 		:= dict at: 'execonf' 			ifAbsent: [nil].
	gemstone 	:= dict at: 'GEMSTONE' 	ifAbsent: [nil].
	imageIndex 	:= Object icon imageIndex.
!

logfile
	^logfile!

logfile: anObject
	logfile := anObject!

name
	^name!

name: anObject
	name := anObject!

options
	^options!

options: anObject
	options := anObject!

owner
	^owner!

owner: anObject
	owner := anObject!

pid
	^pid!

pid: anObject
	pid := anObject!

port
	^port!

port: anObject
	port := anObject!

processType

	self subclassResponsibility.
	^'process'.
!

serviceName

	^self processType.
!

started
	^started!

started: anObject
	started := anObject!

status
	^status!

status: anObject
	status := anObject!

sysConf
	^sysConf!

sysConf: anObject
	sysConf := anObject!

version
	^version!

version: anObject
	version := anObject! !
!GsHostProcess categoriesFor: #applyToLogin:!public! !
!GsHostProcess categoriesFor: #exeConf!accessing!public! !
!GsHostProcess categoriesFor: #exeConf:!accessing!public! !
!GsHostProcess categoriesFor: #gemstone!accessing!public! !
!GsHostProcess categoriesFor: #gemstone:!accessing!public! !
!GsHostProcess categoriesFor: #imageIndex!public! !
!GsHostProcess categoriesFor: #imageIndex:!public! !
!GsHostProcess categoriesFor: #initializeFromStream:!private! !
!GsHostProcess categoriesFor: #logfile!accessing!public! !
!GsHostProcess categoriesFor: #logfile:!accessing!public! !
!GsHostProcess categoriesFor: #name!accessing!public! !
!GsHostProcess categoriesFor: #name:!accessing!public! !
!GsHostProcess categoriesFor: #options!accessing!public! !
!GsHostProcess categoriesFor: #options:!accessing!public! !
!GsHostProcess categoriesFor: #owner!accessing!public! !
!GsHostProcess categoriesFor: #owner:!accessing!public! !
!GsHostProcess categoriesFor: #pid!accessing!public! !
!GsHostProcess categoriesFor: #pid:!accessing!public! !
!GsHostProcess categoriesFor: #port!accessing!public! !
!GsHostProcess categoriesFor: #port:!accessing!public! !
!GsHostProcess categoriesFor: #processType!public! !
!GsHostProcess categoriesFor: #serviceName!public! !
!GsHostProcess categoriesFor: #started!accessing!public! !
!GsHostProcess categoriesFor: #started:!accessing!public! !
!GsHostProcess categoriesFor: #status!accessing!public! !
!GsHostProcess categoriesFor: #status:!accessing!public! !
!GsHostProcess categoriesFor: #sysConf!accessing!public! !
!GsHostProcess categoriesFor: #sysConf:!accessing!public! !
!GsHostProcess categoriesFor: #version!accessing!public! !
!GsHostProcess categoriesFor: #version:!accessing!public! !

!GsHostProcess class methodsFor!

fromStream: aStream

	| processName array status type newClass |
	processName := aStream nextLine.
	array := aStream nextLine subStrings: $=.
	array isEmpty ifTrue: [array := #( '' )].
	status := array last trimBlanks.
	array := aStream nextLine subStrings: $=.
	array isEmpty ifTrue: [array := #( '' )].
	type := array last trimBlanks.
	newClass := self subclasses 
		detect: [:each | each type = type]
		ifNone: [^nil].
	^newClass new
		name: processName;
		status: status;
		initializeFromStream: aStream.
!

type

	self subclassResponsibility.
! !
!GsHostProcess class categoriesFor: #fromStream:!public! !
!GsHostProcess class categoriesFor: #type!public! !


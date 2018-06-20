"Filed out from Dolphin Smalltalk 7"!

Object subclass: #GsEnvironment
	instanceVariableNames: 'gemstone gemstoneChildLog gemstoneExeConf gemstoneLang gemstoneLog gemstoneMaxFd gemstoneNrsAll gemstoneSysConf gsCoreTimeOut gsDisableKeepalive gsDisableWarning upgradeLogDir'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsEnvironment guid: (GUID fromString: '{d4f01bbd-8871-4860-bc98-f34c0726acd3}')!
GsEnvironment comment: ''!
!GsEnvironment categoriesForClass!Unclassified! !
!GsEnvironment methodsFor!

gemstone
	"the location of the GemStone Object Server software, which must be a full path:
		unix:		/user3/GemStone6.1-hppa.hpux
		win:		D:\GemStone60"

	^gemstone!

gemstone: anObject
	gemstone := anObject!

gemstoneChildLog
	"Used by parent process to tell child process the name of its log file. 
	To keep the child's log from being deleted when the process terminates 
	normally, unset this variable in the appropriate script or batch file, such as 
		unix:		$GEMSTONE/sys/gemnetobject
		win:		%GEMSTONE%\sys\gemnetobject.bat "

	^gemstoneChildLog!

gemstoneChildLog: anObject
	gemstoneChildLog := anObject!

gemstoneExeConf
	"The location of an executable-dependent configuration file.
	See 'Creating an Executable Configuration File' on page A-5 
	in the GemStone/S System Administration Guide"

	^gemstoneExeConf!

gemstoneExeConf: anObject
	gemstoneExeConf := anObject!

gemstoneLang
	"The name of a translation message file in
		unix:	$GEMSTONE/bin
		win:		%GEMSTONE%\bin
	This file is not provided with GemStone. For further information, 
	see 'Specifying a Language' on page G-1 of the 
	GemStone/S System Administration Guide."

	^gemstoneLang!

gemstoneLang: anObject
	gemstoneLang := anObject!

gemstoneLog
	"The location of system log files for the Stone repository monitor
	and its child processes. For further information see 'GemStone System Logs'
	on page 8-2 of the System Administration Guide."

	^gemstoneLog!

gemstoneLog: anObject
	gemstoneLog := anObject!

gemstoneMaxFd
	"Limits the number of file descriptors requested by a GemStone process.
	For further information, see 'Estimating File Descriptor Needs' on page 1-12
	of the System Administration Guide."

	^gemstoneMaxFd!

gemstoneMaxFd: anObject
	gemstoneMaxFd := anObject!

gemstoneNrsAll
	"Sets a number of network-related defaults, including the type of user 
	authentication that GemStone expects. For further information, see
	'To Set a Default NRS' on page 3-13 of the System Administration Guide."

	^gemstoneNrsAll!

gemstoneNrsAll: anObject
	gemstoneNrsAll := anObject!

gemstoneSysConf
	"Location of a system-wide configuration file; see
	'How GemStone uses Configuration Files' on page A-2
	of the System Administration Guide."

	^gemstoneSysConf!

gemstoneSysConf: anObject
	gemstoneSysConf := anObject!

get: aString

	^System clientEnvironmentVariable: aString.
!

gsCoreTimeOut
	"The number of seconds to wait before a catastrophically failing GemStone/S
	process writes a core file and terminates--by default, 60 seconds. To 
	determine the cause of a problem, GemStone/S Technical Support needs a
	stack trace, usually derived from the core file. Under some circumstances,
	however, core files may be impractically large or otherwise unusable; in such
	cases, a stack trace can be extracted directly from the failing but not yet 
	terminated process by attaching a debugger to it. Increasing the value of this
	variable increases the time available to attach the debugger. If you are afcing
	this situation, GemStone/S Technical Support will recommend a new value
	for this variable and work with you to analyze the problem."

	^gsCoreTimeOut!

gsCoreTimeOut: anObject
	gsCoreTimeOut := anObject!

gsDisableKeepalive
	"A non-empty string disables the network keepalive facility. For further
	information about keepalive, see 'Disrupted Communications' on page 3-8
	of the System Administration Guide."

	^gsDisableKeepalive!

gsDisableKeepalive: anObject
	gsDisableKeepalive := anObject!

gsDisableWarning
	"A non-empty string disables a warning that GemStone is using
	/opt/gemstone instead of /usr/gemstone for log and lock files
	when both directories exist. Use of /usr/gemstone is only for
	compatibility with previous releases; the default location is
	/opt/gemstone."

	^gsDisableWarning!

gsDisableWarning: anObject
	gsDisableWarning := anObject!

readLocal

	gemstone 		:= self get: 'GEMSTONE'.
	gemstoneChildLog 	:= self get: 'GEMSTONE_CHILD_LOG'.
	gemstoneExeConf 	:= self get: 'GEMSTONE_EXE_CONF'.
	gemstoneLang 	:= self get: 'GEMSTONE_LANG'.
	gemstoneLog 		:= self get: 'GEMSTONE_LOG'.
	gemstoneMaxFd 	:= self get: 'GEMSTONE_MAX_FD'.
	gemstoneNrsAll 	:= self get: 'GEMSTONE_NRS_ALL'.
	gemstoneSysConf 	:= self get: 'GEMSTONE_SYS_CONF'.
	gsCoreTimeOut 	:= self get: 'GS_CORE_TIME_OUT'.
	gsDisableKeepalive := self get: 'GS_DISABLE_KEEPALIVE'.
	gsDisableWarning 	:= self get: 'GS_DISABLE_WARNING'.
	upgradeLogDir 	:= self get: 'upgradeLogDir'.
!

set: nameString value: valueString

	SessionManager current 
		setenv: nameString 
		value: valueString.
!

upgradeLogDir
	"The location for log files produced during the upgrade of a repository for a 
	new version of GemStone."


	^upgradeLogDir!

upgradeLogDir: anObject
	upgradeLogDir := anObject! !
!GsEnvironment categoriesFor: #gemstone!accessing!public! !
!GsEnvironment categoriesFor: #gemstone:!accessing!private! !
!GsEnvironment categoriesFor: #gemstoneChildLog!accessing!public! !
!GsEnvironment categoriesFor: #gemstoneChildLog:!accessing!private! !
!GsEnvironment categoriesFor: #gemstoneExeConf!accessing!public! !
!GsEnvironment categoriesFor: #gemstoneExeConf:!accessing!private! !
!GsEnvironment categoriesFor: #gemstoneLang!accessing!public! !
!GsEnvironment categoriesFor: #gemstoneLang:!accessing!private! !
!GsEnvironment categoriesFor: #gemstoneLog!accessing!public! !
!GsEnvironment categoriesFor: #gemstoneLog:!accessing!private! !
!GsEnvironment categoriesFor: #gemstoneMaxFd!accessing!public! !
!GsEnvironment categoriesFor: #gemstoneMaxFd:!accessing!private! !
!GsEnvironment categoriesFor: #gemstoneNrsAll!accessing!public! !
!GsEnvironment categoriesFor: #gemstoneNrsAll:!accessing!private! !
!GsEnvironment categoriesFor: #gemstoneSysConf!accessing!public! !
!GsEnvironment categoriesFor: #gemstoneSysConf:!accessing!private! !
!GsEnvironment categoriesFor: #get:!private! !
!GsEnvironment categoriesFor: #gsCoreTimeOut!accessing!public! !
!GsEnvironment categoriesFor: #gsCoreTimeOut:!accessing!private! !
!GsEnvironment categoriesFor: #gsDisableKeepalive!accessing!public! !
!GsEnvironment categoriesFor: #gsDisableKeepalive:!accessing!private! !
!GsEnvironment categoriesFor: #gsDisableWarning!accessing!public! !
!GsEnvironment categoriesFor: #gsDisableWarning:!accessing!private! !
!GsEnvironment categoriesFor: #readLocal!public! !
!GsEnvironment categoriesFor: #set:value:!private! !
!GsEnvironment categoriesFor: #upgradeLogDir!accessing!public! !
!GsEnvironment categoriesFor: #upgradeLogDir:!accessing!private! !

!GsEnvironment class methodsFor!

gemstone
"
GsEnvironment gemstone
"
	^self local gemstone.
!

isWindowsNT

	^(System clientVersionReport at: #osName) = 'Windows NT'.
!

local

	^self new
		readLocal;
		yourself.
! !
!GsEnvironment class categoriesFor: #gemstone!public! !
!GsEnvironment class categoriesFor: #isWindowsNT!public! !
!GsEnvironment class categoriesFor: #local!public! !


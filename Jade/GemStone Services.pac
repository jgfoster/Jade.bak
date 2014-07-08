| package |
package := Package name: 'GemStone Services'.
package paxVersion: 1;
	basicComment: ''.

package basicPackageVersion: '0.006'.

package basicScriptAt: #postinstall put: '''Loaded: GemStone Services'' yourself.'.

package classNames
	add: #CopyDBF;
	add: #GsConfiguration;
	add: #GsEnvironment;
	add: #GsFile;
	add: #GsHostProcess;
	add: #GsList;
	add: #GsNRS;
	add: #GsShellCommand;
	add: #GsWin32Service;
	add: #NetLDI;
	add: #NetLDICommand;
	add: #NetLDIService;
	add: #PageAudit;
	add: #SharedPageCacheMonitor;
	add: #StartStone;
	add: #Stone;
	add: #StoneCommand;
	add: #StoneService;
	add: #StopStone;
	add: #System;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\Object Arts\Dolphin\ActiveX\Shell\Windows Shell';
	yourself).

package!

"Class Definitions"!

Object subclass: #GsConfiguration
	instanceVariableNames: 'path dictionary'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #GsEnvironment
	instanceVariableNames: 'gemstone gemstoneChildLog gemstoneExeConf gemstoneLang gemstoneLog gemstoneMaxFd gemstoneNrsAll gemstoneSysConf gsCoreTimeOut gsDisableKeepalive gsDisableWarning upgradeLogDir'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #GsFile
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #GsHostProcess
	instanceVariableNames: 'name status version owner started pid port options logfile sysConf exeConf execConf gemstone imageIndex'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #GsNRS
	instanceVariableNames: 'protocol node authorization encrypted netldi dir log resource body'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #GsShellCommand
	instanceVariableNames: 'arguments'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #GsWin32Service
	instanceVariableNames: 'name startup account path creator created version options'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #System
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsHostProcess subclass: #NetLDI
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsHostProcess subclass: #SharedPageCacheMonitor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsHostProcess subclass: #Stone
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsShellCommand subclass: #CopyDBF
	instanceVariableNames: 'sourceNRS destinationNRS'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsShellCommand subclass: #GsList
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsShellCommand subclass: #NetLDICommand
	instanceVariableNames: 'command'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsShellCommand subclass: #PageAudit
	instanceVariableNames: 'stoneName'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsShellCommand subclass: #StoneCommand
	instanceVariableNames: 'command stoneName'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StoneCommand subclass: #StartStone
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StoneCommand subclass: #StopStone
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsWin32Service subclass: #NetLDIService
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsWin32Service subclass: #StoneService
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

GsConfiguration guid: (GUID fromString: '{9952C933-2F68-4E71-B69F-3BFE2FC514AF}')!
GsConfiguration comment: ''!
!GsConfiguration categoriesForClass!Unclassified! !
!GsConfiguration methodsFor!

dbfExtentNames: aString

	dictionary 
		at: 'DBF_EXTENT_NAMES'
		put: aString.

!

defaultPath

	^GsEnvironment gemstone , '/data/myConfig.conf'.
!

extentNames: aString

	dictionary 
		at: 'DBF_EXTENT_NAMES'
		put: aString.

!

initialize

	path := self defaultPath.
	dictionary := Dictionary new
		at: 'STN_TRAN_FULL_LOGGING'			put: 'FALSE';
		at: 'STN_TRAN_LOG_DIRECTORIES'		put: '$GEMSTONE/data/, $GEMSTONE/data/';
		at: 'STN_TRAN_LOG_SIZES'					put: '10, 10';
		yourself.
	self
		dbfExtentNames: '$GEMSTONE/data/myExtent0.dbf';
		yourself.
!

path
	^path!

path: anObject
	path := anObject!

stnTranLogPrefix: aString

	dictionary 
		at: 'STN_TRAN_LOG_PREFIX'
		put: aString.
!

write

	| stream |
	stream := GsFile openWriteOnServer: path.
	[
		dictionary keys asSortedCollection do: [:each | 
			stream
				nextPutAll: each;
				nextPutAll: ' = ';
				nextPutAll: (dictionary at: each);
				nextPutAll: ';';
				lf.
		].
	] ensure: [
		stream close.
	].
! !
!GsConfiguration categoriesFor: #dbfExtentNames:!public! !
!GsConfiguration categoriesFor: #defaultPath!public! !
!GsConfiguration categoriesFor: #extentNames:!public! !
!GsConfiguration categoriesFor: #initialize!public! !
!GsConfiguration categoriesFor: #path!accessing!public! !
!GsConfiguration categoriesFor: #path:!accessing!public! !
!GsConfiguration categoriesFor: #stnTranLogPrefix:!public! !
!GsConfiguration categoriesFor: #write!public! !

!GsConfiguration class methodsFor!

emptyExtentPath

	^GsEnvironment gemstone , '\bin\extent0.dbf'.
!

new

	^super new
		initialize;
		yourself.
! !
!GsConfiguration class categoriesFor: #emptyExtentPath!public! !
!GsConfiguration class categoriesFor: #new!public! !

GsEnvironment guid: (GUID fromString: '{DC5543F4-9706-455B-91ED-E7CF9168805F}')!
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

GsFile guid: (GUID fromString: '{8823EBBA-371A-4FF7-A4CC-F0322B38BBCB}')!
GsFile comment: ''!
!GsFile categoriesForClass!Unclassified! !
!GsFile class methodsFor!

existsOnServer: aPathName

	^File exists: aPathName.
!

openWriteOnServer: aPathName

	^FileStream write: aPathName.
!

removeServerFile: aPathName

	^File delete: aPathName.
! !
!GsFile class categoriesFor: #existsOnServer:!public! !
!GsFile class categoriesFor: #openWriteOnServer:!public! !
!GsFile class categoriesFor: #removeServerFile:!public! !

GsHostProcess guid: (GUID fromString: '{4D8250F8-AC41-45BD-A32D-CF9D1B068234}')!
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
	array isNilOrEmpty ifTrue: [array := #( '' )].
	status := array last trimBlanks.
	array := aStream nextLine subStrings: $=.
	array isNilOrEmpty ifTrue: [array := #( '' )].
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

GsNRS guid: (GUID fromString: '{6E44EBAE-896C-4799-978F-F24BD09AF923}')!
GsNRS comment: ''!
!GsNRS categoriesForClass!Unclassified! !
!GsNRS methodsFor!

authorization: aString
	"username [@password]
	A valid user on the target network (default is the current user). 
	A valid password is needed only if the resource type requires authentication.
	If no authentication information is specified, the system will try to get it from the .netrc file
		(his type of authorization is the default)."

	(aString isKindOf: String) ifFalse: [self error: 'parameter must be a String'].
	authorization := aString.
!

beEncrypted

	encrypted := true.
!

beNotEncrypted

	encrypted := false.
!

body: aString
	"The <body> is interpreted according to the context established by the <resource>.
	No extended identifier expansion is done in the ,body>, and no special escapes are needed."

	(aString isKindOf: String) ifFalse: [self error: 'parameter must be a String'].
	body := aString.
!

dir: aString
	"<dir> sets the default directory of the network resource.
	It has no effect if the resource already exists.
	If a directory is not set, the pattern %H is used.

	The string can contain patterns that are expanded in the context of the created resource.
	The following patterns are supported:
		%H	home directory
		%M	machine's network node name
		%N	executable's base name
		%P	process pid
		%U	user name
		%%	%"

	(aString isKindOf: String) ifFalse: [self error: 'parameter must be a String'].
	dir := aString.
!

initialize

	protocol := ''.
	node := ''.
	authorization := ''.
	encrypted := false.
	netldi := ''.
	dir := ''.
	log := ''.
	resource := ''.
	body := ''.
!

log: aString
	"<log> sets the name of the log file of the network resource.
	It has no effect if the resource already exists.
	If the log name is a relative path, it is relative to the working directory.
	If a log name is not set, the pattern '%N%P%M.log' is used.

	The string can contain patterns that are expanded in the context of the created resource.
	The following patterns are supported:
		%H	home directory
		%M	machine's network node name
		%N	executable's base name
		%P	process pid
		%U	user name
		%%	%"


	(aString isKindOf: String) ifFalse: [self error: 'parameter must be a String'].
	log := aString.
!

netldi: aString
	"Use the named NetLDI to service the request.
	If no NetLDI is specified, the default depends on the product:
		netldi60	GemStone/S 6.x"

	(aString isKindOf: String) ifFalse: [self error: 'parameter must be a String'].
	netldi := aString.
!

node: aString
	"Specifies the host on which the resource exists.
	If no node is specified, the current machine's network node name is used.
	The identifier may be an Internet-style numeric address."

	(aString isKindOf: String) ifFalse: [self error: 'parameter must be a String'].
	node := aString.
!

printOn: aStream

	| bang |
	bang := (protocol , node , authorization , netldi , dir , log , resource) isEmpty
		ifTrue: ['']
		ifFalse: ['!!'].
	aStream nextPutAll: bang.
	protocol notEmpty ifTrue: [
		aStream nextPutAll: protocol.
	].
	node notEmpty ifTrue: [
		aStream nextPutAll: '@' , node.
	].
	authorization notEmpty ifTrue: [
		aStream nextPutAll: '#auth:' , authorization.
	].
	encrypted ifTrue: [
		aStream nextPutAll: '#encrypted'.
	].
	netldi notEmpty ifTrue: [
		aStream nextPutAll: '#netldi:' , netldi.
	].
	dir notEmpty ifTrue: [
		aStream nextPutAll: '#dir:' , dir .
	].
	log notEmpty ifTrue: [
		aStream nextPutAll: '#log:' , log.
	].
	resource notEmpty ifTrue: [
		aStream nextPutAll: '#' , resource.
	].
	aStream
		nextPutAll: bang;
		nextPutAll: body.
!

protocol: aString
	"( tcp | decnet | serial | default )
	default is TCP"

	(aString isKindOf: String) ifFalse: [self error: 'parameter must be a String'].
	(#('' 'tcp' 'decnet' 'serial' 'default' ) includes aString) ifFalse: [self error: 'Invalid protocol'].
	protocol := aString.
!

resource: aString
	"Identifies the intended purpose of the <body>. 
	An NRS can contain only one resource modifier.
	The default resource modifier is context sensitive.
	For instance, if the system expects an NRS for a database file
	(say, in the context of copydbf), then the default is #dbf.

	'server' directs the NetLDI to search for the network address of a server,
	such as a Stone or another NetLDI. If successful, it returns the address.
	The <body> is a network server name. A successful lookup means only
	that the service has been defined; it does not indicate whether the service
	is currently running. A new process will not be started.
	(Authorization is needed only if the NetLDI is on a remote node
	and is running in secure mode.)

	'task' starts a new Gem. The <body> is a NetLDI service name
	(such as 'gemnetobject'), followed by arguments to the command line.
	The NetLDI creates the named service by looking first for an entry in
	$GEMSTONE/bin/services.dat, and then in the user's home directory
	for an executable having that name. 
	The NetLDI returns the network address of the service.
	(Authorization is needed to create a new process unless the NetLDI
	is in guest mode.)
	The 'task' resource modifier is also used internally to create page servers.

	'dbf' is used to access a database file. The <body> is the file spec 
	of a GemStone dataabse file. 
	The NetLDI creates a page server on the given node to access
	the database and returns the network address of the page server.
	(Authorization is needed unless the NetLDI is in guest mode.)

	'spawn' is used internally to start the garbage-collection Gem process.

	'monitor' is used internally to start up a shared page cache monitor."

	(aString isKindOf: String) ifFalse: [self error: 'parameter must be a String'].
	(#('server' 'task' 'dbf' 'spawn' 'monitor' ) includes aString) ifFalse: [self error: 'Invalid resource'].
	resource := aString.
! !
!GsNRS categoriesFor: #authorization:!accessing!public! !
!GsNRS categoriesFor: #beEncrypted!accessing!public! !
!GsNRS categoriesFor: #beNotEncrypted!accessing!public! !
!GsNRS categoriesFor: #body:!accessing!public! !
!GsNRS categoriesFor: #dir:!accessing!public! !
!GsNRS categoriesFor: #initialize!private! !
!GsNRS categoriesFor: #log:!accessing!public! !
!GsNRS categoriesFor: #netldi:!accessing!public! !
!GsNRS categoriesFor: #node:!accessing!public! !
!GsNRS categoriesFor: #printOn:!public! !
!GsNRS categoriesFor: #protocol:!accessing!public! !
!GsNRS categoriesFor: #resource:!accessing!public! !

!GsNRS class methodsFor!

body: aString

	^self new 
		body: aString;
		yourself.
!

new

	^super new
		initialize;
		yourself.
! !
!GsNRS class categoriesFor: #body:!public! !
!GsNRS class categoriesFor: #new!public! !

GsShellCommand guid: (GUID fromString: '{66A016F6-AC46-48AD-B33B-83A1148D2F67}')!
GsShellCommand comment: ''!
!GsShellCommand categoriesForClass!Unclassified! !
!GsShellCommand methodsFor!

addArgumentStringTo: aStream

	arguments do: [:each | aStream space nextPutAll: each].
!

argumentString

	| stream |
	stream := WriteStream on: String new.
	arguments do: [:each | stream nextPutAll: each; space].
	^stream contents.
!

commandName

	self subclassResponsibility.
!

initialize

	arguments := OrderedCollection new.
!

result

	| stream |
	stream := WriteStream on: String new.
	stream nextPutAll: self commandName.
	self addArgumentStringTo: stream.
	^System performOnServer: stream contents.
!

services

	| stream list |
	stream := ReadStream on: self query.
	list := OrderedCollection new.
	[
		stream atEnd not.
	] whileTrue: [
		list add: (self serviceClass new
			initializeFrom: stream;
			yourself).
	].
	^list.
! !
!GsShellCommand categoriesFor: #addArgumentStringTo:!public! !
!GsShellCommand categoriesFor: #argumentString!public! !
!GsShellCommand categoriesFor: #commandName!public! !
!GsShellCommand categoriesFor: #initialize!private! !
!GsShellCommand categoriesFor: #result!public! !
!GsShellCommand categoriesFor: #services!public! !

!GsShellCommand class methodsFor!

executables

	^self find: self exeName.
!

find

	^self find: self exeName.
!

find: aString

	| path list |
	(path := System clientEnvironmentVariable: 'PATH') isNil ifTrue: [^#()].
	list := path subStrings: $;.
	list := list collect: [:each | each , '\' , aString].
	list := list select: [:each | GsFile existsOnServer: each].
	^list.
!

new

	^super new
		initialize;
		yourself.
! !
!GsShellCommand class categoriesFor: #executables!public! !
!GsShellCommand class categoriesFor: #find!public! !
!GsShellCommand class categoriesFor: #find:!public! !
!GsShellCommand class categoriesFor: #new!private! !

GsWin32Service guid: (GUID fromString: '{CFC54123-6EE9-4EC1-BE79-9E7F68A4D9A2}')!
GsWin32Service comment: ''!
!GsWin32Service categoriesForClass!Unclassified! !
!GsWin32Service methodsFor!

account
	^account!

created
	^created!

creator
	^creator!

initializeFrom: aStream

	| stream dict line |
	dict := Dictionary new.
	[
		aStream atEnd not and: [
		(line := aStream nextLine) notEmpty].
	] whileTrue: [
		stream := ReadStream on: line.
		dict
			at: (stream upTo: $:) trimBlanks
			put: stream nextLine trimBlanks.
	].
	name 	:= dict at: 'Name' 		ifAbsent: [''].
	startup 	:= dict at: 'Startup' 	ifAbsent: [''].
	account 	:= dict at: 'Account' 	ifAbsent: [''].
	path 	:= dict at: 'Path' 		ifAbsent: [''].
	creator 	:= dict at: 'Creator' 	ifAbsent: [''].
	created 	:= dict at: 'Created' 	ifAbsent: [''].
	version 	:= dict at: 'Version' 	ifAbsent: [''].
	options 	:= dict at: 'Options' 	ifAbsent: [''].
!

name
	^name!

options
	^options!

path
	^path!

serviceName

	self subclassResponsibility.
!

startup
	^startup!

version
	^version! !
!GsWin32Service categoriesFor: #account!public! !
!GsWin32Service categoriesFor: #created!public! !
!GsWin32Service categoriesFor: #creator!public! !
!GsWin32Service categoriesFor: #initializeFrom:!public! !
!GsWin32Service categoriesFor: #name!public! !
!GsWin32Service categoriesFor: #options!public! !
!GsWin32Service categoriesFor: #path!public! !
!GsWin32Service categoriesFor: #serviceName!public! !
!GsWin32Service categoriesFor: #startup!public! !
!GsWin32Service categoriesFor: #version!public! !

System guid: (GUID fromString: '{BE3A2D98-7395-4E3B-9CC9-4D18F69E26DE}')!
System comment: ''!
!System categoriesForClass!Unclassified! !
!System class methodsFor!

clientEnvironmentVariable: aString

	^SessionManager current getenv: aString.
!

gemEnvironmentVariable: aString

	^SessionManager current getenv: aString.
!

performOnServer: aString

	| stream answer string counter batPath tmpPath outPath |
	batPath := File tempPath , 'gsCmd.bat'.
	tmpPath := File tempPath , 'gsCmd.tmp'.
	outPath := File tempPath , 'gsCmd.out'.
	self removeIfPresent: tmpPath.
	self removeIfPresent: outPath.
	stream := FileStream write: batPath.
	[
		stream nextPutAll: aString.
		stream 
			nextPutAll: ' > gsCmd.tmp 2>&1'; cr;
			nextPutAll: 'copy gsCmd.tmp gsCmd.out'; cr;
			nextPutAll: 'del gsCmd.tmp'; cr;
			yourself.
	] ensure: [
		stream close.
	].
	answer := ShellLibrary default
		shellExecute: View foregroundHandle
		lpOperation: 'open'
		lpFile: 'gsCmd.bat'
		lpParameters: nil
		lpDirectory: File tempPath
		nShowCmd: 0.
	answer := answer asInteger.
	answer <= 32 ifTrue: [self error: 'ShellExecute() failed with error # ' , answer printString].

	counter := 0.
	[
		stream := FileStream read: outPath.
	] on: Error do: [:ex | 
		(counter := counter + 1) > 100 ifTrue: [
			self error: 'Timeout (~50 seconds) wating for results.'.
		].
		Processor sleep: counter * 10.
		ex retry.
	].			
	[
		string := stream upToEnd.
	] ensure: [
		stream close.
	].
	counter := 0.
	[
		(File exists: batPath) ifTrue: [File delete: batPath].
		(File exists: outPath) ifTrue: [File delete: outPath].
	] on: Error do: [:ex | 
		(counter := counter + 1) > 10 ifTrue: [
			self error: 'Timeout (~10 seconds) waiting to delete file.'.
		].
		Processor sleep: counter * 10.
		ex retry.
	].			
	^string.
!

removeIfPresent: aString

	[
		File exists: aString.
	] whileTrue: [
		[
			GsFile removeServerFile: aString.
		] on: Error do: [:ex | 
			ex return.
		].
	].

! !
!System class categoriesFor: #clientEnvironmentVariable:!public! !
!System class categoriesFor: #gemEnvironmentVariable:!public! !
!System class categoriesFor: #performOnServer:!public! !
!System class categoriesFor: #removeIfPresent:!public! !

NetLDI guid: (GUID fromString: '{3ABB464C-2B04-4907-AAB0-A2516187802D}')!
NetLDI comment: ''!
!NetLDI categoriesForClass!Unclassified! !
!NetLDI methodsFor!

account

	^'??'.
!

applyToLogin: aJadeLogin

	aJadeLogin
		gemHost: 	self gemHost;
		gemService: self gemService;
		gemType: 	self gemType;
		yourself.
!

created

	^'??'.
!

creator

	^'??'.
!

gemHost

	^self gemType = #'remote'
		ifTrue:  [(self name subStrings: $:) first]
		ifFalse: [''].
!

gemService

	^(self name subStrings: $:) last.
!

gemType

	^(self name includes: $:)
		ifTrue:  [#'remote']
		ifFalse: [#'local'].
!

path

	^'??'.
!

processType

	^'NetLDI'.
!

startup

	^'??'.
! !
!NetLDI categoriesFor: #account!public! !
!NetLDI categoriesFor: #applyToLogin:!public! !
!NetLDI categoriesFor: #created!public! !
!NetLDI categoriesFor: #creator!public! !
!NetLDI categoriesFor: #gemHost!public! !
!NetLDI categoriesFor: #gemService!public! !
!NetLDI categoriesFor: #gemType!public! !
!NetLDI categoriesFor: #path!public! !
!NetLDI categoriesFor: #processType!public! !
!NetLDI categoriesFor: #startup!public! !

!NetLDI class methodsFor!

type

	^'Netldi'.
! !
!NetLDI class categoriesFor: #type!public! !

SharedPageCacheMonitor guid: (GUID fromString: '{55394EAE-4FB3-4E22-84EC-32FA775C8625}')!
SharedPageCacheMonitor comment: ''!
!SharedPageCacheMonitor categoriesForClass!Unclassified! !
!SharedPageCacheMonitor methodsFor!

processType

	^'Cache'.
! !
!SharedPageCacheMonitor categoriesFor: #processType!public! !

!SharedPageCacheMonitor class methodsFor!

type

	^'cache'.
! !
!SharedPageCacheMonitor class categoriesFor: #type!public! !

Stone guid: (GUID fromString: '{0FD79054-79B1-4DEB-887B-4EDBD9A1758D}')!
Stone comment: ''!
!Stone categoriesForClass!Unclassified! !
!Stone methodsFor!

applyToLogin: aJadeLogin

	aJadeLogin
		stoneHost: 	self stoneHost;
		stoneName:  self stoneName;
		yourself.
!

processType

	^'Stone'.
!

stoneHost

	^(self name includes: $:)
		ifTrue:  [(self name subStrings: $:) first]
		ifFalse: [''].
!

stoneName

	^(self name subStrings: $:) last.
! !
!Stone categoriesFor: #applyToLogin:!public! !
!Stone categoriesFor: #processType!public! !
!Stone categoriesFor: #stoneHost!public! !
!Stone categoriesFor: #stoneName!public! !

!Stone class methodsFor!

test

	| extentPath config copyResult startResult |
	extentPath := 'c:\gemstone\614\data\myExtent0.dbf'.
	(File exists: extentPath) ifTrue: [File delete: extentPath].
	copyResult := CopyDBF new
		sourceNRS: 		(GsNRS body: GsConfiguration emptyExtentPath);
		destinationNRS: 	(GsNRS body: extentPath);
		result.
	config := GsConfiguration new
		extentNames: extentPath;
		write;
		yourself.
	startResult := StartStone new
		sysConfig: config path;
		stoneName: 'myStone';
		command: 'tmpstart';
		result.
	self halt.
!

test1

	| extentPath config copyResult |
	extentPath := 'c:/gemstone/614/data/myExtent0.dbf'.
	(GsFile existsOnServer: extentPath) ifTrue: [GsFile removeServerFile: extentPath].
	copyResult := CopyDBF new
		sourceNRS: 		(GsNRS body: GsConfiguration emptyExtentPath);
		destinationNRS: 	(GsNRS body: extentPath);
		result.
	config := GsConfiguration new
		dbfExtentNames: extentPath;
		stnTranLogPrefix: 'myTranLog';
		write;
		yourself.
	^StartStone new
		sysConfig: config path;
		stoneName: 'myStone';
		command: 'tmpstart';
		result.
!

test2

	^StopStone new
		stoneName: 'myStone';
		command: 'tmpstop';
		user: 'SystemUser';
		password: 'swordfish';
		result.
!

type

	^'Stone'.
! !
!Stone class categoriesFor: #test!public! !
!Stone class categoriesFor: #test1!public! !
!Stone class categoriesFor: #test2!public! !
!Stone class categoriesFor: #type!public! !

CopyDBF guid: (GUID fromString: '{99DF6986-C176-4009-9E4E-2EDEBF810C98}')!
CopyDBF comment: ''!
!CopyDBF categoriesForClass!Unclassified! !
!CopyDBF methodsFor!

addArgumentStringTo: aStream 

	super addArgumentStringTo: aStream.
	sourceNRS notNil ifTrue: [
		aStream space; nextPutAll: '"' , sourceNRS printString , '"'.
	].
	destinationNRS notNil ifTrue: [
		aStream space; nextPutAll: '"' , destinationNRS printString , '"'.
	].
!

commandName

	^'copydbf'.
!

destinationNRS: aGsNRS

	(aGsNRS isKindOf: GsNRS) ifFalse: [self error: 'Invalid parameter type'].
	destinationNRS := aGsNRS!

information
	"Information only. When this option si present without <destinationNRS>, 
	information about <sourceNRS> is printed without performing a file copy.
	This option is ignored if <destinationNRS> is present."

	arguments add: '-i'.
	^self result.
!

sourceNRS: aGsNRS

	(aGsNRS isKindOf: GsNRS) ifFalse: [self error: 'Invalid parameter type'].
	sourceNRS := aGsNRS! !
!CopyDBF categoriesFor: #addArgumentStringTo:!public! !
!CopyDBF categoriesFor: #commandName!public! !
!CopyDBF categoriesFor: #destinationNRS:!accessing!public! !
!CopyDBF categoriesFor: #information!public! !
!CopyDBF categoriesFor: #sourceNRS:!accessing!public! !

!CopyDBF class methodsFor!

exeName

	^'copydbf.exe'.
!

sample1

	^CopyDBF new
		sourceNRS: 		(GsNRS body: 'c:\gemstone\614\data\extent0.dbf');
		destinationNRS: 	(GsNRS body: 'c:\gemstone\614\data\myExtent0.dbf');
		result.
!

sample2
	^(CopyDBF new)
		sourceNRS: ((GsNRS new)
					node: 'localhost';
					body: 'c:\gemstone\614\data\extent0.dbf';
					yourself);
		information! !
!CopyDBF class categoriesFor: #exeName!public! !
!CopyDBF class categoriesFor: #sample1!public! !
!CopyDBF class categoriesFor: #sample2!public! !

GsList guid: (GUID fromString: '{4337F4C0-1552-4A70-AE55-EA882DCFC496}')!
GsList comment: ''!
!GsList categoriesForClass!Unclassified! !
!GsList methodsFor!

clearLocks
	"removes locks left by servers that have been killed"

	arguments add: '/c'.
!

commandName
	^'gslist'!

exhaustive
	"exhaustive listing with each item on a separate line"

	arguments add: '/x'.
!

help
	"On Windows, the help is sent to the console rather than to standard output"

	arguments add: '/h'.
!

long
	"include pid and port"

	arguments add: '/l'.
!

machine: aString
	"only list servers on machine <aString>; default is the local host"

	(aString isKindOf: String) ifFalse: [self error: 'parameter must be a String'].
	arguments add: '/m ' , aString.
!

name: aString
	"only list server <aString>"

	(aString isKindOf: String) ifFalse: [self error: 'parameter must be a String'].
	arguments add: '/n ' , aString.
!

pid
	"pid only"

	arguments add: '/p'.
!

quiet
	"exclude header"

	arguments add: '/q'.
!

servers

	| stream list item |
	stream := ReadStream on: (self exhaustive; result).
	list := OrderedCollection new.
	[
		stream atEnd not.
	] whileTrue: [
		item := GsHostProcess fromStream: stream.
		item notNil ifTrue: [list add: item].
	].
	^list.

!

services

	| string stream list item |
	string := self exhaustive; result.
	(string beginsWith: 'gslist[Warning]:') ifTrue: [
		MessageBox warning: string.
		^#().
	].
	stream := ReadStream on: string.
	list := OrderedCollection new.
	[
		stream atEnd not.
	] whileTrue: [
		item := GsHostProcess fromStream: stream.
		item notNil ifTrue: [list add: item].
	].
	^list.

!

timeout: anInteger
	"wait <anInteger> seconds for server to respond (only with /v); default is 2 seconds"

	(anInteger isKindOf: Integer) ifFalse: [self error: 'parameter must be an Integer'].
	arguments add: '/t ' , anInteger printString.
!

user: aString
	"only list servers started by user <aString>"

	(aString isKindOf: String) ifFalse: [self error: 'parameter must be a String'].
	arguments add: '/u ' , aString.
!

verify
	"verify the status of each server"

	arguments add: '/v'.
! !
!GsList categoriesFor: #clearLocks!public! !
!GsList categoriesFor: #commandName!public! !
!GsList categoriesFor: #exhaustive!public! !
!GsList categoriesFor: #help!public! !
!GsList categoriesFor: #long!public! !
!GsList categoriesFor: #machine:!public! !
!GsList categoriesFor: #name:!public! !
!GsList categoriesFor: #pid!public! !
!GsList categoriesFor: #quiet!public! !
!GsList categoriesFor: #servers!public! !
!GsList categoriesFor: #services!public! !
!GsList categoriesFor: #timeout:!public! !
!GsList categoriesFor: #user:!public! !
!GsList categoriesFor: #verify!public! !

!GsList class methodsFor!

exeName

	^'gslist.exe'.
!

servers

	^GsList new servers.
!

services

	^GsList new services.
! !
!GsList class categoriesFor: #exeName!public! !
!GsList class categoriesFor: #servers!public! !
!GsList class categoriesFor: #services!public! !

NetLDICommand guid: (GUID fromString: '{0EC19966-CCD6-4F33-B7F5-7DE111CC9EDB}')!
NetLDICommand comment: ''!
!NetLDICommand categoriesForClass!Unclassified! !
!NetLDICommand methodsFor!

addArgumentStringTo: aStream

	command isNil ifTrue: [self error: 'Missing command!!'].
	aStream space; nextPutAll: command.
	super addArgumentStringTo: aStream.
!

command: aString

	(aString isKindOf: String) ifFalse: [self error: 'parameter must be a String'].
	(#('help' 'create' 'delete' 'start' 'stop' 'query' ) includes: aString) 
		ifFalse: [self error: 'Invalid resource'].
	command := aString.
!

commandName

	^'netldi'.
!

query

	command := 'query'.
	^self result.
!

serviceClass

	^NetLDIService.
! !
!NetLDICommand categoriesFor: #addArgumentStringTo:!public! !
!NetLDICommand categoriesFor: #command:!public! !
!NetLDICommand categoriesFor: #commandName!public! !
!NetLDICommand categoriesFor: #query!public! !
!NetLDICommand categoriesFor: #serviceClass!public! !

!NetLDICommand class methodsFor!

exeName

	^'netldi.exe'.
!

sample1

	^NetLDICommand new query.
!

sample2
	^(NetLDICommand new)
		command: 'help';
		result!

sample3
	^(NetLDICommand new)
		command: 'query';
		help! !
!NetLDICommand class categoriesFor: #exeName!public! !
!NetLDICommand class categoriesFor: #sample1!public! !
!NetLDICommand class categoriesFor: #sample2!public! !
!NetLDICommand class categoriesFor: #sample3!public! !

PageAudit guid: (GUID fromString: '{1F76FD0A-2C91-4344-BE74-CE7A1ABBCA30}')!
PageAudit comment: ''!
!PageAudit categoriesForClass!Unclassified! !
!PageAudit methodsFor!

addArgumentStringTo: aStream

	stoneName notNil ifTrue: [
		aStream space; nextPutAll: stoneName.
	].
	super addArgumentStringTo: aStream.
!

commandName

	^'stone audit'.
!

initialize

	super initialize.
	stoneName := ''.
!

stoneName: aString 

	(aString isKindOf: String) ifFalse: [self error: 'parameter must be a String'].
	stoneName := aString.
! !
!PageAudit categoriesFor: #addArgumentStringTo:!public! !
!PageAudit categoriesFor: #commandName!public! !
!PageAudit categoriesFor: #initialize!public! !
!PageAudit categoriesFor: #stoneName:!accessing!public! !

!PageAudit class methodsFor!

exeName

	^'stoned.exe'.
!

sample1
	^(PageAudit new)
		stoneName: 'gemserver60';
		result! !
!PageAudit class categoriesFor: #exeName!public! !
!PageAudit class categoriesFor: #sample1!public! !

StoneCommand guid: (GUID fromString: '{879E630A-6998-4427-AEBF-14A83DCA8E46}')!
StoneCommand comment: ''!
!StoneCommand categoriesForClass!Unclassified! !
!StoneCommand methodsFor!

addArgumentStringTo: aStream

	command isNil ifTrue: [self error: 'Missing command!!'].
	aStream space; nextPutAll: command.
	stoneName notNil ifTrue: [
		aStream space; nextPutAll: stoneName.
	].
	super addArgumentStringTo: aStream.
!

command: aString

	(aString isKindOf: String) ifFalse: [self error: 'parameter must be a String'].
	(#('help' 'create' 'delete' 'start' 'tmpstart' 'stop' 'tmpstop' 'pause' 'continue' 'query' ) includes: aString) 
		ifFalse: [self error: 'Invalid resource'].
	command := aString!

commandName

	^'stone'.
!

help

	arguments add: '/h'.
	^self result.
!

initialize

	super initialize.
	stoneName := ''.
!

query

	command := 'query'.
	^self result.
!

serviceClass

	^StoneService.
!

stoneName: aString 

	(aString isKindOf: String) ifFalse: [self error: 'parameter must be a String'].
	stoneName := aString.
! !
!StoneCommand categoriesFor: #addArgumentStringTo:!public! !
!StoneCommand categoriesFor: #command:!accessing!public! !
!StoneCommand categoriesFor: #commandName!public! !
!StoneCommand categoriesFor: #help!public! !
!StoneCommand categoriesFor: #initialize!public! !
!StoneCommand categoriesFor: #query!public! !
!StoneCommand categoriesFor: #serviceClass!public! !
!StoneCommand categoriesFor: #stoneName:!accessing!public! !

!StoneCommand class methodsFor!

exeName

	^'stoned.exe'.
!

sample1

	^StoneCommand new query.
!

sample2
	^(StoneCommand new)
		command: 'help';
		result!

sample3
	^(StoneCommand new)
		command: 'query';
		help! !
!StoneCommand class categoriesFor: #exeName!public! !
!StoneCommand class categoriesFor: #sample1!public! !
!StoneCommand class categoriesFor: #sample2!public! !
!StoneCommand class categoriesFor: #sample3!public! !

StartStone guid: (GUID fromString: '{77627D3A-E2B8-4CF7-9D01-A599D8CE1677}')!
StartStone comment: ''!
!StartStone categoriesForClass!Unclassified! !
!StartStone methodsFor!

exeConfig: aString

	(aString isKindOf: String) ifFalse: [self error: 'parameter must be a String'].
	arguments add: '/e ' , aString.
!

initialize

	super initialize.
	command := 'start'.
!

log: aString

	(aString isKindOf: String) ifFalse: [self error: 'parameter must be a String'].
	arguments add: '/l ' , aString.
!

sysConfig: aString

	(aString isKindOf: String) ifFalse: [self error: 'parameter must be a String'].
	arguments add: '/z ' , aString.
!

temp

	command := 'tmpstart'.
! !
!StartStone categoriesFor: #exeConfig:!public! !
!StartStone categoriesFor: #initialize!public! !
!StartStone categoriesFor: #log:!public! !
!StartStone categoriesFor: #sysConfig:!public! !
!StartStone categoriesFor: #temp!public! !

!StartStone class methodsFor!

sample1
	^StartStone new help! !
!StartStone class categoriesFor: #sample1!public! !

StopStone guid: (GUID fromString: '{7995E10B-D8AA-4EDE-8642-50B6F20F20E1}')!
StopStone comment: ''!
!StopStone categoriesForClass!Unclassified! !
!StopStone methodsFor!

immediate

	arguments add: '/i'.
!

initialize

	super initialize.
	command := 'stop'.
!

password: aString

	(aString isKindOf: String) ifFalse: [self error: 'parameter must be a String'].
	arguments add: '/p ' , aString.
!

temp

	command := 'tmpstop'.
!

user: aString

	(aString isKindOf: String) ifFalse: [self error: 'parameter must be a String'].
	arguments add: '/u ' , aString.
! !
!StopStone categoriesFor: #immediate!public! !
!StopStone categoriesFor: #initialize!public! !
!StopStone categoriesFor: #password:!public! !
!StopStone categoriesFor: #temp!public! !
!StopStone categoriesFor: #user:!public! !

!StopStone class methodsFor!

sample1
	^StopStone new help! !
!StopStone class categoriesFor: #sample1!public! !

NetLDIService guid: (GUID fromString: '{913AFDF5-70DB-41D1-9C9B-21796B4179F3}')!
NetLDIService comment: ''!
!NetLDIService categoriesForClass!Unclassified! !
!NetLDIService methodsFor!

serviceName

	^'NetLDI'.
!

start

	^NetLDICommand new 
		command: 'start';
		result.
!

stop

	^NetLDICommand new 
		command: 'stop';
		result.
! !
!NetLDIService categoriesFor: #serviceName!public! !
!NetLDIService categoriesFor: #start!public! !
!NetLDIService categoriesFor: #stop!public! !

StoneService guid: (GUID fromString: '{2A7F76CE-EA21-455B-82FF-BBF91C334B88}')!
StoneService comment: ''!
!StoneService categoriesForClass!Unclassified! !
!StoneService methodsFor!

serviceName

	^'Stone'.
!

start

	^StartStone new 
		stoneName: name;
		result.
!

stop

	^StopStone new 
		stoneName: name;
		result.
! !
!StoneService categoriesFor: #serviceName!public! !
!StoneService categoriesFor: #start!public! !
!StoneService categoriesFor: #stop!public! !

"Binary Globals"!


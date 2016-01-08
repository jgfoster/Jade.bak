"Filed out from Dolphin Smalltalk 7"!

Object subclass: #GsNRS
	instanceVariableNames: 'protocol node authorization encrypted netldi dir log resource body'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsNRS guid: (GUID fromString: '{DA99D714-2195-4901-A28B-776D4F8178A0}')!
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


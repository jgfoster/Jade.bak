! ------------------- Class definition for UserInteraction
expectvalue /Class
doit
Object subclass: 'UserInteraction'
  instVarNames: #()
  classVars: #()
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: UserGlobals
  options: #()

%
expectvalue /Class
doit
UserInteraction category: 'Kernel'
%

! ------------------- Remove existing behavior from UserInteraction
expectvalue /Metaclass3       
doit
UserInteraction removeAllMethods.
UserInteraction class removeAllMethods.
%
! ------------------- Class methods for UserInteraction
! ------------------- Instance methods for UserInteraction
set compile_env: 0
category: 'other'
method: UserInteraction
alert: aString

	^(System __sessionStateAt: 3)
		message: aString
		caption: 'Jade'
		icon: #prompt
		buttons: #yesNoCancel.
%
category: 'other'
method: UserInteraction
message: messageString caption: captionString icon: iconSymbol buttons: buttonSymbol

	^(System __sessionStateAt: 3)
		message: messageString
		caption: captionString
		icon: iconSymbol
		buttons: buttonSymbol.
%
category: 'other'
method: UserInteraction
prompt: promptString

	^(System __sessionStateAt: 3)
		prompt: promptString
		caption: 'Jade'.
%
category: 'other'
method: UserInteraction
prompt: promptString caption: captionString

	^(System __sessionStateAt: 3)
		prompt: promptString
		caption: captionString
%

"Filed out from Dolphin Smalltalk 7"!

DolphinProfessional subclass: #Jade
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Jade guid: (GUID fromString: '{127BC143-2779-43EA-B88D-0E575A9EE8B4}')!
Jade comment: 'This class helps us build the Jade Development image'!
!Jade categoriesForClass!Development! !
!Jade methodsFor!

contents
	"Answer the list of package contents owned by the receiver"

	^super contents
		add: #('Jade\source\JGF Dolphin.pax' 			#plain #imageBased);
		add: #('Jade\source\JGF Dolphin MVP Base.pax' 	#plain #imageBased);
		add: #('Jade\source\JGF Fading Dialog.pax' 		#plain #imageBased);
		add: #('Jade\source\GemStone C Interface.pax' 	#plain #imageBased);
		add: #('Jade\source\GemStone Objects.pax' 		#plain #imageBased);
		add: #('Jade\source\GemStone Session.pax' 		#plain #imageBased);
		add: #('Jade\source\GemStone Services.pax' 	#plain #imageBased);
		add: #('Jade\source\VisualWorks Component.pax' 	#plain #imageBased);
		add: #('Jade\source\Jade UI Base.pax' 			#plain #imageBased);
		add: #('Jade\source\Jade from Dolphin.pax' 		#plain #imageBased);
		add: #('Jade\source\Jade Autocompletation.pax' 	#plain #imageBased);
		add: #('Jade\source\Jade Inspector.pax' 		#plain #imageBased);
		add: #('Jade\source\Jade Class Browser.pax' 		#plain #imageBased);
		add: #('Jade\source\Jade Method Browser.pax' 	#plain #imageBased);
		add: #('Jade\source\Jade User Browser.pax' 		#plain #imageBased);
		add: #('Jade\source\Monticello.pax' 			#plain #imageBased);
		add: #('Jade\source\Jade UI.pax' 				#plain #imageBased);
		add: #('Jade\source\Jade System Browser.pax' 	#plain #imageBased);
		add: #('Jade\source\Jade Transcript.pax' 		#plain #imageBased);
		add: #('Jade\source\Jade Process Browser.pax' 	#plain #imageBased);
		add: #('Jade\source\Object Log Browser.pax' 	#plain #imageBased);
		add: #('Jade\source\Jade Login.pax' 			#plain #imageBased);
		add: #('Jade\source\Jade Deployment.pax' 		#plain #imageBased);
		add: #('Jade\source\Contacts Demo.pax' 		#plain #imageBased);
		add: #('Jade\source\Jade in Dolphin.pax' 		#plain #imageBased);
		add: #('Jade\source\Jade Development.pax' 	#plain #imageBased);
		yourself!

fullName
	"Answer the full product name associated with the receiver"

	^'Jade Development'!

shortProductName
	"Answer the abbreviated product name"
	
	^'Jade'! !
!Jade categoriesFor: #contents!public! !
!Jade categoriesFor: #fullName!public! !
!Jade categoriesFor: #shortProductName!public! !

!Jade class methodsFor!

productName
	"Answer the product name"
	
	^'for Jade'!

shortProductName
	"Answer the abbreviated product name"
	
	^'Jade'! !
!Jade class categoriesFor: #productName!accessing!public! !
!Jade class categoriesFor: #shortProductName!accessing!public! !


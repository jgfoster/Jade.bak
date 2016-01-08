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
		add: #('Jade\JGF Dolphin.pax' 			#plain #imageBased);
		add: #('Jade\JGF Dolphin MVP Base.pax' 	#plain #imageBased);
		add: #('Jade\JGF Fading Dialog.pax' 		#plain #imageBased);
		add: #('Jade\GemStone C Interface.pax' 	#plain #imageBased);
		add: #('Jade\GemStone Objects.pax' 		#plain #imageBased);
		add: #('Jade\GemStone Session.pax' 		#plain #imageBased);
		add: #('Jade\GemStone Services.pax' 	#plain #imageBased);
		add: #('Jade\VisualWorks Component.pax' 	#plain #imageBased);
		add: #('Jade\Jade UI Base.pax' 			#plain #imageBased);
		add: #('Jade\Jade from Dolphin.pax' 		#plain #imageBased);
		add: #('Jade\Jade Autocompletation.pax' 	#plain #imageBased);
		add: #('Jade\Jade Inspector.pax' 		#plain #imageBased);
		add: #('Jade\Jade Class Browser.pax' 		#plain #imageBased);
		add: #('Jade\Jade Method Browser.pax' 	#plain #imageBased);
		add: #('Jade\Jade User Browser.pax' 		#plain #imageBased);
		add: #('Jade\Monticello.pax' 			#plain #imageBased);
		add: #('Jade\Jade UI.pax' 				#plain #imageBased);
		add: #('Jade\Jade System Browser.pax' 	#plain #imageBased);
		add: #('Jade\Jade Transcript.pax' 		#plain #imageBased);
		add: #('Jade\Jade Process Browser.pax' 	#plain #imageBased);
		add: #('Jade\Object Log Browser.pax' 	#plain #imageBased);
		add: #('Jade\Jade Login.pax' 			#plain #imageBased);
		add: #('Jade\Jade Deployment.pax' 		#plain #imageBased);
		add: #('Jade\Contacts Demo.pax' 		#plain #imageBased);
		add: #('Jade\Jade in Dolphin.pax' 		#plain #imageBased);
		add: #('Jade\Jade Development.pax' 	#plain #imageBased);
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


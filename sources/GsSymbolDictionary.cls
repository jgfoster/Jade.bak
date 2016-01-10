"Filed out from Dolphin Smalltalk 7"!

GsObject subclass: #GsSymbolDictionary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsSymbolDictionary guid: (GUID fromString: '{EAE064DD-2265-4B2F-ACA0-766DFBFA394C}')!
GsSymbolDictionary comment: ''!
!GsSymbolDictionary categoriesForClass!Unclassified! !
!GsSymbolDictionary methodsFor!

categoryList

	| string list |
	string := gciSession 
		serverPerform: #'categoryListFor:' 
		with: self.
	list := GsString
		listFromString: string 
		session: gciSession.
	^list.
!

classesInCategory: aGsString 

	| string |
	string := gciSession 
		serverPerform: #classListFor:category:
		with: self
		with: aGsString.
	^GsClass 
		listFromString: string 
		session: gciSession.
!

globals

	| string |
	string := gciSession serverPerform: #globalsFor: with: self.
	^GsGlobal 
		listFromString: string 
		session: gciSession.
! !
!GsSymbolDictionary categoriesFor: #categoryList!public! !
!GsSymbolDictionary categoriesFor: #classesInCategory:!public! !
!GsSymbolDictionary categoriesFor: #globals!public! !


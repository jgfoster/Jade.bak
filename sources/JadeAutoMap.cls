"Filed out from Dolphin Smalltalk 7"!

Object subclass: #JadeAutoMap
	instanceVariableNames: 'name gsClassShapeName'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeAutoMap guid: (GUID fromString: '{15764ac6-5fd1-4c64-94f6-4dfe155108d4}')!
JadeAutoMap comment: 'This class is used to map <names> with JadeGsClassShape.

The idea is to create instances of this classes to relate names with autocompleation classes.

JadeAutoMap new name: ''html'' map: (JadeAutocompletationRegistry default getClass: ''WAHtmlCanvas'').

Each time ''html'' is used as variable then autocompletation it will assume that <html> is an instances of WAHtmlCanvas.'!
!JadeAutoMap categoriesForClass!Unclassified! !
!JadeAutoMap methodsFor!

gsClassShapeName
	^gsClassShapeName!

gsClassShapeName: anObject
	gsClassShapeName := anObject!

name
	^name!

name: anObject
	name := anObject! !
!JadeAutoMap categoriesFor: #gsClassShapeName!accessing!private! !
!JadeAutoMap categoriesFor: #gsClassShapeName:!accessing!private! !
!JadeAutoMap categoriesFor: #name!accessing!private! !
!JadeAutoMap categoriesFor: #name:!accessing!private! !


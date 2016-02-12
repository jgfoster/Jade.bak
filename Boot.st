"Boot a Jade development image"

| list |
list := #(
	'JGF Dolphin'
	'JGF Dolphin MVP Base'
	'JGF Fading Dialog'
	'GemStone C Interface'
	'GemStone Objects'
	'GemStone Session'
	'GemStone Services'
	'VisualWorks Component'
	'Jade UI Base'
	'Jade from Dolphin'
	'Jade Autocompletation'
	'Jade Inspector'
	'Jade Class Browser'
	'Jade Method Browser'
	'Jade User Browser'
	'Monticello'
	'Jade UI'
	'Jade System Browser'
	'Jade Transcript'
	'Jade Process Browser'
	'Object Log Browser'
	'Jade Login'
	'Jade Deployment'
	'Contacts Demo'
	'Jade in Dolphin'
	'Jade Tests'
).
ProgressDialog showModalWhile: [:progress |
	1 to: list size do: [:i |
		| name percent |
		name := list at: i.
		percent := (i * 100 / list size) asInteger.
		progress
			value: percent;
			text: 'Loading package: ' , name;
			yourself.
		PackageManager current install: 'sources\' , name , '.pax'.
	].
].
SessionManager current splashShowAtStartup: false.
SourceManager default compressSources.
SessionManager current saveImage.
!

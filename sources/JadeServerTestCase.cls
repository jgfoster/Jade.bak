"Filed out from Dolphin Smalltalk 7"!

TestCase subclass: #JadeServerTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeServerTestCase guid: (GUID fromString: '{494dcd74-2e61-4aa2-9836-c23515c37947}')!
JadeServerTestCase comment: ''!
!JadeServerTestCase categoriesForClass!Unclassified! !
!JadeServerTestCase methodsFor!

testSocket

	| encoder stream x |
	encoder := JadeServer new.
	stream := WriteStream on: ByteArray new.
	encoder
		add: nil toByteStream: stream;
		add: true toByteStream: stream;
		add: false toByteStream: stream;
		add: (OopType32 fromInteger: 123456) toByteStream: stream;
		add: (OopType64 fromInteger: 12345678) toByteStream: stream;
		add: #'foo' toByteStream: stream;
		add: 0 toByteStream: stream;
		add: 255 toByteStream: stream;
		add: 'bar' toByteStream: stream;
		add: ((String new: 256) atAllPut: $x) asSymbol toByteStream: stream;
		add: -1 toByteStream: stream;
		add: -256 toByteStream: stream;
		add: 256 toByteStream: stream;
		add: SmallInteger maximum toByteStream: stream;
		add: SmallInteger minimum toByteStream: stream;
		add: SmallInteger maximum + 1 toByteStream: stream;
		add: SmallInteger minimum - 1 toByteStream: stream;
		add: ((String new: 256) atAllPut: $x) toByteStream: stream;
		add: #(nil true false #(1 'abc' #'foo')) toByteStream: stream;
		add: nil toByteStream: stream;
		yourself.
	stream := ReadStream on: stream contents.
	self 
		assert: (encoder readObjectFrom: stream) == nil;
		assert: (encoder readObjectFrom: stream) == true;
		assert: (encoder readObjectFrom: stream) == false;
		assert: (x := encoder readObjectFrom: stream) class == OopType32;
		assert: x value == 123456;
		assert: (x := encoder readObjectFrom: stream) class == OopType64;
		assert: x value == 12345678;
		assert: (encoder readObjectFrom: stream) == #'foo';
		assert: (encoder readObjectFrom: stream) == 0;
		assert: (encoder readObjectFrom: stream) == 255;
		assert: (encoder readObjectFrom: stream) = 'bar';
		assert: (x := encoder readObjectFrom: stream) class == Symbol;
		assert: x size == 256;
		assert: (encoder readObjectFrom: stream) == -1;
		assert: (encoder readObjectFrom: stream) == -256;
		assert: (encoder readObjectFrom: stream) == 256;
		assert: (encoder readObjectFrom: stream) == SmallInteger maximum;
		assert: (encoder readObjectFrom: stream) == SmallInteger minimum;
		assert: (encoder readObjectFrom: stream) = (SmallInteger maximum + 1);
		assert: (encoder readObjectFrom: stream) = (SmallInteger minimum - 1);
		assert: (x := encoder readObjectFrom: stream) class == String;
		assert: x size == 256;
		assert: (x := encoder readObjectFrom: stream) = #(nil true false #(1 'abc' #'foo'));
		assert: (encoder readObjectFrom: stream) == nil;
		yourself.
! !
!JadeServerTestCase categoriesFor: #testSocket!public! !


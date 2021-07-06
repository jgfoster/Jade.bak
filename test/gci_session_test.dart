import 'package:flutter_test/flutter_test.dart';

import 'package:jade/gci_session.dart';

void main() {
  final gci = GciSession('https://localhost:8888/');

  test('getVersion', () async {
    expect(
        await gci.getVersion(),
        equals(
            '3.6.1 build 2021-04-05T17:07:25-07:00 de66ed854d1f7b2adcb344f27982f568dfd5bdd2'));
  });

  test('login with invalid password', () async {
    var flag = false;
    try {
      await gci.login('DataCurator', 'spearfish');
    } on GciError catch (e) {
      expect(e.error['number'], equals(4051));
      flag = true;
    }
    expect(flag, isTrue);
  });

  test('login with valid password', () async {
    await gci.login('DataCurator', 'swordfish');
  });

  test('another login should fail', () async {
    var flag = false;
    try {
      await gci.login('DataCurator', 'swordfish');
    } on StateError catch (e) {
      expect(e.message, equals('Session already logged in!'));
      flag = true;
    }
    expect(flag, isTrue);
  });

  test('hardBreak', () async {
    await gci.hardBreak();
  });

  test('softBreak', () async {
    await gci.softBreak();
  });

  test('getError', () async {
    expect(await gci.getError(), isNull);
  });

  test('abort', () async {
    await gci.abort();
  });

  test('begin', () async {
    await gci.begin();
  });

  test('commit', () async {
    expect(await gci.commit(), isTrue);
  });

  test('isCallInProgress', () async {
    expect(await gci.isCallInProgress(), isFalse);
  });

  test('nbEnd with no result', () async {
    final response = await gci.nbEnd();
    expect(response['progress'], equals(2)); // GCI_RESULT_READY
    expect(response['result'], equals(0));
  });

  test('nbExecuteStr', () async {
    await gci.nbExecuteStr('2 + 3');
  });

  test('nbEnd with result of 5', () async {
    final response = await gci.nbEnd();
    expect(response['progress'], equals(2)); // GCI_RESULT_READY
    expect(response['result'], equals(42));
  });

  test('logout', () async {
    expect(await gci.getSessionId(), equals(1));
    await gci.logout();
    expect(await gci.getSessionId(), equals(0));
  });
}

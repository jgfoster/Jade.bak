import 'package:http/http.dart' as http;
import 'dart:convert';

class GciError extends StateError {
  late final error;
  GciError(Map<String, dynamic> gciError)
      : error = gciError,
        super(gciError['message']);
}

class GciSession {
  late final host;
  int session = 0;
  final headers = {'Content-Type': 'application/json'};

  GciSession(this.host);

  Future<Map<String, dynamic>?> getError() async {
    final url = Uri.parse(host + 'getError.gs');
    var body = {'session': session};
    final response = await http.post(
      url,
      headers: headers,
      body: jsonEncode(body),
    );
    if (response.statusCode != 200) {
      throw StateError(response.body);
    }
    if (response.body == '{}') {
      return null;
    }
    return jsonDecode(response.body)['result'];
  }

  Future<void> abort() async {
    final url = Uri.parse(host + 'abort.gs');
    var body = {'session': session};
    final response = await http.post(
      url,
      headers: headers,
      body: jsonEncode(body),
    );
    if (response.statusCode != 200) {
      throw StateError(response.body);
    }
  }

  Future<void> begin() async {
    final url = Uri.parse(host + 'begin.gs');
    var body = {'session': session};
    final response = await http.post(
      url,
      headers: headers,
      body: jsonEncode(body),
    );
    if (response.statusCode != 200) {
      throw StateError(response.body);
    }
  }

  Future<bool> commit() async {
    final url = Uri.parse(host + 'commit.gs');
    var body = {'session': session};
    final response = await http.post(
      url,
      headers: headers,
      body: jsonEncode(body),
    );
    if (response.statusCode != 200) {
      throw StateError(response.body);
    }
    return jsonDecode(response.body)['result'];
  }

  Future<int> getSessionId() async {
    final url = Uri.parse(host + 'getSessionId.gs');
    var body = {};
    final response = await http.post(
      url,
      headers: headers,
      body: jsonEncode(body),
    );
    if (response.statusCode != 200) {
      throw StateError(response.body);
    }
    return jsonDecode(response.body)['result'];
  }

  Future<String> getVersion() async {
    final url = Uri.parse(host + 'version.gs');
    var body = {};
    final response = await http.post(
      url,
      headers: headers,
      body: jsonEncode(body),
    );
    if (response.statusCode != 200) {
      throw StateError(response.body);
    }
    return jsonDecode(response.body)['result'];
  }

  Future<void> hardBreak() async {
    final url = Uri.parse(host + 'hardBreak.gs');
    var body = {'session': session};
    final response = await http.post(
      url,
      headers: headers,
      body: jsonEncode(body),
    );
    if (response.statusCode != 200) {
      throw StateError(response.body);
    }
  }

  Future<bool> isCallInProgress() async {
    final url = Uri.parse(host + 'callInProgress.gs');
    var body = {'session': session};
    final response = await http.post(
      url,
      headers: headers,
      body: jsonEncode(body),
    );
    if (response.statusCode != 200) {
      throw StateError(response.body);
    }
    return jsonDecode(response.body)['result'];
  }

  Future<void> login(String username, String password) async {
    if (session > 0) {
      throw StateError('Session already logged in!');
    }
    final url = Uri.parse(host + 'login.gs');
    var body = {
      'username': username,
      'password': password,
    };
    final response = await http.post(
      url,
      headers: headers,
      body: jsonEncode(body),
    );
    if (response.statusCode != 200) {
      throw StateError(response.body);
    }
    final result = jsonDecode(response.body);
    session = result['result'] ?? 0;
    if (session > 0) {
      return;
    }
    throw GciError(result);
  }

  Future<void> logout() async {
    final url = Uri.parse(host + 'logout.gs');
    var body = {'session': session};
    final response = await http.post(
      url,
      headers: headers,
      body: jsonEncode(body),
    );
    session = 0;
    if (response.statusCode != 200) {
      throw StateError(response.body);
    }
  }

  Future<void> nbExecuteStr(String source) async {
    final url = Uri.parse(host + 'nbExecuteStr.gs');
    var body = {'session': session, 'source': source};
    final response = await http.post(
      url,
      headers: headers,
      body: jsonEncode(body),
    );
    if (response.statusCode != 200) {
      throw StateError(response.body);
    }
  }

  /*
  typedef enum {
    GCI_RESULT_NOT_READY  = 0,  /* nothing happened */
    GCI_RESULT_PROGRESSED = 1,	/* a packet was received */
    GCI_RESULT_READY      = 2,  /* your result is now ready */
    GCI_RESULT_HAS_ERROR  = 3   /* result is ready with error */
  } GciNbProgressEType;
  */
  Future<Map<String, dynamic>> nbEnd() async {
    final url = Uri.parse(host + 'nbEnd.gs');
    var body = {'session': session};
    final response = await http.post(
      url,
      headers: headers,
      body: jsonEncode(body),
    );
    if (response.statusCode != 200) {
      throw StateError(response.body);
    }
    return jsonDecode(response.body);
  }

  Future<void> softBreak() async {
    final url = Uri.parse(host + 'softBreak.gs');
    var body = {'session': session};
    final response = await http.post(
      url,
      headers: headers,
      body: jsonEncode(body),
    );
    if (response.statusCode != 200) {
      throw StateError(response.body);
    }
  }
}

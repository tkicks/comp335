import 'dart:html';

import 'package:chrome/chrome_app.dart' as chrome;

InputElement outputText;

/**
 * A `hello world` application for Chrome Apps written in Dart.
 *
 * For more information, see:
 * - http://developer.chrome.com/apps/api_index.html
 * - https://github.com/dart-gde/chrome.dart
 */
void main() {
  querySelector("#input").onInput.listen(updateN);
}

void updateN(Event e) {
  int n = (e.target as InputElement).valueAsNumber;
//  if (n.toDouble().isNaN) {
//    outputText = querySelector("#output");
//    outputText.text = "Enter a value n";
//  }
//  else {
  fib(n.toDouble());
//  }
}

void fib(n) {
  outputText = querySelector("#output");
  outputText.text = n.toString();
}
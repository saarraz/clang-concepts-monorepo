// RUN: %clang_cc1 -x c++ -std=c++2a -fconcepts-ts -emit-pch %S/cxx-concepts-deduction-guides-crash.h -o %t
// RUN: %clang_cc1 -x c++ -std=c++2a -fconcepts-ts -include-pch %t -verify %s
// Just don't crash.
// expected-no-diagnostics

auto VarA2 = AAA('5');
auto VarB2 = BBB('5');
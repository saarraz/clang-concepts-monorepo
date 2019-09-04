// RUN: %clang_cc1 -std=c++1z -verify %s

template<typename T> struct A { constexpr A(int = 0) {} void foo() { }};
A() -> A<int>;
A(int) -> A<char>;

static constexpr inline const volatile A a = {}; // ok, specifiers are permitted
A b;
A c [[]] {};

A d = {}, e = {};
A f(0), g{}; // expected-error {{template arguments deduced as 'A<char>' in declaration of 'f' and deduced as 'A<int>' in declaration of 'g'}}
template<typename T>
void bar(T t) {
  A(t).foo();
}

struct B {
  static A a; // expected-error {{requires an initializer}}
};
extern A x; // expected-error {{requires an initializer}}
static A y;


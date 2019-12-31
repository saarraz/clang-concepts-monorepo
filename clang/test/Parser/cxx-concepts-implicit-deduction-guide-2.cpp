// RUN: %clang_cc1 -std=c++2a -fconcepts-ts -x c++ %s -verify

// expected-no-diagnostics

template <typename T1, typename T2>
struct is_same {
  static constexpr bool Value = false;
};

template <typename T>
struct is_same<T, T> {
  static constexpr bool Value = true;
};

template <typename T>
concept IsInt = is_same<T, int>::Value;

template <typename T>
concept IsChar = is_same<T, char>::Value;

template <typename T = double>
struct AAA {
  template <typename T2>
  AAA(T2 P1) requires IsInt<T2> {}

  template <typename T2>
  AAA(T2 P1) requires IsChar<T2> {}
};

auto Var = AAA(5);

// RUN: %clang_cc1 -std=c++2a -fconcepts-ts -x c++ %s -verify

static_assert(requires { requires true; });

template<typename T> requires requires { requires false; } // expected-note{{because 'false' evaluated to false}}
struct r1 {};

using r1i = r1<int>; // expected-error{{constraints not satisfied for class template 'r1' [with T = int]}}

template<typename T> requires requires { requires sizeof(T) == 0; } // expected-note{{because 'sizeof(int) == 0' (4 == 0) evaluated to false}}
struct r2 {};

using r2i = r2<int>; // expected-error{{constraints not satisfied for class template 'r2' [with T = int]}}

template<typename T> requires requires (T t) { requires sizeof(t) == 0; } // expected-note{{because 'sizeof (t) == 0' (4 == 0) evaluated to false}}
struct r3 {};

using r3i = r3<int>; // expected-error{{constraints not satisfied for class template 'r3' [with T = int]}}

template<typename T>
struct X {
    template<typename U> requires requires (U u) { requires decltype(u)::hey == sizeof(T); }
    // expected-note@-1{{because substituted constraint expression is ill-formed: type 'decltype(u)' (aka 'int') cannot be used prior to '::' because it has no members}}
    struct r4 {};
};

using r4i = X<int>::r4<int>; // expected-error{{constraints not satisfied for class template 'r4' [with U = int]}}

// C++ [expr.prim.req.nested] Examples
namespace std_example {
  template<typename U> concept C1 = sizeof(U) == 1; // expected-note{{because 'sizeof(int) == 1' (4 == 1) evaluated to false}}
  template<typename T> concept D =
    requires (T t) {
      requires C1<decltype (+t)>; // expected-note{{because 'decltype(+t)' (aka 'int') does not satisfy 'C1'}}
  };

  struct T1 { char operator+() { return 'a'; } };
  static_assert(D<T1>);
  template<D T> struct D_check {}; // expected-note{{because 'short' does not satisfy 'D'}}
  using dc1 = D_check<short>; // expected-error{{constraints not satisfied for class template 'D_check' [with T = short]}}

  template<typename T>
  concept C2 = requires (T a) { // expected-note{{'a' declared here}}
      requires sizeof(a) == 4; // OK
      requires a == 0; // expected-error{{constraint variable 'a' cannot be used in an evaluated context}}
    };
}

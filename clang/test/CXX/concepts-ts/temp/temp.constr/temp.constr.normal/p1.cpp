// RUN: %clang_cc1 -std=c++2a -fconcepts-ts -x c++ -verify %s

namespace maybe_incorrect_args {
  template<typename U, typename T>
  concept C = true;

  // diagnostic expected here - the C<Ts...>s are treated as atomic, and since
  // they originate from different source-level constructs, they do not subsume
  // each other.
  template<typename... Ts> requires C<Ts...>
  // expected-note@-1{{'C<Ts...>' in the two declarations is not considered equivalent - move it to a concept and reference it from here:}}
  struct A {}; // expected-note{{template is declared here}}

  template<typename... Ts> requires C<Ts...> && true
  // expected-note@-1{{and here}}
  struct A<Ts...> {}; // expected-error{{class template partial specialization is not more specialized than the primary template}}
}

namespace ill_formed_subst {

  template<typename T>
  struct B; // expected-note 2{{template is declared here}}

  template<typename T>
  concept C1 = true;

  template<typename T, typename U>
  concept C2 = C1<typename B<U>::foo>;
  // expected-error@-1 2{{implicit instantiation of undefined template 'ill_formed_subst::B<int>'}}

  template<typename T> requires C2<T, int>
  // expected-note@-1{{while substituting into constraints of 'C2' [with T = type-parameter-0-0, U = int] here}}
  struct A {};

  template<typename T> requires C2<T, int> && true
  // expected-note@-1{{while substituting into constraints of 'C2' [with T = type-parameter-0-0, U = int] here}}
  struct A<T> {}; // expected-error {{class template partial specialization is not more specialized than the primary template}}
}

namespace incorrect_args_after_subst {
  template<typename T>
  concept C1 = true; // expected-note 2{{template is declared here}}

  template<typename... Ts>
  concept C2 = C1<Ts...>;
  // expected-error@-1 2{{too many template arguments for concept 'C1'}}

  template<typename T> requires C2<T, T>
  // expected-note@-1 {{while substituting into constraints of 'C2' [with Ts = <type-parameter-0-0, type-parameter-0-0>] here}}
  struct A {};

  template<typename T> requires C2<T, T> && true
  // expected-note@-1 {{while substituting into constraints of 'C2' [with Ts = <type-parameter-0-0, type-parameter-0-0>] here}}
  struct A<T> {}; // expected-error{{class template partial specialization is not more specialized than the primary template}}
}

namespace maybe_incorrect_args_after_subst {
  template<typename T, typename U>
  concept C1 = true;

  template<typename... Us>
  concept C2 = C1<Us...>;

  // no diagnostic expected here - C1<Us...> is treated as atomic, and since it
  // originates at the same source level construct, the specialized subsumes the
  // primary.
  template<typename... Ts> requires C2<Ts...>
  struct A {};

  template<typename... Ts> requires C2<Ts...> && true
  struct A<Ts...> {};
}

namespace incorrect_subst {
  template<typename T>
  concept C = true;

  template<typename U>
  struct S {
    template<typename T> requires C<typename U::x>
    // expected-error@-1{{type 'int' cannot be used prior to '::' because it has no members}}
    struct A { };
    // expected-note@-1{{while substituting into constraints of 'A' [with T = int] here}}

    template<typename T> requires C<typename U::x> && true
    // expected-error@-1{{type 'int' cannot be used prior to '::' because it has no members}}
    struct A<T> { };
    // expected-error@-1{{class template partial specialization is not more specialized than the primary template}}
    // expected-note@-2{{while substituting into constraints of 'A<T>' [with T = int] here}}
  };

  S<int> s;
  // expected-note@-1 3{{in instantiation of template class 'incorrect_subst::S<int>' requested here}}
}

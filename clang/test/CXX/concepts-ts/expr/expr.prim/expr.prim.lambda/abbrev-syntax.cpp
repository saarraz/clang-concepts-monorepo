// RUN: %clang_cc1 -std=c++2a -fconcepts-ts -x c++ -verify %s

template<typename>
concept One = true;
// expected-note@-1 {{template is declared here}}

template<typename, typename>
concept Two = true;
// expected-note@-1 3{{template is declared here}}

auto acc_1 = [](One auto) { return 0; };
auto rej_1 = [](One<int> auto) { return 0; };
// expected-error@-1 {{too many template arguments for concept}}
auto rej_2_1 = [](Two auto) { return 0; };
// expected-error@-1 {{too few template arguments for concept}}
auto acc_2_1 = [](Two<int> auto) { return 0; };
auto rej_2_2 = [](Two<int, int> auto) { return 0; };
// expected-error@-1 {{too many template arguments for concept}}
auto rej_2_3 = [](Two<int, int, int> auto) { return 0; };
// expected-error@-1 {{too many template arguments for concept}}

template<typename T, unsigned Size>
concept LargerThan = sizeof(T) > Size;
// expected-note@-1 3{{because 'sizeof(char) > 1U' (1 > 1) evaluated to false}}

template<typename T>
concept Large = LargerThan<T, 1>;
// expected-note@-1 3{{because 'LargerThan<char, 1>' evaluated to false}}

auto l1 = [](Large auto l) { return l; };
// expected-note@-1{{candidate template ignored: constraints not satisfied [with l:auto = char]}}
// expected-note@-2{{because 'char' does not satisfy 'Large'}}
auto l1t1 = l1('a');
// expected-error@-1{{no matching function for call to object of type '(lambda at}}
auto l1t2 = l1(1);

auto l2 = [](Large auto ...ls) { return 0; };
// expected-note@-1{{candidate template ignored: constraints not satisfied [with ls:auto = <char>]}}
// expected-note@-2{{candidate template ignored: constraints not satisfied [with ls:auto = <int, char>]}}
// expected-note@-3 2{{because 'char' does not satisfy 'Large'}}
auto l2t1 = l2('a');
// expected-error@-1{{no matching function for call to object of type '(lambda at}}
auto l2t2 = l2(1, 'a');
// expected-error@-1{{no matching function for call to object of type '(lambda at}}
auto l2t3 = l2(1, 2);


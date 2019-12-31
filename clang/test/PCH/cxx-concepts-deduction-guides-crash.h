// Header for PCH test cxx-concepts-deduction-guides-crash.cpp

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
struct AAA;

template <typename T>
struct AAA {
  template <typename T2>
  AAA(T2 P1) requires IsInt<T2> {}

  template <typename T2>
  AAA(T2 P1) requires IsChar<T2> {}
};

template <typename T = double>
struct BBB;

template <typename T>
struct BBB {
  template <typename T2> requires IsInt<T2>
  BBB(T2 P1) {}

  template <typename T2> requires IsChar<T2>
  BBB(T2 P1) {}
};

// this forces creation of implicit deduction guides
auto VarA = AAA(5);
auto VarB = BBB(5);

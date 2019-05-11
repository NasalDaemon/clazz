# clazz
First clazz structural typing in C++.

C++ is a continually evolving language, but one thing hasn't changed: the type system is nominal. Nominal typing is a subset of structural typing, where the structure of two objects is assumed to be identical only if they share the same type name. That's just the compiler being lazy! However, with structural typing, structures are compared irrespective of the name you give to a type. You can get the compiler to do your work, so there's more time for coffee breaks.

There are some features in C++ which are the building blocks of structural programming: tuples, structured bindings, template and concepts. This library has the aim of exploiting these as much as possible to allow you to express yourself in a structural manner without getting bogged down with pesky names. I can recognise that famous actor's facial structure any day, but ask me what his name is and I'll have to ask my wife.

> In vanilla C++, even before a class has a body defined, it is assigned a unique name. Relationships between similar classes can only be expressed nominally via explicit inheritance or composition. Does your class draw? You better make sure you inherit from IDrawable, and don't forget IDrawToo, IDrawableReally, and IDrawableReallyIMeanIt! If it doesn't then it doesn't draw. Not even stick-men.
>
> Ah, but we can avoid this with templates you say. Just create a function template and hope it compiles! With concepts the compiler error messages will be intelligible. Yeah, just make sure you define a bunch of uniquely and carefully named one-time-use concepts before hand, or write out a detailed requires expression on the function signature every time. We all know how pretty and concise requires clauses are.
>
> But really -- I don't care what the class is called, and I don't care what the concepts are called, I just have something that looks like this: struct { void draw(); }, and I want to call something that takes it, and I want to know straight away, just by looking at the signature if I can -- don't waste my time.
>
> Why stop there? I want reflection of the names, types and signatures of the members of my classes, and I want my classes to be able to interact with each other based on knowledge of their respective structures. I want a hash function for all types, comparison, interop.
>
> Why do I have to specifically define serialiser methods or ORMs every time I create a new class? The compiler knows everything it needs to, why do I have to type so much? It's the compilers job to type.

The future is here. Get out of economy and into first clazz. You still have to do some work, but you get a bunch of stuff for free, like universally objective strict total ordering between all conceivable clazzes. And nuples.

# Greatest hits
1. [Inline class definitions](#greatest-hits-inline-definitions)
2. [Structural comparison of clazzes by field names, objective strict total ordering for all conceivable clazzes](#greatest-hits-comparison)
3. [Unordered designated initialisation](#greatest-hits-desi-ini)
4. [Named tuples, interop with existing std::tuple, structured bindings](#greatest-hits-nuple)
5. [Interop with existing vanilla classes, structural extraction of fields by name](#greatest-hits-interop)
6. [Runtime polymorphism without vtables or std::visit (runtime polymorphism which can be inlined)](#greatest-hits-rt-poly)

### 1. Inline class definitions<a name="greatest-hits-inline-definitions"></a>
```c++
clazz<var first_name<std::string>, var last_name<std::string>> name() {
  return {"John", "Smith"};
}

auto n1 = name();
auto n2 = clazz{set last_name = "Smith"s, set first_name = "John"s};

cout << n1.first_name << " " << n1.last_name << '\n'; // John Smith
cout << n2.first_name << " " << n2.last_name << '\n'; // John Smith
```
### 2. Structural comparison of clazzes by field names, objective strict total ordering for all conceivable clazzes<a name="greatest-hits-comparison"></a>
```c++
using A   = clazz<var a<int>>;
using C   = clazz<var c<int>>;
using BA  = clazz<var b<int>, var a<int>>;
using AC  = clazz<var a<int>, var c<int>>;
using ABC = clazz<var a<int>, var b<int>, var c<int>>;
using CBA = clazz<var c<int>, var b<int>, var a<int>>;

// Strict equality tests, only clazzes with exactly the same fields can be equal
// if the respective values of the fields of the same name are equal
assert(BA{1, 2} == BA{1, 2}); // 1 == 1, 2 == 2
assert(ABC{1, 2, 3} == CBA{3, 2, 1}); // 1 == 1, 2 == 2, 3 == 3

// Strict lexicographical inequality tests. Order of comparison is defined by the
// field order of the clazz with fewer fields or fields in a lower lexicographical order

// compare in order b, a
assert(BA{1, 3} < BA{2, 0}); // 1 < 2. 3 > 0 is disregarded in lexicographical comparison
// compare values in order b, a as BA has fewer fields than ABC, c is not compared
assert(BA{1, 2} < ABC{2, 1, 3}); // Values of shared fields a, b are equal, but BA has fewer fields than ABC
// compare values in order b, a as BA has fewer fields than ABC, c is not compared
assert(ABC{2, 1, 3} > BA{1, 2}); // Values of shared fields a, b are equal, but ABC has more fields than BA
// compare values in order a, b, c as {a,b,c} is lower lexicographical order to {c,b,a}
assert(ABC{1, 2, 3} < CBA{1, 3, 2}); // 1 < 2. 2 < 3, 3 > 1 are disregarded
// compare values in order a, b, c as {a,b,c} is lower lexicographical order to {c,b,a}
assert(CBA{1, 3, 2} > ABC{1, 2, 3}); // 2 > 1. 3 > 2, 1 < 3 are disregarded 
// compare only value of a, as it is the only shared field, values of b and c are not compared
assert(BA{99, 1} < AC{0, 2}); // Value of shared field a is equal, both have same number of fields, but mutually exclusive field names b < c
// no comparison of field values, as none are shared
assert(A{1} < C{1}); // No shared fields, but field names a < c
```
### 3. Unordered designated initialisation<a name="greatest-hits-desi-ini"></a>
```c++
// Using deduction guides
auto amount1 = clazz{set amount = 1, set currency = "GBP"sv}; // {amount: 1, currency: "GBP"}
auto amount2 = clazz{set currency = "GBP"sv, set amount = 1}; // {currency: "GBP", amount: 1}

// Named clazz with default values
using amount_t = clazz <
  var amount<int, 1>, // Default amount is 1
  var currency<std::string, []{ return "GBP"; }> // Default currency is GBP
>;

auto amount3 = amount_t(); // {amount: 1, currency: "GBP"}
auto amount4 = amount_t(set amount = 1); // {amount: 1, currency: "GBP"}
auto amount5 = amount_t(set currency = "GBP"); // {amount: 1, currency: "GBP"}
// Reverse order works too!
auto amount6 = amount_t(set currency = "GBP", set amount = 1); // {amount: 1, currency: "GBP"}
auto amount7 = amount_t(1, "GBP"); // {amount: 1, currency: "GBP"}

auto get_amount(amount_t amount) {
  return amount;
}

auto amountGBP = get_amount({}); // {amount: 1, currency: "GBP"}
// Set currency with multiple parameter constructor of std::string
auto amountYYY = get_amount({set currency(3, 'Y')}); // {amount: 1, currency: "YYY"} type of currency inferred as string
// Set currency of inline unnamed clazz with multiple parameter constructor of std::string
auto currencyHHH = clazz{set currency.as<std::string>(3, 'H')}; // {currency: "HHH"} must explicitly name type to construct
auto amountHHH = get_amount(currencyHHH); // {currency: "HHH"} converted to {amount: 1, currency: "HHH"}

// Amounts 1-7 are structurally equal, even though field order and types do not match
assert(amount1 == amount2 && amount1 == amount3 && amount1 == amount4);
assert(amount1 == amount5 && amount1 == amount6 && amount1 == amount7);

// amountHHH is different
assert(amount1 != amountHHH && amountGBP != amountHHH); // 1 GBP != 1 HHH
// amountYYY is different
assert(amountHHH != ammountYYY);

```
### 4. Named tuples (aka indexed clazzes), interop with existing std::tuple, structured bindings<a name="greatest-hits-nuple"></a>
```c++
// Fields are named _i where i is 1, ..., 22
auto n1 = nuple{2, 5, 8}; // nuple<int, int> {_1: 2, _2: 5, _3: 8}

// Conversions from tuple available:
auto t = std::tuple{1, 2, 3}; // tuple<int, int> 
auto n2 = nuple(t); // nuple<int, int> {_1: 1, _2: 2, _3: 3}
auto& n3 = as_named_tuple<nuple>(tuple); // nuple<int, int>& via reinterpret_cast {_1: 1, _2: 2, _3: 3}
view_t<decltype(n2)> n4 = t // nuple<int&, int&> {_1: &1, _2: &2, _3: &3}

assert(n1 == n2 && n1 == n2 && n1 == n3 && n1 == n4); // All structurally equal

// All named tuples can be viewed as anonymous tuples with nuple->tuple
assert(t == n1->tuple && t == n2->tuple && t == n3->tuple); // All nuple-tuples equal to the original tuple, t

// Structured bindings via proxy tuple
auto [a1, b1, c1] = n1->tuple; // aka: auto [a1, b1, c1] = union_cast<std::tuple<int, int, int>&>(n1);
assert(a1 == n1._1 && b1 == n1._2 && c1 == n1._3);

// Direct structured bindings of nuple without any casting involved at any stage
auto [a2, b2, c2] = n1;
assert(a2 == n1._1 && b2 == n1._2 && c2 == n1._3);
```
### 5. Interop with existing vanilla classes, structural extraction of fields by name<a name="greatest-hits-interop"></a>
```c++
struct {
    int a = 0, b = 1;
} anon_ab{};

struct {
    int a = 0, b = 1, c = 2;
} anon_abc{};

using ABC = clazz<var a<int, 7>, var b<int, 8>, var c<int, 9>>;

auto get_abc(ABC abc) { return abc; }

auto abc1 = make_clazz<tag a, tag b, tag c>(anon_abc); // {a: 0, b: 1, c: 2}
auto abc2 = ABC{anon_abc}; // {a: 0, b: 1, c: 2}
auto abc3 = get_abc{anon_ab}; // {a: 0, b: 1, c: 9} c is missing from anon_ab, but default value of c in ABC is 9
```
### 6. Runtime polymorphism without vtables or std::visit (runtime polymorphism which can be inlined)<a name="greatest-hits-rt-poly"></a>
```c++
using printable = trait<dec print<void() const>>;

using position = clazz <
    var x <int>, 
    var y <int>, 
    var z <int>, 
    def print <void() const, [](auto& self) { 
        cout << '(' << self.x << ',' << self.y << ',' << self.z << ")\n"; 
    }>
>;

using person = clazz <
    var name <std::string>,
    var age <int>,
    def print <void() const, [](auto& self) {
        cout << self.name << " aged " << self.age << '\n';
    }>
>;

// Packed vector of elements of printable trait, restricted to types position, and person
auto vvec = vvector<printable, position, person>();
//                  ^ trait    ^ option1 ^ option2 ...
vvec.emplace_back<position>(0, 0, 0); // Add {x: 0, y: 0, z: 0}
vvec.emplace_back<position>(3, 2, 1); // Add {x: 3, y: 2, z: 1}
vvec.emplace_back<person>("John Smith", 21); // Add {name: "John Smith", age: 21}

// vvector uses minimal space required by packing the clazzes together
static_assert(sizeof(position) == 12 && sizeof(person) == 40);
assert(vvec.data_size() == 2 * sizeof(position) + sizeof(person) + 3);
// Uses 67 bytes instead of 123, which would have been the case with a plain vector of variants

for (const auto& element : vvec)
    element.print(); // Call print() directly on element, which is declared in the printable trait

// Prints:
// (0,0,0)
// (3,2,1)
// John Smith aged 21
```

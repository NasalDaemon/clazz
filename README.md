# clazz
First clazz structural typing in C++2a.

C++ is a continually evolving language, but one thing hasn't changed: the type system is nominal. Nominal typing is a subset of structural typing, where the structure of two objects is assumed to be identical only if they share the same type name. That's just the compiler being lazy! However, with structural typing, structures are compared irrespective of the name you give to a type. You can get the compiler to do your work, so there's more time for coffee breaks.

There are some features in C++ which are the building blocks of structural programming: tuples, structured bindings, templates and concepts. This library has the aim of exploiting these as much as possible to allow you to express yourself in a structural manner without getting bogged down with pesky names. I can recognise that famous actor's facial structure any day, but ask me what their name is and I couldn't tell you -- I'll have to ask my compiler.

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

# Supported compilers
At the moment, only GCC trunk with "-std=c++2a". More compilers will be compatible once they catch up to the C++2a standard, which still needs to be finalised.

# Getting started

For each symbol name you would like to use in a clazz, you must create a header file in your include directory, called "clazz/symbols/{symbol_name}.h", which contains the following code:
```c++
#pragma once
#include <clazz.h>
DECLARE_STRUPLE_SYMBOL(symbol_name);
```

In any file which uses a clazz symbol must include such a file, as follows:
```c++
#include <clazz/symbols/a.h>
#include <clazz/symbols/b.h>
#include <clazz/symbols/c.h>

using ABC = clz::clazz <
    clz::var::a <int>,
    clz::var::b <int>,
    clz::var::c <int>
>;
```

If you find the fully qualified namespaces ugly and distracting, and want to use the more readable keywords {clazz, nuple, var, val, tag, mut, arg, args, dec, def, fun, ovl, tpe}, you can use the following technique to temporarily import keyword macros, which replace ```::clz::keyword::``` with ```keyword```. This greatly reduces smelly colon-induced noise (nobody likes a party-pooper):

```c++
// Somewhere at the top of your file
#include <clazz/keywords_begin.h>

using ABC = clazz <
    var a <int>,
    var b <int>,
    var c <int>
>;

// Somewhere at the bottom of your file (to undefine the keyword macros if you are using this technique in a header file)
#include <clazz/keywords_end.h>
```

# To do
- [ ] Install a proper IDE, and stop just using Godbolt. The lack of project structure is getting unwieldy.
- [x] Hash for any clazz (waiting on GCC to add more constexpr support approved for C++2a)
- [x] Binary de/serialisation 
- [ ] Check serialisation is working for all common types
- [ ] Use modules for symbols instead of headers to reduce compile-time cost of symbols
- [ ] Use a friendlier license when this becomes usable as a library

# Greatest hits
1. [clz::clazz<>: Inline class definitions](#greatest-hits-inline-definitions)
2. [Structural comparison of clazzes by field names in objective strict total ordering](#greatest-hits-comparison)
3. [Unordered designated initialisation](#greatest-hits-desi-ini)
4. [Interop with existing vanilla classes, structural extraction of fields by name](#greatest-hits-interop)
5. [clz::nuple<>: Named tuples, interop with existing std::tuple, structured bindings](#greatest-hits-nuple)
6. [clz::sort_asc<>: Sort data members by size to minimise padding](#greatest-hits-sort-mem)
7. [def: Powerful member function overloading](#greatest-hits-def)
8. [clz::hvector<>: Heterogeneous vector of clazzes implementing a trait](#greatest-hits-hvector)
9. [clz::cvector<>: First clazz SoA - column-wise vector that is as easy to use as a vector of structs](#greatest-hits-cvector)
10. [clz::hash(): Constexpr hash for any clazz](#greatest-hits-hash)
11. [clz::ser(): Free binary de/serialisation available for all clazzes](#greatest-hits-serialisation)

### 1. <a name="greatest-hits-inline-definitions"></a>clz::clazz<>: Inline class definitions
Use a clazz directly, without having defined a name for it.
```c++
clazz<var first_name<std::string>, var last_name<std::string>> name() {
    return {"John", "Smith"};
}

auto n1 = name();
auto n2 = clazz{arg last_name = "Smith"s, arg first_name = "John"s};

cout << n1.first_name << " " << n1.last_name << '\n'; // John Smith
cout << n2.first_name << " " << n2.last_name << '\n'; // John Smith

// Specify acceptable interface of clazz parameter inline
void print1(const clz::Clazz<dec first_name<std::string>, dec last_name<std::string>> auto& person) {
    cout << n1.last_name ", " << n1.first_name << '\n';
}

print1(n1); // Smith, John
print1(n2); // Smith, John

// Or reference a named trait to implement
using person = clz::trait<dec first_name<std::string>, dec last_name<std::string>>;
void print2(const clz::Clazz<person> auto& person) {
    print1(person);
}

print2(n1); // Smith, John
print2(n2); // Smith, John

```
### 2. <a name="greatest-hits-comparison"></a>Structural comparison of clazzes by field names in objective strict total ordering
All clazzes which share field names, where those fields have comparable types, are comparable in objective strict total order.
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
// field order of the clazz with fewer fields or field names in a lower lexicographical order

// compare values in order b, a
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
assert(BA{99, 1} < AC{1, 2}); // Value of shared field a is equal, both have same number of fields, but mutually exclusive field names b < c
// no comparison of field values, as none are shared
assert(A{1} < C{1}); // No shared fields, but field names a < c
```
### 3. <a name="greatest-hits-desi-ini"></a>Unordered designated initialisation
Clazzes can be constructed by their field names in any order.
```c++
// Using deduction guides
auto amount1 = clazz{arg amount = 1, arg currency = "GBP"sv}; // {amount: 1, currency: "GBP"}
auto amount2 = clazz{arg currency = "GBP"sv, arg amount = 1}; // {currency: "GBP", amount: 1}

// Named clazz with default values
using amount_t = clazz <
    var amount   <int, 1>, // Default amount is 1
    var currency <std::string, []{ return "GBP"; }> // Default currency is GBP
>;

auto amount3 = amount_t(); // {amount: 1, currency: "GBP"}
auto amount4 = amount_t(arg amount = 1); // {amount: 1, currency: "GBP"}
auto amount5 = amount_t(arg currency = "GBP"); // {amount: 1, currency: "GBP"}
// Reverse order works too!
auto amount6 = amount_t(arg currency = "GBP", arg amount = 1); // {amount: 1, currency: "GBP"}
auto amount7 = amount_t(1, "GBP"); // {amount: 1, currency: "GBP"}

auto get_amount(amount_t amount) {
    return amount;
}

auto amountGBP = get_amount({}); // {amount: 1, currency: "GBP"}
// Set currency with multiple parameter constructor of std::string
auto amountYYY = get_amount({arg currency(3, 'Y')}); // {amount: 1, currency: "YYY"} type of currency inferred as string
// Set currency of inline unnamed clazz with multiple parameter constructor of std::string
auto currencyHHH = clazz{arg currency.as<std::string>(3, 'H')}; // {currency: "HHH"} must explicitly name type to construct
auto amountHHH = get_amount(currencyHHH); // {currency: "HHH"} converted to {amount: 1, currency: "HHH"}

// Amounts 1-7 are structurally equal, even though field order and types do not match
assert(amount1 == amount2 && amount1 == amount3 && amount1 == amount4);
assert(amount1 == amount5 && amount1 == amount6 && amount1 == amount7);

// amountHHH is different
assert(amount1 != amountHHH && amountGBP != amountHHH); // 1 GBP != 1 HHH
// amountYYY is different
assert(amountHHH != ammountYYY);

// Setting multiple fields to the same value is also easy
using ABC = clazz<var a<int>, var b<int>, var c<int>>;
auto abc1 = ABC{args<tag a, tag b, tag c> = 42};

// Unfortunately, deduction guides are not flexible enough to avoid a make_ helper when setting multiple arguments at once
auto abc2 = clz::make_clazz(args<tag a, tag b, tag c> = 42);

// a, b, and c have all been set to 42
assert(abc1 == ABC{42, 42, 42} && abc1 == abc2);

```
### 4. <a name="greatest-hits-interop"></a>Interop with existing vanilla classes, structural extraction of fields by name
Existing structs can be converted into clazzes with specified field names. The types will be inferred from the supplied struct.
```c++
struct {
    int a = 0, b = 1;
} anon_ab{};

struct {
    int a = 0, b = 1, c = 2;
} anon_abc{};

using ABC = clazz<var a<int, 7>, var b<int, 8>, var c<int, 9>>;

auto get_abc(ABC abc) { return abc; }

auto abc1 = clz::make_clazz<tag a, tag b, tag c>(anon_abc); // {a: 0, b: 1, c: 2}
auto abc2 = ABC{anon_abc}; // {a: 0, b: 1, c: 2}
auto abc3 = get_abc(anon_ab); // {a: 0, b: 1, c: 9} c is missing from anon_ab, but default value of c in ABC is 9
```
### 5. <a name="greatest-hits-nuple"></a>clz::nuple<>: Named tuples (aka indexed clazzes), interop with existing std::tuple, structured bindings
clz::nuple is a clazz with the same semantics and syntax as std::tuple, where the fields are accessible by their indexed name - like a generic std::pair - rather than via the convoluted std::get<> function.
```c++
// Fields are named _i where i is 1, ..., 22
auto n1 = nuple{2, 5, 8}; // nuple<int, int, int> {_1: 2, _2: 5, _3: 8}
assert(n1._1 == 2 && n1._2 == 5 && n1._3 == 8);

// Conversions from tuple available:
auto t = std::tuple{1, 2, 3}; // tuple<int, int, int> 
auto n2 = nuple(t); // nuple<int, int, int> {_1: 1, _2: 2, _3: 3}
auto& n3 = clz::as_named_tuple<nuple>(tuple); // nuple<int, int, int>& via reinterpret_cast {_1: 1, _2: 2, _3: 3}
auto& n4 = clz::as_nuple(t); // nuple<int, int, int>& via reinterpret_cast {_1: 1, _2: 2, _3: 3}
clz::view_t<decltype(n2)> n5 = t // nuple<int&, int&, int&> {_1: &1, _2: &2, _3: &3}

assert(n1 == n2 && n1 == n2 && n1 == n3 && n1 == n4 && n1 == n5); // All structurally equal

// All named tuples can be viewed as anonymous tuples with nuple->tuple
assert(t == n1->tuple && t == n2->tuple && t == n3->tuple 
    && t == n4->tuple && t == n5->tuple); // All nuple-tuples equal to the original tuple, t

// Structured bindings via proxy tuple
auto [a1, b1, c1] = n1->tuple; // aka: auto [a1, b1, c1] = union_cast<std::tuple<int, int, int>&>(n1);
assert(a1 == n1._1 && b1 == n1._2 && c1 == n1._3);

// Direct structured bindings of nuple without any casting involved at any stage
auto [a2, b2, c2] = n1;
assert(a2 == n1._1 && b2 == n1._2 && c2 == n1._3);
```
### 6. <a name="greatest-hits-sort-mem"></a>clz::sort_asc<>: Sort data members by size to minimise padding
Data members in normal structs are placed in memory in declaration order. This can result in wasted space due to padding between data members. It is possible to manually reorder the declaration of these members in structs, but it is brittle to future code changes, which could silently reduce space efficiency, and makes the struct declaration less readable.

With clazzes, you can declare the members in logical order, but have them laid out in memory in ascending or descending order of size, resulting in minimal padding.

To order the data members in memory in:
* order of increasing size stable with declaration order, use ```clazz_asc``` or ```sort_asc``` (which minimises padding)
* order of decreasing size stable with declaration order, use ```clazz_desc``` or ```sort_desc``` (which minimises padding)
* order of declaration, use ```clazz_decl``` (like with a vanilla struct)
* std::tuple memory layout order, use ```clazz``` (for easy and unsurprising std::tuple interop)
```c++
// Manually order fields to maximise space lost due to padding
using bad_padding = clz::clazz_decl <
    var s1 <std::string>, // 32
    var c1 <char>,        // 1
    // clz::padding<7>,   // 7 (wasted)
    var s2 <std::string>, // 32
    var c2 <char>,        // 1
    // clz::padding<7>,   // 7 (wasted)
    var s3 <std::string>, // 32
    var c3 <char>,        // 1
    // clz::padding<7>,   // 7 (wasted)
    var s4 <std::string>, // 32
    var c4 <char>         // 1
    // clz::padding<7>    // 7 (wasted)
>;
// 4*7 = 28 bytes wasted due to padding
static_assert(sizeof(bad_padding) == 160);

// Manually order fields to minimise space lost due to padding
using good_padding = clz::clazz_decl <
    var c1 <char>,        // 1
    var c2 <char>,        // 1
    var c3 <char>,        // 1
    var c4 <char>,        // 1
    // clz::padding<4>,   // 4 (wasted)
    var s1 <std::string>, // 32
    var s2 <std::string>, // 32
    var s3 <std::string>, // 32
    var s4 <std::string>  // 32
>;
// Only 4 bytes wasted on padding
static_assert(sizeof(good_padding) == 136);

// Using sort_asc on bad_padding results in good_padding
static_assert(std::is_same_v<clz::sort_asc<bad_padding>, good_padding>>);

// Sorting in descending order also results in efficient packing
static_assert(sizeof(clz::sort_asc<bad_padding>) == sizeof(clz::sort_desc<bad_padding>));
```
### 7. <a name="greatest-hits-def"></a>def: Powerful member function overloading
Rather than having to specify multiple overloads with identical names and signatures for different calling contexts, the context of the call can be inferred from first parameter of each clazz def.

This makes it much easier to program generically, since you can open up your function to multiple call scenarios, while still inferring under which context the call was made. You can modify the return type based on the calling context from within the same definition. How cool is that?

If you really want to restrict to the classic C++ calling contexts, these would be the clazz def equivalents:

|vanilla C++                        | clazz                                      |
|-----------------------------------|--------------------------------------------|
|```c++void run()          {...}``` | ```def run<[](auto&&       self) {...}>``` |
|```c++void run() const    {...}``` | ```def run<[](const auto&& self) {...}>``` |
|```c++void run() &        {...}``` | ```def run<[](mut   auto&  self) {...}>``` |
|```c++void run() const &  {...}``` | ```def run<[](const auto&  self) {...}>``` |
|```c++void run() &&       {...}``` | ```def run<[](rmut  auto&& self) {...}>``` |
|```c++void run() const && {...}``` | ```def run<[](const auto&& self) {...}>``` |

But of course, the standard function definitions are limited, and you end up repeating yourself when you want to specify a mix of the above. Here is an example of the power of clazz defs:
```c++
+--------------------------------------------+--------------------------------------------------------+
| vanilla C++ function definitions           | clazz defs                                             |
+--------------------------------------------+--------------------------------------------------------+
| struct PersonBuilder {                     | using PersonBuilder = clazz <                          |
|     std::string        name;               |     var name <std::string>,                            |
|     std::optional<int> age = std::nullopt; |     var age  <std::optional<int>, std::nullopt>,       |
|                                            |                                                        |
|     auto& with_age(int age_) & {           |     def with_age<[]<mut T>(T&& self, int age) -> T&& { |
|         age = age_;                        |         self.age = age;                                |
|         return *this;                      |         return std::forward<T>(self);                  |
|     }                                      |     }>,                                                |
|     auto&& with_age(int age_) && {         |                                                        |
|         age = age_;                        |     def get_age<[](auto& self) -> auto& {              |
|         return std::move(*this);           |         return self.age.value();                       |
|     }                                      |     }, [](rval auto&& self) -> auto&& {                |
|                                            |         return std::move(self.age).value();            |
|     const auto& get_age() const & {        |     }>,                                                |
|         return age.value();                | >;                                                     |
|     }                                      |                                                        |
|     auto& () & {                           |                                                        |
|         return age.value();                |                                                        |
|     }                                      |                                                        |
|     auto&& get_age() && {                  |                                                        |
|         return std::move(age).value();     |                                                        |
|     }                                      |                                                        |
|     const auto&& get_age() const && {      |                                                        |
|         return std::move(age).value();     |                                                        |
|     }                                      |                                                        |
| };                                         |                                                        |
+--------------------------------------------+--------------------------------------------------------+
```
### 8. <a name="greatest-hits-hvector"></a>clz::hvector<>: Heterogeneous vector of clazzes implementing a trait
Runtime polymorphism without vtables or std::visit (runtime polymorphism which can be inlined).
```c++
using printable = clz::trait<dec print<void() const>>;

using position = clazz <
    var x <int>, 
    var y <int>, 
    var z <int>, 
    def print <[](auto& self) { // Adheres to trait print declaration without specifically declaring type signature
        cout << '(' << self.x << ',' << self.y << ',' << self.z << ")\n"; 
    }>
>;

// clazz with data members sorted in ascending order of size to help packing (char) variant index in hvector
using person = clz::clazz_asc <
    var name  <std::string>,
    var age   <int>,
    def print <[](auto& self) { // Adheres to trait print declaration without specifically declaring type signature
        cout << self.name << " aged " << self.age << '\n';
    }>
>;

// Packed vector of elements of printable trait, restricted to types position, and person
auto hvec = clz::hvector<printable, position, person>();
//                       ^ trait    ^ option1 ^ option2 ...
hvec.emplace_back<position>(0, 0, 0); // Add {x: 0, y: 0, z: 0}
hvec.emplace_back<position>(3, 2, 1); // Add {x: 3, y: 2, z: 1}
hvec.emplace_back<person>(arg name = "John Smith", arg age = 21); // Add {name: "John Smith", age: 21}

// hvector uses minimal space required by packing the clazzes together
static_assert(sizeof(position) == 12 && sizeof(person) == 40);
assert(hvec.size_bytes() == 2 * 16 + 40); // 2 * {char,int,int,int} + {char,int,std::string}
// Uses 72 bytes vs 144, which is 3 * sizeof(std::variant<person,position>)

// Call print(), which is declared in the printable trait, directly on element without ugly visitor syntax
for (const auto& element : hvec)
    element.print();

// Prints:
// (0,0,0)
// (3,2,1)
// John Smith aged 21
```
### 9. <a name="greatest-hits-cvector"></a>clz::cvector<>: First clazz SoA (column-wise vector that is as easy to use as a vector of structs)
Create a "vector of clazzes", with the memory arranged column-wise contiguously in memory. 
* The beginning of each field array is aligned to a cache-line by default (64 bytes)
* Only one memory allocation per resize.
* Optimised sorting algorithm to avoid cache-thrashing versus the naive implementation.
* Optimised structural comparison with other cvectors
```c++
using person = clazz <
    var name  <std::string>,
    var age   <int>,
    def print <[](auto& self) {
        std::cout << self.name << " is " << self.age << " years old\n";
    }>
>;

// Default reserved memory is for 12 elements
auto persons = clz::cvector<person>(); // Similar memory structure to: clazz <
                                       //   var name <std::vector<std::string>>,
                                       //   var age  <std::vector<int>>
                                       // >;

// Add some elements to the SoA
persons.push_back("John Smith", 21); // Construct and move in place
persons.emplace_back("John Doe", 22); // Construct in place
persons.emplace_back(arg age = 23, arg name = "Joe Moe"); // Construct in place with designated initialiser

// Creates one block of memory in one memory allocation for use by all fields,
// with elements to be arranged in column-wise order in the memory block.
// Moves existing elements into the newly allocated memory column-wise and frees the old.
persons.reserve(20);

// Sort in ascending order of name then age.
// Sort algorithm is cache optimised for SoA by first finding a sorting order
// before moving elements into their final position in memory, field by field.
// In this particular case, the age column is only accessed when ages for
// the respective elements are moved into their final position, since the
// sorting order is found without ever needing to query the age column.
persons.sort();

for (auto person_view : persons) {
    std::cout << person_view.name << " is " << person_view.age << " years old\n";
    // Each element is a view of a person, with each field in the view being a reference to 
    // the respective element on each column in the SoA
    // Always iterate by value, as the compiler can optimise away the unused fields for temporary objects
    static_assert(std::is_same_v<view_t<person>, decltype(person_view)>);
}

// Prints:
// Joe Moe is 23 years old
// John Doe is 22 years old
// John Smith is 21 years old

// Sort in ascending order of age
// In this particular case, the name column is only accessed when names for
// the respective elements are moved into their final position, since the
// sorting order is found without ever needing to query the name column.
persons.sort([](auto left, auto right) {
    return left.age < right.age;
});

// Call defs on element type directly
for (auto person_view : persons)
    person_view.print();

// Prints:
// John Smith is 21 years old
// John Doe is 22 years old
// Joe Moe is 23 years old

// Can consume arbitrary (std) algorithms which move elements around based on the indices of the cvector.
// Ensures that elements of cvector are moved column-wise when the final element permutation is found.
// Note: persons->name[index] is equivalent to persons[index].name, although the former is a slightly more
// efficient way to get the value of name at index, as it calculates the offset for the name column only.
size_t offset = persons.permute_by_index([names = persons->name](size_t* begin, size_t* end) {
    return std::stable_partition(begin, end, [=](size_t index) {
        return names[index].ends_with("oe");
    });
});

assert(persons[0] == person{"John Doe", 22});
assert(persons[1] == person{"John Moe", 23});
assert(persons[offset] == person{"John Smith", 21});

// Efficient column-wise removal of elements with specialisation of std::erase_if
std::erase_if(persons, [&](size_t index) { return persons->age[index] > 21; });

// Equivalently, the predicate could be for persons::reference. 
// std::erase_if(persons, [](clz::Clazz auto person_view) { return person_view.age > 21; }); 

for (auto person_view : persons)
    person.print();

// Prints:
// John Smith is 21 years old
```
### 10. <a name="greatest-hits-hash"></a>clz::hash(): Constexpr hash for any clazz
The hash for a clazz hashes:
1. All field names in alphabetical order (constexpr)
2. All field type names in order of field name (constexpr)
3. All field values in order of field name (constexpr for trivial types and all types convertible to std::string_view)
```c++
// All field values affect hash
// {(int)a: 0} != {(int)a: 1}
static_assert(hash(clazz{arg a = 0}) != hash(clazz{arg a = 1}));

// All field types affect hash
// {(int)a: 0} != {(double)a: 0}
static_assert(hash(clazz{arg a = 0}) != hash(clazz{arg a = 0.0}));

// All field names affect hash
// {(int)a: 0} != {(int)b: 0}
static_assert(hash(clazz{arg a = 0}) != hash(clazz{arg b = 0}));

// Field order does not affect hash
// {(int)a: 0, (int)b: 0} == {(int)b: 0, (int)a: 0}
static_assert(hash(clazz{arg a = 0, arg b = 0}) == hash(clazz{arg b = 0, arg a = 0}));

// Every clazz has a clazz id, that hashes everything but the values of an instance
using AB = clazz<var a<int>, var b<double>>;
using AC = clazz<var a<int>, var c<double>>; 
using BA = clazz<var b<double>, var a<int>>;

static_assert(clazz_id<AB> != clazz_id<ABC>);

// Field order does not affect clazz_id
static_assert(clazz_id<AB> == clazz_id<BA>);
```
### 11. <a name="greatest-hits-serialisation"></a>clz::ser(): Free binary de/serialisation available for all clazzes
Any tree structured clazz can be serialised into a byte array and back without any boilerplate.

At the moment, only the current types are supported:
* Any primitive type (long, double, int, etc)
* std::string
* clazz with fields of any of the above
* std::array/std::vector of any of the above

```c++
auto stuff = clazz {
    arg _1 = 42, 
    arg _2 = 22.0/7.0, 
    arg _3 = clazz {
        arg x = "hey"s,
        arg y = std::vector{"how"s, "you"s, "doin"s},
        arg z = std::array {
            clazz {
                arg x = 1,
                arg y = 2,
                arg z = 1.0/2.0
            },
            clazz {
                arg x = 3,
                arg y = 4,
                arg z = 3.0/4.0
            }
        }
    }
};

using stuff_t = std::decay_t<decltype(stuff)>;

// Serialise stuff into a buffer
auto buffer = ser(stuff);

std::cout << to_string(stuff) << '\n'
          << "clazz size: " << sizeof(stuff) << '\n'
          << "buffer size: " << buffer.size() << '\n'
          << buffer << '\n';
          
// Prints:
// {_1: 42, _2: 3.14286, _3: {x: hey, y: [how, you, doin], z: [{x: 1, y: 2, z: 0.5}, {x: 3, y: 4, z: 0.75}]}}
// clazz size: 104
// buffer size: 94
// _1,_2,_3.F*I�$I�$	@x,y,z.,heyhowyoudoinx,y,z.�?�?

// Can regenerate the same data with the buffer, even though the requested type will have the field in a different order
auto stuff_copy = deser<sort_desc<stuff_t>>(buffer.c_str());
assert(stuff == stuff_copy);

// Construct a type that has a subset of the values, also in a different order
auto less_stuff = clazz { 
    arg _3 = clazz {
        arg z = std::array {
            clazz{arg z = 1.0/2.0}, 
            clazz{arg z = 3.0/4.0}
        }
    },
    arg _1 = 42
};

using sub_t = std::decay_t<decltype(less_stuff)>;

// Can construct any sub-type from the original serialised data
auto deser_sub_from_full = deser<sub_t>(buffer.c_str());
assert(less_stuff == deser_sub_from_full);
```

// GPLv3 @ github.com/MarkMitch/clazz

/* TODO: Report the many new GCC concepts bugs:
1. Cannot use local type alias in requires expressions (compiler crashes)
2. Cannot use template boolean variables in requires expressions (compiler crashes)
3. Template class specialisation does not observe concept subsumption rules (compiler erroneously complains)
4. Sometimes requires expression fails compile rather than return false when checking invocability of lambdas
*/

#include <algorithm>
#include <concepts>
#include <tuple>
#include <span>
#include <utility>
#include <type_traits>
#include <functional>
#include <string>
#include <iostream>
#include <memory>
#include <numeric>
#include <bit>
#include <bitset> // just for printing hashes
#include <cstring> // memcpy
#include <cassert> // assert

#ifdef _MSC_VER
#define EBO_MSVC __declspec(empty_bases)
#else
#define EBO_MSVC
#endif

// All currently supported tuple implementations store elements in memory in reverse order of declaration
#if 1
#define CLAZZ_MEM_REV_ORDER
#endif

#define CLAZZ_NS clz

namespace CLAZZ_NS {

template<class = void>
constexpr bool false_v = false;

template <class... T>
struct type_list {
    constexpr type_list(T...) {}
    constexpr type_list() requires (sizeof...(T) > 0) {}
};

template<class T>
struct type_list<T> {
    constexpr type_list(T) {}
    constexpr type_list() {}
    using type = T;
};

template <class... T>
type_list(T...) -> type_list<T...>;

template<class, size_t>
struct type_index {};

template<size_t I, class... Ts>
requires (I < sizeof...(Ts))
struct type_at;

// template<size_t I, class T, class... Ts>
// struct type_at<I, T, Ts...> : type_at<I-1, Ts...> {}; 

template<class T, class... Ts>
struct type_at<0, T, Ts...> {
    using type = T;
}; 

template<class T, class U, class... Ts>
struct type_at<1, T, U, Ts...> {
    using type = U;
};

template<class T, class U, class V, class... Ts>
struct type_at<2, T, U, V, Ts...> {
    using type = V;
};

template<class T, class U, class V, class W, class... Ts>
struct type_at<3, T, U, V, W, Ts...> {
    using type = W;
};

template<class T, class U, class V, class W, class X, class... Ts>
struct type_at<4, T, U, V, W, X, Ts...> {
    using type = X;
};

template<class T, class U, class V, class W, class X, class Y, class... Ts>
struct type_at<5, T, U, V, W, X, Y, Ts...> {
    using type = Y;
};

template<class T, class U, class V, class W, class X, class Y, class Z, class... Ts>
struct type_at<6, T, U, V, W, X, Y, Z, Ts...> {
    using type = Z;
};

template<size_t I, class T, class U, class V, class W, class X, class Y, class Z, class... Ts>
requires (I > 6)
struct type_at<I, T, U, V, W, X, Y, Z, Ts...> : type_at<I-7, Ts...> {};

template<size_t I, class... Ts>
using type_at_t = typename type_at<I, Ts...>::type;

template<size_t, class>
struct tuple_element;

template<size_t I, template<class...> class W, class... Ts>
struct tuple_element<I, W<Ts...>> : type_at<I, Ts...> {};

template<size_t I, class Tuple>
using tuple_element_t = typename tuple_element<I, Tuple>::type;

template<class To, class From>
requires (sizeof(std::decay_t<To>) == sizeof(std::decay_t<From>))
constexpr To union_cast(From&& in) noexcept {
    return reinterpret_cast<To>(in);
}

// https://stackoverflow.com/questions/35941045/can-i-obtain-c-type-names-in-a-constexpr-way/35943472#35943472
template<class T>
static constexpr std::string_view type_name_v = []<class U>(type_list<U>){
    char const* p = __PRETTY_FUNCTION__;
    while (*p++ != '=');
    for (; *p == ' '; ++p);
    char const* p2 = p;
    int count = 1;
    for (;;++p2) {
        switch (*p2) {
        case '[':
            ++count;
            break;
        case ']':
            --count;
            if (!count)
                return std::string_view{p, size_t(p2 - p)};
        }
    }
    throw "Failed";
}(type_list<T>{});

// Inspired by https://playfulprogramming.blogspot.com/2017/06/constexpr-quicksort-in-c17.html
namespace detail::sort {
    template <class T, class... Ts>
    constexpr auto head(type_list<T, Ts...>) {
        return type_list<T>{};
    }

    template <class T, class... Ts>
    constexpr auto tail(type_list<T, Ts...>) {
        return type_list<Ts...>{};
    }

    template <class... Ts, class... Us>
    constexpr auto operator|(type_list<Ts...>, type_list<Us...>) {
        return type_list<Ts..., Us...>{};
    }

    template <typename Compare, class P, class... Ts>
    constexpr auto partition(Compare compare, P pivot, type_list<Ts...> tl) {
        if constexpr (sizeof...(Ts) == 0) {
            return std::pair(type_list{}, type_list{});
        } else {
            constexpr auto h = head(tl);
            constexpr auto r = partition(compare, pivot, tail(tl));
            if constexpr (compare(h, pivot))
                return std::pair(h | r.first, r.second);
            else
                return std::pair(r.first, h | r.second);
        }
    }

    template <typename Compare, class... T>
    constexpr auto sort(type_list<T...> tl, Compare compare) {
        if constexpr (sizeof...(T) == 0)
            return type_list{};
        else {
            constexpr auto pivot = head(tl);
            constexpr auto r = partition(compare, pivot, tail(tl));
            return sort(r.first, compare) | pivot | sort(r.second, compare);
        }
    }

    template<size_t... I, class... T>
    constexpr auto remove_indices(type_list<type_index<T, I>...>) {
        return type_list<T...>{};
    }

    template <typename Compare, size_t... I, class... T>
    constexpr auto stable_sort(std::index_sequence<I...>, type_list<T...>, Compare compare) {
        return remove_indices(sort(type_list<type_index<T, I>...>{}, [=]<class Lt, size_t Li, class Rt, size_t Ri>
            (type_list<type_index<Lt, Li>>, type_list<type_index<Rt, Ri>>) {
                if (compare(type_list<Lt>{}, type_list<Rt>{})) return true;
                if (compare(type_list<Rt>{}, type_list<Lt>{})) return false;
                return Li < Ri;
            }));
    }

    template <typename Compare, class... T>
    constexpr auto stable_sort(type_list<T...> tl, Compare compare) {
        return stable_sort(std::index_sequence_for<T...>{}, tl, compare);
    }
}

enum class symbol_type {
    invalid,
    padding,
    var, 
    val,
    def,
    fun,
    ovl,
    tpe
};

template<class>
struct symbol_info {
    using tag_t = void;
    static constexpr symbol_type type = symbol_type::invalid;
    using value_t = void;
    using tuple_t = void;
    template<class>
    using struple_element_t = void;
    template<class Dec>
    static constexpr bool has_dec = false;
    template<class Sym>
    static constexpr bool shares_dec = false;
    template<class Dec>
    static constexpr bool has_wider_dec = false;
    template<class Dec>
    static constexpr bool has_narrower_dec = false;
    template<class Sym>
    static constexpr bool shares_wider_dec = false;
    template<class Sym>
    static constexpr bool shares_narrower_dec = false;
    template<class Name>
    static constexpr bool has_name = false;
    template<class Field>
    static constexpr bool shares_name = false;
    template<class Field>
    struct assert_unique_name;
    static constexpr bool has_default_ctor = false;
};

template<class T>
using symbol_tag_t =  typename symbol_info<T>::tag_t;

template<class T>
using symbol_tuple_t = typename symbol_info<T>::tuple_t;

template<class T>
using symbol_value_t = typename symbol_info<T>::value_t;

template<class Top, class T>
using symbol_element_t = typename symbol_info<T>::template struple_element_t<Top>;

template<class T>
concept Symbol = symbol_info<T>::type != symbol_type::invalid && sizeof(T) == sizeof(symbol_tuple_t<T>);

template<class T>
concept PrivateSymbol = Symbol<T> && symbol_info<T>::is_private;

template<class T>
concept EmptySymbol = Symbol<T> && std::is_same_v<symbol_tuple_t<T>, std::tuple<>>;

template<class T>
concept DataSymbol = Symbol<T> && !EmptySymbol<T>;

template<class T>
concept Padding = DataSymbol<T> && symbol_info<T>::type == symbol_type::padding;

template<class T>
concept Variable = DataSymbol<T> && symbol_info<T>::type == symbol_type::var;

template<class T>
concept StaticValue = EmptySymbol<T> && symbol_info<T>::type == symbol_type::val;

template<class T>
concept Value = Variable<T> || StaticValue<T>;

template<class T>
concept Definition = EmptySymbol<T> && symbol_info<T>::type == symbol_type::def;

template<class T>
concept DefinitionTemplate = Definition<T> && symbol_info<T>::is_tmpl_def;

template<class T>
concept Function = EmptySymbol<T> && symbol_info<T>::type == symbol_type::fun;

template<class T>
concept Overload = EmptySymbol<T> && symbol_info<T>::type == symbol_type::ovl;

template<class T>
concept FunctionOverload = Overload<T> && symbol_info<T>::is_all_funs;

template<class T>
concept DefinitionOverload = Overload<T> && !FunctionOverload<T> && symbol_info<T>::is_all_defs;

template<class T>
concept Method = Definition<T> || Function<T> || Overload<T>;

template<class T>
concept StaticMethod = Function<T> || FunctionOverload<T>;

template<class T>
concept StaticSymbol = StaticValue<T> || StaticMethod<T>;

template<class>
struct dec_info {
    static constexpr bool is_dec = false;
};

template<class T>
concept Declaration = dec_info<T>::is_dec;

template<class T>
concept MethodDeclaration = Declaration<T> && dec_info<T>::is_method;

template<class T>
concept StaticDeclaration = Declaration<T> && dec_info<T>::is_static;

template<class>
struct tag_info {
    static constexpr bool is_tag = false;
};

template<Symbol S>
using symbol_tag_info = tag_info<symbol_tag_t<S>>;

template<class T>
concept Tag = tag_info<T>::is_tag;

template<Tag... Tags>
struct tags_info {
    static constexpr size_t size = sizeof...(Tags);
    static constexpr bool empty = size == 0;

    static constexpr std::array<std::string_view, sizeof...(Tags)> names_array = {tag_info<Tags>::name...};
};

template<class S, class N>
concept SymbolNamed = Symbol<S> && Tag<N> && symbol_info<S>::template has_name<N>;

template<class M, class N>
concept DefinitionNamed = SymbolNamed<M, N> && Definition<M>;

template<class M, class N>
concept FunctionNamed = SymbolNamed<M, N> && Function<M>;

template<class M, class N>
concept OverloadNamed = SymbolNamed<M, N> && Overload<M>;

template<class M, class N>
concept MethodNamed = DefinitionNamed<M, N> || FunctionNamed<M, N> || OverloadNamed<M, N>;

template<Symbol...>
struct clazz;

template<DataSymbol... X>
using pod = clazz<X...>;

template<Symbol...>
struct meta_clazz;

template<class... Ts>
constexpr inline meta_clazz<Ts...>& meta_clazz_of(clazz<Ts...>& clz) {
    return union_cast<meta_clazz<Ts...>&>(clz);
}

template<class... Ts>
constexpr inline meta_clazz<Ts...>&& meta_clazz_of(clazz<Ts...>&& clz) {
    return union_cast<meta_clazz<Ts...>&&>(clz);
}

template<class... Ts>
constexpr inline const meta_clazz<Ts...>& meta_clazz_of(const clazz<Ts...>& clz) {
    return union_cast<const meta_clazz<Ts...>&>(clz);
}

template<DataSymbol... X>
using meta_pod = meta_clazz<X...>;

namespace detail {
    template<class T, class...>
    struct is_clazz : std::false_type{};

    template<class... Ts>
    struct is_clazz<clazz<Ts...>> : std::true_type {};

    template<class T>
    concept IsClazz = is_clazz<T>::value;

    template<class T, class U>
    concept IsClazzWith = is_clazz<T, U>::value;
}

template<class T, class... Ts>
concept Clazz = detail::IsClazz<std::decay_t<T>> && (detail::IsClazzWith<std::decay_t<T>, std::decay_t<Ts>> && ...);

template<Clazz C>
struct clazz_info : clazz_info<std::decay_t<C>> {};

namespace detail {
    template<Clazz C, Declaration D>
    struct is_clazz<C, D> : std::bool_constant<clazz_info<C>::template implements<D>> {};

    template<Clazz C, Symbol S>
    struct is_clazz<C, S> : std::bool_constant<clazz_info<C>::template has_symbol<S>> {};

    template<Clazz C, Tag T>
    struct is_clazz<C, T> : std::bool_constant<clazz_info<C>::template has_name<T>> {};
}

template<class>
struct is_tuple : std::false_type {};

template<class... Ts>
struct is_tuple<std::tuple<Ts...>> : std::true_type {};

template<class T>
concept Tuple = is_tuple<std::decay_t<T>>::value;

template<class>
struct is_named_tuple : std::false_type {};

template<template<class> class>
struct is_named_tuple_wrapper : std::false_type {};

template<class T>
concept NamedTuple = is_named_tuple<std::decay_t<T>>::value;

template<template<class> class W>
concept NamedTupleWrapper = is_named_tuple_wrapper<W>::value;

template<Clazz T>
using meta_clazz_t = typename std::decay_t<T>::meta_clazz_t;

template<Clazz T>
using meta_values_t = typename meta_clazz_t<T>::meta_values_t;

template<class T>
concept Pod = Clazz<T> && clazz_info<std::decay_t<T>>::is_pod;

template<class T>
struct is_meta_clazz : std::false_type{};

template<Symbol... Ts>
struct is_meta_clazz<meta_clazz<Ts...>> : std::true_type {};

template<class T>
concept MetaClazz = is_meta_clazz<std::decay_t<T>>::value;

template<class T>
concept MetaPod = MetaClazz<T> && std::decay_t<T>::is_pod;

template<Clazz, Symbol...>
struct struple;

template<Declaration... Ds>
struct trait {};

template<class>
struct trait_info {
    static constexpr bool is_trait = false;
    static constexpr bool pure = false;
};

template<class T>
concept Trait = trait_info<std::decay_t<T>>::is_trait;

template<class T>
concept PureTrait = Trait<T> && trait_info<std::decay_t<T>>::pure;

template<class>
struct type_list_of_clazzes : std::false_type {};

template<Clazz... C>
struct type_list_of_clazzes<type_list<C...>> : std::true_type {};

template<class C>
concept Variant = Clazz<C> && type_list_of_clazzes<typename C::variants>::value;

namespace detail {
    template<Clazz C, Trait T>
    struct is_clazz<C, T> : std::bool_constant<trait_info<T>::template implementor<C>> {};

    template<Clazz C, class T>
    struct implements : std::false_type {};
    template<Clazz C, class T>
    struct co_implements : std::false_type {};
    template<Clazz C, class T>
    struct contra_implements : std::false_type {};

    template<Clazz C, Declaration D>
    struct implements<C, D> : std::bool_constant<clazz_info<C>::template implements<D>> {};
    template<Clazz C, Declaration D>
    struct co_implements<C, D> : std::bool_constant<clazz_info<C>::template co_implements<D>> {};
    template<Clazz C, Declaration D>
    struct contra_implements<C, D> : std::bool_constant<clazz_info<C>::template contra_implements<D>> {};

    template<Clazz C, Trait T>
    struct implements<C, T> : std::bool_constant<trait_info<T>::template implementor<C>> {};
    template<Clazz C, Trait T>
    struct co_implements<C, T> : std::bool_constant<trait_info<T>::template co_implementor<C>> {};
    template<Clazz C, Trait T>
    struct contra_implements<C, T> : std::bool_constant<trait_info<T>::template contra_implementor<C>> {};

    template<class C, class T>
    concept Implements = implements<C, T>::value;
    template<class C, class T>
    concept CoImplements = co_implements<C, T>::value;
    template<class C, class T>
    concept ContraImplements = contra_implements<C, T>::value;
}

template<class C, class... T>
concept Implements = (detail::Implements<C, T> && ...);
template<class C, class... T>
concept CoImplements = (detail::CoImplements<C, T> && ...);
template<class C, class... T>
concept ContraImplements = (detail::ContraImplements<C, T> && ...);

template<Clazz SubClazz, Clazz SuperClazz>
struct clazz_is_subclazz_of : std::false_type {};

template<Clazz SubClazz, Symbol... Sups>
struct clazz_is_subclazz_of<SubClazz, clazz<Sups...>> {
    static constexpr bool value = clazz_info<SubClazz>::template implements_of<Sups...>;
};

template<class SubClazz, class SuperClazz>
concept SubClazzOf = clazz_is_subclazz_of<SubClazz, SuperClazz>::value;

template<class SubClz, class... Symbols>
concept SubClazz = SubClazzOf<SubClz, clazz<Symbols...>>;

template<class SuperClazz, class SubClazz>
concept SuperClazzOf = SubClazzOf<SubClazz, SuperClazz>;

template<class SupClz, class... Symbols>
concept SuperClazz = SuperClazzOf<SupClz, clazz<Symbols...>>;

template<class C1, class C2>
concept ClazzOf = SuperClazzOf<C1, C2> && SubClazzOf<C1, C2>;

template<class T, class... Symbols>
concept ClazzEq = SubClazz<T, Symbols...> && SuperClazz<T, Symbols...>;

template<Clazz SubClazz, Clazz SuperClazz>
struct clazz_is_co_subclazz_of : std::false_type {};

template<Clazz SubClazz, Symbol... Sups>
struct clazz_is_co_subclazz_of<SubClazz, clazz<Sups...>> {
    static constexpr bool value = clazz_info<SubClazz>::template co_implements_of<Sups...>;
};

template<class SubClazz, class SuperClazz>
concept CoSubClazzOf = clazz_is_co_subclazz_of<SubClazz, SuperClazz>::value;

template<class SubClazz, class... Symbols>
concept CoSubClazz = CoSubClazzOf<SubClazz, clazz<Symbols...>>;

template<class SuperClazz, class SubClazz>
concept CoSuperClazzOf = CoSubClazzOf<SubClazz, SuperClazz>;

template<class T, class... Symbols>
concept CoSuperClazz = CoSuperClazzOf<T, clazz<Symbols...>>;

template<class C1, class C2>
concept CoClazzOf = CoSuperClazzOf<C1, C2> && CoSubClazzOf<C1, C2>;

template<class T, class... Symbols>
concept CoClazz = CoSubClazz<T, Symbols...> && CoSuperClazz<T, Symbols...>;

template<Clazz SubClazz, Clazz SuperClazz>
struct clazz_is_contra_subclazz_of : std::false_type {};

template<Clazz SubClazz, Symbol... Sups>
struct clazz_is_contra_subclazz_of<SubClazz, clazz<Sups...>> {
    static constexpr bool value = clazz_info<SubClazz>::template contra_implements_of<Sups...>;
};

template<class SubClazz, class SuperClazz>
concept ContraSubClazzOf = clazz_is_contra_subclazz_of<SubClazz, SuperClazz>::value;

template<class SubClazz, class... Symbols>
concept ContraSubClazz = ContraSubClazzOf<SubClazz, clazz<Symbols...>>;

template<class SuperClazz, class SubClazz>
concept ContraSuperClazzOf = ContraSubClazzOf<SubClazz, SuperClazz>;

template<class T, class... Symbols>
concept ContraSuperClazz = ContraSuperClazzOf<T, clazz<Symbols...>>;

template<class C1, class C2>
concept ContraClazzOf = ContraSuperClazzOf<C1, C2> && ContraSubClazzOf<C1, C2>;

template<class T, class... Symbols>
concept ContraClazz = ContraSubClazz<T, Symbols...> && ContraSuperClazz<T, Symbols...>;

namespace detail {
    template<class R, class F, class... Args>
    concept InvocableExact = requires (F&& f, Args&&... args) {
        { std::invoke(std::forward<F>(f), std::forward<Args>(args)...) } -> std::same_as<R>;
    };
    template<class R, class F, class... Args>
    concept InvocableConvertible = requires (F&& f, Args&&... args) {
        R(std::invoke(std::forward<F>(f), std::forward<Args>(args)...));
    };
}

template<class T>
struct function_traits {
    static constexpr bool is_function_sig = false;
    using return_type = T;
};

template<class T>
concept CallSignature = function_traits<T>::is_function_sig;

template<class T>
concept CallSignatureConst = CallSignature<T> && function_traits<T>::is_const;

template<class T>
concept CallSignatureGetter = CallSignatureConst<T> && function_traits<T>::is_getter;

template<class R, class... T>
struct fun_sig_props {
    bool is_const;
    bool lvalue;
    bool rvalue;
    bool is_noexcept;
    template<class OR, class... OT>
    constexpr bool convertible_to(fun_sig_props<OR, OT...> other) const {
        if constexpr (!detail::InvocableConvertible<OR, R(T...), OT...>) return false;
        if (other.is_const && !is_const) return false;
        if (other.lvalue && !lvalue) return false;
        if (other.rvalue && !rvalue) return false;
        if (other.is_noexcept && !is_noexcept) return false;
        return true;
    }
    template<class OR, class... OT>
    constexpr bool equivalent_to(fun_sig_props<OR, OT...> other) const {
        if (!convertible_to(other)) return false;
        return (detail::InvocableExact<OR, R(T...), OT...>);
    }
};

#define DECLARE_FUN_TR(sig, ...) \
template<class R, class... T>\
struct function_traits<sig> {\
    /* TODO: Get rid of this as_const nonsense*/\
    using as_const = R(T...) const;\
    using return_type = R;\
    using arg_types = type_list<T...>;\
    static constexpr auto details = fun_sig_props<R, T...>{__VA_ARGS__};\
    static constexpr auto is_getter = sizeof...(T) == 0;\
    static constexpr bool is_function_sig = true;\
    static constexpr bool is_const = details.is_const;\
    static constexpr bool lvalue = details.lvalue;\
    static constexpr bool rvalue = details.rvalue;\
    static constexpr bool is_noexcept = details.is_noexcept;\
    template<CallSignature F>\
    static constexpr bool convertible_to = details.convertible_to(function_traits<F>::details);\
    template<CallSignature F>\
    static constexpr bool equivalent_to = details.equivalent_to(function_traits<F>::details);\
};

DECLARE_FUN_TR(R(T...), .is_const = false, .lvalue = true, .rvalue = true, .is_noexcept = false)
DECLARE_FUN_TR(R(T...) const, .is_const = true, .lvalue = true, .rvalue = true, .is_noexcept = false)
DECLARE_FUN_TR(R(T...) &, .is_const = false, .lvalue = true, .rvalue = false, .is_noexcept = false)
DECLARE_FUN_TR(R(T...) const &, .is_const = true, .lvalue = true, .rvalue = false, .is_noexcept = false)
DECLARE_FUN_TR(R(T...) &&, .is_const = false, .lvalue = false, .rvalue = true, .is_noexcept = false)
DECLARE_FUN_TR(R(T...) const &&, .is_const = true, .lvalue = false, .rvalue = true, .is_noexcept = false)

DECLARE_FUN_TR(R(T...) noexcept, .is_const = false, .lvalue = true, .rvalue = true, .is_noexcept = true)
DECLARE_FUN_TR(R(T...) const noexcept, .is_const = true, .lvalue = true, .rvalue = true, .is_noexcept = true)
DECLARE_FUN_TR(R(T...) & noexcept, .is_const = false, .lvalue = true, .rvalue = false, .is_noexcept = true)
DECLARE_FUN_TR(R(T...) const & noexcept, .is_const = true, .lvalue = true, .rvalue = false, .is_noexcept = true)
DECLARE_FUN_TR(R(T...) && noexcept, .is_const = false, .lvalue = false, .rvalue = true, .is_noexcept = true)
DECLARE_FUN_TR(R(T...) const && noexcept, .is_const = true, .lvalue = false, .rvalue = true, .is_noexcept = true)

// // Can wrap a function of lhs call sig with a function of rhs call sig
// static_assert(!function_traits<void(int)>::convertible_to<int(double)>);
// static_assert(function_traits<int(int)>::convertible_to<void(double)>);
// static_assert(function_traits<int(double)>::convertible_to<void(double)>);
// static_assert(function_traits<int(double)>::convertible_to<void(int)>);

// // Const is stricter
// static_assert(!function_traits<int()>::convertible_to<int() const>);
// static_assert(function_traits<int() const>::convertible_to<int() const>);
// static_assert(function_traits<int()>::convertible_to<int()>);
// static_assert(function_traits<int() const>::convertible_to<int()>);

// // Ref qual is stricter and mutually excl.
// static_assert(!function_traits<int() &&>::convertible_to<int() &>);
// static_assert(!function_traits<int() &>::convertible_to<int() &&>);
// static_assert(!function_traits<int() &&>::convertible_to<int()>);
// static_assert(!function_traits<int() &>::convertible_to<int()>);
// static_assert(function_traits<int()>::convertible_to<int() &>);
// static_assert(function_traits<int()>::convertible_to<int() &&>);
// static_assert(function_traits<int() &>::convertible_to<int() &>);
// static_assert(function_traits<int() &&>::convertible_to<int() &&>);

// // noexcept is stricter
// static_assert(!function_traits<int()>::convertible_to<int() noexcept>);
// static_assert(function_traits<int() noexcept>::convertible_to<int() noexcept>);
// static_assert(function_traits<int() noexcept>::convertible_to<int()>);

template<class From, class To>
static constexpr bool is_convertible = std::is_convertible_v<From, To>;

template<CallSignature From, CallSignature To>
static constexpr bool is_convertible<From, To> = function_traits<From>::template convertible_to<To>;

template<class T, class U>
static constexpr bool is_equivalent = std::is_same_v<T, U>;

template<CallSignature F1, CallSignature F2>
static constexpr bool is_equivalent<F1, F2> = function_traits<F1>::template equivalent_to<F2>;

template<class From, class To>
concept Convertible = is_convertible<From, To>;

template<class F1, class F2>
concept Equivalent = is_equivalent<F1, F2>;

static_assert(Convertible<char*(), std::string()>);
static_assert(Equivalent<char*, char*>);
static_assert(!(Equivalent<char*, std::string>));
static_assert(Equivalent<char*(), char*()>);
static_assert(!(Equivalent<char*(), std::string()>));

template<class F>
class function_query : function_traits<F> {
    template<class T>
    static constexpr auto qualify_class_(T) {
        using ctype = std::conditional_t<function_query::is_const, const T, T>;
        using crtype = std::conditional_t<function_query::rvalue && !function_query::lvalue, ctype&&, ctype&>;
        return type_list<crtype>{};
    }

public:
    template<class X>
    using qualify_class = typename decltype(qualify_class_(std::declval<X>()))::type;

private:
    template<bool Exact, class Lam, class Top, class... T>
    static constexpr bool is_invocable(type_list<T...>) {
        if constexpr (Exact) {
            if constexpr ((detail::InvocableExact<typename function_query::return_type, Lam, Top, T...>)) {
                if constexpr (function_query::is_noexcept)
                    return noexcept(std::invoke(std::declval<Lam>(), std::declval<Top>(), std::declval<T>()...));
                return true;
            }
        } else {
            if constexpr ((detail::InvocableConvertible<typename function_query::return_type, Lam, Top, T...>)) {
                if constexpr (function_query::is_noexcept)
                    return noexcept(std::invoke(std::declval<Lam>(), std::declval<Top>(), std::declval<T>()...));
                return true;
            }
        }

        return false;
    }
    
public:
    template<class Top, class Lam>
    static constexpr bool is_lambda_conformant = [] {
        if constexpr (function_query::is_function_sig)
            return is_invocable<false, Lam, qualify_class<Top>>(typename function_query::arg_types{});
        return false;
    }();
    
    template<class Top, class Lam>
    static constexpr bool is_lambda_exactly = [] {
        if constexpr (function_query::is_function_sig)
            return is_invocable<true, Lam, qualify_class<Top>>(typename function_query::arg_types{});
        return false;
    }();
};

// static_assert(std::is_same_v<std::string&, typename function_query<void()>::template qualify_class<std::string>>);
// static_assert(std::is_same_v<const std::string&, typename function_query<void() const>::template qualify_class<std::string>>);
// static_assert(std::is_same_v<std::string&&, typename function_query<void() &&>::template qualify_class<std::string>>);
// static_assert(std::is_same_v<const std::string&&, typename function_query<void() const &&>::template qualify_class<std::string>>);

template<class T>
concept mut = !std::is_const_v<std::remove_reference_t<T>>;
template<class T>
concept rval = std::is_rvalue_reference_v<T&&>;
template<class T>
concept rmut = mut<T> && rval<T>;

// auto l = [](auto&& self, int i) {
//     return self + i;
// };
// using L = decltype(l);
// static_assert(function_query<int(int)>::is_lambda_conformant<int, L>);
// static_assert(function_query<int(int) &>::is_lambda_conformant<int, L>);
// static_assert(function_query<int(int) &&>::is_lambda_conformant<int, L>);
// static_assert(function_query<int(int) const>::is_lambda_conformant<int, L>);


// auto lmut = [](mut auto& self, int i) {
//     return self += i;
// };
// using LMut = decltype(lmut);
// static_assert(function_query<int(int)>::is_lambda_conformant<int, LMut>);
// static_assert(function_query<int(int) &>::is_lambda_conformant<int, LMut>);
// static_assert(!function_query<int(int) &&>::is_lambda_conformant<int, LMut>);
// static_assert(!function_query<int(int) const>::is_lambda_conformant<int, LMut>);

// auto lrmut = [](mut auto&& self, int i) {
//     return self += i;
// };
// using LRMut = decltype(lrmut);
// static_assert(function_query<int(int)>::is_lambda_conformant<int, LRMut>);
// static_assert(function_query<int(int) &>::is_lambda_conformant<int, LRMut>);
// static_assert(function_query<int(int) &&>::is_lambda_conformant<int, LRMut>);
// static_assert(!function_query<int(int) const>::is_lambda_conformant<int, LRMut>);

// auto rrmut = [](rmut auto&& self, int i) {
//     return self += i;
// };
// using RRMut = decltype(rrmut);
// static_assert(!function_query<int(int)>::is_lambda_conformant<int, RRMut>);
// static_assert(!function_query<int(int) &>::is_lambda_conformant<int, RRMut>);
// static_assert(function_query<int(int) &&>::is_lambda_conformant<int, RRMut>);
// static_assert(!function_query<int(int) const>::is_lambda_conformant<int, RRMut>);

// auto lconst = [](const auto& self, int i) noexcept {
//     return self + i;
// };
// using LConst = decltype(lconst);
// static_assert(function_query<int(int)>::is_lambda_conformant<int, LConst>);
// static_assert(function_query<int(int) &>::is_lambda_conformant<int, LConst>);
// static_assert(function_query<int(int) &&>::is_lambda_conformant<int, LConst>);
// static_assert(function_query<int(int) const>::is_lambda_conformant<int, LConst>);
// static_assert(function_query<int(int) const noexcept>::is_lambda_exactly<int, LConst>);

// template<class T>
// struct call_signature {
//     using return_t = T;
//     static constexpr bool is_call = false;
//     static constexpr bool is_const = std::is_const_v<T>;
//     using as_const = std::add_const_t<T>;
//     template<class F, class... Ts>
//     static constexpr bool is_partial_application_of = false;
//     template<class F, class... Ts>
//     static constexpr bool is_exact_partial_application_of = false;
// };

// template<class R, class... Args>
// struct call_signature<R(Args...)> {
//     using return_t = R;
//     static constexpr bool is_call = true;
//     static constexpr bool is_const = false;
//     using as_const = R(Args...) const;
//     template<class F, class... Ts>
//     static constexpr bool is_partial_application_of = detail::InvocableConvertible<R, F, Ts..., Args...>;
//     template<class F, class... Ts>
//     static constexpr bool is_exact_partial_application_of = detail::InvocableExact<R, F, Ts..., Args...>;
// };

// template<class R, class... Args>
// struct call_signature<R(Args...) const> {
//     using return_t = R;
//     static constexpr bool is_call = true;
//     static constexpr bool is_const = true;
//     static constexpr bool is_getter = sizeof...(Args) == 0;
//     using as_const = R(Args...) const;
//     template<class F, class... Ts>
//     static constexpr bool is_partial_application_of = detail::InvocableConvertible<R, F, Ts..., Args...>;
//     template<class F, class... Ts>
//     static constexpr bool is_exact_partial_application_of = detail::InvocableExact<R, F, Ts..., Args...>;
// };

template<class T>
struct is_struple : std::false_type {};

template<class... Ts>
struct is_struple<struple<Ts...>> : std::true_type {};

template<class T>
concept Struple = is_struple<std::decay_t<T>>::value;

template<class, class>
struct assign_tuple;

template<template<class...> class Tup, class C>
struct assign_tuple<Tup<>, C> {
    using type = Tup<>;
};

template<template<class...> class Tup, class T, class C>
struct assign_tuple<Tup<T>, C> {
    using type = Tup<C>;
};

template<class T, class C>
using assign_tuple_t = typename assign_tuple<T, C>::type;

template<class, template<class> class>
struct map_tuple;

template<class... Ts, template<class...> class Tup, template<class> class W>
struct map_tuple<Tup<Ts...>, W> {
    using type = Tup<W<Ts>...>;
};

template<class T, template<class> class W>
using map_tuple_t = typename map_tuple<T, W>::type;

template<class, template<class...> class>
struct apply_tuple;

template<class... Ts, template<class...> class Tup, template<class...> class W>
struct apply_tuple<Tup<Ts...>, W> {
    using type = W<Ts...>;
};

template<class T, template<class...> class W>
using apply_tuple_t = typename apply_tuple<T, W>::type;

template<class...>
struct flatten_tuples;

template<class... Ts>
using flatten_tuples_t = typename flatten_tuples<Ts...>::type;

template<>
struct flatten_tuples<> {
    using type = std::tuple<>;
};

template<template<class...> class W, class... Ts>
struct flatten_tuples<W<Ts...>> {
    using type = W<Ts...>;
};

template<template<class...> class W, class... T1, class... T2, class... Rest>
struct flatten_tuples<W<T1...>, W<T2...>, Rest...> : flatten_tuples<W<T1..., T2...>, Rest...> {};

template<class... Ts>
using prune_clazz_t = flatten_tuples_t<std::conditional_t<Symbol<Ts>, clazz<Ts>, clazz<>>...>;

template<class...>
struct flatten_indices;

template<>
struct flatten_indices<> {
    using type = std::index_sequence<>;
};

template<size_t... Is>
struct flatten_indices<std::index_sequence<Is...>> {
    using type = std::index_sequence<Is...>;
};

template<size_t... I1, size_t... I2, class... Rest>
struct flatten_indices<std::index_sequence<I1...>, std::index_sequence<I2...>, Rest...> : flatten_indices<std::index_sequence<I1..., I2...>, Rest...> {};

template<class... Is>
using flatten_indices_t = typename flatten_indices<Is...>::type;

template<template<class...> class, class>
struct bind_to;

template<template<class...> class To, template<class...> class From, class... Xs>
struct bind_to<To, From<Xs...>> {
    using type = To<Xs...>;
};

template<template<class...> class W, class T>
using bind_to_t = typename bind_to<W, T>::type;

namespace detail {
    template<class...>
    struct reverse_types;

    template<template<class...> class W, class T, class... Ts>
    struct reverse_types<W<T, Ts...>> : reverse_types<W<Ts...>, W<T>> {};

    template<template<class...> class W, class T, class... Ts, class... Us>
    struct reverse_types<W<T, Ts...>, W<Us...>> : reverse_types<W<Ts...>, W<T, Us...>> {};

    template<template<class...> class W, class... Us>
    struct reverse_types<W<>, W<Us...>> {
        using type = W<Us...>;
    };
}

template<class Tuple>
using reverse_types = typename detail::reverse_types<Tuple>::type;

template<Clazz C>
struct clazz_sort;

namespace detail {
    struct empty {};
    template<class T>
    struct empty_checker : empty {
        char a;
        [[no_unique_address]] T b;
    };
}

template<Symbol... S>
struct clazz_sort<clazz<S...>> {
private:
    template<class CompSize>
    static constexpr auto sort_by_size(CompSize) {
        return detail::sort::stable_sort(type_list<S...>{},
            []<class T, class U>(type_list<T>, type_list<U>) {
                // Put static symbols to the back
                if constexpr (EmptySymbol<T> != EmptySymbol<U>)
                    return !EmptySymbol<T>;

                auto sizel = sizeof(T);
                if (sizeof(detail::empty_checker<T>) == sizeof(char)) 
                    sizel = 0;
                auto sizer = sizeof(U);
                if (sizeof(detail::empty_checker<U>) == sizeof(char)) 
                    sizer = 0;

                // If one of the symbols has no size, put it towards the back
                if ((sizel == 0) != (sizer == 0))
                    return sizel != 0;

                return CompSize{}(sizel, sizer);
            });
    }

#ifdef CLAZZ_MEM_REV_ORDER
    using less_t    = std::greater<>;
    using greater_t = std::less<>;
#else
    using less_t    = std::less<>;
    using greater_t = std::greater<>;
#endif

public:
    template<class = void>
    using ascending_size_t = bind_to_t<clazz, decltype(sort_by_size(less_t{}))>;
    
    template<class = void>
    using descending_size_t = bind_to_t<clazz, decltype(sort_by_size(greater_t{}))>;
};

template<Clazz C>
using sort_asc = typename clazz_sort<C>::template ascending_size_t<>;

template<Clazz C>
using sort_desc = typename clazz_sort<C>::template descending_size_t<>;

#ifdef CLAZZ_MEM_REV_ORDER
    template<Symbol... S>
    using clazz_decl = reverse_types<clazz<S...>>;
#else
    template<Symbol... S>
    using clazz_decl = clazz<S...>;
#endif

template<Symbol... S>
using clazz_asc = sort_asc<clazz_decl<S...>>;

template<Symbol... S>
using clazz_desc = sort_desc<clazz_decl<S...>>;

namespace detail {
    template<class I, Symbol... X>
    struct indices_of_vars;

    template<size_t... I, Symbol... X>
    struct indices_of_vars<std::index_sequence<I...>, X...> {
        static_assert(sizeof...(I) == sizeof...(X));
        using indices_t = bind_to_t<flatten_indices_t, flatten_tuples_t<assign_tuple_t<symbol_tuple_t<X>, std::index_sequence<I>>...>>;
    };
}

template<Symbol... X>
using indices_of_vars_t = typename detail::indices_of_vars<std::index_sequence_for<X...>, X...>::indices_t;

template<class, class...>
struct index_of_type {
    static constexpr size_t value = 1;
};

template<class T, class U, class... Ts>
struct index_of_type<T, U, Ts...> {
    static constexpr size_t value = 1 + index_of_type<T, Ts...>::value;
};

template<class T, class... Ts>
struct index_of_type<T, T, Ts...> {
    static constexpr size_t value = 0;
};

template<class T, class... Ts>
struct is_one_of {
    static constexpr bool value = (std::is_same_v<T, Ts> || ...);
};

template<Clazz T, Clazz... Ts>
struct is_one_of<T, Ts...> {
    static constexpr bool value = (ClazzOf<T, Ts> || ...);
};

template<template<class> class W1, template<class> class W2>
struct is_same_wrapper : std::false_type {};

template<template<class> class W>
struct is_same_wrapper<W, W> : std::true_type {};

template<Symbol...>
struct assert_unique_symbol_names;

template<>
struct assert_unique_symbol_names<> : std::true_type {};

template<Symbol Field, Symbol... Fields>
struct assert_unique_symbol_names<Field, Fields...> {
    static constexpr bool value = (symbol_info<Field>::template assert_unique_name<Fields>::value && ...) && assert_unique_symbol_names<Fields...>::value;
};

template<class, class...>
struct index_of;

namespace detail::arg {
    template<template<bool> class Assertion>
    struct undetermined {
        int i;
        template<class... Ts>
        undetermined(Ts&&...) {
            Assertion<false> a;
        }
    };

    template<class>
    struct is_undetermined : std::false_type {};

    template<template<bool> class Assertion>
    struct is_undetermined<undetermined<Assertion>> : std::true_type, Assertion<false> {};

    template<class NamesTuple, class Target, class ArgsTuple>
    // void Target means Target is yet to be determined by the clazz to be constructed
    // When void, Target cannot be used for template argument deduction of clazz
    struct holder;

    template<Tag... Names, class Target, class ArgsTuple>
    requires (sizeof...(Names) > 0)
    struct holder<type_list<Names...>, Target, ArgsTuple> : ArgsTuple {
        using tuple_t = ArgsTuple;
        using tuple_t::tuple;

        // Only the first tag is important
        using tag_t = type_at_t<0, Names...>;
        using var_t = typename tag_info<tag_t>::template var_t<Target>;
        using clazz_t = clazz<typename tag_info<Names>::template var_t<Target>...>;

        template<class F>
        constexpr decltype(auto) invoke(F&& f) && noexcept {
            return std::invoke(std::forward<F>(f), std::move(*this));
        }

        template<class Type>
        constexpr inline decltype(auto) make() && noexcept {
            if constexpr (std::tuple_size_v<tuple_t> == 1 && std::is_invocable_r_v<Type, tuple_element_t<0, tuple_t>>)
                return std::invoke(std::get<0>(static_cast<tuple_t&&>(*this)));
            else
                return std::make_from_tuple<Type>(static_cast<tuple_t&&>(*this));
        }

        constexpr inline tuple_t&& as_tuple() && noexcept {
            return static_cast<tuple_t&&>(*this);
        }

        static constexpr bool is_single = sizeof...(Names) == 1;

        static constexpr bool is_undetermined = is_undetermined<Target>::value;

        template<Tag T>
        static constexpr bool has_name = (std::same_as<T, Names> || ...);

        template<Symbol S>
        static constexpr bool shares_name = (symbol_info<S>::template has_name<Names> || ...);        

        template<Symbol... S>
        static constexpr bool shares_subset_names = (clazz_info<clazz<S...>>::template has_name<Names> && ...);        
    };
}

template<class P>
struct is_arg_holder : std::false_type {};

template<class NamesTuple, class Target, class ArgsTuple>
struct is_arg_holder<detail::arg::holder<NamesTuple, Target, ArgsTuple>> : std::true_type {};

template<class A>
concept ArgHolder = is_arg_holder<A>::value;

template<class A>
concept SingleArgHolder = ArgHolder<A> && A::is_single;

namespace detail::arg {
    template<ArgHolder... Ps>
    struct holder_set : std::tuple<Ps&&...> {
        using tuple_t = std::tuple<Ps&&...>;

        using tuple_t::tuple;

        template<Tag T>
        static constexpr bool has_name = (Ps::template has_name<T> || ...);

        template<Symbol S>
        static constexpr bool shares_name = (Ps::template shares_name<S> || ...);
        
        template<DataSymbol S>
        requires (holder_set::template shares_name<S>)
        constexpr inline S make_symbol() && {
            constexpr size_t idx = find_ph_for_symbol<0, S>();
            return std::get<idx>(std::move(*this)).template make<S>();
        }

        template<Tag T, class Type>
        requires (holder_set::template has_name<T>)
        constexpr inline decltype(auto) make_type() && {
            constexpr size_t idx = find_ph_for_tag<0, T>();
            return std::get<idx>(std::move(*this)).template make<Type>();
        }

        template<Tag T>
        requires (has_name<T>)
        constexpr inline decltype(auto) get_tuple() && {
            constexpr size_t idx = find_ph_for_tag<0, T>();
            return std::get<idx>(std::move(*this)).as_tuple();
        }

    private:
        template<size_t I, DataSymbol S>
        static constexpr inline size_t find_ph_for_symbol() {
            using argholder_t = std::decay_t<tuple_element_t<I, tuple_t>>;
            if constexpr(argholder_t::template shares_name<S>) {
                return I;
            } else {
                return find_ph_for_symbol<I+1, S>();
            }
        }

        template<size_t I, Tag T>
        static constexpr inline size_t find_ph_for_tag() {
            using argholder_t = std::decay_t<tuple_element_t<I, tuple_t>>;
            if constexpr(argholder_t::template has_name<T>) {
                return I;
            } else {
                return find_ph_for_tag<I+1, T>();
            }
        }
    };

    template<ArgHolder... Ps>
    holder_set(Ps&&...) -> holder_set<Ps...>;
}

template<class P>
struct is_arg_holder_set : std::false_type {};

template<class... T>
struct is_arg_holder_set<detail::arg::holder_set<T...>> : std::true_type {};

template<class P>
concept ArgHolderSet = is_arg_holder_set<std::decay_t<P>>::value;

template<class T>
struct index_of<T> {
    static constexpr int value = -1;
};

template<DataSymbol S, Tag N, Tag... Ns>
requires (symbol_info<S>::template has_name<N>)
struct index_of<S, N, Ns...> {
    static constexpr int value = 0;
};

template<DataSymbol S, Tag N, Tag... Ns>
requires (!symbol_info<S>::template has_name<N>)
struct index_of<S, N, Ns...> {
    static constexpr int value = sizeof...(Ns) == 0
                                     ? -1
                                     : 1 + index_of<S, Ns...>::value;
};

template<DataSymbol S1, DataSymbol S2, Symbol... Ss>
requires (symbol_info<S1>::template shares_name<S2>)
struct index_of<S1, S2, Ss...> {
    static constexpr int value = 0;
};

template<DataSymbol S1, DataSymbol S2, Symbol... Ss>
requires (!symbol_info<S1>::template shares_name<S2>)
struct index_of<S1, S2, Ss...> {
    static constexpr int value = sizeof...(Ss) == 0 
                                     ? -1 
                                     : 1 + index_of<S1, Ss...>::value;
};

// Skip EmptySymbols
template<DataSymbol S1, EmptySymbol S2, Symbol... Ss>
struct index_of<S1, S2, Ss...> {
    static constexpr int value = sizeof...(Ss) == 0 
                                     ? -1 
                                     : index_of<S1, Ss...>::value;
};

template<Tag T, Symbol S, Symbol... Ss>
requires (symbol_info<S>::template has_name<T>)
struct index_of<T, S, Ss...> {
    static constexpr int value = 0;
};

template<Tag T, Symbol S, Symbol... Ss>
requires (!symbol_info<S>::template has_name<T>)
struct index_of<T, S, Ss...> {
    static constexpr int value = sizeof...(Ss) == 0 
                                     ? -1 
                                     : 1 + index_of<T, Ss...>::value;
};

template<Tag T, ArgHolder P, ArgHolder... Ps>
requires (P::template has_name<T>)
struct index_of<T, P, Ps...> {
    static constexpr int value = 0;
};

template<Tag T, ArgHolder P, ArgHolder... Ps>
requires (!P::template has_name<T>)
struct index_of<T, P, Ps...> {
    static constexpr int value = sizeof...(Ps) == 0 
                                     ? -1 
                                     : 1 + index_of<T, Ps...>::value;
};

template<Clazz Top>
struct EBO_MSVC struple<Top> {
    template<class... Ts>
    constexpr struple(Ts&&...) noexcept {}
};

template<Clazz Top, class X, class... Xs>
struct struple<Top, X, Xs...> 
#ifdef CLAZZ_MEM_REV_ORDER
    : struple<Top, Xs...>, symbol_element_t<Top, X> 
#else 
    : symbol_element_t<Top, X>, struple<Top, Xs...>
#endif
{
protected:
    using this_t = struple<Top, X, Xs...>;
    using base_t = struple<Top, Xs...>;
    using element_t = symbol_element_t<Top, X>;
    using tag_t = symbol_tag_t<X>;
    static_assert(sizeof(base_t) == sizeof(flatten_tuples_t<symbol_tuple_t<Xs>...>));

    constexpr struple() noexcept : base_t{}, element_t{} {}

    template<class... Ts>
    requires (EmptySymbol<X> || Padding<X>)
    constexpr struple(Ts&&... ins) noexcept
        : base_t(std::forward<Ts>(ins)...)
        , element_t{}
    {
    }

    template<class T, class ... Ts>
    requires (DataSymbol<X>)
    constexpr struple(T&& in, Ts&&... ins) noexcept 
        : base_t(std::forward<Ts>(ins)...)
        , element_t{std::forward<T>(in)}
    {
    }

    template<ArgHolderSet T>
    requires (DataSymbol<X> && T::template shares_name<X>)
    constexpr struple(T&& setter_collection) noexcept
        : base_t{std::move(setter_collection)}
        , element_t{std::move(setter_collection).template make_symbol<X>()}
    {
    }

    template<ArgHolderSet T>
    requires (DataSymbol<X> && !T::template shares_name<X> && symbol_info<X>::has_default_ctor)
    constexpr struple(T&& setter_collection) noexcept
        : base_t{std::move(setter_collection)}
        , element_t{}
    {
    }

    template<Clazz Clz>
    requires DataSymbol<X>
         && (clazz_info<Clz>::template has_name<symbol_tag_t<X>>)
          && Value<typename clazz_info<Clz>::template tag_symbol_t<tag_t>>
    constexpr struple(Clz&& clz) noexcept
        : base_t{std::forward<Clz>(clz)}
        , element_t{get<tag_t>(std::forward<Clz>(clz))}
    {
    }

    template<Clazz Clz>
    requires DataSymbol<X>
        && (!clazz_info<Clz>::template has_name<symbol_tag_t<X>>)
         && (symbol_info<X>::has_default_ctor)
    constexpr struple(Clz&& clz) noexcept
        : base_t{std::forward<Clz>(clz)}
        , element_t{}
    {
    }
};

template<class... Names, class... Targets, class... Ts>
requires (!detail::arg::holder<type_list<Names>, Targets, Ts>::is_undetermined && ...)
clazz(detail::arg::holder<type_list<Names>, Targets, Ts>&&...) 
    -> pod<typename detail::arg::holder<type_list<Names>, Targets, Ts>::var_t...>;

template<ArgHolder... As>
auto make_clazz(As&&... argholders) {
    using clazz_t = flatten_tuples_t<typename As::clazz_t...>;
    return clazz_t{std::move(argholders)...};
}

template<ArgHolder... As>
auto make_clazz_asc(As&&... argholders) {
    using clazz_t = flatten_tuples_t<typename As::clazz_t...>;
    return sort_asc<clazz_t>{std::move(argholders)...};
}

template<ArgHolder... As>
auto make_clazz_desc(As&&... argholders) {
    using clazz_t = flatten_tuples_t<typename As::clazz_t...>;
    return sort_desc<clazz_t>{std::move(argholders)...};
}

template<ArgHolder... As>
auto make_clazz_decl(As&&... argholders) {
    using clazz_t = flatten_tuples_t<typename As::clazz_t...>;
    return bind_to_t<clazz_decl, clazz_t>{std::move(argholders)...};
}

template<class... Names, class... Targets, class... Ts>
requires (!detail::arg::holder<type_list<Names>, Targets, Ts>::is_undetermined && ...)
meta_clazz(detail::arg::holder<type_list<Names>, Targets, Ts>&&...) 
    -> meta_pod<typename detail::arg::holder<type_list<Names>, Targets, Ts>::var_t...>;

namespace detail {
    template<class, class...>
    struct build_clazz;

    template<Symbol... S, Symbol... O, class... Ts>
    struct build_clazz<type_list<S...>, clazz<O...>, Ts...> : build_clazz<type_list<S..., O...>, Ts...> {};

    template<Symbol... S, Symbol O, class... Ts>
    struct build_clazz<type_list<S...>, O, Ts...> : build_clazz<type_list<S..., O>, Ts...> {};

    template<Symbol... S>
    struct build_clazz<type_list<S...>> {
        using type = clazz<S...>;
    };
}

template<class... Ts>
using build_clazz = typename detail::build_clazz<type_list<>, Ts...>::type;

template<class... Ts>
using build_clazz_asc = sort_asc<build_clazz<Ts...>>;

template<class... Ts>
using build_clazz_desc = sort_desc<build_clazz<Ts...>>;

template<class... Ts>
using build_clazz_decl = bind_to_t<clazz_decl, build_clazz<Ts...>>;

template<class... F>
struct overload : F... {
    using F::operator()...;
};

template<class... T>
overload(T...) -> overload<T...>;

template<class... Fs>
struct try_all : Fs... {
    template<class... Ts>
    constexpr inline size_t operator()(Ts&&... in) const {
        return ([&] { 
            if constexpr (std::is_invocable_v<Fs, Ts&&...>) {
                Fs::operator()(std::forward<Ts>(in)...);
                return 1;
            } else {
                return 0;
            }
        }() + ... + 0);
    }
};

template<class... Fs>
try_all(Fs...) -> try_all<Fs...>;

template<class... Fs>
struct try_first : Fs... {
    template<class... Ts>
    constexpr inline bool operator()(Ts&&... in) const {
        return ([&] { 
            if constexpr (std::is_invocable_v<Fs, Ts&&...>) {
                Fs::operator()(std::forward<Ts>(in)...);
                return true;
            } else {
                return false;
            }
        }() || ...);
    }
};

template<class... Fs>
try_first(Fs...) -> try_first<Fs...>;

template<class... Fs>
struct do_all : Fs... {
    template<class... Ts>
    constexpr inline void operator()(Ts&&... in) const {
        (Fs::operator()(std::forward<Ts>(in)...), ...);
    }
};

template<class... Fs>
do_all(Fs...) -> do_all<Fs...>;

namespace detail {
    template<template<size_t, class> class, class...>
    struct make_meta_clazz_t;
    
    template<template<size_t, class> class name_wrapper, size_t... I, class... X>
    struct make_meta_clazz_t<name_wrapper, std::index_sequence<I...>, X...> {
        using type = meta_clazz<typename name_wrapper<I, X>::type...>;
    };
}

template<template<size_t, class> class name_wrapper, class... X>
using make_meta_clazz_t = typename detail::make_meta_clazz_t<name_wrapper, std::index_sequence_for<X...>, X...>::type;

template<template<size_t, class> class name_wrapper, class... X>
using make_clazz_t = typename make_meta_clazz_t<name_wrapper, X...>::clazz_t;
}

#define DECLARE_NAMED_TUPLE_VAR_WRAPPER(struple_symbol) \
    namespace CLAZZ_NS::struple_symbols { template<size_t I, class T> struct struple_symbol { using type = void; }; }

#define DECLARE_NAMED_TUPLE_OR_META_CLAZZ(tuple_name, clazz_or_meta_clazz) \
    DECLARE_NAMED_TUPLE_VAR_WRAPPER(tuple_name)\
    namespace CLAZZ_NS {\
        /* Unfortunately, both an alias and concrete type are needed for deduction guides */\
        template<class... X>\
        using tuple_name ## _t = make_ ## clazz_or_meta_clazz ## _t<struple_symbols::tuple_name, X...>;\
        template<class... X>\
        struct tuple_name : tuple_name ## _t<X...> {\
            using type = tuple_name ## _t <X...>;\
            static_assert(sizeof(type) == sizeof(std::tuple<X...>));\
            template<class... Ts>\
            constexpr tuple_name(Ts&&... in) noexcept : type(std::forward<Ts>(in)...) {}\
        };\
        template<class... X>\
        struct is_named_tuple<tuple_name<X...>> : std::true_type {};\
        template<>\
        struct is_named_tuple_wrapper<tuple_name> : std::true_type {};\
        /* Deduction guides only possible on concrete classes, and not on aliases */\
        template<class... Ts>\
        tuple_name(Ts...) -> tuple_name<Ts...>;\
        template<class... Ts>\
        tuple_name(std::tuple<Ts...>) -> tuple_name<Ts...>;\
    }
    
#define DECLARE_NAMED_META_CLAZZ(tuple_name) DECLARE_NAMED_TUPLE_OR_META_CLAZZ(tuple_name, meta_clazz)
#define DECLARE_NAMED_TUPLE(tuple_name) DECLARE_NAMED_TUPLE_OR_META_CLAZZ(tuple_name, clazz)

#define DECLARE_NAMED_TUPLE_VAR(tuple_name, index, struple_tag_name) \
    namespace CLAZZ_NS::struple_symbols {\
        template<class T>\
        struct tuple_name<index, T> { using type = ::CLAZZ_NS::var::struple_tag_name<T>; };\
        static_assert(Symbol<typename tuple_name<std::max(0, index-1), int>::type>,\
                      "Previous index of "#tuple_name" is not valid");\
    }

namespace CLAZZ_NS {
template<template<class, bool> class DecWrapper, class DecType, bool Static, Tag TagName>
struct dec_info_impl {
    template<Declaration To>
    struct is_convertible_to_dec : std::false_type {};
    template<class To, bool S>
    requires Convertible<DecType, To> && (Static || !S)
    struct is_convertible_to_dec<DecWrapper<To, S>> : std::true_type {};

    template<Declaration To>
    struct is_equivalent_to_dec : std::false_type {};
    template<class To, bool S>
    requires Equivalent<DecType, To> && (Static || !S)
    struct is_equivalent_to_dec<DecWrapper<To, S>> : std::true_type {};

    template<Tag Name>
    static constexpr bool has_name = std::same_as<Name, TagName>;
    template<Tag Name, class Top, class F>
    static constexpr bool is_applicable_for_def = std::is_same_v<Name, TagName> &&
        function_query<DecType>::template is_lambda_conformant<Top, F>;
    template<Tag Name, class Top, class F>
    static constexpr bool is_match_for_def = std::is_same_v<Name, TagName> &&
        function_query<DecType>::template is_lambda_exactly<Top, F>;
};

namespace detail {
    template<class R, class... Variants, size_t I, size_t... Is, class F, class... Ts>
    constexpr R visit_clazz_variant(std::index_sequence<I, Is...>, const unsigned char index, void* const ptr, F&& f, Ts&&... args);
    
    template<class R, class... Variants, size_t I, size_t... Is, class F, class... Ts>
    constexpr R visit_clazz_variant(std::index_sequence<I, Is...>, const unsigned char index, const void* const ptr, F&& f, Ts&&... args);

    template<class R, class... Variants, class T, class F, class... Ts>
    inline constexpr R visit_clazz_variant(std::index_sequence<>, unsigned char, T*, F&&, Ts&&...) noexcept;
}
}

#define FWD_DECLARE_STRUPLE_DEC(tag_name) \
    namespace CLAZZ_NS::dec {\
        template<class DecType, bool Static = false>\
        struct tag_name;\
    }

#define DECLARE_STRUPLE_DEC(tag_name) \
    namespace CLAZZ_NS {\
        template<class DecType, bool Static>\
        struct dec_info<dec::tag_name<DecType, Static>> : dec_info_impl<dec::tag_name, DecType, Static, ::CLAZZ_NS::tag::tag_name> {\
            using return_t = typename function_traits<DecType>::return_type;\
            static constexpr bool is_dec = true;\
            static constexpr bool is_static = Static;\
            static constexpr bool is_method = (CallSignature<DecType>);\
            template<class... Variants>\
            struct variant_def {\
                /*TODO: Add back template parameters instead of auto when GCC re-implements them*/\
                using type = detail::def::tag_name ## _i<int, DecType, decltype([](auto& self, auto&&... args) {\
                    return detail::visit_clazz_variant<return_t, Variants...>(\
                        std::index_sequence_for<Variants...>{},\
                        (const char&)self, &self,\
                        [](auto&& c, auto&&... ins) -> decltype(auto) {\
                            return std::forward<decltype(c)>(c).tag_name(std::forward<decltype(ins)>(ins)...);\
                        },\
                        std::forward<decltype(args)>(args)...);\
                })>;\
            };\
            template<class... Variants>\
            using variant_def_t = typename variant_def<Variants...>::type;\
        };\
    }

#define FWD_DECLARE_STRUPLE_TAG(tag_name) \
    namespace CLAZZ_NS::tag {\
        struct tag_name;\
    }

#define DECLARE_STRUPLE_TAG(tag_name) \
    namespace CLAZZ_NS {\
        namespace tag {\
            struct tag_name {};\
        }\
        namespace detail::tag::concepts::tag_name {\
            template<class Struct>\
            concept HasMemVar = requires (Struct s) { {s.tag_name}; };\
        }\
        template<>\
        struct tag_info<tag::tag_name> {\
            static constexpr bool is_tag = true;\
            STRUPLE_ALIAS_VAR(var_t, tag_name);\
            STRUPLE_ALIAS_VAL(val_t, tag_name);\
            STRUPLE_ALIAS_TPE(tpe_t, tag_name);\
            STRUPLE_ALIAS_DEF(def_t, tag_name);\
            STRUPLE_ALIAS_DFN(dfn_t, tag_name);\
            STRUPLE_ALIAS_FUN(fun_t, tag_name);\
            STRUPLE_ALIAS_OVL(ovl_t, tag_name);\
            static constexpr auto& arg = arg::tag_name;\
            static constexpr auto name = std::string_view(#tag_name);\
            template<Symbol S>\
            struct assert_unique_name {\
                static constexpr bool value = !symbol_info<S>::template has_name<tag::tag_name>;\
                static_assert(value, "Field "#tag_name" is not unique.\n"\
                                     "If defining multiple overloads of def/fun "#tag_name", use ovl "#tag_name);\
            };\
            template<class Struct>\
            static constexpr bool has_mem_var = detail::tag::concepts::tag_name::HasMemVar<Struct>;\
            template<class Struct>\
            using mem_var_t = var_t<typename class_data_member_pointer_info<decltype(&Struct::tag_name)>::member_t>;\
            /*TODO: Avoid using offsetof*/\
            template<Clazz Top>\
            static constexpr size_t offset = __builtin_offsetof(Top, tag_name);\
        };\
    }

namespace CLAZZ_NS {
template<class>
struct class_data_member_pointer_info;

template<class Outer, class Member>
struct class_data_member_pointer_info<Member Outer::*> {
    static constexpr bool is_data_member = true;
    using member_t = Member;
};

namespace detail::arg {
    template<class Tg, template<bool> class Assert>
    struct setter {
        template<class T>
        constexpr auto operator=(T&& in) const {
            return detail::arg::holder<type_list<Tg>, std::decay_t<T>, std::tuple<T&&>>{std::forward<T>(in)};
        }
        template<class T>
        constexpr auto operator=(std::initializer_list<T> in) const {
            return detail::arg::holder<type_list<Tg>, std::decay_t<T>, std::tuple<std::initializer_list<T>&&>>{std::move(in)};
        }
        template<class T>
        constexpr auto operator()(T&& in) const {
            return detail::arg::holder<type_list<Tg>, std::decay_t<T>, std::tuple<T&&>>{std::forward<T>(in)};
        }
        template<class... Ts>
        constexpr auto operator()(Ts&&... ins) const {
            return detail::arg::holder<type_list<Tg>, detail::arg::undetermined<Assert>, std::tuple<Ts&&...>>{std::forward<Ts>(ins)...};
        }
        template<class Target, class... Ts>
        constexpr auto as(Ts&&... ins) const {
            return detail::arg::holder<type_list<Tg>, Target, std::tuple<Ts&&...>>{std::forward<Ts>(ins)...};
        }
        template<class T>
        constexpr auto fwd(T&& in) const {
            return detail::arg::holder<type_list<Tg>, T&&, std::tuple<T&&>>{std::forward<T>(in)};
        }
    };

    template<bool>
    struct assert_targeted_default {
        static_assert("clazz<...> template deduction failure" && false_v<>,\
            "Cannot deduce template arguments for clazz<...> with args<names...>(params...), "\
            "use args<names...>.as<TargetType>(params...) to set a target type for deduction "\
            "when calling a constructor of a clazz member with multiple parameters.");\
    };

    template<Tag... Tags>
    struct multi_setter {
        template<class T>
        constexpr auto operator=(T&& in) const {
            return detail::arg::holder<type_list<Tags...>, std::decay_t<T>, std::tuple<T&&>>{std::forward<T>(in)};
        }
        template<class T>
        constexpr auto operator()(T&& in) const {
            return detail::arg::holder<type_list<Tags...>, std::decay_t<T>, std::tuple<T&&>>{std::forward<T>(in)};
        }
        template<class... Ts>
        constexpr auto operator()(Ts&&... ins) const {
            return detail::arg::holder<type_list<Tags...>, detail::arg::undetermined<assert_targeted_default>, std::tuple<Ts&&...>>{std::forward<Ts>(ins)...};
        }
        template<class Target, class... Ts>
        constexpr auto as(Ts&&... ins) const {
            return detail::arg::holder<type_list<Tags...>, Target, std::tuple<Ts&&...>>{std::forward<Ts>(ins)...};
        }
        template<class T>
        constexpr auto fwd(T&& in) const {
            return detail::arg::holder<type_list<Tags...>, T&&, std::tuple<T&&>>{std::forward<T>(in)};
        }
    };
}

template<Tag... Tags>
constexpr detail::arg::multi_setter<Tags...> args;
}

#define DECLARE_STRUPLE_ARG(tag_name) \
    namespace CLAZZ_NS {\
        namespace detail::arg::assert_targeted {\
            template<bool E>\
            struct tag_name {\
                static_assert("clazz<...> template deduction failure" && false_v<>,\
                    "arg::"#tag_name"(params...):\n"\
                    "Cannot deduce template arguments for clazz<...> with arg::"#tag_name"(params...), "\
                    "use arg::"#tag_name".as<TargetType>(params...) to set a target type for deduction "\
                    "when calling a constructor of a clazz member with multiple parameters.");\
            };\
        }\
        namespace arg {\
            constexpr detail::arg::setter<tag::tag_name, detail::arg::assert_targeted::tag_name> tag_name;\
        }\
    }

namespace CLAZZ_NS {
template<Tag TagName>
struct tag_queries {
    using tag_t = TagName;
    template<Tag Name>
    static constexpr bool has_name = std::same_as<Name, tag_t>;
    template<Symbol S>
    static constexpr bool shares_name = symbol_info<S>::template has_name<tag_t>;
    template<Symbol S>
    struct assert_unique_name : tag_info<tag_t>::template assert_unique_name<S> {};
};

template<Declaration Dec>
struct dec_queries;

template<Declaration Dec>
struct dec_queries {
    template<Declaration D>
    static constexpr bool has_dec = dec_info<Dec>::template is_equivalent_to_dec<D>::value;
    template<Symbol S>
    static constexpr bool shares_dec = symbol_info<S>::template has_dec<Dec>;
    template<Declaration D>
    static constexpr bool has_wider_dec = dec_info<Dec>::template is_convertible_to_dec<D>::value;
    template<Declaration D>
    static constexpr bool has_narrower_dec = dec_info<D>::template is_convertible_to_dec<Dec>::value;
    template<Symbol S>
    static constexpr bool shares_wider_dec = symbol_info<S>::template has_narrower_dec<Dec>;
    template<Symbol S>
    static constexpr bool shares_narrower_dec = symbol_info<S>::template has_wider_dec<Dec>;
};

template<class Sym, Tag TagName, class Top, class F>
struct tmpl_dec_queries {
    // See if call sig on dec is invocable with this function template
    template<Declaration Dec>
    static constexpr bool has_dec = dec_info<Dec>::template is_match_for_def<TagName, Top, F>;
    template<class S>
    static constexpr bool shares_dec = [] {
        static_assert(Symbol<S>); // TODO: Add back as template requirement when GCC fixes bug
        if constexpr (tag_queries<TagName>::template shares_name<S>) {
            static_assert(!DefinitionTemplate<S>, "Cannot compare two template definitions to each other");
            return symbol_info<S>::template shares_dec<Sym>;
        }
        return false;
    }();
    template<Declaration Dec>
    static constexpr bool has_wider_dec = dec_info<Dec>::template is_applicable_for_def<TagName, Top, F>;
    template<Declaration D>
    static constexpr bool has_narrower_dec = [] {
        static_assert(false_v<>, "Not enough type information to determine");
    }();
    template<Symbol S>
    static constexpr bool shares_wider_dec = [] {
        if constexpr (tag_queries<TagName>::template shares_name<S>) {
            static_assert(!DefinitionTemplate<S>, "Cannot compare two template definitions to each other");
            return symbol_info<S>::template shares_narrower_dec<Sym>;
        }
        return false;
    }();
    template<Symbol S>
    static constexpr bool shares_narrower_dec = [] {
        static_assert(false_v<>, "Not enough type information to determine");
    }();
};

template<Tag TagName, Declaration Dec>
struct symbol_queries : tag_queries<TagName>, dec_queries<Dec> {};

}

#define FWD_DECLARE_STRUPLE_VAR(tag_name) \
    namespace CLAZZ_NS::detail::var {\
        template<class T, class... DefaultValue>\
        requires (sizeof...(DefaultValue) <= 1 && (std::is_invocable_r<T, DefaultValue>::value && ...))\
        struct tag_name ## _i;\
    }

#define STRUPLE_ALIAS_VAR_(alias_name) \
    template<class T, auto... DefaultValue>\
    using alias_name

#define STRUPLE_ALIAS_VAR(alias_name, tag_name) \
    STRUPLE_ALIAS_VAR_(alias_name) =\
        detail::var::tag_name ## _i<T, std::conditional_t<std::is_invocable_v<decltype(DefaultValue)>,\
                                                                              decltype(DefaultValue),\
                                                                              decltype([]{ return DefaultValue; })>...>

#define DECLARE_STRUPLE_VAR(tag_name) \
    namespace CLAZZ_NS {\
        namespace detail::var {\
            template<class T>\
            struct tag_name ## _i<T> {\
                [[no_unique_address]] T tag_name;\
                template<class... Ts>\
                constexpr tag_name ## _i(Ts&&... ins) noexcept(noexcept(T(std::forward<Ts>(ins)...)))\
                    : tag_name(std::forward<Ts>(ins)...) {}\
            };\
            template<class T, class DefaultValue>\
            struct tag_name ## _i<T, DefaultValue> {\
                [[no_unique_address]] T tag_name;\
                template<class... Ts>\
                constexpr tag_name ## _i(Ts&&... ins) noexcept(noexcept(T(std::forward<Ts>(ins)...)))\
                    : tag_name(std::forward<Ts>(ins)...) {}\
                constexpr tag_name ## _i() noexcept(noexcept(DefaultValue{}()))\
                    : tag_name(DefaultValue{}()) {}\
            };\
        }\
        namespace var {\
            STRUPLE_ALIAS_VAR(tag_name, tag_name);\
        }\
        template<class T, class... DefaultValue>\
        struct symbol_info<detail::var::tag_name ## _i<T, DefaultValue...>> : symbol_queries<tag::tag_name, dec::tag_name<T>> {\
            using value_t = T;\
            using symbol_t = detail::var::tag_name ## _i<T, DefaultValue...>;\
            static constexpr symbol_type type = symbol_type::var;\
            using tuple_t = std::tuple<T>;\
            template<class>\
            using struple_element_t = symbol_t;\
            static constexpr bool has_default_ctor = sizeof...(DefaultValue) == 1;\
            static constexpr decltype(auto) default_value() requires (has_default_ctor) { return (DefaultValue{}(), ...); }\
            template<Clazz C>\
            requires (clazz_info<C>::template has_symbol<symbol_t>)\
            static constexpr decltype(auto) invoke(C&& c) {\
                if constexpr(std::is_rvalue_reference_v<C&&>) return static_cast<T&&>(c.tag_name);\
                else return (c.tag_name);\
            }\
            template<class Test>\
            static constexpr bool has_compatible_field = requires(Test test) { {test.tag_name} -> std::convertible_to<T>; };\
            template<class Struct>\
            requires (symbol_info::has_compatible_field<Struct>)\
            static constexpr decltype(auto) extract_compatible_value(Struct&& s) {\
                if constexpr(requires(Struct s) { {s.tag_name} -> std::convertible_to<T>; }) {\
                    if constexpr (std::is_lvalue_reference_v<Struct>) return (s.tag_name);\
                    else return std::forward<decltype(s.tag_name)>(s.tag_name);\
                }\
                else return s.tag_name();\
            }\
        };\
    }
    
#define FWD_DECLARE_STRUPLE_VAL(tag_name) \
    namespace CLAZZ_NS::detail::val {\
        template<class T, class Value>\
        requires (std::is_invocable_r<T, Value>::value)\
        struct tag_name ## _i;\
    }

#define STRUPLE_ALIAS_VAL_(alias_name) \
    template<class T, auto Value>\
    using alias_name

#define STRUPLE_ALIAS_VAL(alias_name, tag_name) \
    STRUPLE_ALIAS_VAL_(alias_name) =\
        detail::val::tag_name ## _i<T, std::conditional_t<std::is_invocable_v<decltype(Value)>,\
                                                                              decltype(Value),\
                                                                              decltype([]{ return Value; })>>

#define DECLARE_STRUPLE_VAL(tag_name) \
    namespace CLAZZ_NS {\
        namespace detail::val {\
            template<class T, class Value>\
            requires (std::is_invocable_r<T, Value>::value)\
            struct tag_name ## _i {\
                static constexpr T tag_name = Value{}();\
            };\
        }\
        namespace val {\
            STRUPLE_ALIAS_VAL(tag_name, tag_name);\
        }\
        template<class T, class Value>\
        struct symbol_info<detail::val::tag_name ## _i<T, Value>> : symbol_queries<tag::tag_name, dec::tag_name<T, true>> {\
            using value_t = T;\
            using symbol_t = detail::val::tag_name ## _i<T, Value>;\
            static constexpr symbol_type type = symbol_type::val;\
            using tuple_t = std::tuple<>;\
            template<class>\
            using struple_element_t = symbol_t;\
            static constexpr bool has_default_ctor = true;\
            static constexpr T invoke() { return Value{}(); }\
            template<class Test>\
            static constexpr bool has_compatible_field = requires { {Test::tag_name} -> std::convertible_to<T>; };\
            template<class Struct>\
            requires (symbol_info::has_compatible_field<std::decay_t<Struct>>)\
            static constexpr decltype(auto) extract_compatible_value(Struct&& s) {\
                if constexpr(requires { {Struct::tag_name} -> std::convertible_to<T>; }) return (s.tag_name);\
                else return s.tag_name();\
            }\
        };\
    }

#define STRUPLE_ALIAS_TPE_(alias_name) \
    template<class T>\
    using alias_name

#define STRUPLE_ALIAS_TPE(alias_name, tag_name) \
    STRUPLE_ALIAS_TPE_(alias_name) = detail::tpe::tag_name ## _i<T>

#define FWD_DECLARE_STRUPLE_TPE(tag_name) \
    namespace CLAZZ_NS::detail::tpe {\
        template<class T>\
        struct tag_name ## _i;\
    }

#define DECLARE_STRUPLE_TPE(tag_name) \
    namespace CLAZZ_NS {\
        namespace detail::tpe {\
            template<class T>\
            struct tag_name ## _i {\
                using tag_name = T;\
            };\
        }\
        namespace tpe {\
            STRUPLE_ALIAS_TPE(tag_name, tag_name);\
        }\
        template<class T>\
        struct symbol_info<tpe::tag_name<T>> : symbol_queries<tag::tag_name, dec::tag_name<T>> {\
            using symbol_t = tpe::tag_name<T>;\
            using value_t = void;\
            static constexpr symbol_type type = symbol_type::val;\
            using tuple_t = std::tuple<>;\
            template<class>\
            using struple_element_t = symbol_t;\
            static constexpr bool has_default_ctor = true;\
            template<class Test>\
            static constexpr bool has_compatible_field = false;\
        };\
    }

#define FWD_DECLARE_STRUPLE_DEF(tag_name) \
    namespace CLAZZ_NS::detail::def {\
        template<class Top, class Sig, class... F>\
        struct tag_name ## _i;\
    }

#define STRUPLE_ALIAS_DFN_(alias_name) \
    template<class Sig, auto... F>\
    requires (CallSignature<Sig> && sizeof...(F) > 0)\
    using alias_name

#define STRUPLE_ALIAS_DFN(alias_name, tag_name) \
    STRUPLE_ALIAS_DFN_(alias_name) = detail::def::tag_name ## _i<int, Sig, decltype(F)...>

#define STRUPLE_ALIAS_DEF_(alias_name) \
    template<auto... F>\
    requires (sizeof...(F) > 0)\
    using alias_name

#define STRUPLE_ALIAS_DEF(alias_name, tag_name) \
    STRUPLE_ALIAS_DEF_(alias_name) = detail::def::tag_name ## _i<int, void, decltype(F)...>

#define DECLARE_STRUPLE_DEF(tag_name) \
    namespace CLAZZ_NS {\
        namespace detail::def {\
            template<class Top, class R, class... Args, class... F>\
            /*TODO: Use function_traits instead of manual R(Args...) pattern matching*/\
            struct tag_name ## _i<Top, R(Args...) const, F...> : protected F... {\
                constexpr tag_name ## _i() {\
                    static_assert((std::is_invocable_r_v<R, F, const Top&, Args...> || ...) ||\
                                  (std::is_invocable_r_v<R, F, const Top&&, Args...> || ...),\
                                  "No implementations of def::"#tag_name" are reachable given the call signature.");\
                }\
                template<class... Ts>\
                requires std::invocable<R(Args...), Ts&&...> && (std::is_invocable_r<R, F, const Top&, Ts&&...>::value || ...)\
                inline constexpr R tag_name(Ts&&... in) const & {\
                    return (*this)(static_cast<const Top&>(*this), std::forward<Ts>(in)...);\
                }\
                template<class... Ts>\
                requires std::invocable<R(Args...), Ts&&...> && (std::is_invocable_r<R, F, const Top&&, Ts&&...>::value || ...)\
                inline constexpr R tag_name(Ts&&... in) const && {\
                    return (*this)(static_cast<const Top&&>(*this), std::forward<Ts>(in)...);\
                }\
            protected:\
                using F::operator()...;\
                using top_t = Top;\
            };\
            template<class Top, class R, class... Args, class... F>\
            struct tag_name ## _i<Top, R(Args...), F...> : protected F... {\
                constexpr tag_name ## _i() {\
                    static_assert((std::is_invocable_r_v<R, F, Top&, Args...> || ...) ||\
                                  (std::is_invocable_r_v<R, F, Top&&, Args...> || ...),\
                                  "No implementations of def::"#tag_name" are reachable given the call signature.");\
                }\
                template<class... Ts>\
                requires std::invocable<R(Args...), Ts&&...> && (std::is_invocable_r<R, F, Top&, Ts&&...>::value || ...)\
                inline constexpr R tag_name(Ts&&... in) & {\
                    return (*this)(static_cast<Top&>(*this), std::forward<Ts>(in)...);\
                }\
                template<class... Ts>\
                requires std::invocable<R(Args...), Ts&&...> && (std::is_invocable_r<R, F, Top&&, Ts&&...>::value || ...)\
                inline constexpr R tag_name(Ts&&... in) && {\
                    return (*this)(static_cast<Top&&>(*this), std::forward<Ts>(in)...);\
                }\
            protected:\
                using F::operator()...;\
                using top_t = Top;\
            };\
            template<class Top, class... F>\
            struct tag_name ## _i<Top, void, F...> : protected F... {\
                template<class... Ts>\
                requires (std::invocable<F, Top&, Ts&&...> || ...)\
                inline constexpr decltype(auto) tag_name(Ts&&... in) &\
                    noexcept(noexcept((*this)(static_cast<Top&>(*this), std::forward<Ts>(in)...)))\
                {\
                    return (*this)(static_cast<Top&>(*this), std::forward<Ts>(in)...);\
                }\
                template<class... Ts>\
                requires (std::invocable<F, Top&&, Ts&&...> || ...)\
                inline constexpr decltype(auto) tag_name(Ts&&... in) &&\
                    noexcept(noexcept((*this)(static_cast<Top&&>(*this), std::forward<Ts>(in)...)))\
                {\
                    return (*this)(static_cast<Top&&>(*this), std::forward<Ts>(in)...);\
                }\
                template<class... Ts>\
                requires (std::invocable<F, const Top&, Ts&&...> || ...)\
                inline constexpr decltype(auto) tag_name(Ts&&... in) const &\
                    noexcept(noexcept((*this)(static_cast<const Top&>(*this), std::forward<Ts>(in)...)))\
                {\
                    return (*this)(static_cast<const Top&>(*this), std::forward<Ts>(in)...);\
                }\
                template<class... Ts>\
                requires (std::invocable<F, const Top&&, Ts&&...> || ...)\
                inline constexpr decltype(auto) tag_name(Ts&&... in) const &\
                    noexcept(noexcept((*this)(static_cast<const Top&&>(*this), std::forward<Ts>(in)...)))\
                {\
                    return (*this)(static_cast<const Top&&>(*this), std::forward<Ts>(in)...);\
                }\
            protected:\
                using F::operator()...;\
                using top_t = Top;\
            };\
        }\
        namespace def {\
            STRUPLE_ALIAS_DEF(tag_name, tag_name);\
        }\
        namespace dfn {\
            STRUPLE_ALIAS_DFN(tag_name, tag_name);\
        }\
        template<class Top, class F>\
        struct symbol_info<detail::def::tag_name ## _i<Top, void, F>> \
            : tag_queries<tag::tag_name>\
            , tmpl_dec_queries<detail::def::tag_name ## _i<Top, void, F>, tag::tag_name, Top, F> {\
            using value_t = void;\
            using symbol_t = detail::def::tag_name ## _i<Top, void, F>;\
            static constexpr symbol_type type = symbol_type::def;\
            static constexpr bool is_tmpl_def = true;\
            using tuple_t = std::tuple<>;\
            template<Clazz TTop>\
            using struple_element_t = detail::def::tag_name ## _i<TTop, void, F>;\
            static constexpr bool has_default_ctor = true;\
            template<class Test>\
            static constexpr bool has_compatible_field = false;\
            template<Clazz C, class... Ts>\
            requires (clazz_info<C>::template has_dec_of<symbol_t>)\
            static constexpr decltype(auto) invoke(C&& c, Ts&&... ins) {\
                return c.tag_name(std::forward<Ts...>(ins)...);\
            }\
        };\
        template<class Top, CallSignature Sig, class... F>\
        struct symbol_info<detail::def::tag_name ## _i<Top, Sig, F...>> : symbol_queries<tag::tag_name, dec::tag_name<Sig>> {\
            using value_t = void;\
            using symbol_t = detail::def::tag_name ## _i<Top, Sig, F...>;\
            static constexpr symbol_type type = symbol_type::def;\
            using tuple_t = std::tuple<>;\
            template<Clazz TTop>\
            using struple_element_t = detail::def::tag_name ## _i<TTop, Sig, F...>;\
            static constexpr bool has_default_ctor = true;\
            template<class Test>\
            static constexpr bool has_compatible_field = false;\
            template<Clazz C, class... Ts>\
            requires (clazz_info<C>::template has_dec_of<symbol_t>)\
            static constexpr decltype(auto) invoke(C&& c, Ts&&... ins) {\
                return c.tag_name(std::forward<Ts...>(ins)...);\
            }\
        };\
    }

#define FWD_DECLARE_STRUPLE_FUN(tag_name) \
    namespace CLAZZ_NS::detail::fun {\
        template<CallSignatureConst Sig, class... F>\
        struct tag_name ## _i;\
    }

// TODO: Make this work with void argument (template)
#define STRUPLE_ALIAS_FUN_(alias_name) \
    template<CallSignature Sig, auto... F>\
    using alias_name

#define STRUPLE_ALIAS_FUN(alias_name, tag_name) \
    STRUPLE_ALIAS_FUN_(alias_name) = detail::fun::tag_name ## _i<typename function_traits<Sig>::as_const, decltype(F)...>

#define DECLARE_STRUPLE_FUN(tag_name) \
    namespace CLAZZ_NS {\
        namespace detail::fun {\
            template<class R, class... Args, class... F>\
            struct tag_name ## _i<R(Args...) const, F...> {\
                constexpr tag_name ## _i() {\
                    static_assert((std::is_invocable_r_v<R, F, Args...> || ...),\
                                  "No implementations of fun::"#tag_name" are reachable given the call signature.");\
                }\
                template<class... Ts>\
                requires std::invocable<R(Args...), Ts&&...> && (std::is_invocable_r<R, F, Ts&&...>::value || ...)\
                inline static constexpr R tag_name(Ts&&... in) {\
                    static_assert((std::is_convertible_v<Ts&&, Args> && ...),\
                                  "Cannot call fun::"#tag_name" with these arguments.");\
                    return std::invoke(overload{F{}...}, std::forward<Ts>(in)...);\
                }\
            };\
        }\
        namespace fun {\
            STRUPLE_ALIAS_FUN(tag_name, tag_name);\
        }\
        template<CallSignatureConst Sig, class... F>\
        struct symbol_info<detail::fun::tag_name ## _i<Sig, F...>> : symbol_queries<tag::tag_name, dec::tag_name<Sig, true>> {\
            using value_t = void;\
            using symbol_t = detail::fun::tag_name ## _i<Sig, F...>;\
            static constexpr symbol_type type = symbol_type::fun;\
            using tuple_t = std::tuple<>;\
            template<class>\
            using struple_element_t = symbol_t;\
            static constexpr bool has_default_ctor = true;\
            template<class Test>\
            static constexpr bool has_compatible_field = false;\
            template<class... Ts>\
            static constexpr decltype(auto) invoke(Ts&&... ins) {\
                return symbol_t::tag_name(std::forward<Ts...>(ins)...);\
            }\
        };\
    }

#define STRUPLE_ALIAS_OVL(alias_name, tag_name) \
    template<MethodNamed<tag::tag_name>... methods>\
    using alias_name = detail::ovl::tag_name ## _i<methods...>

#define FWD_DECLARE_STRUPLE_OVL(tag_name) \
    namespace CLAZZ_NS::detail::ovl {\
        /* TODO: Add back constraints when new GCC concepts bug disappears */\
        template<class... F>\
        struct tag_name ## _i;\
    }

#define DECLARE_STRUPLE_OVL(tag_name) \
    namespace CLAZZ_NS {\
        namespace detail::ovl {\
            /* TODO: Add back constraints when new GCC concepts bug disappears */\
            template<class... F>\
            struct tag_name ## _i : F... {\
                using F::tag_name...;\
            };\
            template<DefinitionNamed<::CLAZZ_NS::tag::tag_name>... Defs>\
            struct tag_name ## _i<Defs...> : protected Defs...{\
            protected:\
                using top_t = typename type_at_t<0, Defs...>::top_t;\
                using Defs::operator()...;\
            public:\
                template<class... Ts>\
                /*TODO: Add back when GCC can deal with it
                requires (std::invocable<Defs, top_t&, Ts&&...> || ...)*/\
                inline constexpr auto tag_name(Ts&&... in)\
                    -> decltype((*this)(std::declval<top_t&>(), std::declval<Ts>()...))\
                {\
                    static_assert((std::is_same_v<top_t, typename Defs::top_t> && ...));\
                    return (*this)(static_cast<top_t&>(*this), std::forward<Ts>(in)...);\
                }\
                template<class... Ts>\
                /*TODO: Add back when GCC can deal with it
                requires (std::invocable<Defs, const top_t&, Ts&&...> || ...)*/\
                inline constexpr auto tag_name(Ts&&... in) const\
                    -> decltype((*this)(std::declval<const top_t&>(), std::declval<Ts>()...))\
                {\
                    static_assert((std::is_same_v<top_t, typename Defs::top_t> && ...));\
                    return (*this)(static_cast<const top_t&>(*this), std::forward<Ts>(in)...);\
                }\
            };\
        }\
        namespace ovl {\
            STRUPLE_ALIAS_OVL(tag_name, tag_name);\
        }\
        template<MethodNamed<tag::tag_name>... F>\
        struct symbol_info<detail::ovl::tag_name ## _i<F...>> : tag_queries<tag::tag_name> {\
            using value_t = void;\
            using symbol_t = detail::ovl::tag_name ## _i<F...>;\
            static constexpr symbol_type type = symbol_type::ovl;\
            using tuple_t = std::tuple<>;\
            template<class Top>\
            using struple_element_t = detail::ovl::tag_name ## _i<symbol_element_t<Top, F>...>;\
            static constexpr bool has_default_ctor = true;\
            template<Declaration D>\
            static constexpr bool has_dec = (symbol_info<F>::template has_dec<D> || ...);\
            template<Symbol S>\
            static constexpr bool shares_dec = (symbol_info<F>::template shares_dec<S> || ...);\
            template<Declaration D>\
            static constexpr bool has_wider_dec = (symbol_info<F>::template has_wider_dec<D> || ...);\
            template<Declaration D>\
            static constexpr bool has_narrower_dec = (symbol_info<F>::template has_narrower_dec<D> || ...);\
            template<Symbol S>\
            static constexpr bool shares_wider_dec = (symbol_info<F>::template shares_wider_dec<S> || ...);\
            template<Symbol S>\
            static constexpr bool shares_narrower_dec = (symbol_info<F>::template shares_narrower_dec<S> || ...);\
            static constexpr bool is_all_defs = (Definition<F> && ...);\
            static constexpr bool is_all_funs = (Function<F> && ...);\
            template<class Test>\
            static constexpr bool has_compatible_field = false;\
            template<Clazz C, class... Ts>\
            requires (clazz_info<C>::template has_dec_of<symbol_t>)\
            static constexpr decltype(auto) invoke(C&& c, Ts&&... ins) {\
                return c.tag_name(std::forward<Ts...>(ins)...);\
            }\
        };\
    }

#define FWD_DECLARE_STRUPLE_SYMBOL(tag_name) \
    FWD_DECLARE_STRUPLE_TAG(tag_name)\
    FWD_DECLARE_STRUPLE_DEC(tag_name)\
    FWD_DECLARE_STRUPLE_VAR(tag_name)\
    FWD_DECLARE_STRUPLE_VAL(tag_name)\
    FWD_DECLARE_STRUPLE_TPE(tag_name)\
    FWD_DECLARE_STRUPLE_DEF(tag_name)\
    FWD_DECLARE_STRUPLE_FUN(tag_name)\
    FWD_DECLARE_STRUPLE_OVL(tag_name)

#define DECLARE_STRUPLE_SYMBOL(tag_name) \
    FWD_DECLARE_STRUPLE_SYMBOL(tag_name)\
    DECLARE_STRUPLE_ARG(tag_name)\
    DECLARE_STRUPLE_TAG(tag_name)\
    DECLARE_STRUPLE_DEC(tag_name)\
    DECLARE_STRUPLE_VAR(tag_name)\
    DECLARE_STRUPLE_VAL(tag_name)\
    DECLARE_STRUPLE_TPE(tag_name)\
    DECLARE_STRUPLE_DEF(tag_name)\
    DECLARE_STRUPLE_FUN(tag_name)\
    DECLARE_STRUPLE_OVL(tag_name)

namespace CLAZZ_NS {

template<size_t Size>
struct padding {
    constexpr padding() {}
private:
    struct {
        std::byte _[Size];
    } _;
};

template<>
struct padding<0> {};

namespace detail::tag {
    template<size_t Size>
    struct padding {};
}

template<size_t Size>
struct tag_info<detail::tag::padding<Size>> {
    static constexpr bool is_tag = true;
    STRUPLE_ALIAS_VAR_(var_t) = padding<Size>;
    STRUPLE_ALIAS_VAL_(val_t) = padding<Size>;
    STRUPLE_ALIAS_TPE_(tpe_t) = padding<Size>;
    STRUPLE_ALIAS_DEF_(def_t) = padding<Size>;
    STRUPLE_ALIAS_FUN_(fun_t) = padding<Size>;
    template<class...>
    using ovl_t = padding<Size>;
    template<Symbol S>
    struct assert_unique_name : std::true_type {};
    template<class Struct>
    static constexpr bool has_mem_var = false;
    template<class Struct>
    using mem_var_t = padding<Size>;
    template<Clazz Top>
    static constexpr size_t offset = -1;
};

template<size_t Size>
struct symbol_info<padding<Size>> {
    using value_t = void;
    static constexpr symbol_type type = symbol_type::padding;
    using tuple_t = std::tuple<std::byte[Size]>;
    using tag_t = detail::tag::padding<Size>;
    using symbol_t = padding<Size>;
    template<class>
    using struple_element_t = symbol_t;
    static constexpr bool has_default_ctor = true;
    template<class Dec>
    static constexpr bool has_dec = false;
    template<class Sym>
    static constexpr bool shares_dec = false;
    template<class Dec>
    static constexpr bool has_wider_dec = false;
    template<class Dec>
    static constexpr bool has_narrower_dec = false;
    template<class Sym>
    static constexpr bool shares_wider_dec = false;
    template<class Sym>
    static constexpr bool shares_narrower_dec = false;
    template<class Name>
    static constexpr bool has_name = false;
    template<class Field>
    static constexpr bool shares_name = false;
    template<Symbol S>
    struct assert_unique_name : std::true_type {};
};

}

DECLARE_STRUPLE_SYMBOL(_1)
DECLARE_STRUPLE_SYMBOL(_2)
DECLARE_STRUPLE_SYMBOL(_3)
DECLARE_STRUPLE_SYMBOL(_4)
DECLARE_STRUPLE_SYMBOL(_5)
DECLARE_STRUPLE_SYMBOL(_6)
DECLARE_STRUPLE_SYMBOL(_7)
DECLARE_STRUPLE_SYMBOL(_8)
DECLARE_STRUPLE_SYMBOL(_9)
DECLARE_STRUPLE_SYMBOL(_10)
DECLARE_STRUPLE_SYMBOL(_11)
DECLARE_STRUPLE_SYMBOL(_12)
DECLARE_STRUPLE_SYMBOL(_13)
DECLARE_STRUPLE_SYMBOL(_14)
DECLARE_STRUPLE_SYMBOL(_15)
DECLARE_STRUPLE_SYMBOL(_16)
DECLARE_STRUPLE_SYMBOL(_17)
DECLARE_STRUPLE_SYMBOL(_18)
DECLARE_STRUPLE_SYMBOL(_19)
DECLARE_STRUPLE_SYMBOL(_20)
DECLARE_STRUPLE_SYMBOL(_21)
DECLARE_STRUPLE_SYMBOL(_22)

DECLARE_NAMED_TUPLE(nuple)
DECLARE_NAMED_TUPLE_VAR(nuple, 0, _1)
DECLARE_NAMED_TUPLE_VAR(nuple, 1, _2)
DECLARE_NAMED_TUPLE_VAR(nuple, 2, _3)
DECLARE_NAMED_TUPLE_VAR(nuple, 3, _4)
DECLARE_NAMED_TUPLE_VAR(nuple, 4, _5)
DECLARE_NAMED_TUPLE_VAR(nuple, 5, _6)
DECLARE_NAMED_TUPLE_VAR(nuple, 6, _7)
DECLARE_NAMED_TUPLE_VAR(nuple, 7, _8)
DECLARE_NAMED_TUPLE_VAR(nuple, 8, _9)
DECLARE_NAMED_TUPLE_VAR(nuple, 9, _10)
DECLARE_NAMED_TUPLE_VAR(nuple, 10, _11)
DECLARE_NAMED_TUPLE_VAR(nuple, 11, _12)
DECLARE_NAMED_TUPLE_VAR(nuple, 12, _13)
DECLARE_NAMED_TUPLE_VAR(nuple, 13, _14)
DECLARE_NAMED_TUPLE_VAR(nuple, 14, _15)
DECLARE_NAMED_TUPLE_VAR(nuple, 15, _16)
DECLARE_NAMED_TUPLE_VAR(nuple, 16, _17)
DECLARE_NAMED_TUPLE_VAR(nuple, 17, _18)
DECLARE_NAMED_TUPLE_VAR(nuple, 18, _19)
DECLARE_NAMED_TUPLE_VAR(nuple, 19, _20)
DECLARE_NAMED_TUPLE_VAR(nuple, 20, _21)
DECLARE_NAMED_TUPLE_VAR(nuple, 21, _22)

// Increment and decrement
DECLARE_STRUPLE_SYMBOL(operator_increment) // ++a, a++
DECLARE_STRUPLE_SYMBOL(operator_decrement) // --a, a--

// Member access
DECLARE_STRUPLE_SYMBOL(operator_pointer) // a->b
DECLARE_STRUPLE_SYMBOL(operator_deref) // *a
DECLARE_STRUPLE_SYMBOL(operator_address_of) // &a
DECLARE_STRUPLE_SYMBOL(operator_subscript) // a[b]

// Arithemtic
DECLARE_STRUPLE_SYMBOL(operator_plus) // a + b
DECLARE_STRUPLE_SYMBOL(operator_minus) // a - b
DECLARE_STRUPLE_SYMBOL(operator_mul) // a * b
DECLARE_STRUPLE_SYMBOL(operator_div) // a / b
DECLARE_STRUPLE_SYMBOL(operator_mod) // a % b
DECLARE_STRUPLE_SYMBOL(operator_amp) // a & b
DECLARE_STRUPLE_SYMBOL(operator_pipe) // a | b
DECLARE_STRUPLE_SYMBOL(operator_hat) // a ^ b
DECLARE_STRUPLE_SYMBOL(operator_tilde) // a ~ b
DECLARE_STRUPLE_SYMBOL(operator_lshift) // a << b
DECLARE_STRUPLE_SYMBOL(operator_rshift) // a >> b

// Logical
DECLARE_STRUPLE_SYMBOL(operator_and) // a && b
DECLARE_STRUPLE_SYMBOL(operator_or) // a || b
DECLARE_STRUPLE_SYMBOL(operator_not) // !a

// Comparison
DECLARE_STRUPLE_SYMBOL(operator_eq) // a == b
DECLARE_STRUPLE_SYMBOL(operator_not_eq) // a != b
DECLARE_STRUPLE_SYMBOL(operator_less) // a < b
DECLARE_STRUPLE_SYMBOL(operator_less_eq) // a <= b
DECLARE_STRUPLE_SYMBOL(operator_greater) // a > b
DECLARE_STRUPLE_SYMBOL(operator_greater_eq) // a >= b
DECLARE_STRUPLE_SYMBOL(operator_comp) // a <=> b

// Assignment
DECLARE_STRUPLE_SYMBOL(operator_assign) // a = b
DECLARE_STRUPLE_SYMBOL(operator_minus_assign) // a -= b
DECLARE_STRUPLE_SYMBOL(operator_plus_assign) // a += b
DECLARE_STRUPLE_SYMBOL(operator_mul_assign) // a *= b
DECLARE_STRUPLE_SYMBOL(operator_div_assign) // a /= b
DECLARE_STRUPLE_SYMBOL(operator_mod_assign) // a %= b
DECLARE_STRUPLE_SYMBOL(operator_lshift_assign) // a <<= b
DECLARE_STRUPLE_SYMBOL(operator_rshift_assign) // a >>= b
DECLARE_STRUPLE_SYMBOL(operator_amp_assign) // a &= b
DECLARE_STRUPLE_SYMBOL(operator_pipe_assign) // a |= b
DECLARE_STRUPLE_SYMBOL(operator_hat_assign) // a ^= b

// Other
DECLARE_STRUPLE_SYMBOL(operator_type) // (type)a
DECLARE_STRUPLE_SYMBOL(operator_paren) // a(b)
DECLARE_STRUPLE_SYMBOL(operator_comma) // a,b
DECLARE_STRUPLE_SYMBOL(operator_ctor) // A()
DECLARE_STRUPLE_SYMBOL(operator_dtor) // a.~A()
DECLARE_STRUPLE_SYMBOL(swap) // swap(a, b)

// Reserved keywords
DECLARE_STRUPLE_SYMBOL(_) // Reserved member name for library generated clazzes
DECLARE_STRUPLE_SYMBOL(size)
DECLARE_STRUPLE_SYMBOL(index)
DECLARE_STRUPLE_SYMBOL(variants)

namespace CLAZZ_NS {
// Get by index
template<size_t I, Clazz C>
requires (Variable<typename meta_values_t<C>::template index_symbol_t<I>>)
constexpr inline decltype(auto) get(C&& c) noexcept {
    using symbol_t = typename meta_values_t<C>::template index_symbol_t<I>;
    return symbol_info<symbol_t>::invoke(std::forward<C>(c));
}

template<size_t I, Clazz C>
requires (StaticValue<typename meta_values_t<C>::template index_symbol_t<I>>)
constexpr inline decltype(auto) get(C&& c) noexcept {
    using symbol_t = typename clazz_info<C>::template index_symbol_t<I>;
    return symbol_info<symbol_t>::invoke();
}

// Get variable by tag
template<Tag tag, Clazz C>
requires (clazz_info<C>::template has_name<tag> )
      && Variable<typename clazz_info<C>::template tag_symbol_t<tag>>
constexpr inline decltype(auto) get(C&& c) noexcept {
    using symbol_t = typename clazz_info<C>::template tag_symbol_t<tag>;
    return symbol_info<symbol_t>::invoke(std::forward<C>(c));
}

// Get static value by tag
template<Tag tag, Clazz C>
requires (clazz_info<C>::template has_name<tag> )
      && StaticValue<typename clazz_info<C>::template tag_symbol_t<tag>>
constexpr inline auto get(C&& c) noexcept {
    using symbol_t = typename clazz_info<C>::template tag_symbol_t<tag>;
    return symbol_info<symbol_t>::invoke();
}

// Get a super clazz by indices
template<size_t... I, Clazz C>
requires (sizeof...(I) > 1)
constexpr inline auto get(C&& c) noexcept {
    using clazz_t = clazz<typename meta_values_t<C>::template index_symbol_t<I>...>;
    return clazz_t(std::forward<C>(c));
}

// Get a super clazz by Tags
template<Tag... Tags, Clazz C>
requires (sizeof...(Tags) > 1)
constexpr inline auto get(C&& c) noexcept {
    using clazz_t = clazz<typename meta_values_t<C>::template tag_symbol_t<Tags>...>;
    return clazz_t(std::forward<C>(c));
}

// Tie a super clazz by indices
template<size_t... I, Clazz C>
requires (sizeof...(I) > 1)
constexpr inline auto tie(C&& c) noexcept {
    using clazz_t = clazz<
        std::conditional_t<
            StaticValue<typename meta_values_t<C>::template index_symbol_t<I>>, 
            typename meta_values_t<C>::template index_symbol_t<I>,
            typename tag_info<typename meta_values_t<C>::template index_tag_t<I>>::template var_t<decltype(get<I>(std::forward<C>(c)))>
        >...
    >;
    return clazz_t(std::forward<C>(c));
}

// Tie a super clazz by Tags
template<Tag... Tags, Clazz C>
requires (sizeof...(Tags) > 1 && (clazz_info<C>::template has_name<Tags> && ...))
constexpr inline auto tie(C&& c) noexcept {
    using clazz_t = clazz<
        std::conditional_t<
            StaticValue<typename meta_values_t<C>::template tag_symbol_t<Tags>>, 
            typename meta_values_t<C>::template tag_symbol_t<Tags>,
            typename tag_info<Tags>::template var_t<decltype(get<Tags>(std::forward<C>(c)))>
        >...
    >;
    return clazz_t(std::forward<C>(c));
}

// Try get values by tag
template<Tag tag, Clazz C, class T>
requires (!clazz_info<C>::template has_name<tag> )
constexpr inline auto get_or(C&&, T&& in) noexcept {
    return std::forward<T>(in);
}

template<Tag tag, Clazz C, class T>
requires (clazz_info<C>::template has_name<tag> )
      && Value<typename clazz_info<C>::template tag_symbol_t<tag>>
constexpr inline decltype(auto) get_or(C&& c, T&&) noexcept {
    return get<tag>(std::forward<C>(c));
}

template<Value... X, class F>
constexpr inline auto map(const clazz<X...>& c, F&& f) {
    using clazz_t = clazz <
        typename symbol_tag_info<X>::template var_t<std::invoke_result_t<F, decltype(get<symbol_tag_t<X>>(c))>>...
    >;
    return clazz_t{std::invoke(std::forward<F>(f), get<symbol_tag_t<X>>(c))...};
}

template<Value... X, class F>
constexpr inline auto map(clazz<X...>&& c, F&& f) {
    using clazz_t = clazz <
        typename symbol_tag_info<X>::template var_t<std::invoke_result_t<F, decltype(get<symbol_tag_t<X>>(c))>>...
    >;
    return clazz_t{std::invoke(std::forward<F>(f), get<symbol_tag_t<X>>(std::move(c)))...};
}

auto it = []<class T>(T&& in) { return std::forward<T>(in); };
auto _ = [](auto&&){};

namespace detail {
    template<Pod C>
    struct clazz_map_helper;

    template<class... X>
    struct clazz_map_helper<clazz<X...>> {
        template<class... T>
        using invokes_apply_t = prune_clazz_t <
            std::conditional_t <
                std::is_same_v<T, void>,
                void,
                typename symbol_tag_info<X>::template var_t<T>
            >...
        >;
    };
}

template<Value... X, class... F>
requires (sizeof...(X) == sizeof...(F))
constexpr inline auto map(const clazz<X...>& c, F&&... f) {
    // using invokes_t = std::tuple<std::invoke_result_t<F, decltype(get<symbol_tag_t<X>>(c))>...>;
    // using clazz_t = apply_tuple_t<invokes_t, typename detail::clazz_map_helper<X...>::template invokes_apply_t>;
    using clazz_t = clazz <
        std::conditional_t <
            std::is_same_v<std::invoke_result_t<F, decltype(get<symbol_tag_t<X>>(c))>, void>,
            std::tuple<>,
            typename symbol_tag_info<X>::template var_t<std::invoke_result_t<F, decltype(get<symbol_tag_t<X>>(c))>>
        >...
    >;

    return [&]<Value... V, ArgHolder... Fs>(clazz_info<clazz<V...>>, 
                                            detail::arg::holder_set<Fs...>&& fs) {
        return clazz_t{std::invoke(
            std::get<0>(std::move(fs).template get_tuple<symbol_tag_t<V>>()), 
            get<symbol_tag_t<V>>(c))...};
    }(clazz_info<clazz_t>{}, detail::arg::holder_set{symbol_tag_info<X>::arg(std::forward<F>(f))...});
}

template<Tag... Tags, Clazz C, class... F>
requires (clazz_info<C>::template has_name<Tags> && ...)
      && (!ArgHolder<F> && ...)
constexpr inline auto xmap(C&& c, F&&... f) {
    return xmap(std::forward<C>(c), tag_info<Tags>::arg(std::forward<F>(f))...);
}

template<Clazz C, ArgHolder... As>
requires (clazz_info<C>::template compatible_arg<As> && ...)
constexpr auto xmap(C&& c, As&&... phs) {
    const auto lam = [&]<class... Names, class... Rest>
        (detail::arg::holder<type_list<Names...>, Rest...>&& argholder) {
            static_assert((Tag<Names> && ...)); // TODO: Add back as template requirement when GCC fixes bug
            return std::invoke(std::get<0>(std::move(argholder).as_tuple()), 
                               get<Names>(std::forward<C>(c))...);
        };
    return clazz{tag_info<typename As::tag_t>::arg(lam(std::move(phs)))...};
}

template<Clazz C, Tag... Tags, class... Funcs, class... Ts>
requires (clazz_info<C>::template has_name<Tags> && ...)
constexpr auto imap(C&& c, detail::arg::holder<Tags, Funcs, Ts>&&... phs) {
    // using invokes_t = std::tuple<std::invoke_result_t<Funcs, decltype(get<Tags>(std::forward<C>(c)))>...>;
    // using clazz_t = apply_tuple_t<invokes_t, typename detail::clazz_map_helper<X...>::template invokes_apply_t>;
    using clazz_t = clazz <
        std::conditional_t <
            std::is_same_v<std::invoke_result_t<Funcs, decltype(get<Tags>(std::forward<C>(c)))>, void>,
            std::tuple<>,
            typename tag_info<Tags>::template var_t<std::invoke_result_t<Funcs, decltype(get<Tags>(std::forward<C>(c)))>>
        >...
    >;

    return [&]<Value... V, ArgHolder... Fs>(clazz_info<clazz<V...>>, 
                                            detail::arg::holder_set<Fs...>&& fs) {
        return clazz_t{std::invoke(
            std::get<0>(std::move(fs).template get_tuple<symbol_tag_t<V>>()), 
            get<symbol_tag_t<V>>(c))...};
    }(clazz_info<clazz_t>{}, detail::arg::holder_set{std::move(phs)...});
}

template<Symbol... X>
struct clazz_info<clazz<X...>> {
    template<Symbol S>
    using info = symbol_info<symbol_element_t<clazz<X...>, S>>;
    // using info = symbol_info<S>;

    using clazz_t = clazz<X...>;
    using meta_clazz_t = meta_clazz<X...>;

    static constexpr size_t size = sizeof...(X);

    // TODO: Avoid using offsetof
    static constexpr auto offsets() {
        return std::array{symbol_tag_info<X>::template offset<clazz_t>...};
        // return std::array{std::size_t(static_cast<symbol_element_t<clazz_t, X>*>(static_cast<clazz_t*>(nullptr))) - std::size_t(static_cast<clazz_t*>(nullptr))...};
    }

    template<Symbol S>
    static constexpr bool has_symbol = (std::is_same_v<info<S>, info<X>> || ...);
    
    template<class Struct>
    static constexpr bool importable = ((EmptySymbol<X> || symbol_tag_info<X>::template has_mem_var<std::decay_t<Struct>>) && ...);

    template<class Struct>
    static constexpr bool partially_importable = ((EmptySymbol<X> || symbol_tag_info<X>::template has_mem_var<std::decay_t<Struct>> || info<X>::has_default_ctor) && ...);

    template<Declaration D>
    static constexpr bool has_dec = (info<X>::template has_dec<D> || ...);
    
    template<Declaration... Decs>
    static constexpr bool implements = (has_dec<Decs> && ...);
    
    template<Symbol S>
    static constexpr bool has_dec_of = (info<X>::template shares_dec<S> || ...);

    template<Symbol... S>
    static constexpr bool implements_of = (has_dec_of<S> && ...);

    template<Declaration D>
    static constexpr bool has_co_dec = (info<X>::template has_wider_dec<D> || ...);
    
    template<Declaration... Decs>
    static constexpr bool co_implements = (has_co_dec<Decs> && ...);

    template<Symbol S>
    static constexpr bool has_co_dec_of = (info<X>::template shares_wider_dec<S> || ...);

    template<Symbol... S>
    static constexpr bool co_implements_of = (has_co_dec_of<S> && ...);

    template<Declaration D>
    static constexpr bool has_contra_dec = (info<X>::template has_narrower_dec<D> || ...);
    
    template<Declaration... Decs>
    static constexpr bool contra_implements = (has_contra_dec<Decs> && ...);

    template<Symbol S>
    static constexpr bool has_contra_dec_of = (info<X>::template shares_narrower_dec<S> || ...);

    template<Symbol... S>
    static constexpr bool contra_implements_of = (has_contra_dec_of<S> && ...);

    template<Tag tag>
    static constexpr size_t tag_index = index_of<tag, X...>::value;

    template<Tag tag>
    using tag_symbol_t = type_at_t<tag_index<tag>, X...>;

    template<size_t I>
    using index_symbol_t = type_at_t<I, X...>;

    template<size_t I>
    using index_element_t = typename info<index_symbol_t<I>>::value_t;

    template<size_t I>
    using index_tag_t = symbol_tag_t<index_symbol_t<I>>;

    using tags_info_t = tags_info<symbol_tag_t<X>...>;

    template<Symbol... Ts>
    using clazz_info_wrapper = clazz_info<clazz<Ts...>>;

    using pod_info_t = bind_to_t<clazz_info_wrapper, flatten_tuples_t<assign_tuple_t<symbol_tuple_t<X>, X>...>>;
    static constexpr bool is_pod = (DataSymbol<X> && ...);

    using values_info_t = bind_to_t<clazz_info_wrapper, flatten_tuples_t<std::conditional_t<Value<X>, type_list<X>, type_list<>>...>>;
    static constexpr bool is_values = (Value<X> && ...);
    
    using variables_info_t = bind_to_t<clazz_info_wrapper, flatten_tuples_t<std::conditional_t<Variable<X>, type_list<X>, type_list<>>...>>;
    static constexpr bool is_variables = (Variable<X> && ...);

    static constexpr bool is_default_assignable = ((!Variable<X> || 
        std::is_assignable_v<std::add_lvalue_reference_t<symbol_value_t<X>>, 
                             std::add_lvalue_reference_t<symbol_value_t<X>>>) && ...);

    template<Tag tag>
    static constexpr bool has_name = (symbol_info<X>::template has_name<tag> || ...);
    
    template<Symbol S>
    static constexpr bool shares_name = (symbol_info<X>::template shares_name<S> || ...);

    template<ArgHolder A>
    static constexpr bool compatible_arg = std::decay_t<A>::template shares_subset_names<X...>;

    template<Tag tag>
    static constexpr bool is_clazz_var = Variable<tag_symbol_t<tag>> && Clazz<symbol_value_t<tag_symbol_t<tag>>>;
};

static_assert(sizeof(clazz_info<clazz<>>) == sizeof(std::index_sequence<>));
static_assert(sizeof(clazz_info<clazz<var::_<int>>>) == sizeof(std::index_sequence<0>));

template<Symbol... X>
struct meta_clazz : clazz_info<clazz<X...>> {
    using clazz_t = clazz<X...>;
    using tuple_t = typename clazz_t::tuple_t;
    using meta_clazz_t = meta_clazz;
    using clazz_info_t = clazz_info<clazz_t>;

#define info_v clazz_info_t::template
#define info_t typename clazz_info_t::template
    
    static_assert(sizeof(tuple_t) == sizeof(clazz_t));
    
    template<EmptySymbol... Ts>
    using with_symbols = meta_clazz<X..., Ts...>;

    template<DataSymbol... Ts>
    using with_data = meta_clazz<X..., Ts...>;

#ifdef CLAZZ_MEM_REV_ORDER
    template<DataSymbol T>
    using with_data_at_mem_front = meta_clazz<X..., T>;
#else
    template<DataSymbol T>
    using with_data_at_mem_front = meta_clazz<T, X...>;
#endif

    union {
        clazz_t fields;
        tuple_t tuple;
    };
    
    constexpr ~meta_clazz() noexcept {
        fields.~clazz_t();
    }
    
    template<class... Ts>
    constexpr meta_clazz(Ts&&... in) noexcept : fields(std::forward<Ts>(in)...) {
        static_assert(sizeof(tuple_t) == sizeof(meta_clazz_t));
    }

    template<class T>
    requires std::same_as<tuple_t, std::decay_t<T>>
    constexpr meta_clazz(T&& tup) noexcept : tuple(std::forward<T>(tup)) {
        static_assert(sizeof(tuple_t) == sizeof(meta_clazz_t));
    }

    template<class Struct>
    requires (!Clazz<Struct> && info_v importable<Struct>)
    static constexpr clazz_t import(Struct&& in) noexcept {
        return make_clazz(indices_of_vars_t<X...>{}, std::forward<Struct>(in));
    }

    template<class Struct>
    requires (!Clazz<Struct> && info_v partially_importable<Struct>)
    static constexpr clazz_t import_with_defaults(Struct&& in) noexcept { 
        return make_clazz_with_defaults(indices_of_vars_t<X...>{}, std::forward<Struct>(in));
    }

    template<class Struct>
    requires (!Clazz<Struct>)
    static constexpr auto partial_import(Struct&& in) noexcept {
        return make_partial_clazz(indices_of_vars_t<X...>{}, std::forward<Struct>(in));
    }

private:
    template<size_t... I, class Struct>
    static constexpr auto make_clazz(std::index_sequence<I...>, Struct&& in) noexcept {
        return clazz_t(symbol_info<info_t index_symbol_t<I>>::extract_compatible_value(std::forward<Struct>(in))...);
    }

    template<size_t... I, class Struct>
    static constexpr auto make_clazz_with_defaults(std::index_sequence<I...>, Struct&& in) noexcept {
        using extractible_tuple_t = flatten_tuples_t<std::conditional_t<tag_info<info_t index_tag_t<I>>::template has_mem_var<std::decay_t<Struct>>, type_list<std::index_sequence<I>>, type_list<>>...>;
        using extractible_indices_t = bind_to_t<flatten_indices_t, extractible_tuple_t>;
        return make_clazz_with_defaults_2(extractible_indices_t{}, std::forward<Struct>(in));
    }

    template<size_t... I, class Struct>
    static constexpr auto make_clazz_with_defaults_2(std::index_sequence<I...>, Struct&& in) noexcept {
        return clazz_t(tag_info<info_t index_tag_t<I>>::arg = symbol_info<info_t index_symbol_t<I>>::extract_compatible_value(std::forward<Struct>(in))...);
    }

    template<size_t... I, class Struct>
    static constexpr auto make_partial_clazz(std::index_sequence<I...>, Struct&& in) noexcept {
        using extractible_symbols_t = flatten_tuples_t<std::conditional_t<tag_info<info_t index_tag_t<I>>::template has_mem_var<std::decay_t<Struct>>, type_list<info_t index_symbol_t<I>>, type_list<>>...>;
        using partial_meta_clazz_t = bind_to_t<meta_clazz, extractible_symbols_t>;
        return partial_meta_clazz_t::import(std::forward<Struct>(in));
    }

public:
    constexpr clazz_t* operator->() noexcept {
        return &fields;
    }
    
    constexpr const clazz_t* operator->() const noexcept {
        return &fields;
    }
    
    constexpr operator tuple_t&() noexcept {
        return tuple;
    }
    
    constexpr operator const tuple_t&() const noexcept {
        return tuple;
    }
    
    constexpr operator tuple_t&&() && noexcept {
        return std::move(tuple);
    }

    template<class... Tags>
    using super_meta_clazz_t = meta_clazz<info_t tag_symbol_t<Tags>...>;

    using meta_pod_t = bind_to_t<meta_clazz, flatten_tuples_t<assign_tuple_t<symbol_tuple_t<X>, X>...>>;
    static constexpr bool is_pod = (DataSymbol<X> && ...);

    using meta_values_t = bind_to_t<meta_clazz, flatten_tuples_t<std::conditional_t<Value<X>, type_list<X>, type_list<>>...>>;
    static constexpr bool is_values = (Value<X> && ...);

    meta_pod_t& as_pod() & {
        return union_cast<meta_pod_t&>(*this);
    }

    const meta_pod_t& as_pod() const & {
        return union_cast<const meta_pod_t&>(*this);
    }

    meta_pod_t&& as_pod() && {
        return union_cast<meta_pod_t&&>(*this);
    }

    // Get by index
    template<size_t I>
    constexpr inline decltype(auto) get() & {
        return CLAZZ_NS::get<I>(fields);
    }

    template<size_t I>
    constexpr inline decltype(auto) get() const & {
        return CLAZZ_NS::get<I>(fields);
    }

    template<size_t I>
    constexpr inline decltype(auto) get() && {
        return CLAZZ_NS::get<I>(std::move(fields));
    }

    template<Tag tag>
    static constexpr bool has_name = (symbol_info<X>::template has_name<tag> || ...);

    // Get variable by tag
    template<Tag tag>
    constexpr inline decltype(auto) get() & noexcept {
        return CLAZZ_NS::get<tag>(fields);
    }

    template<Tag tag>
    constexpr inline decltype(auto) get() const & noexcept {
        return CLAZZ_NS::get<tag>(fields);
    }

    template<Tag tag>
    constexpr inline decltype(auto) get() && noexcept {
        return CLAZZ_NS::get<tag>(std::move(fields));
    }

    // Get static value by tag
    template<Tag tag>
    constexpr inline auto get() const noexcept {
        return CLAZZ_NS::get<tag>(fields);
    }

    // Try get values by tag
    template<Tag tag, class T>
    requires (!info_v has_name<tag>)
    constexpr inline auto get_or(T&& in) const noexcept {
        return std::forward<T>(in);
    }

    template<Tag tag, class T>
    requires (info_v has_name<tag> && Value<info_t tag_symbol_t<tag>>)
    constexpr inline decltype(auto) get_or(T&&) & noexcept {
        return get<tag>();
    }

    template<Tag tag, class T>
    requires (info_v has_name<tag> && Value<info_t tag_symbol_t<tag>>)
    constexpr inline decltype(auto) get_or(T&&) const & noexcept {
        return get<tag>();
    }

    template<Tag tag, class T>
    requires (info_v has_name<tag> && Value<info_t tag_symbol_t<tag>>)
    constexpr inline decltype(auto) get_or(T&&) && noexcept {
        return get<tag>();
    }

    template<Tag tag>
    static constexpr bool is_clazz_var = Variable<info_t tag_symbol_t<tag>> 
                                      && Clazz<symbol_value_t<info_t tag_symbol_t<tag>>>;

    // Matching
    template<class F>
    constexpr inline void for_each_as_var(F&& f) {
        ([&] { if constexpr (Variable<X>)
            for_each_as_var<symbol_tag_t<X>>(std::forward<F>(f));
        }(), ...);
    }

    template<class... Tags, class F>
    requires (sizeof...(Tags) > 0 && (Variable<info_t tag_symbol_t<Tags>> && ...))
    constexpr inline void for_each_as_var(F&& f) {
        (std::invoke(std::forward<F>(f), static_cast<info_t tag_symbol_t<Tags>&>(fields)), ...);
    }

    template<class F>
    constexpr inline void for_each_var(F&& f) {
        ([&] { if constexpr (Variable<X>)
            for_each<symbol_tag_t<X>>(std::forward<F>(f));
        }(), ...);
    }

    template<class F>
    constexpr inline void for_each_value(F&& f) {
        ([&] { if constexpr (Value<X>)
            for_each<symbol_tag_t<X>>(std::forward<F>(f));
        }(), ...);
    }

    template<class... Tags, class F>
    requires (sizeof...(Tags) > 0 && (Value<info_t tag_symbol_t<Tags>> && ...))
    constexpr inline void for_each(F&& f) {
        (std::invoke(std::forward<F>(f), get<Tags>()), ...);
    }

    template<class... tag>
    constexpr inline clazz<info_t tag_symbol_t<tag>...> super_clazz() const & {
        return *this;
    }

    template<class... tag>
    constexpr inline clazz<info_t tag_symbol_t<tag>...> super_clazz() && {
        return std::move(*this);
    }

    template<class F, class... Ts>
    requires (std::is_invocable<F, clazz_t, Ts...>::value)
    constexpr inline decltype(auto) call(F&& f, Ts&&... params) {
        return std::invoke(std::forward<F>(f), fields, std::forward<Ts>(params)...);
    }

    template<class F, class... Ts>
    requires (std::is_invocable<F, const clazz_t, Ts...>::value)
    constexpr inline decltype(auto) call(F&& f, Ts&&... params) const {
        return std::invoke(std::forward<F>(f), fields, std::forward<Ts>(params)...);
    }

    template<class F>
    requires (Variable<X> && ...)
    constexpr inline auto map(F&& f) const {
        return CLAZZ_NS::map(fields, std::forward<F>(f));
    }

#undef info_t
#undef info_v
};

template<class, class>
struct set_difference;

template<template<class...> class W1, template<class...> class W2, class... Ts, class... Us>
struct set_difference<W1<Ts...>, W2<Us...>> {
    using type = bind_to_t <
        W1,
        flatten_tuples_t <
            std::conditional_t <
                is_one_of<Ts, Us...>::value,
                type_list<>, 
                type_list<Ts>
            >...
        >
    >;
};

template<class T, class U>
using set_difference_t = typename set_difference<T, U>::type;

template<Clazz, Clazz>
struct shared_vars;

template<Tag... Tags>
struct clazz_comparator {
    // Tags in C which are not these Tags
    template<Clazz C>
    using tags_diff = set_difference_t<typename meta_values_t<C>::tags_info_t, tags_info<Tags...>>;

    template<Clazz L, Clazz R>
    requires (meta_values_t<L>::template has_name<Tags> && ...) 
          && (meta_values_t<R>::template has_name<Tags> && ...)
    static constexpr bool weak_less_than(const L& l, const R& r) {
        return std::forward_as_tuple(get<Tags>(l)..., meta_values_t<L>::size) 
             < std::forward_as_tuple(get<Tags>(r)..., meta_values_t<R>::size);
    }
    
    template<Clazz L, Clazz R>
    requires (meta_values_t<L>::template has_name<Tags> && ...) 
          && (meta_values_t<R>::template has_name<Tags> && ...)
    static constexpr bool strong_less_than(const L& l, const R& r) {
        if constexpr (meta_values_t<L>::size != meta_values_t<R>::size)
            // When they have a different number of value fields, weak_less_than is enough to disambiguate
            return weak_less_than(l, r);
        else {
            // Disambiguate by the lexicographical order of the missing fields of each clazz
            constexpr auto ldiffnames = tags_diff<L>::names_array;
            constexpr auto rdiffnames = tags_diff<R>::names_array;
            // Equality iff comparing all value fields of both clazzes
            static_assert(ldiffnames != rdiffnames || tags_diff<L>::empty, 
                "Unable to determine a strong strict inequality: only a subset of the mutually "
                "shared fields of two structurally indistinguishable clazzes are being compared.");
            // TODO: sort the names before ascertaining lexicographical order
            constexpr auto lless = ldiffnames < rdiffnames ? 0 : 1;
            return std::forward_as_tuple(get<Tags>(l)..., lless) 
                 < std::forward_as_tuple(get<Tags>(r)..., 1);
        }
    }
    
    template<Clazz L, Clazz R>
    requires (meta_values_t<L>::template has_name<Tags> && ...) 
          && (meta_values_t<R>::template has_name<Tags> && ...)
    static constexpr bool weak_eqivalent(const L& l, const R& r) {
        // Can only be equivalent if L and R have the same number of value fields
        if constexpr (meta_values_t<L>::size == meta_values_t<R>::size)
            return std::forward_as_tuple(get<Tags>(l)...) == std::forward_as_tuple(get<Tags>(r)...);
        else
            return false;
    }
    
    template<Clazz L, Clazz R>
    requires (meta_values_t<L>::template has_name<Tags> && ...) 
          && (meta_values_t<R>::template has_name<Tags> && ...)
    static constexpr bool strong_equals(const L& l, const R& r) {
        // Can only be equal if all value fields are included in comparison (which implies two clazzes share all value fields)
        if constexpr (tags_diff<L>::empty && tags_diff<R>::empty)
            return std::forward_as_tuple(get<Tags>(l)...) == std::forward_as_tuple(get<Tags>(r)...);
        else 
            return false;
    }
};

template<Symbol... Ls, Symbol... Rs>
struct shared_vars<clazz<Ls...>, clazz<Rs...>> {
    using L = meta_clazz<Ls...>;
    using R = meta_clazz<Rs...>;
    
    template<Symbol S>
    static constexpr bool r_shares_name = R::template has_name<symbol_tag_t<S>>;
    
    template<Symbol S>
    using r_symbol_of = typename R::template tag_symbol_t<symbol_tag_t<S>>;

    template<Symbol S>
    static constexpr bool l_value_shared_by_r = Value<S> && r_shares_name<S> && Value<r_symbol_of<S>>;
        
    using l_shared_symbols_tuple_t = flatten_tuples_t<
        std::conditional_t<l_value_shared_by_r<Ls>,
            type_list<Ls>,
            type_list<>
        >...
    >;

    using l_shared_clazz_t = bind_to_t<clazz, l_shared_symbols_tuple_t>;
    
    using l_shared_names_tuple_t = map_tuple_t<l_shared_symbols_tuple_t, symbol_tag_t>;
    
    using l_clazz_comparator_t = bind_to_t<clazz_comparator, l_shared_names_tuple_t>;
};

template<Clazz L, Clazz R>
using l_clazz_comparator_t = typename shared_vars<L, R>::l_clazz_comparator_t;

/* Objective less than comparison between two clazzes
 * Field order for the comparison operation is defined by the order of the fields in the clazz:
 * (1) which has less value fields, or else the clazz:
 * (2) which has the names of the shared value fields in a lower lexicographical order
 *
 * When values of the respective shared value fields are equal between two instances, the instance
 * of the clazz:
 * (1) which has fewer value fields, or else the clazz: 
 * (2) with the names of mutually exclusive value fields in a lower lexicographical order
 * is seen as less than the other.
 *
 * Examples:
 *
 * For operation between two clazzes, LHS and RHS, with the value fields named: LHS{b, a}, RHS{a, b, c}
 * (1) lhs < rhs => LHS defines order of comparison, i.e. field b compared first then a 
 *                  c is disregarded from comparison, as it is mutually exclusive between two clazzes
 * Why: LHS has fewer value fields
 * If lhs.a == rhs.a && lhs.b == rhs.b, then lhs < rhs
 * Why: LHS has fewer value fields
 *
 * For operation between two clazzes, LHS and RHS, with the value fields named: LHS{e, b, a, c} and RHS{e, a, b, d} 
 * (2) lhs < rhs => RHS defines order of comparison, i.e. field e compared first then a, then b
 *                  c and d are disregarded, as it is mutually exclusive between two clazzes
 * Why: LHS and RHS have the same number of value fields
 *  BUT LHS and RHS share fields named {a, b, e}
 *  AND the order of shared value fields on RHS are lexicographically less than the order of shared value fields on RHS
 *  i.e. ["e", "a", "b"] < ["e", "b", "a"]
 * If lhs.e == rhs.e && lhs.a == rhs.a && lhs.b == rhs.b, then lhs < rhs
 * Why: LHS and RHS have the same number of value fields, but the mutually exclusive fields ["c"] < ["d"]
 */
template<Clazz L, Clazz R>
constexpr bool strong_less_than(const L& l, const R& r) {
    constexpr auto lsize = clazz_info<L>::values_info_t::size;
    constexpr auto rsize = clazz_info<R>::values_info_t::size;
    if constexpr (lsize < rsize)
        return l_clazz_comparator_t<L, R>::strong_less_than(l, r);
    else if constexpr (lsize > rsize)
        return l_clazz_comparator_t<R, L>::strong_less_than(l, r);
    else {
        using lshared = typename shared_vars<L, R>::l_shared_clazz_t;
        using rshared = typename shared_vars<R, L>::l_shared_clazz_t;
        if constexpr (clazz_info<lshared>::tags_info_t::names_array <= clazz_info<rshared>::tags_info_t::names_array)
            return l_clazz_comparator_t<L, R>::strong_less_than(l, r);
        else
            return l_clazz_comparator_t<R, L>::strong_less_than(l, r);
    }
}

template<Tag... Tags, Clazz L, Clazz R>
requires (sizeof...(Tags) > 0)
constexpr bool strong_less_than(const L& l, const R& r) {
    return clazz_comparator<Tags...>::strong_less_than(l, r);
}

template<MetaClazz M1, MetaClazz M2>
constexpr bool operator<(const M1& l, const M2& r) {
    return strong_less_than(l.fields, r.fields);
}

template<Clazz L, Clazz R>
constexpr bool strong_equals(const L& l, const R& r) {
    return l_clazz_comparator_t<L, R>::strong_equals(l, r);
}

template<Tag... Tags, Clazz L, Clazz R>
requires (sizeof...(Tags) > 0)
constexpr bool strong_equals(const L& l, const R& r) {
    return clazz_comparator<Tags...>::strong_equals(l, r);
}

template<MetaClazz M1, MetaClazz M2>
constexpr bool operator==(const M1& l, const M2& r) {
    return strong_equals(l.fields, r.fields);
}

template<Tag... Tags, class Struct>
requires (sizeof...(Tags) > 0) && (tag_info<Tags>::template has_mem_var<std::decay_t<Struct>> && ...)
static constexpr inline auto make_clazz(Struct&& s) {
    using meta_clazz_t = meta_clazz<typename tag_info<Tags>::template mem_var_t<std::decay_t<Struct>>...>;
    return meta_clazz_t::import(std::forward<Struct>(s));
}

namespace detail {
    // CRC32 Table (zlib polynomial)
    static constexpr uint32_t crc_table[256] = {
        0x00000000L, 0x77073096L, 0xee0e612cL, 0x990951baL, 0x076dc419L,
        0x706af48fL, 0xe963a535L, 0x9e6495a3L, 0x0edb8832L, 0x79dcb8a4L,
        0xe0d5e91eL, 0x97d2d988L, 0x09b64c2bL, 0x7eb17cbdL, 0xe7b82d07L,
        0x90bf1d91L, 0x1db71064L, 0x6ab020f2L, 0xf3b97148L, 0x84be41deL,
        0x1adad47dL, 0x6ddde4ebL, 0xf4d4b551L, 0x83d385c7L, 0x136c9856L,
        0x646ba8c0L, 0xfd62f97aL, 0x8a65c9ecL, 0x14015c4fL, 0x63066cd9L,
        0xfa0f3d63L, 0x8d080df5L, 0x3b6e20c8L, 0x4c69105eL, 0xd56041e4L,
        0xa2677172L, 0x3c03e4d1L, 0x4b04d447L, 0xd20d85fdL, 0xa50ab56bL,
        0x35b5a8faL, 0x42b2986cL, 0xdbbbc9d6L, 0xacbcf940L, 0x32d86ce3L,
        0x45df5c75L, 0xdcd60dcfL, 0xabd13d59L, 0x26d930acL, 0x51de003aL,
        0xc8d75180L, 0xbfd06116L, 0x21b4f4b5L, 0x56b3c423L, 0xcfba9599L,
        0xb8bda50fL, 0x2802b89eL, 0x5f058808L, 0xc60cd9b2L, 0xb10be924L,
        0x2f6f7c87L, 0x58684c11L, 0xc1611dabL, 0xb6662d3dL, 0x76dc4190L,
        0x01db7106L, 0x98d220bcL, 0xefd5102aL, 0x71b18589L, 0x06b6b51fL,
        0x9fbfe4a5L, 0xe8b8d433L, 0x7807c9a2L, 0x0f00f934L, 0x9609a88eL,
        0xe10e9818L, 0x7f6a0dbbL, 0x086d3d2dL, 0x91646c97L, 0xe6635c01L,
        0x6b6b51f4L, 0x1c6c6162L, 0x856530d8L, 0xf262004eL, 0x6c0695edL,
        0x1b01a57bL, 0x8208f4c1L, 0xf50fc457L, 0x65b0d9c6L, 0x12b7e950L,
        0x8bbeb8eaL, 0xfcb9887cL, 0x62dd1ddfL, 0x15da2d49L, 0x8cd37cf3L,
        0xfbd44c65L, 0x4db26158L, 0x3ab551ceL, 0xa3bc0074L, 0xd4bb30e2L,
        0x4adfa541L, 0x3dd895d7L, 0xa4d1c46dL, 0xd3d6f4fbL, 0x4369e96aL,
        0x346ed9fcL, 0xad678846L, 0xda60b8d0L, 0x44042d73L, 0x33031de5L,
        0xaa0a4c5fL, 0xdd0d7cc9L, 0x5005713cL, 0x270241aaL, 0xbe0b1010L,
        0xc90c2086L, 0x5768b525L, 0x206f85b3L, 0xb966d409L, 0xce61e49fL,
        0x5edef90eL, 0x29d9c998L, 0xb0d09822L, 0xc7d7a8b4L, 0x59b33d17L,
        0x2eb40d81L, 0xb7bd5c3bL, 0xc0ba6cadL, 0xedb88320L, 0x9abfb3b6L,
        0x03b6e20cL, 0x74b1d29aL, 0xead54739L, 0x9dd277afL, 0x04db2615L,
        0x73dc1683L, 0xe3630b12L, 0x94643b84L, 0x0d6d6a3eL, 0x7a6a5aa8L,
        0xe40ecf0bL, 0x9309ff9dL, 0x0a00ae27L, 0x7d079eb1L, 0xf00f9344L,
        0x8708a3d2L, 0x1e01f268L, 0x6906c2feL, 0xf762575dL, 0x806567cbL,
        0x196c3671L, 0x6e6b06e7L, 0xfed41b76L, 0x89d32be0L, 0x10da7a5aL,
        0x67dd4accL, 0xf9b9df6fL, 0x8ebeeff9L, 0x17b7be43L, 0x60b08ed5L,
        0xd6d6a3e8L, 0xa1d1937eL, 0x38d8c2c4L, 0x4fdff252L, 0xd1bb67f1L,
        0xa6bc5767L, 0x3fb506ddL, 0x48b2364bL, 0xd80d2bdaL, 0xaf0a1b4cL,
        0x36034af6L, 0x41047a60L, 0xdf60efc3L, 0xa867df55L, 0x316e8eefL,
        0x4669be79L, 0xcb61b38cL, 0xbc66831aL, 0x256fd2a0L, 0x5268e236L,
        0xcc0c7795L, 0xbb0b4703L, 0x220216b9L, 0x5505262fL, 0xc5ba3bbeL,
        0xb2bd0b28L, 0x2bb45a92L, 0x5cb36a04L, 0xc2d7ffa7L, 0xb5d0cf31L,
        0x2cd99e8bL, 0x5bdeae1dL, 0x9b64c2b0L, 0xec63f226L, 0x756aa39cL,
        0x026d930aL, 0x9c0906a9L, 0xeb0e363fL, 0x72076785L, 0x05005713L,
        0x95bf4a82L, 0xe2b87a14L, 0x7bb12baeL, 0x0cb61b38L, 0x92d28e9bL,
        0xe5d5be0dL, 0x7cdcefb7L, 0x0bdbdf21L, 0x86d3d2d4L, 0xf1d4e242L,
        0x68ddb3f8L, 0x1fda836eL, 0x81be16cdL, 0xf6b9265bL, 0x6fb077e1L,
        0x18b74777L, 0x88085ae6L, 0xff0f6a70L, 0x66063bcaL, 0x11010b5cL,
        0x8f659effL, 0xf862ae69L, 0x616bffd3L, 0x166ccf45L, 0xa00ae278L,
        0xd70dd2eeL, 0x4e048354L, 0x3903b3c2L, 0xa7672661L, 0xd06016f7L,
        0x4969474dL, 0x3e6e77dbL, 0xaed16a4aL, 0xd9d65adcL, 0x40df0b66L,
        0x37d83bf0L, 0xa9bcae53L, 0xdebb9ec5L, 0x47b2cf7fL, 0x30b5ffe9L,
        0xbdbdf21cL, 0xcabac28aL, 0x53b39330L, 0x24b4a3a6L, 0xbad03605L,
        0xcdd70693L, 0x54de5729L, 0x23d967bfL, 0xb3667a2eL, 0xc4614ab8L,
        0x5d681b02L, 0x2a6f2b94L, 0xb40bbe37L, 0xc30c8ea1L, 0x5a05df1bL,
        0x2d02ef8dL
     };

    constexpr uint32_t crc32(const char * str, size_t size) {
        uint32_t result = 0xFFFFFFFF;
        for (size_t i = 0; i < size; ++i)
            result = (result >> 8) ^ crc_table[(result ^ str[i]) & 0xFF];
        result = result ^ 0xFFFFFFFF;
        return result;
    }
}

constexpr uint32_t ctcrc32(std::string_view str) {
    return detail::crc32(&str[0], str.size()) ^ 0xFFFFFFFF;
}

constexpr void hash_combine(size_t& seed, const auto& v);

template<class T>
constexpr size_t hash(const T& value) {
    return std::hash<T>()(value);
}

template<class T>
requires (std::is_convertible<T, std::string_view>::value)
constexpr size_t hash(const T& value) {
    return ctcrc32(value);
}

template<class T>
requires (std::is_trivial<T>::value)
constexpr size_t hash(const T& value) {
    return value;
    // TODO: Add back when GCC adds bit_cast
    // if constexpr (sizeof(T) > sizeof(size_t) && (sizeof(T) % sizeof(size_t)) == 0) {
    //     constexpr size_t array_size = sizeof(T)/sizeof(size_t);
    //     auto array = std::bit_cast<std::array<size_t, array_size>>(value);
    //     size_t seed = 0;
    //     detail::hash_combine_array(seed, array);
    //     return seed;
    // } else if (sizeof(T) == sizeof(size_t))
    //     return std::bit_cast<size_t>(value);
    // else if (sizeof(T) == 32)
    //     return std::bit_cast<uint32_t>(value);
    // else if (sizeof(T) == 16)
    //     return std::bit_cast<uint32_t>(value);
    // else if (sizeof(T) == 8)
    //     return std::bit_cast<uint32_t>(value);
    // else 
    //     static_assert(false_v<>, "Type unsupported");
}

template<class T, class A>
constexpr size_t hash(const std::vector<T, A>& vec) {
    size_t seed = 0;
    for (auto& el : vec) {
        hash_combine(seed, el);
    }
    return seed;
}

template<class T, size_t N>
constexpr size_t hash(const std::array<T, N>& vec) {
    size_t seed = 0;
    for (auto& el : vec) {
        hash_combine(seed, el);
    }
    return seed;
}

template <std::unsigned_integral T>
constexpr T circadd(const T a, const T b) {
    const T t = a+b;
    return t+(t<a);
}

template <std::unsigned_integral T>
constexpr T circdff(const T a, const T b) {
    const T t = a-b;
    return t-(t>a);
}

constexpr void hash_combine(size_t& seed, const auto& v) {
    auto h = clz::hash(v);
    seed = (std::rotr(circadd(seed, h), 49) * 0x9FB21C651E98DF25UL) ^ std::rotr(circdff(seed, h), 24);
    seed ^= seed >> 28;
}

namespace detail {
    template<size_t... I>
    constexpr void hash_combine_clazz(size_t& seed, const Clazz auto& c) {
        (hash_combine(seed, get<I>(c)), ...);
    }

    template<size_t N, class T>
    constexpr void hash_combine_array(size_t& seed, const std::array<T, N>& a) {
        for (auto&& el : a) {
            hash_combine(seed, el);
        }
    }

    template<Clazz C, size_t... I>
    constexpr auto fields_by_name() {
        auto fields = meta_values_t<C>::tags_info_t::names_array;
        auto indices = std::array{I...};
        std::sort(begin(indices), end(indices), [&](auto l, auto r) {
            return fields[l] < fields[r];
        });
        std::sort(begin(fields), end(fields));
        return std::pair{indices, fields}; 
    }
}

// Hash of value fields oblivious to field order (fields are ordered lexicographically before hashing)
// TODO: Make this work with non-pod clazzes
template<Pod C>
static constexpr size_t clazz_id = []<size_t... Is>(std::index_sequence<Is...>) {
    constexpr auto _ = detail::fields_by_name<C, Is...>();
    constexpr auto indices = _.first;
    constexpr auto fields = _.second;

    size_t seed = 0;
    // Hash the alphabetically sorted value field names
    detail::hash_combine_array(seed, fields);
    // Hash the type names of the fields in alphabetical order
    // TODO: Make portable somehow
    constexpr auto var_types = std::array{type_name_v<typename clazz_info<C>::template index_symbol_t<indices[Is]>>...};
    detail::hash_combine_array(seed, var_types);
    return seed;
}(std::make_index_sequence<meta_values_t<C>::size>{});

// TODO: Make this work with non-pod clazzes
template<Pod C>
constexpr size_t hash(const C& c) {
    return [&]<size_t... Is>(std::index_sequence<Is...>) {
        constexpr auto indices = detail::fields_by_name<C, Is...>().first;
    
        // Seed with clazz id
        constexpr size_t id = clazz_id<C>;
        size_t seed = id;
        // Hash values in alphabetical field order
        detail::hash_combine_clazz<indices[Is]...>(seed, c);
        return seed;
    }(std::make_index_sequence<meta_values_t<C>::size>{});
}

namespace detail {
    template<class T>
    constexpr static auto normalise_reference(T&&) -> T&&;
}

// Get rid of weird type annotations, like __restrict
template<class T>
using normalise_reference_t = decltype(detail::normalise_reference(std::declval<T>()));

template<Symbol... X>
struct clazz : struple<clazz<X...>, X...> {
    using struple_t = struple<clazz<X...>, X...>;
    using tuple_t = flatten_tuples_t<symbol_tuple_t<X>...>;
    using meta_clazz_t = meta_clazz<X...>;
    
    static_assert(sizeof(struple_t) == sizeof(tuple_t));
    
    static_assert(assert_unique_symbol_names<X...>::value, "Fields names are not unique.");
                
    // Construct with a struct with the necessary fields of the same name
    template<class Struct>
    requires (!Clazz<Struct> // Is not a class)
          && (symbol_tag_info<X>::template has_mem_var<std::decay_t<Struct>> || ...) // Has at least one matching field
          && ((symbol_tag_info<X>::template has_mem_var<std::decay_t<Struct>>
            || symbol_info<X>::has_default_ctor) && ...)) // Has all matching fields were no default ctors are defined
    constexpr clazz(Struct&& s) noexcept : clazz(meta_clazz_t::import_with_defaults(std::forward<Struct>(s))) {}
    
    // Construct from tuple with the same number of fields as we have data fields
    template<Tuple T>
    requires (std::tuple_size<std::decay_t<T>>::value == clazz_info<clazz>::pod_info_t::size)
    constexpr clazz(T&& tuple) noexcept : clazz(std::make_from_tuple<clazz>(std::forward<T>(tuple))) {}

private:
    struct use_struple_ctor_tag{};
    template<class... Ts>
    constexpr clazz(use_struple_ctor_tag, Ts&&... ins) noexcept : struple_t(std::forward<Ts>(ins)...) {}
    
    struct ctor_delegator_tag{};
    template<class... Ts>
    constexpr clazz(ctor_delegator_tag, Ts&&... ins) noexcept : clazz([&]() -> clazz {
        // Construct with "designated initialiser" style arg syntax
        if constexpr (sizeof...(Ts) > 0 && (ArgHolder<Ts> && ...)) {
            static_assert((std::is_rvalue_reference_v<Ts&&> && ...));
            static_assert((clazz_info<clazz>::template compatible_arg<Ts> && ...), 
                "Initialising a field which doesn't exist in clazz");
            return clazz(use_struple_ctor_tag{}, detail::arg::holder_set<Ts...>(std::forward<Ts>(ins)...));
        }
        // Construct with user defined ctor if it exists
        else if constexpr (clazz_info<clazz>::template has_dec<dec::operator_ctor<clazz(Ts&&...) const, true>>) {
            return clazz::operator_ctor(std::forward<Ts>(ins)...);
        } 
        // Else forward all arguments to struple to deal with
        else {
            return clazz(use_struple_ctor_tag{}, std::forward<Ts>(ins)...);
        }
    }()) {}

public:
    // Delegate all variadic ctor args to a single constructor to simplify ctor resolution
    template<class... Ts>
    constexpr clazz(Ts&&... ins) noexcept : clazz(ctor_delegator_tag{}, std::forward<Ts>(ins)...) {}
    // Copy and move constructors must not be templates
    constexpr clazz(const clazz& other) noexcept : clazz(ctor_delegator_tag{}, other) {}
    constexpr clazz(clazz&& other) noexcept : clazz(ctor_delegator_tag{}, std::move(other)) {}

    constexpr inline auto operator->() {
        if constexpr (clazz_info<clazz>::template has_co_dec<dec::operator_pointer<void()>>)
            return this->operator_pointer();
        else 
            return &meta_clazz_of(*this);
    }

    constexpr inline auto operator->() const {
        if constexpr (clazz_info<clazz>::template has_co_dec<dec::operator_pointer<void() const>>)
            return this->operator_pointer();
        else
            return &meta_clazz_of(*this);
    }

    constexpr decltype(auto) operator=(const clazz& other) & {
        return _operator_assign(*this, other);
    }
    constexpr decltype(auto) operator=(const clazz& other) && {
        return _operator_assign(std::move(*this), other);
    }
    constexpr decltype(auto) operator=(clazz&& other) & {
        return _operator_assign(*this, std::move(other));
    }
    constexpr decltype(auto) operator=(clazz&& other) && {
        return _operator_assign(std::move(*this), std::move(other));
    }

private:
    template<class T, class U>
    constexpr static decltype(auto) _operator_assign(T&& self, U&& other) {
        using dec_t = dec::operator_assign<std::conditional_t<std::is_rvalue_reference_v<T&&>, void(U&&) &&, void(U&&)>>;
        if constexpr (clazz_info<clazz>::template has_co_dec<dec_t>) {
            using return_t = decltype(std::forward<T>(self).operator_assign(std::forward<U>(other)));
            if constexpr (std::is_same_v<void, return_t>) {
                std::forward<T>(self).operator_assign(std::forward<U>(other));
                return std::forward<T>(self);
            } else {
                return std::forward<T>(self).operator_assign(std::forward<U>(other));
            }
        } else if constexpr (clazz_info<clazz>::is_default_assignable) {
            [&self, &other]<class... V>(clazz_info<clazz<V...>>) {
                static_assert((Variable<V> && ...)); // TODO: Add back as template requirement when GCC fixes bug
                // TODO: Check if self should be forwarded or taken as lvalue reference
                if constexpr (!std::is_const_v<std::remove_reference_t<U>>)
                    ((get<symbol_tag_t<V>>(self) =
                        std::forward<normalise_reference_t<symbol_value_t<V>>>(get<symbol_tag_t<V>>(other))), ...);
                else
                    ((get<symbol_tag_t<V>>(self) =
                        std::forward<normalise_reference_t<const symbol_value_t<V>>>(get<symbol_tag_t<V>>(other))), ...);
            }(typename clazz_info<clazz>::variables_info_t{});
            return std::forward<T>(self);
        } else {
            // TODO: Delete function
            // static_assert(false_v<>, "clazz not move-assignable");
        }
    }

public:
    // Custom clazz swap for same type
    friend constexpr void swap(clazz& l, clazz& r) {
        if constexpr (clazz_info<clazz>::template has_dec<dec::swap<void(clazz&)>>) {
            l.swap(r);
        } else {
            [&]<class... V>(clazz_info<clazz<V...>>) {
                static_assert((Variable<V> && ...)); // TODO: Add back as template requirement when GCC fixes bug
                using std::swap;
                (swap(get<symbol_tag_t<V>>(l), get<symbol_tag_t<V>>(r)), ...);
            }(typename clazz_info<clazz>::variables_info_t{});
        }
        std::cout << "inner friend swapped\n";
    }    
    
    // Custom clazz rvalue swap for same type
    // NB: Must opt-in at clazz definition, since this is not an idiomatic swap
    friend constexpr void swap(clazz&& l, clazz&& r) 
    requires (clazz_info<clazz>::template has_dec<dec::swap<void(clazz&&)>>)
    {
        l.swap(r);
        std::cout << "inner friend rvalue swapped\n";
    }

    // Custom clazz swap for clazzes with the same variables
    template<Clazz R>
    requires ClazzOf<typename clazz_info<clazz>::variables_info_t::clazz_t,
                     typename clazz_info<R>::variables_info_t::clazz_t>
    friend constexpr void swap(clazz& l, R& r) {
        if constexpr (clazz_info<clazz>::template has_dec<dec::swap<void(R&)>>) {
            l.swap(r);
        } else {
            [&]<class... V>(clazz_info<clazz<V...>>) {
                static_assert((Variable<V> && ...)); // TODO: Add back as template requirement when GCC fixes bug
                using std::swap;
                (swap(get<symbol_tag_t<V>>(l), get<symbol_tag_t<V>>(r)), ...);
            }(typename clazz_info<clazz>::variables_info_t{});
        }
        std::cout << "general inner friend swapped\n";
    }

    // Custom clazz rvalue swap for clazzes with the same variables
    // NB: Must opt-in at clazz definition, since this is not an idiomatic swap
    template<Clazz R>
    requires ClazzOf<typename clazz_info<clazz>::variables_info_t::clazz_t,
                     typename clazz_info<R>::variables_info_t::clazz_t>
          && clazz_info<clazz>::template has_dec<dec::swap<void(R&&)>>
    friend constexpr void swap(clazz&& l, R&& r) {
        l.swap(r);
        std::cout << "general inner friend rvalue swapped\n";
    }

    template<Clazz T>
    friend constexpr inline decltype(auto) operator==(const clazz& l, const T& r) {
        if constexpr (clazz_info<clazz>::template has_co_dec<dec::operator_eq<void(const T&)>>)
            return l.operator_eq(r);
        else if constexpr (clazz_info<T>::template has_co_dec<dec::operator_eq<void(const clazz&)>>) 
            return r.operator_eq(l);
        else
            return strong_equals(l, r);
    }

    template<Clazz T>
    friend constexpr inline decltype(auto) operator!=(const clazz& l, const T& r) {
        if constexpr (clazz_info<clazz>::template has_co_dec<dec::operator_not_eq<void(const T&)>>)
            return l.operator_not_eq(r);
        else if constexpr (clazz_info<clazz>::template has_co_dec<dec::operator_eq<void(const T&)>>)
            return !l.operator_eq(r);
        else if constexpr (clazz_info<T>::template has_co_dec<dec::operator_eq<void(const clazz&)>>) 
            return !r.operator_eq(l);
        else
            return !strong_equals(l, r);
    }

    template<Clazz T>
    friend constexpr inline decltype(auto) operator<(const clazz& l, const T& r) {
        if constexpr (clazz_info<clazz>::template has_co_dec<dec::operator_less<void(const T&)>>)
            return l.operator_less(r);
        else
            return strong_less_than(l, r);
    }

    template<Clazz T>
    friend constexpr inline decltype(auto) operator>=(const clazz& l, const T& r) {
        if constexpr (clazz_info<clazz>::template has_co_dec<dec::operator_greater_eq<void(const T&)>>)
            return l.operator_greater_eq(r);
        else
            return !strong_less_than(l, r);
    }

    template<Clazz T>
    friend constexpr inline decltype(auto) operator>(const clazz& l, const T& r) {
        if constexpr (clazz_info<clazz>::template has_co_dec<dec::operator_greater<void(const T&)>>)
            return l.operator_greater(r);
        else
            return strong_less_than(r, l);
    }

    template<Clazz T>
    friend constexpr inline decltype(auto) operator<=(const clazz& l, const T& r) {
        if constexpr (clazz_info<clazz>::template has_co_dec<dec::operator_less_eq<void(const T&)>>)
            return l.operator_less_eq(r);
        else
            return !strong_less_than(r, l);
    }

    constexpr ~clazz() {
        if constexpr (clazz_info<clazz>::template has_dec<dec::operator_dtor<void(clazz&)>>) {
            this->operator_dtor();
        }
    }
    
    // constexpr inline decltype(auto) operator*() {
    //     if constexpr (clazz_info<clazz>::template has_co_dec<dec::operator_deref<void()>>)
    //         return this->operator_deref();
    //     else
    //         return *this;
    // }

    // constexpr inline decltype(auto) operator*() const {
    //     if constexpr (clazz_info<clazz>::template has_co_dec<dec::operator_deref<void() const>>)
    //         return this->operator_deref();
    //     else
    //         return *this;
    // }

};

template<Clazz C, Tuple T>
requires std::same_as<T, typename meta_clazz_t<C>::tuple_t>
const C& as_clazz(const T& tuple) {
    return union_cast<const C&>(tuple);
}

template<Clazz C, Tuple T>
requires std::same_as<T, typename meta_clazz_t<C>::tuple_t>
C& as_clazz(T& tuple) {
    return union_cast<C&>(tuple);
}

template<Clazz C>
const auto& as_tuple(const C& c) {
    return meta_clazz_of(c).tuple;
}

template<Clazz C>
requires (std::is_rvalue_reference<C&&>::value)
const auto&& as_tuple(C&& c) {
    return std::move(meta_clazz_of(c).tuple);
}

template<template<class> class N, Tuple T>
requires NamedTupleWrapper<N>
auto& as_named_tuple(const T& tuple) {
    using nup_t = bind_to_t<N, T>;
    return union_cast<const nup_t&>(tuple);
}

template<template<class> class N, Tuple T>
requires NamedTupleWrapper<N>
auto& as_named_tuple(T& tuple) {
    using nup_t = bind_to_t<N, T>;
    return union_cast<nup_t&>(tuple);
}

template<template<class> class N, Tuple T>
requires NamedTupleWrapper<N>
auto& as_nuple(const T& tuple) {
    return as_named_tuple<nuple>(tuple);
}

template<template<class> class N, Tuple T>
requires NamedTupleWrapper<N>
auto& as_nuple(T& tuple) {
    return as_named_tuple<nuple>(tuple);
}

template<Symbol S>
struct private_  : private S {};

template<Symbol S>
struct private_info {
    static constexpr bool is_private = false;
    using public_t = S;
};

template<Symbol S>
struct private_info<private_<S>> {
    static constexpr bool is_private = true;
    using public_t = S;
};

template<class S>
struct symbol_info<private_<S>> : symbol_info<S> {
    static constexpr bool is_private = true;
    using symbol_t = private_<S>;
    template<class>
    using struple_element_t = symbol_t;
    template<class Dec>
    static constexpr bool has_dec = false;
    template<class Sym>
    static constexpr bool shares_dec = false;
    template<class Dec>
    static constexpr bool has_wider_dec = false;
    template<class Dec>
    static constexpr bool has_narrower_dec = false;
    template<class Sym>
    static constexpr bool shares_wider_dec = false;
    template<class Sym>
    static constexpr bool shares_narrower_dec = false;
    template<class Name>
    static constexpr bool has_name = false;
    template<class Field>
    static constexpr bool shares_name = false;
};

template<class, bool Const, Tag... Tags>
struct view;

template<class... X, bool Const, Tag... Tags>
requires (sizeof...(Tags) > 0)
struct view<clazz<X...>, Const, Tags...> 
     : view<typename meta_clazz<X...>::template super_meta_clazz_t<Tags...>::clazz_t, Const> {};

template<class... X, bool Const>
struct view<clazz<X...>, Const> {
    template<class T>
    using const_t = std::conditional_t<Const, std::add_const_t<T>, T>;

    using type = clazz <
        std::conditional_t <
            Variable<X>, 
            typename symbol_tag_info<X>::template var_t<std::add_lvalue_reference_t<const_t<symbol_value_t<X>>>>,
            X
        >...
    >;
};

template<template<class> class N, class... X, bool Const, Tag... Tags>
requires (sizeof...(Tags) > 0) && NamedTupleWrapper<N>
struct view<N<X...>, Const, Tags...> 
     : view<typename N<X...>::type::template super_meta_clazz_t<Tags...>::clazz_t, Const> {};

template<template<class> class N, class... X, bool Const>
requires NamedTupleWrapper<N>
struct view<N<X...>, Const> {
    template<class T>
    using const_t = std::conditional_t<Const, std::add_const_t<T>, T>;
    using type = N<std::add_lvalue_reference_t<const_t<symbol_value_t<X>>>...>;
};

template<class T, Tag... Tags>
using view_t = typename view<T, false, Tags...>::type;

template<class T, Tag... Tags>
using const_view_t = typename view<T, true, Tags...>::type;

template<class... Ds>
struct trait_info<trait<Ds...>> {
    static constexpr bool is_trait = true;
    static constexpr bool pure = (MethodDeclaration<Ds> && ...);

    template<Clazz C>
    static constexpr bool implementor = clazz_info<C>::template implements<Ds...>;
    template<Clazz C>
    static constexpr bool co_implementor = clazz_info<C>::template co_implements<Ds...>;
    template<Clazz C>
    static constexpr bool contra_implementor = clazz_info<C>::template contra_implements<Ds...>;

    template<Clazz C>
    requires (pure)
    using variant_clazz_t = typename meta_clazz_t<C>::template with_data_at_mem_front<padding<1>>::clazz_t;
    
    template<CoImplements<Ds...>... Variants>
    requires (pure)
    using variants_clazz_t = clazz < 
        tpe::variants<type_list<variant_clazz_t<Variants>...>>,
        // Variant index stored in a padding char
        padding<1>,
        // All trait defs callable with object.trait_def(...)
        typename dec_info<Ds>::template variant_def_t<variant_clazz_t<Variants>...>...
    >;
};

template<PureTrait Trt, CoImplements<Trt>... Variants>
class hvector {
    template<class T>
    static constexpr bool is_variant = index_of_type<sort_asc<T>, sort_asc<Variants>...>::value < sizeof...(Variants);

    template<class T>
    requires (is_variant<T>)
    static constexpr bool index_of_variant = index_of_type<sort_asc<T>, sort_asc<Variants>...>::value;

    template<class T>
    requires (is_variant<T>)
    using variant_t = typename trait_info<Trt>::template variant_clazz_t<type_at_t<index_of_variant<T>, Variants...>>;

public:
    using size_type = unsigned int;
    using value_type = typename trait_info<Trt>::template variants_clazz_t<Variants...>;
    using reference = value_type&;
    using const_reference = const value_type&;

private:    
    static constexpr size_type Align = std::max({alignof(variant_t<Variants>)...});
    std::vector<size_type> positions;

    struct freer {
        void operator()(void* p) const { std::free(p); }
    };
    using buffer_t = std::unique_ptr<std::byte, freer>;
    buffer_t buffer;
    size_type write_head = 0;
    size_type capacity = 0;

    inline std::byte* get_buffer() {
        return std::assume_aligned<Align>(buffer.get());
    }
    inline const std::byte* get_buffer() const {
        return std::assume_aligned<Align>(buffer.get());
    }

    static constexpr size_type var_sizes[] = { sizeof(variant_t<Variants>)... };

    inline static constexpr size_type pad_to_align(size_type to_pad, size_type align = Align) {
        return (to_pad + (align - 1)) & -align;
    }

    std::pair<buffer_t, size_type> _create_buffer(size_type size) {
        size = pad_to_align(size);
        return {buffer_t((std::byte*)aligned_alloc(Align, size)), size};
    }

    void _replace_buffer(size_type size) {
        if (capacity < size) {
            auto [new_buffer, new_capacity] = _create_buffer(size);
            for (size_type new_write_head = 0; auto& i : *this) {
                detail::visit_clazz_variant<void, variant_t<Variants>...>(
                    std::index_sequence_for<Variants...>{},
                    (const char&)i, &i,
                    [&, i = (const std::byte&)i, new_buffer = std::assume_aligned<Align>(new_buffer.get())]
                    <class T>(T& self) {
                        new_write_head = pad_to_align(new_write_head, alignof(T));
                        new (new_buffer + new_write_head) T(std::move(self));
                        new_buffer[new_write_head] = i;
                        self.~T();
                        new_write_head += sizeof(T);
                    });
            }
            buffer = std::move(new_buffer);
            capacity = new_capacity;
        }
    }

public:
    template<class T>
    void push_back(T&& value) {
        void(emplace_back<std::decay_t<T>>(std::forward<T>(value)));
    }
    
    template<class T, class... Ts>
    requires (hvector::is_variant<T>)
    reference emplace_back(Ts&&... args) {
        using value_t = variant_t<T>;
        constexpr size_type value_size = sizeof(value_t);

        write_head = pad_to_align(write_head, alignof(value_t));
        positions.push_back(write_head);
        if (capacity < write_head + value_size)
            _replace_buffer(2 * (write_head + value_size));

        new (get_buffer() + write_head) value_t(std::forward<Ts>(args)...);
        get_buffer()[write_head] = std::byte{index_of_variant<T>};

        write_head += value_size;
        return back();
    }
    
    reference at(size_type pos) {
        return reinterpret_cast<reference>(get_buffer()[positions.at(pos)]);
    }
    const_reference at(size_type pos) const {
        return reinterpret_cast<const_reference>(get_buffer()[positions.at(pos)]);
    }
    reference operator[](size_type pos) {
        return reinterpret_cast<reference>(get_buffer()[positions[pos]]);
    }
    const_reference operator[](size_type pos) const {
        return reinterpret_cast<const_reference>(get_buffer()[positions[pos]]);
    }
    reference front() {
        return reinterpret_cast<reference>(get_buffer()[0]);
    }
    const_reference front() const {
        return reinterpret_cast<const_reference>(get_buffer()[0]);
    }
    reference back() {
        return reinterpret_cast<reference>(get_buffer()[positions.back()]);
    }
    const_reference back() const {
        return reinterpret_cast<const_reference>(get_buffer()[positions.back()]);
    }
    size_type size() const noexcept {
        return positions.size();
    }
    size_t size_bytes() const noexcept {
        return write_head;
    }
    bool empty() const noexcept {
        return positions.empty();
    }
    void pop_back() {
        auto& back = get_buffer()[positions.back()];

        // Destruct back element
        detail::visit_clazz_variant<void, variant_t<Variants>...>(
            std::index_sequence_for<Variants...>{},
            (const char&)back, &back, []<class T>(T& self) { self.~T(); });

        // Remove position and truncate buffer
        positions.pop_back();
        write_head = positions.back() + var_sizes[static_cast<size_type>(get_buffer()[positions.back()])];
    }

private:
    template<bool Const>
    class _iterator {
        using value_type = hvector::value_type;
        using reference = value_type&;
        using pointer = value_type*;

        using container_t = std::conditional_t<Const, const hvector, hvector>;
        using value_t = std::conditional_t<Const, const value_type, value_type>;

        friend container_t;

        _iterator(size_type p, container_t& u) : pos(p), underlying(u) {}
        size_type pos = 0;
        container_t& underlying;        

    public:
        auto& operator++() {
            pos += var_sizes[static_cast<size_type>(underlying.get_buffer()[pos])];
            return *this;
        }
        
        inline value_t& operator*() const {
            return reinterpret_cast<value_t&>(underlying.get_buffer()[pos]);
        }
        
        inline value_t* operator->() const {
            return &**this;
        }
        
        inline bool operator==(const _iterator& other) const {
            return &underlying == &other.underlying && pos == other.pos;
        }
        
        inline bool operator!=(const _iterator& other) const {
            return !(*this == other);
        }
    };

public:
    using iterator = _iterator<false>;
    using const_iterator = _iterator<true>;

    inline iterator begin() {
        return {0, *this};
    }
    inline const_iterator begin() const {
        return cbegin();
    }
    inline const_iterator cbegin() const {
        return {0, *this};
    }
    inline iterator end() {
        return {write_head, *this};
    }
    inline const_iterator end() const {
        return cend();
    }
    inline const_iterator cend() const {
        return {write_head, *this};
    }

    using transpose_t = nuple_t<std::vector<Variants>...>;

    transpose_t transpose() const & {
        return transpose_copy(std::index_sequence_for<Variants...>{});
    }

    transpose_t transpose() && {
        return transpose_move(std::index_sequence_for<Variants...>{});
    }

private:
    template<size_t... I>
    auto transpose_copy(std::index_sequence<I...>) const {
        using visitor = overload<decltype([](const variant_t<Variants>& self, auto& out) { get<I>(out).emplace_back(self); })...>;
        return transpose_visit(*this, visitor{});
    }

    template<size_t... I>
    auto transpose_move(std::index_sequence<I...>) {
        using visitor = overload<decltype([](variant_t<Variants>& self, auto& out) { get<I>(out).emplace_back(std::move(self)); })...>;
        return transpose_visit(*this, visitor{});
    }

    template<class T, class U>
    static auto transpose_visit(T& self, U&& visitor) {
        transpose_t out;
        for (auto& i : self) {
            detail::visit_clazz_variant<void, variant_t<Variants>...>(
                std::index_sequence_for<Variants...>{},
                (const char&)i, &i, visitor, out);
        }
        return out; 
    }

public:
    void reserve(size_type elements) {
        positions.reserve(elements);

        constexpr auto max = std::max({sizeof(variant_t<Variants>)...});
        if (capacity < max * elements) {
            _replace_buffer(max * elements);
        }
    }

    hvector() {
        reserve(14);
    }

private:
    void destruct_all() {
        for (auto& i : *this)
            detail::visit_clazz_variant<void, variant_t<Variants>...>(
                std::index_sequence_for<Variants...>{},
                (const char&)i, &i,
                []<class T>(T& self) { self.~T(); });
    }

public:
    void clear() {
        destruct_all();
        write_head = 0;
        positions.clear();
    }

    ~hvector() {
        destruct_all();
    }
};

template<class, size_t>
struct restrict_aligned;

template<class T, size_t A>
requires (A <= 1)
struct restrict_aligned<T*, A> {
    using type =  T * __restrict;
};

template<class T, size_t A>
requires (A > 1)
struct restrict_aligned<T*, A> {
    using type =  T __attribute__((aligned(A))) * __restrict;
};

template<class T, size_t A>
requires (A <= 1)
struct restrict_aligned<T&, A> {
    using type = T & __restrict;
};

template<class T, size_t A>
requires (A > 1)
struct restrict_aligned<T&, A> {
    using type = T __attribute__((aligned(A))) & __restrict;
};

template<class T, size_t A>
using restrict_aligned_t = typename restrict_aligned<T, A>::type;

template<class T>
struct restrict;

template<class T>
struct restrict<T*> {
    using type = T* __restrict;
};

template<class T>
struct restrict<T&> {
    using type = T& __restrict;
};

template<class T>
using restrict_t = typename restrict<T>::type;

template<Pod P, size_t Align, SubClazzOf<P> C>
requires (std::ispow2(Align))
struct cvector_impl;

// Default alignment to cache-line
template<Clazz C, size_t Align = 64>
requires (std::ispow2(Align))
using cvector = cvector_impl<typename clazz_info<C>::pod_info_t::clazz_t, Align, C>;

template<template<class...> class Wrapper, Symbol S>
struct mapped_symbol;

template<template<class...> class Wrapper, Variable S>
struct mapped_symbol<Wrapper, S> {
    using type = typename symbol_tag_info<S>::template var_t<Wrapper<symbol_value_t<S>>>;
};

template<template<class...> class Wrapper, EmptySymbol S>
struct mapped_symbol<Wrapper, S> {
    using type = S;
};

template<template<class...> class Wrapper, Symbol S>
using mapped_symbol_t = typename mapped_symbol<Wrapper, S>::type;

template<class, bool>
struct cvector_iterator;

template<Variable... X, size_t Align, Symbol... S>
struct cvector_impl<clazz<X...>, Align, clazz<S...>> {
    template<Pod P, size_t, SubClazzOf<P>>
    friend struct cvector_impl;

    template<class, bool>
    friend struct cvector_iterator;

    using size_type = size_t;
    using clazz_t = clazz<S...>;

private:
    template<class T>
    using reference_wrapper = restrict_t<std::add_lvalue_reference_t<T>>;
    
    template<class T>
    using const_reference_wrapper = restrict_t<std::add_lvalue_reference_t<std::add_const_t<T>>>;

public:
    using value_type = clazz<mapped_symbol_t<reference_wrapper, S>...>;
    using reference = value_type;
    using const_reference = clazz<mapped_symbol_t<const_reference_wrapper, S>...>;

private:
    using value_types = type_list<symbol_value_t<X>...>;

    template<size_t I>
    using value_t = tuple_element_t<I, value_types>;

    static constexpr auto sizes = std::array{sizeof(symbol_value_t<X>)...};
    static constexpr auto aligns = std::array{std::max(Align, alignof(symbol_value_t<X>))...};

    // First index used to store real capacity (instead of always being 0)
    using offsets_t = std::array<size_type, sizeof...(X)>;

    template<class T>
    using restrict_aligned = restrict_aligned_t<T, Align>;

    template<class T>
    using arrays_wrapper = restrict_aligned<std::add_pointer_t<T>>;

    using arrays_t = clazz<mapped_symbol_t<arrays_wrapper, X>...>;

    size_type _capacity;
    size_type _size;

    template<class T, size_t align = std::max(alignof(T), Align)>
    struct buffer_t {     
        buffer_t(size_type buffer_size) noexcept {
            ptr = (T*)std::aligned_alloc(align, pad_to_align(buffer_size, align));
        }

        buffer_t(buffer_t&& other) noexcept : ptr{other.ptr} {
            other.ptr = nullptr;
        }

        buffer_t& operator=(buffer_t&& other) noexcept {
            std::free(ptr);
            ptr = other.ptr;
            other.ptr = nullptr;
            return *this;
        }

        inline void swap(buffer_t& other) noexcept { std::swap(ptr, other.ptr); }
        friend void swap(buffer_t& l, buffer_t& r) noexcept { l.swap(r); }

        auto get() noexcept { return _assume_aligned(ptr); }
        auto get() const noexcept { return _assume_aligned((const T*)ptr); }

        ~buffer_t() { std::free(ptr); }
    private:
        T* ptr;
    };
    buffer_t<std::byte, std::max(alignof(value_t<0>), Align)> _buffer;
    arrays_t _arrays;

    inline static constexpr size_type pad_to_align(size_type to_pad, size_type align = Align) {
        return (to_pad + align - 1) & -align;
    }

    template<size_t size, size_t align>
    inline static constexpr size_type extra_capacity(size_type n) {
        constexpr size_type res = size % align;
        return ((-n * res) & (align - 1)) / size;
    }

    static constexpr size_type real_capacity(size_type capacity) {
        constexpr bool extra_capacity_possible = []<size_t... I>(std::index_sequence<I...>) {
            // Padding is defined by alignment subsequent field, or the first field in the case of the last
            return ((sizes[I] < aligns[(I+1)%sizeof...(X)]) && ...);
        }(std::index_sequence_for<X...>{});

        if constexpr (!extra_capacity_possible)
            // There is never extra capacity if there is a field with a larger size than its available padding
            return capacity;
        else {
            return capacity + [=]<size_t... I>(std::index_sequence<I...>) {
                return std::min({extra_capacity<sizes[I], aligns[(I + 1) % sizeof...(X)]>(capacity)...});
            }(std::index_sequence_for<X...>{});
        }
    }

    static constexpr size_type min_capacity = real_capacity(4);
    static constexpr size_type default_capacity = std::max(min_capacity, real_capacity(12));

    cvector_impl(const offsets_t& offsets, size_type size)
        : _capacity{offsets[0]}
        , _size{size}
        , _buffer{_create_buffer(offsets)}
        , _arrays{_create_arrays(_buffer.get(), offsets)}
    {}

public:
    cvector_impl(size_type capacity = default_capacity) : cvector_impl{_create_offsets(capacity), 0} {}

    cvector_impl(const cvector_impl& other) 
        : cvector_impl(_create_offsets(2 * other._size), other._size)
    {
        _for_each(other, [this]<class T>(T* __restrict our_buffer, T* __restrict other_buffer) {
            our_buffer = _assume_aligned(our_buffer);
            other_buffer = _assume_aligned(other_buffer);

            for (size_t i = 0; i < _size; ++i) {
                new (our_buffer + i) T(other_buffer[i]);
            }
        });
    }

    cvector_impl(cvector_impl&& other) : cvector_impl() {
        swap(other);
    }

    void swap(cvector_impl& other) noexcept {
        using std::swap;
        swap(_capacity, other._capacity);
        swap(_size, other._size);
        swap(_buffer, other._buffer);
        swap(_arrays, other._arrays);
    }

    friend void swap(cvector_impl& l, cvector_impl& r) noexcept {
        l.swap(r);
    }

    void reserve(size_type n) {
        if (n > _capacity) {
            _resize_buffer_force(n);
        }
    }

    void reserve_extra(size_type n) {
        reserve(_size + n);
    }

    void resize(size_type n) {
        reserve(n);
        
        if (n < _size) {
            _for_each([n, this]<class T>(T* __restrict array) {
                if constexpr (!std::is_trivially_destructible_v<T>) {
                    array = _assume_aligned(array);
                    for (size_type i = n; i < _size; ++i) {
                        array[i].~T();
                    }
                }
            });
            _size = n;
        } else if (n > _size) {
            _for_each([n, this]<class T>(T* __restrict array) {
                array = _assume_aligned(array);
                for (size_type i = n; i < _size; ++i) {
                    new (array + i) T();
                }
            });
        }
    }

    void shrink_to_fit() {
        auto size = std::max(_size, min_capacity);
        if (size < _capacity) {
            _resize_buffer(size);
        }
    }

private:
    void _grow_if_full() {
        if (_capacity == _size)
            _resize_buffer_force(_size * 2);
    }

    void _resize_buffer(size_type capacity) {
        if (real_capacity(capacity) != _capacity) {
            _resize_buffer_force(capacity);
        }
    }

    void _resize_buffer_force(size_type capacity) {
        _map_to_new_buffer(capacity, [this]<class T>(T* __restrict old_buffer, T* __restrict new_buffer) {
            old_buffer = _assume_aligned(old_buffer);
            new_buffer = _assume_aligned(new_buffer);

            for (size_type i = 0; i < _size; ++i) {
                new (new_buffer + i) T(std::move(old_buffer[i]));
                if constexpr (!std::is_trivially_destructible_v<T>)
                    old_buffer[i].~T();
            }
        });
    }

    template<class F>
    void _map_to_new_buffer(size_type capacity, F&& f) {
        auto offsets = _create_offsets(capacity);
        auto new_buffer = _create_buffer(offsets);

        if (!empty()) {
            _for_each(new_buffer.get(), offsets, std::forward<F>(f));
        }

        _buffer.swap(new_buffer);
        _arrays = _create_arrays(_buffer.get(), offsets);
        _capacity = offsets[0];
    }

    constexpr offsets_t _create_offsets(size_type capacity) {
        offsets_t offsets = {real_capacity(capacity)};
        size_type offset = 0;
        for (size_type i = 1; i < sizeof...(X); ++i) {
            offset += capacity * sizes[i-1];
            offset = pad_to_align(offset, aligns[i]);
            offsets[i] = offset;
        }
        return offsets;
    }

    static buffer_t<std::byte> _create_buffer(const offsets_t& offsets) {
        if constexpr (sizeof...(X) > 1) {
            size_type size = offsets.back() + offsets[0] * sizes.back();
            return buffer_t<std::byte>(size);
        } else {
            return buffer_t<std::byte>(offsets[0] * sizes[0]);
        }
    }

    auto _create_arrays(std::byte* buffer, const offsets_t& offsets) {
        return _create_arrays(std::index_sequence_for<X...>{}, buffer, offsets);
    }

    template<size_t... I>
    auto _create_arrays(std::index_sequence<I...>, std::byte* buffer, const offsets_t& offsets) {
        return arrays_t{_get_array<I>(buffer, offsets)...};
    }

    template<class F>
    void _for_each(F&& f) {
        _for_each(std::index_sequence_for<X...>{}, std::forward<F>(f));
    }
    template<size_t... I, class F>
    void _for_each(std::index_sequence<I...>, F&& f) {
        (std::invoke(std::forward<F>(f), _get_array<I>()), ...);
    }
    template<class F>
    void _for_each(const cvector_impl& other, F&& f) {
        _for_each(std::index_sequence_for<X...>{}, other, std::forward<F>(f));
    }
    template<size_t... I, class F>
    void _for_each(std::index_sequence<I...>, const cvector_impl& other, F&& f) {
        (std::invoke(std::forward<F>(f), other._get_array<I>(), _get_array<I>()), ...);
    }
    template<class F>
    void _for_each(std::byte* other_buffer, const offsets_t& other_offsets, F&& f) {
        _for_each(std::index_sequence_for<X...>{}, other_buffer, other_offsets, std::forward<F>(f));
    }
    template<size_t... I, class F>
    void _for_each(std::index_sequence<I...>, std::byte* other_buffer, const offsets_t& other_offsets, F&& f) {
        (std::invoke(std::forward<F>(f), _get_array<I>(), _get_array<I>(other_buffer, other_offsets)), ...);
    }
    template<class F>
    void _for_each_tagged(F&& f) {
        _for_each_tagged(std::index_sequence_for<X...>{}, std::forward<F>(f));
    }
    template<size_t... I, class F>
    void _for_each_tagged(std::index_sequence<I...>, F&& f) {
        (std::invoke(std::forward<F>(f), _get_array<I>(), type_list<X>{}), ...);
    }
    
    template<class F>
    decltype(auto) _apply_arrays(F&& f) {
        return _apply_arrays(std::index_sequence_for<X...>{}, std::forward<F>(f));
    }
    template<size_t... I, class F>
    decltype(auto) _apply_arrays(std::index_sequence<I...>, F&& f) {
        return std::invoke(std::forward<F>(f), _get_array<I>()...);
    }
    template<class F>
    decltype(auto) _apply_arrays(F&& f) const {
        return _apply_arrays(std::index_sequence_for<X...>{}, std::forward<F>(f));
    }
    template<size_t... I, class F>
    decltype(auto) _apply_arrays(std::index_sequence<I...>, F&& f) const {
        return std::invoke(std::forward<F>(f), _get_array<I>()...);
    }

    template<class T>
    inline static constexpr restrict_aligned_t<T*, std::max(Align, alignof(T))> _assume_aligned(T* array) noexcept {
        return std::assume_aligned<std::max(Align, alignof(T))>(array);
    }

    template<size_t I>
    decltype(auto) _get_array() const {
        return _assume_aligned(get<I>(_arrays));
    }
    template<size_t I>
    decltype(auto) _get_array(std::byte* buffer, const offsets_t& offsets) const {
        if constexpr (I > 0)
            return _assume_aligned(reinterpret_cast<value_t<I>*>(buffer + offsets[I]));
        else 
            return _assume_aligned(reinterpret_cast<value_t<I>*>(buffer));
    }

public:
    void push_back(const clazz_t& value) {
        _grow_if_full();

        _apply_arrays([&, this]<class... T>(T* __restrict... array) {
            ((array = _assume_aligned(array)), ...);
            (new (array + _size) T(get<symbol_tag_t<X>>(value)), ...);
        });
        ++_size;
    }

    void push_back(clazz_t&& value) {
        _grow_if_full();

        _apply_arrays([&, this]<class... T>(T* __restrict... array) {
            ((array = _assume_aligned(array)), ...);
            (new (array + _size) T(get<symbol_tag_t<X>>(std::move(value))), ...);
        });
        ++_size;
    }

    template<class... Ts>
    void push_back(Ts&&... ins) {
        push_back(clazz_t{std::forward<Ts>(ins)...});
    }

    template<class... Ts>
    requires (ArgHolder<Ts> && ...)
    reference emplace_back(Ts&&... ins) {
        _grow_if_full();

        auto phc = detail::arg::holder_set<Ts...>(std::forward<Ts>(ins)...);
        _apply_arrays([&, this]<class... T>(T* __restrict... array) {
            ((array = _assume_aligned(array)), ...);
            (new (array + _size) T(std::move(phc).template make_type<symbol_tag_t<X>, T>()), ...);
        });
        ++_size;

        return back();
    }

    template<class... Ts>
    requires (!ArgHolder<Ts> && ...)
    reference emplace_back(Ts&&... ins) {
        _grow_if_full();

        _apply_arrays([&, this]<class... T>(T* __restrict... array) {
            ((array = _assume_aligned(array)), ...);
            (new (array + _size) T(std::forward<Ts>(ins)), ...);
        });
        ++_size;

        return back();
    }

    void _grow_on_append(size_type size) {
        size_type new_size = _size + size;
        if (_capacity < new_size)
            _resize_buffer_force(new_size * 2);
    }

    // Append elements of cvector column-wise
    template<class... V, size_t A, class C>
    requires (clazz_info<clazz<V...>>::template shares_name<X> && ...)
          && (std::is_constructible<
                symbol_value_t<X>, 
                symbol_value_t<typename clazz_info<clazz<V...>>::template tag_symbol_t<symbol_tag_t<X>>>
              >::value && ...)
    void append(const cvector_impl<clazz<V...>, A, C>& other) {
        _grow_on_append(other._size);

        _for_each_tagged([this, &other]<class T, class Sym>(T* __restrict array, type_list<Sym>) {
            array = _assume_aligned(array);
            for (size_type i = 0; i < other._size; ++i) {
                new (array + _size + i) T(other.template array<symbol_tag_t<Sym>>()[i]);
            }
        });
        _size += other._size;
    }

    // Append elements of std::vector<clazz> column-wise (may thrash cache)
    template<class... V>
    requires (clazz_info<clazz<V...>>::template shares_name<X> && ...)
          && (std::is_constructible<
                symbol_value_t<X>, 
                symbol_value_t<typename clazz_info<clazz<V...>>::template tag_symbol_t<symbol_tag_t<X>>>
              >::value && ...)
    void append_cwise(const std::vector<clazz<V...>>& other) {
        auto size = size = other.size();
        _grow_on_append(size);

        _for_each_tagged([this, size, &other]<class T, class Sym>(T* __restrict array, type_list<Sym>) {
            array = _assume_aligned(array);
            for (size_type i = 0; i < size; ++i) {
                new (array + _size + i) T(get<symbol_tag_t<Sym>>(other[i]));
            }
        });
        _size += size;
    }

    // Append elements of std::vector<clazz> row-wise (may thrash cache)
    template<class... V>
    requires (clazz_info<clazz<V...>>::template shares_name<X> && ...)
          && (std::is_constructible<
                symbol_value_t<X>, 
                symbol_value_t<typename clazz_info<clazz<V...>>::template tag_symbol_t<symbol_tag_t<X>>>
              >::value && ...)
    void append_rwise(const std::vector<clazz<V...>>& other) {
        auto size = other.size();
        _grow_on_append(size);

        _apply_arrays([this, size, &other]<class... Ts>(Ts* __restrict... arrays) {
            ((arrays = _assume_aligned(arrays)), ...);
            for (size_type i = 0; i < size; ++i) {
                (new (arrays + _size + i) Ts(other.template array<symbol_tag_t<X>>()[i]), ...);
            }
        });
        _size += size;
    }

    // Append size rows with the respective columns of the arrays clazz column-wise
    template<Clazz C>
    requires (clazz_info<C>::template shares_name<X> && ...)
          && (std::is_constructible<
                symbol_value_t<X>, 
                decltype(std::declval<symbol_value_t<typename clazz_info<C>::template tag_symbol_t<symbol_tag_t<X>>>>()[0])
              >::value && ...)
    void append(size_type size, const C& arrays) {
        _grow_on_append(size);

        _for_each_tagged([this, size, &arrays]<class T, class Sym>(T* __restrict array, type_list<Sym>) {
            array = _assume_aligned(array);
            for (size_type i = 0; i < size; ++i) {
                new (array + _size + i) T(get<symbol_tag_t<Sym>>(arrays)[i]);
            }
        });
        _size += size;
    }

    // Append size rows using the respective callbacks of the generators clazz column-wise
    template<Clazz C>
    requires (clazz_info<C>::template shares_name<X> && ...)
          && (std::is_invocable_r<
                symbol_value_t<X>, 
                symbol_value_t<typename clazz_info<C>::template tag_symbol_t<symbol_tag_t<X>>>,
                size_type
              >::value && ...)
    void append(size_type size, const C& generators) {
        _grow_on_append(size);

        _for_each_tagged([this, size, &generators]<class T, class Sym>(T* __restrict array, type_list<Sym>) {
            array = _assume_aligned(array);
            for (size_type i = 0; i < size; ++i) {
                new (array + _size + i) T(std::invoke(get<symbol_tag_t<Sym>>(generators), i));
            }
        });
        _size += size;
    }

    // Append size rows using the respective callbacks of the generators clazz row-wise, which is
    // more likely to thrash cache of of the cvector. However, this is useful if the generators 
    // perform significantly better with a row-wise calling pattern.
    template<Clazz C>
    requires (clazz_info<C>::template shares_name<X> && ...)
          && (std::is_invocable_r<
                symbol_value_t<X>, 
                symbol_value_t<typename clazz_info<C>::template tag_symbol_t<symbol_tag_t<X>>>,
                size_type
              >::value && ...)
    void append_rwise(size_type size, const C& generators) {
        _grow_on_append(size);

        _apply_arrays([this, size, &generators]<class... Ts>(Ts* __restrict... arrays) {
            ((arrays = _assume_aligned(arrays)), ...);
            for (size_type i = 0; i < size; ++i) {
                (new (arrays + _size + i) Ts(std::invoke(get<symbol_tag_t<X>>(generators), i)), ...);
            }
        });
        _size += size;
    }

    void pop_back() {
        resize(_size - 1);
    }

    reference at(size_type pos) {
        assert(pos < size());
        return *this[pos];
    }
    const_reference at(size_type pos) const {
        assert(pos < size());
        return *this[pos];
    }
    reference operator[](size_type pos) {
        return _apply_arrays([=](auto* __restrict... array) {
            return reference{array[pos]...};
        });
    }
    const_reference operator[](size_type pos) const 
    {
        return _apply_arrays([=](auto* __restrict... array) {
            return const_reference{array[pos]...};
        });
    }
    reference front() {
        return _apply_arrays([](auto* __restrict... array) {
            return reference{array[0]...};
        });
    }
    const_reference front() const {
        return _apply_arrays([](auto* __restrict... array) {
            return const_reference{array[0]...};
        });
    }
    reference back() {
        return _apply_arrays([back = _size - 1](auto* __restrict... array) {
            return reference{array[back]...};
        });
    }
    const_reference back() const {
        return _apply_arrays([back = _size - 1](auto* __restrict... array) {
            return const_reference{array[back]...};
        });
    }
    auto data() noexcept {
        return map(_arrays, [this](auto* array) {
            return std::span(array, _size);
        });
    }
    auto arrays() noexcept {
        return _arrays;
    }
    auto arrays_with_size() noexcept {
        using sized_arrays_t = typename meta_clazz_t<arrays_t>::template with_data<var::_<clazz<var::size<size_type>>>>::clazz_t;
        return sized_arrays_t{get<symbol_tag_t<X>>(_arrays)..., _size};
    }
    template<size_t I>
    auto array() noexcept {
        return _get_array<I>();
    }
    template<size_t I>
    auto array() const noexcept {
        return _get_array<I>();
    }
    template<Tag tag>
    auto array() noexcept {
        return _get_array<index_of<tag, X...>::value>();
    }
    template<Tag tag>
    auto array() const noexcept {
        return _get_array<index_of<tag, X...>::value>();
    }
    const auto& raw_arrays() const noexcept {
        return _arrays;
    }
    const auto* operator->() const noexcept {
        return &_arrays;
    }
    size_type size() const noexcept {
        return _size;
    }
    size_type capacity() const noexcept {
        return _capacity;
    }
    bool empty() const noexcept {
        return _size == 0;
    }
    void clear() {
        resize(0);
    }
    ~cvector_impl() {
        clear();
    }

    using iterator = cvector_iterator<cvector_impl, false>;
    using const_iterator = cvector_iterator<cvector_impl, true>;

    inline iterator begin() {
        return iterator{this, 0};
    }
    inline const_iterator begin() const {
        return cbegin();
    }
    inline const_iterator cbegin() const {
        return const_iterator{this, 0};
    }
    inline iterator end() {
        return iterator{this, _size};
    }
    inline const_iterator end() const {
        return cend();
    }
    inline const_iterator cend() const {
        return const_iterator{this, _size};
    }
    
    void erase(iterator it) {
        assert(it._underlying == this);
        resize(it._index);
    }

private:
    bool _less_than_by_index(size_type l, size_type r) const {
        return _less_than_by_index<0>(l, r);
    }

    template<size_t I>
    [[gnu::always_inline]] inline bool _less_than_by_index(size_type l, size_type r) const {
        if constexpr (I < sizeof...(X)) {
            auto array = _get_array<I>();
            return array[l] < array[r] || (!(array[r] < array[l]) && _less_than_by_index<I + 1>(l, r));
        } else 
            return false;
    }

public:
    // Since most comparisons during sorting are done on one field of each element, find
    // the sorted order first, before sorting all of the elements in memory, field by field.
    // Moving all fields of the elements at once in a single pass sorting process will thrash
    // the cache, as that is a row-wise process, whereas cvector is optimised for column-wise processes.
    // Once we have found an order, then move the elements in each field/column one-by-one.
    // Another added benefit will be fewer swap operations per element on average, as once we have
    // found the order, the number of swaps required to sort in memory will be less.
    inline void sort() {
        sort_by_index([this](size_type l, size_type r) {
            return _less_than_by_index(l, r);
        });
    }

    template<class Comp>
    // TODO: Add back
    // requires (std::is_invocable_r<bool, Comp&&, reference, reference>::value)
    inline std::enable_if_t<std::is_invocable_r_v<bool, Comp&&, reference, reference>> 
    sort(Comp&& comp) {
        sort_by_index([&, this](size_type l, size_type r) -> bool {
            return std::invoke(std::forward<Comp>(comp), (*this)[l], (*this)[r]);
        });
    }

    template<class Comp>
    requires std::predicate<Comp&&, size_type, size_type>
    void sort_by_index(Comp&& comp) {
        permute_by_index([&](auto begin, auto end) {
            std::sort(begin, end, [&](size_type l, size_type r) -> bool {
                return std::invoke(std::forward<Comp>(comp), l, r); 
            });
        });
    }

    template<class Algo>
    requires std::invocable<Algo&&, size_type*, size_type*>
    decltype(auto) permute_by_index(Algo&& algo) {
#define CLAZZ_CVECTOR_FUNC() std::invoke(std::forward<Algo>(algo), indices.get(), indices.get() + _size)

        auto indices = get_indices();
        using result_t = decltype(CLAZZ_CVECTOR_FUNC());
        if constexpr (std::is_same_v<result_t, void>) {
            CLAZZ_CVECTOR_FUNC();
            permute_by_index(indices);
        } else if constexpr (std::is_same_v<result_t, size_type*>) {
            auto res = CLAZZ_CVECTOR_FUNC();
            permute_by_index(indices);
            return res - indices.get();
        } else {
            static_assert(false_v<>, "Return type of algorithm must be size_type* or void");
        }
    }

    template<class Algo>
    requires std::invocable<Algo&&, size_type*, size_type*>
    decltype(auto) move_elements(Algo&& algo) {
        auto indices = get_indices();
        using result_t = decltype(CLAZZ_CVECTOR_FUNC());
        if constexpr (std::is_same_v<result_t, void>) {
            CLAZZ_CVECTOR_FUNC();
            move_by_index(indices);
        } else if constexpr (std::is_same_v<result_t, size_type*>) {
            auto res = CLAZZ_CVECTOR_FUNC();
            move_by_index(indices);
            return res - indices.get();
        } else {
            static_assert(false_v<>, "Return type of algorithm must be size_type* or void");
        }

#undef CLAZZ_CVECTOR_FUNC
    }

    void permute_by_index(const size_type* __restrict indices) {
        // Find out which elements to swap to fulfill the final positions specified by indices
        // indices[i]: current position of element that should be at position i to reach sorted order
        // swaps[i]: position of element that element currently at poisition i should swap with
        //           in single pass to reach sorted order

        // Last element will end up in correct position after all previous elements have been swapped,
        // so there is no need to calculate, store, or use the last swap index
        auto swaps_size = _size - 1; 
        auto buffer = buffer_t<size_type>(swaps_size * sizeof(size_type));
        size_type* __restrict swaps = _assume_aligned(buffer.get());

        for (size_type i = 0; i < swaps_size; ++i) { 
            swaps[i] = indices[i];
            // indices[i] is: position of element before swapping that should be swapped into in position i
            // If element to swap has already been swapped into its final position,
            // i.e. one of the previously visited positions in this pass, find the element it swapped with
            while (swaps[i] < i) 
                swaps[i] = swaps[swaps[i]];
        }

        // For each array in this cvector, swap the elements into their target positions in memory
        _for_each([swaps_size, swaps = _assume_aligned(swaps)](auto* __restrict array) {
            array = _assume_aligned(array);
            for (size_type i = 0; i < swaps_size; ++i) {
                if (swaps[i] != i) {
                    using std::swap;
                    swap(array[i], array[swaps[i]]);
                }
            }
        });
    }

private:
    buffer_t<size_type> get_indices() const {
        auto indices = buffer_t<size_type>(_size * sizeof(size_type));
        std::iota(indices.get(), indices.get() + _size, 0);
        return indices;
    }

    void permute_by_index(buffer_t<size_type>& indices) {
        auto swaps = indices.get();

        // Last element will end up in correct position after all previous elements have been swapped,
        // so there is no need to calculate, store, or use the last swap index
        auto swaps_size = _size - 1;

        // Find out which elements to swap to fulfill the final positions specified by indices
        // Before loop: swaps[i] is current position of element that should be at position i to reach sorted order
        // After loop:  swaps[i] is position of element that element currently at poisition i should swap with
        //                in single pass to reach sorted order
        for (size_type i = 0; i < swaps_size; ++i)
            while (swaps[i] < i) 
                swaps[i] = swaps[swaps[i]];

        // For each array in this cvector, swap the elements into their target positions in memory
        _for_each([swaps_size, swaps = _assume_aligned(swaps)](auto* __restrict array) {
            array = _assume_aligned(array);
            for (size_type i = 0; i < swaps_size; ++i) {
                if (swaps[i] != i) {
                    using std::swap;
                    swap(array[i], array[swaps[i]]);
                }
            }
        });
    }

    void move_by_index(buffer_t<size_type>& indices) {
        auto moves = indices.get();

        // For each array in this cvector, swap the elements into their target positions in memory
        _for_each([this, moves = _assume_aligned(moves)](auto* __restrict array) {
            array = _assume_aligned(array);
            for (size_type i = 0; i < _size; ++i) {
                if (moves[i] != i) {
                    array[i] = std::move(array[moves[i]]);
                }
            }
        });
    }

public:
    // Same as sort(), but moves elements into a newly allocated buffer in sorted order
    // rather than moving elements around inside the existing buffer. New size is 2*size()
    inline void sort_new() {
        sort_new(2 * _size);
    }
    
    inline void sort_new(size_type new_size) {
        sort_new_by_index([this](size_type l, size_type r) {
            return _less_than_by_index(l, r);
        }, new_size);
    }

    // New size is 2*size()
    template<class Comp>
    requires (std::is_invocable_r<bool, Comp&&, reference, reference>::value)
    inline void sort_new(Comp&& comp) {
        sort_new(std::forward<Comp>(comp), 2 * _size);
    }

    template<class Comp>
    requires (std::is_invocable_r<bool, Comp&&, reference, reference>::value)
    inline void sort_new(Comp&& comp, size_type new_size) {
        sort_new_by_index([&, this](size_type l, size_type r) -> bool {
            return std::invoke(std::forward<Comp>(comp), (*this)[l], (*this)[r]);
        }, new_size);
    }
    
    // New size is 2*size()
    template<class Comp>
    requires (std::is_invocable_r<bool, Comp&&, size_type, size_type>::value)
    inline void sort_new_by_index(Comp&& comp) {
        sort_new_by_index(std::forward<Comp>(comp), 2 * _size);
    }

    template<class Comp>
    requires (std::is_invocable_r<bool, Comp&&, size_type, size_type>::value)
    void sort_new_by_index(Comp&& comp, size_type new_size) {
        auto buffer = buffer_t<size_type>(_size * sizeof(size_type));
        size_type* __restrict indices = _assume_aligned(buffer.get());
        
        // Create a list of indices for each element in the cvector
        std::iota(indices, indices + _size, 0);
        // Find the resulting locations of the indices after the cvector is in sorted order
        std::sort(indices, indices + _size, [&, this](size_type l, size_type r) -> bool {
            return std::invoke(std::forward<Comp>(comp), l, r); 
        });
 
        // Move the elements to a new buffer with specified capacity (at least _size) in sorted order
        _map_to_new_buffer(std::max(_size, new_size), 
            [this, indices = _assume_aligned(indices)]<class T>
                (T* __restrict old_buffer, T* __restrict new_buffer) {
            old_buffer = _assume_aligned(old_buffer);
            new_buffer = _assume_aligned(new_buffer);

            for (size_type i = 0; i < _size; ++i) {
                new (new_buffer + i) T(std::move(old_buffer[indices[i]]));
                old_buffer[indices[i]].~T();
            }
        });
    }

    template<class P, size_t A, class C>
    bool operator==(const cvector_impl<P, A, C>& other) const {
        if constexpr (clazz_info<P>::size != sizeof...(X) || (!clazz_info<P>::template shares_name<X> || ...)) {
            return false;
        } else {
            return _size == other._size 
                && (std::equal(get<symbol_tag_t<X>>(_arrays), 
                               get<symbol_tag_t<X>>(_arrays) + _size, 
                               get<symbol_tag_t<X>>(other._arrays)) && ...);
        }
    }
    
    // TODO: Add back when GCC supports three way comparison
    // Columnar strong less than. For row-wise comparison, use strong_less_than_rwise()
//     template<class... S>
//     bool operator<(const cvector_impl<clazz<S...>>& other) const {
//         return strong_less_than_cwise(other);
//     }

//     template<class... S>
//     bool strong_less_than_rwise(const cvector_impl<clazz<S...>>& other) const {
//         auto comp = std::lexicographical_compare_3way(
//             get<symbol_tag_t<X>>(_arrays), 
//             get<symbol_tag_t<X>>(_arrays) + _size,
//             get<symbol_tag_t<X>>(other._arrays), 
//             get<symbol_tag_t<X>>(other._arrays) + _size);

//         return std::forward_as_tuple(comp, lless) < std::forward_as_tuple(0, 1);
//     }

//     template<class... S>
//     bool strong_less_than_cwise(const cvector_impl<clazz<S...>>& other) const {
//         using L = clazz_t;
//         using R = clazz<S...>;

//         using lshared = clazz_info<typename shared_vars<L, R>::l_shared_clazz_t>;
//         using rshared = clazz_info<typename shared_vars<R, L>::l_shared_clazz_t>;

//         constexpr auto lsize = clazz_info<L>::values_info_t::size;
//         constexpr auto rsize = clazz_info<R>::values_info_t::size;
//         if constexpr (lsize < rsize)
//             return strong_less_than_cwise(lshared{}, other);
//         else if constexpr (lsize > rsize)
//             return strong_less_than_cwise(rshared{}, other);
//         else {
//             if constexpr (lshared::tags_info_t::names_array <= rshared::tags_info_t::names_array)
//                 return strong_less_than_cwise(lshared{}, other);
//             else
//                 return strong_less_than_cwise(rshared{}, other);
//         }
//     }

// private:
//     template<class... V, Clazz Other>
//     bool strong_less_than_cwise(clazz_info<clazz<V...>>, const Other& other) const {
//         int comp = 0;
//         ((comp = std::lexicographical_compare_3way(
//             get<symbol_tag_t<V>>(_arrays), 
//             get<symbol_tag_t<V>>(_arrays) + _size,
//             get<symbol_tag_t<V>>(other._arrays), 
//             get<symbol_tag_t<V>>(other._arrays) + _size), comp != 0) || ...);

//         constexpr auto lxnames = set_difference_t<typename clazz_info<clazz_t>::tags_info_t, 
//                                                   tags_info<symbol_tag_t<V>...>>::names_array;
//         constexpr auto rxnames = set_difference_t<typename clazz_info<Other>::tags_info_t, 
//                                                   tags_info<symbol_tag_t<V>...>>::names_array;
//         constexpr auto lless = lxnames < rxnames ? 0 : 1;

//         return std::forward_as_tuple(comp, _size, lless) < std::forward_as_tuple(0, other._size, 1)
//     }
};

template<Symbol... X, size_t Align, Symbol... S, bool Const>
struct cvector_iterator<cvector_impl<clazz<X...>, Align, clazz<S...>>, Const> {
    using difference_type = std::ptrdiff_t;
    using iterator_category = std::random_access_iterator_tag;

    using cvector_t = cvector_impl<clazz<X...>, Align, clazz<S...>>;
    
    friend cvector_t;

private:
    template<class T>
    using pointer_wrapper = std::conditional_t<Const, 
                                                std::add_pointer_t<std::add_const_t<T>> __restrict, 
                                                std::add_pointer_t<T> __restrict>;

    using pointers = clazz <
        mapped_symbol_t<pointer_wrapper, X>...
    >;

    template<class T>
    using reference_wrapper = std::conditional_t<Const, 
                                                 std::add_lvalue_reference_t<std::add_const_t<T>> __restrict, 
                                                 std::add_lvalue_reference_t<T> __restrict>;

    using references = clazz <
        def::swap< 
            []<class T> 
            (T&& self, std::type_identity_t<T>&& other)
            {
                using std::swap;
                (swap(get<symbol_tag_t<X>>(self), get<symbol_tag_t<X>>(other)), ...);
                std::cout << "operator swapped\n";
            }
        >,
        def::operator_pointer<[](auto& self) { return &self; }>,
        // fun::operator_ctor<void, []<class T>(T&& other) {
        //     return T{get<symbol_tag_t<X>>(other)...};
        // }>,
        ovl::operator_assign<
            // Custom move assignment swaps elements (required for efficient sorting)
            def::operator_assign<[]<class T>(T& self, T&& other) {
                // swap(self, other);
                using std::swap;
                (swap(get<symbol_tag_t<X>>(self), get<symbol_tag_t<X>>(other)), ...);
                std::cout << "move operator assign\n";
            }>,
            def::operator_assign<[]<class T>(T& self, T& other) {
                ((get<symbol_tag_t<X>>(self) = get<symbol_tag_t<X>>(other)), ...);
                std::cout << "copy operator assign\n";
            }>
        >,
        mapped_symbol_t<reference_wrapper, S>...
    >;

    using underlying_t = std::conditional_t<Const, const cvector_t, cvector_t>;
    underlying_t* _underlying;
    cvector_t::size_type _index;

    cvector_iterator(underlying_t* underlying, cvector_t::size_type index)
        : _underlying{underlying}
        , _index{index} 
    {}

public:
    cvector_iterator(const cvector_iterator& other) 
        : _underlying{other._underlying}
        , _index{other._index} 
    {}

    using value_type = references;
    using reference = value_type;
    using pointer = value_type;

    cvector_iterator& operator++() {
        ++_index;
        return *this;
    }

    cvector_iterator& operator--() {
        --_index;
        return *this;
    }

    cvector_iterator& operator=(const cvector_iterator& other) {
        _underlying = other._underlying;
        _index = other._index;
        return *this;
    }

    difference_type operator-(const cvector_iterator& other) const {
        return _index - other._index;
    }

    cvector_iterator operator-(int n) const {
        return {_underlying, _index - n};
    }

    cvector_iterator operator+(int n) const {
        return {_underlying, _index + n};
    }

    bool operator<(const cvector_iterator& other) const {
        return _index < other._index;
    }
    
    inline reference operator*() const {
        return {_underlying->template array<symbol_tag_t<X>>()[_index]...};
    }

    inline pointer operator->() const {
        return **this;
    }

    inline bool operator==(const cvector_iterator& other) const {
        return _underlying == other._underlying && _index == other._index;
    }
    
    inline bool operator!=(const cvector_iterator& other) const {
        return !(*this == other);
    }
};

// TODO: Consider binary search for variant index
namespace detail {
    template<class R, class... Variants, size_t I, size_t... Is, class F, class... Ts>
    constexpr R visit_clazz_variant(std::index_sequence<I, Is...>, const unsigned char index, void* const ptr, F&& f, Ts&&... args) {
        using variant_t = type_at_t<I, Variants...>;
        if (index == I)
            return std::invoke(std::forward<F>(f), *reinterpret_cast<variant_t*>(ptr), std::forward<Ts>(args)...);
        else
            return visit_clazz_variant<R, Variants...>(std::index_sequence<Is...>{}, index, ptr, std::forward<F>(f), std::forward<Ts>(args)...);
    }
    
    template<class R, class... Variants, size_t I, size_t... Is, class F, class... Ts>
    constexpr R visit_clazz_variant(std::index_sequence<I, Is...>, const unsigned char index, const void* const ptr, F&& f, Ts&&... args) {
        using variant_t = type_at_t<I, Variants...>;
        if (index == I)
            return std::invoke(std::forward<F>(f), *reinterpret_cast<const variant_t*>(ptr), std::forward<Ts>(args)...);
        else
            return visit_clazz_variant<R, Variants...>(std::index_sequence<Is...>{}, index, ptr, std::forward<F>(f), std::forward<Ts>(args)...);
    }

    template<class R, class... Variants, class T, class F, class... Ts>
    inline constexpr R visit_clazz_variant(std::index_sequence<>, unsigned char, T*, F&&, Ts&&...) noexcept {
        // Unrecoverable error if you are calling a variant with invalid index
        std::terminate();
    }
}

namespace serialisation {
    namespace detail {
        template<Clazz C>
        struct clazz_ser : clazz_ser<typename clazz_info<C>::values_info_t::clazz_t> {};
        template<Clazz C>
        struct clazz_deser : clazz_deser<typename clazz_info<C>::values_info_t::clazz_t> {};
    }

    template<class T>
    struct type_info {
        static_assert(false_v<>, "Not implemented for this type!"); 
    };

    void encode_varint(std::string& output, size_t value) {
        while (value > 127) {
            // Set the next byte flag
            output += ((char)(value & 127)) | 128;
            value >>= 7;
        }
        output += ((char)value) & 127;
    }

    constexpr char varint_size(size_t value) {
        size_t bits_needed = std::log2p1(value);
        return (bits_needed % 7) ? bits_needed / 7 + 1 : bits_needed / 7;
    }

    constexpr size_t decode_varint(const char*& input) {
        size_t ret = 0;
        for (size_t i = 0; ; ++input, ++i) {
            ret |= (*input & 127) << (7 * i);
            // If the next-byte flag is set
            if(!(*input & 128)) {
                break;
            }
        }
        ++input;
        return ret;
    }

    template<class T>
    constexpr size_t get_space_used(const T& in) { 
        return type_info<T>::get_space_used_(in);
    }
    template<class T>
    constexpr void encode_type(std::string& buffer, const T& in) {
        type_info<T>::encode_type_(buffer, in);
    }
    template<class T>
    constexpr T decode_type(const char* buffer, size_t size) {
        return type_info<T>::decode_type_(buffer, size);
    }

    template<class T>
    requires (std::is_trivially_copyable_v<T>)
    struct type_info<T> {
        static constexpr size_t get_space_used_(const T&) { 
            return sizeof(T);
        }

        static constexpr void encode_type_(std::string& buffer, const T& in) {
            buffer.append((char*)&in, sizeof(T));
        }

        static T decode_type_(const char* buffer, size_t) {
            T out;
            std::memcpy(&out, buffer, sizeof(T));
            return out;
        }
    };

    template<class T, size_t N>
    struct type_info<std::array<T, N>> {
        static constexpr size_t get_space_used_(const std::array<T, N>& a) { 
            if constexpr (std::is_trivially_copyable_v<T>)
                return sizeof(T) * N;
            else {
                return accumulate(begin(a), end(a), 0, [](auto acc, auto& el) {
                    size_t space = get_space_used(el);
                    return acc + varint_size(space) + space;
                });
            }
        }

        static constexpr void encode_type_(std::string& buffer, const std::array<T, N>& in) {
            if constexpr (std::is_trivially_copyable_v<T>)
                buffer.append((char*)in.data(), N * sizeof(T));
            else if constexpr ((Clazz<T>)) {
                return [&]<class... V>(clazz_info<clazz<V...>>) {
                    using namespace detail;
                    constexpr auto field_list = clazz_ser<T>::clazz_fields_header;
                    buffer.append(field_list.data(), field_list.size());
                    for (auto& el : in) {
                        auto [body_buffer, sizes] = clazz_ser<T>::clazz_body(el);
                        for (auto size : sizes) {
                            encode_varint(buffer, size);
                        }
                        buffer += body_buffer;
                    }
                }(typename clazz_info<T>::variables_info_t{});
            } else 
                for (auto& el : in) {
                    encode_varint(buffer, get_space_used(el));
                    encode_type(buffer, el);
                }
        }

        static constexpr auto decode_type_(const char* buffer, size_t size) {
            return [=]<size_t... I>(std::index_sequence<I...>) mutable {
                if constexpr (std::is_trivially_copyable_v<T>) {
                    return std::array{((const T*)buffer)[I]...};
                } else if constexpr ((Clazz<T>)) {
                    using namespace detail;
                    const auto header = clazz_deser<T>::read_header(buffer);

                    return std::array{[&](size_t) {
                        return clazz_deser<T>::read_body(buffer, header);
                    }(I)...};
                } else {
                    size_t offset = 0;
                    return std::array{[&](size_t) {
                        size_t size = decode_varint(buffer);
                        auto buf = buffer + offset;
                        offset += size;
                        return decode_type<T>(buf, size);
                    }(I)...};
                }
            }(std::make_index_sequence<N>{});
        }
    };

    template<class T, class A>
    struct type_info<std::vector<T, A>> {
        static constexpr size_t get_space_used_(const std::vector<T, A>& v) { 
            if constexpr (std::is_trivially_copyable_v<T>)
                return sizeof(T) * v.size();
            else {
                return varint_size(v.size()) + accumulate(begin(v), end(v), 0, [](auto acc, auto& el) {
                    size_t space = get_space_used(el);
                    return acc + varint_size(space) + space;
                });
            }
        }

        static constexpr void encode_type_(std::string& buffer, const std::vector<T, A>& in) {
            if constexpr (std::is_trivially_copyable_v<T>)
                buffer.append((char*)in.data(), in.size() * sizeof(T));
            else {
                encode_varint(buffer, in.size());
                for (auto& el : in) {
                    encode_varint(buffer, get_space_used(el));
                    encode_type(buffer, el);
                }
            }
        }

        static std::vector<T, A> decode_type_(const char* buffer, size_t size) {
            if constexpr (std::is_trivially_copyable_v<T>)
                return {(const T*)buffer, (const T*)(buffer + size)};
            else {
                const auto end = buffer + size;
                size_t arity = decode_varint(buffer);
                std::vector<T, A> vec;
                vec.reserve(arity);
                
                while (buffer < end) {                
                    size_t size = decode_varint(buffer);
                    vec.emplace_back(decode_type<T>(buffer, size));
                    buffer += size;
                }
                return vec;
            }
        }
    };

    template<>
    struct type_info<std::string> {
        static size_t get_space_used_(const std::string& s) { 
            return s.size();
        }

        static void encode_type_(std::string& buffer, const std::string& in) {
            buffer.append(in);
        }

        static std::string decode_type_(const char* buffer, size_t size) {
            return {buffer, size};
        }
    };

    template<Symbol... S>
    struct type_info<clazz<S...>> {
        static constexpr size_t get_space_used_(const clazz<S...>& c) {
            using value_clazz_info = typename clazz_info<clazz<S...>>::variables_info_t;
            
            size_t space = 0;
            [&]<class... V>(clazz_info<clazz<V...>>) {
                constexpr auto names_length = [] {
                    auto names = tags_info<symbol_tag_t<V>...>::names_array;
                    size_t len = 0;
                    for (auto name : names) len += name.length() + 1;
                    return len;
                }();
                space += names_length;
                const auto header_loop = [&]<class X>(type_list<X>) {
                    size_t var_space = get_space_used(get<symbol_tag_t<X>>(c));
                    space += var_space;
                    space += varint_size(var_space);
                };
                (header_loop(type_list<V>{}), ...);
            }(value_clazz_info{});
            return space;
        }

        static void encode_type_(std::string& buffer, const clazz<S...>& in) {
            ser(buffer, in);
        }

        static clazz<S...> decode_type_(const char* buffer, size_t) {
            return deser<clazz<S...>>(buffer);
        }
    };

}

template<Symbol... S>
void ser_orig(std::string& buffer, const clazz<S...>& c) {
    using namespace clz::serialisation;
    using value_clazz_info = typename clazz_info<clazz<S...>>::variables_info_t;

    [&]<class... V>(clazz_info<clazz<V...>>) {
        std::string header_buffer;
        const auto header_loop = [&]<class X>(type_list<X>) {
            header_buffer += symbol_tag_info<X>::name;
            header_buffer += '\0';
            encode_varint(header_buffer, get_space_used(get<symbol_tag_t<X>>(c)));
        };
        (header_loop(type_list<V>{}), ...);

        encode_varint(buffer, header_buffer.size());
        buffer += header_buffer;

        const auto body_loop = [&]<class X>(type_list<X>) {
            encode_type(buffer, get<symbol_tag_t<X>>(c));
        };
        (body_loop(type_list<S>{}), ...);
    }(value_clazz_info{});
}

template<Clazz Clz>
Clz deser_orig(const char* const buffer) {
    using value_clazz_info = typename clazz_info<Clz>::variables_info_t;
    return [buf = buffer]<class... V, size_t... I>(clazz_info<clazz<V...>>, std::index_sequence<I...>) mutable {
        using namespace serialisation;
        const size_t header_size = decode_varint(buf);
        const char* const body_start = buf + header_size;

        constexpr auto fields = tags_info<symbol_tag_t<V>...>::names_array;
        std::pair<size_t, size_t> body_locations[std::size(fields)];

        size_t running_total = 0;
        for (const char* name_start = buf; buf < body_start; ++buf) {
            if (*buf != '\0') continue;

            auto name = std::string_view(name_start, buf - name_start);

            // Decode size
            ++buf;
            size_t size = decode_varint(buf);

            // Find field name in array and assign running_total to respective location
            auto it = find(begin(fields), end(fields), name);
            if (it != end(fields)) {
                body_locations[it - begin(fields)] = {running_total, size};
            }

            name_start = buf;
            running_total += size;
        }

        return Clz{(symbol_tag_info<V>::arg = [&] {
            auto ptr = body_start + body_locations[I].first;
            auto size = body_locations[I].second;
            return decode_type<symbol_value_t<V>>(ptr, size);
        })...};
    }(value_clazz_info{}, std::make_index_sequence<value_clazz_info::size>{});
}

namespace serialisation::detail {
    template<Variable... V>
    struct clazz_ser<clazz<V...>> {
        static constexpr auto clazz_fields_header = [] {
            constexpr auto names = tags_info<symbol_tag_t<V>...>::names_array;
            constexpr auto names_length = [=] {
                size_t len = 0;
                for (auto name : names) len += name.length() + 1;
                return len;
            }();

            std::array<char, names_length> field_buffer{};
            for (size_t i = 0, f = 0, fi = 0; i < names_length; ) {
                field_buffer[i] = names[f][fi];
                ++i;
                ++fi;
                if (fi == names[f].length()) { 
                    field_buffer[i] = ',';
                    ++i;
                    ++f;
                    fi = 0;
                }
            }
            field_buffer[names_length-1] = '.';
            return field_buffer;
        }();

        struct clazz_body_template {
            std::string buffer;
            size_t sizes[sizeof...(V)];
        };

        static clazz_body_template clazz_body(const clazz<V...>& c) {
            return clazz_body(c, std::index_sequence_for<V...>{});
        }

    private:
        // Keep in sync with ser()
        template<size_t... I>
        static clazz_body_template clazz_body(const clazz<V...>& c, std::index_sequence<I...>) {
            clazz_body_template result;

            // Write out body and record sizes
            size_t start = 0;
            ([&]<class X>(type_list<X>) {
                encode_type(result.buffer, get<symbol_tag_t<X>>(c));
                result.sizes[I] = result.buffer.size() - start;
                start = result.buffer.size();
            }(type_list<V>{}), ...);

            return result;
        }

    };

}

template<Symbol... S>
void ser(std::string& buffer, const clazz<S...>& c) {
    using namespace serialisation;
    using namespace serialisation::detail;

    auto [body_buffer, sizes] = clazz_ser<clazz<S...>>::clazz_body(c);
    constexpr auto field_list = clazz_ser<clazz<S...>>::clazz_fields_header;

    const auto total_varint_size = std::accumulate(std::begin(sizes), std::end(sizes), 0, [](auto acc, auto size) {
        return acc + varint_size(size);
    });
    buffer.reserve(buffer.size() + field_list.size() + total_varint_size + body_buffer.size());
    buffer.append(field_list.data(), field_list.size());
    for (size_t i = 0; i < std::size(sizes); ++i) {
        encode_varint(buffer, sizes[i]);
    }

    buffer += body_buffer;
}

namespace serialisation::detail {
    struct clazz_header_read {
        uint8_t skip;
        uint8_t id;
    };

    template<size_t N>
    struct clazz_header {
        clazz_header_read read[N];
        uint8_t their_fld_cnt = 0;
    };

    template<Variable... V>
    struct clazz_deser<clazz<V...>> {
        static auto read_header(const char*& buf) {
            clazz_header<sizeof...(V)> header;

            constexpr auto fields = tags_info<symbol_tag_t<V>...>::names_array;
            static_assert(sizeof(decltype(header.read)) == 2*sizeof...(V));

            uint8_t our_fld_cnt = 0;
            uint8_t skip_fld_cnt = 0;
            for (const char* name_start = buf; ; ++buf) {
                if (*buf != ',' && *buf != '.') continue;

                ++header.their_fld_cnt;

                auto name = std::string_view(name_start, buf - name_start);

                // Find field name in array and assign running_total to respective location
                if (auto it = find(begin(fields), end(fields), name); it != end(fields)) {
                    header.read[our_fld_cnt].skip = skip_fld_cnt;
                    header.read[our_fld_cnt].id = it - begin(fields);
                    ++our_fld_cnt;
                    skip_fld_cnt = 0;
                } else {
                    ++skip_fld_cnt;
                }

                if (*buf == '.') break;
                name_start = buf + 1;
            }
            if (our_fld_cnt != sizeof...(V)) {
                std::cout << "our count is " << (int)our_fld_cnt << " but should be " << sizeof...(V);
                assert(!"Not enough fields found in header during deserialisation of clazz");
            }
            buf += 1;

            return header;
        }

        static clazz<V...> read_body(const char*& buf, const clazz_header<sizeof...(V)>& header) {
            return read_body(buf, std::index_sequence_for<V...>{}, header);
        }

    private:
        template<size_t... I>
        static clazz<V...> read_body(const char*& buf, std::index_sequence<I...>, const clazz_header<sizeof...(V)>& header) {
            struct {
                size_t offset;
                size_t size;
            } details[sizeof...(V)];

            size_t offset = 0;
            size_t theirs = 0;
            for (size_t ours = 0; ours < sizeof...(V); ++theirs, ++ours) {
                size_t size = decode_varint(buf);
                for (int skip = 0; skip < header.read[ours].skip; ++skip) {
                    offset += size;
                    ++theirs;
                    size = decode_varint(buf);
                }
                details[header.read[ours].id] = {.offset = offset, .size = size};
                offset += size;
            }
            for (; theirs < header.their_fld_cnt; ++theirs) {
                offset += decode_varint(buf);
            }
            auto body = buf;
            buf += offset;

            return {
                (symbol_tag_info<V>::arg = [buffer = body + details[I].offset, size = details[I].size] {
                    return decode_type<symbol_value_t<V>>(buffer, size);
                })...
            };
        }
    };
}

template<Clazz Clz>
Clz deser(const char* const buffer) {
    using namespace serialisation::detail;
    auto buf = buffer;
    const auto header = clazz_deser<Clz>::read_header(buf);
    return clazz_deser<Clz>::read_body(buf, header);
}

template<Symbol... S>
std::string ser(const clazz<S...>& c) {
    using namespace clz::serialisation;
    std::string buffer;
    ser(buffer, c);
    return buffer;
}

}

namespace std {
    template<size_t I, CLAZZ_NS::Clazz C>
    constexpr inline decltype(auto) get(C&& c) {
        return CLAZZ_NS::get<I>(std::forward<C>(c));
    }

    template<CLAZZ_NS::Clazz C>
    struct tuple_size<C> : std::integral_constant<size_t, CLAZZ_NS::clazz_info<C>::size> {};

    template<size_t N, CLAZZ_NS::Clazz C>
    struct tuple_element<N, C> {
        using type = typename CLAZZ_NS::clazz_info<C>::template index_element_t<N>;
    };

    template<CLAZZ_NS::Clazz C>
    struct hash<C> {
        constexpr size_t operator()(const C& c) const {
            return CLAZZ_NS::hash(c);
        }
    };

    template<CLAZZ_NS::Clazz L>
    void swap(L& l, L& r) {
        swap(l, r);
        std::cout << "std swapped\n";
    }

    template<class CV, bool B>
    void iter_swap(CLAZZ_NS::cvector_iterator<CV, B>& l, CLAZZ_NS::cvector_iterator<CV, B>& r) {
        (*l).swap(*r);
        std::cout << "std iter swapped\n";
    }

    template<class T, size_t A, class U, class V>
    void erase(CLAZZ_NS::cvector_impl<T, A, U>& c, const V& value) {
        c.resize(c.move_elements([&](auto begin, auto end) {
            return std::remove(begin, end, value);
        }));
    }

    template<class T, size_t A, class U, class Pred>
    void erase_if(CLAZZ_NS::cvector_impl<T, A, U>& c, Pred pred) {
        using cvector_t = CLAZZ_NS::cvector_impl<T, A, U>;
        c.resize(c.move_elements([&](auto begin, auto end) {
            if constexpr (std::is_invocable_r_v<bool, Pred, typename cvector_t::size_type>)
                return std::remove_if(begin, end, pred);
            else if constexpr (std::is_invocable_r_v<bool, Pred, typename cvector_t::reference>) {
                return std::remove_if(begin, end, [&](cvector_t::size_type index) -> bool {
                    return std::invoke(pred, c[index]);
                });
            } else
                static_assert(clz::false_v<>, 
                    "Predicate must return bool and take either cvector<...>::size_type "
                    "or cvector<...>::const_reference");
        }));
    }

}

// TEST CODE
DECLARE_STRUPLE_SYMBOL(eat)
DECLARE_STRUPLE_SYMBOL(cats)
DECLARE_STRUPLE_SYMBOL(dogs)
DECLARE_STRUPLE_SYMBOL(cows)
DECLARE_STRUPLE_SYMBOL(print)
DECLARE_STRUPLE_SYMBOL(x)
DECLARE_STRUPLE_SYMBOL(y)
DECLARE_STRUPLE_SYMBOL(z)
DECLARE_STRUPLE_SYMBOL(name)
DECLARE_STRUPLE_SYMBOL(age)

#define arg ::CLAZZ_NS::arg::
#define tag ::CLAZZ_NS::tag::
#define var ::CLAZZ_NS::var::
#define val ::CLAZZ_NS::val::
#define dec ::CLAZZ_NS::dec::
#define def ::CLAZZ_NS::def::
#define dfn ::CLAZZ_NS::dfn::
#define fun ::CLAZZ_NS::fun::
#define ovl ::CLAZZ_NS::ovl::
#define tpe ::CLAZZ_NS::tpe::
#define clazz ::CLAZZ_NS::clazz
#define nuple ::CLAZZ_NS::nuple
#define args ::CLAZZ_NS::args

using namespace CLAZZ_NS;

inline clazz<var _1<int>, var _2<double>> testf() {
    return {1, 2.0};
}

inline nuple<int, double> testf2() {
    return {1, 2.0};
}

size_t test_vectorisation(clz::cvector<clazz<var _1<int>, var _2<double>, var _3<std::string>>>& in) {
// size_t test_vectorisation() {
    // clz::cvector<clazz<var _1<int>, var _2<double>, var _3<std::string>>> in;
    // in.emplace_back(1, 1.0, "hi1");
    // in.emplace_back(2, 2.0, "hi2");
    // std::erase_if(in, [](Clazz auto el) { return el._1 == 2; });
    // return std::accumulate(in.begin(), in.end(), 0, 
    //     [](auto count, auto&& el) { 
    //         return count + el._1; 
    //     });
    size_t icount = 0;
    double dcount = 0;
    for (auto el : in) {
        icount += el._1;
        // dcount += el._2;
    }
    for (auto el : in) {
        // icount += el._1;
        dcount += el._2;
    }
    // for (size_t i = 0; i < in.size(); ++i) {
    //     count += in->_1[i];
    //     // count += in->_2[i];
    // }
    return icount + dcount;
}

using type = meta_pod <
    var _1 <int>,
    var _2 <double>
>:: with_symbols <
    val _3 <int, 100>,
    fun _4 <int(),
        [] { return 1'000; }
    >,
    fun _5 <int(int) const,
        [](int in) {
            return 10'000 * (in % 10);
        }
    >,
    def _6 <
        [](const Clazz<tag _1> auto& clz, int x) {
            return x + clz._1*100'000;
        }
    >,
    def _7 <
        [](mut auto& self) {
            return 1'000'000 * (self._1 *= 1);
        }
    >,
    def _8 <
        [](const auto& self, int in) {
            return 10'000'000 * (in % 10) * self._1;
        }
    >,
    ovl _9 <
        // TODO: Add back when GCC concepts subsumption bug goes
        // fun _9 <int() const,
        //     [] { return 100'000'000; }
        // >,
        def _9 <
            [](auto&, int i) { return 100'000'000 * (i % 10); }
        >
    >
    // ,
    // def operator_pointer<[](auto& ths) {
    //     return &ths;
    // }>
>::clazz_t;

static_assert(std::is_same_v<var _1<int>, clazz_info<type>::tag_symbol_t<tag _1>>);

using type2 = clazz <
    var _1 <int>,
    tpe _2 <double>
>;

static_assert(std::is_same_v<double, typename type2::_2>);

auto named_def = overload {
    [](auto& self) {
        return 10'000'000 * self._1;
    },
    [](auto& self, int in) {
        return 10'000'000 * (in % 10) * self._1;
    }
};

static_assert(clazz_info<type>::has_dec<dec _1<int>>);
static_assert(clazz_info<type>::has_dec<dec _2<double>>);
static_assert(clazz_info<type>::has_dec<dec _3<int, true>>);
static_assert(clazz_info<type>::has_dec<dec _4<int() const, true>>);
static_assert(clazz_info<type>::has_dec<dec _5<int(int) const, true>>);
static_assert(clazz_info<type>::has_dec<dec _6<int(int) const>>);
static_assert(clazz_info<type>::has_dec<dec _7<int()>>);
static_assert(clazz_info<type>::has_dec<dec _8<int(int) const>>);
// TODO: Add back
// static_assert(clazz_info<type>::has_dec<dec _9<int() const, true>>);
static_assert(clazz_info<type>::has_dec<dec _9<int(int) const>>);

inline type testF(const clazz<var _1<int, 1>, var _2<double>>& clz) {
    //return {arg::_1 = 1, arg::_2 = 20.0};
    clz->template get_or<tag _3>(3);
    return clz;
}

using sub_type = clazz <
    dfn _7 <int(), 
        [](mut auto&){return 1;}
    >,
    ovl _9 <
        // fun _9 <int() const,
        //     [] { return 100'000'000; }
        // >,
        dfn _9 <int(int) const,
            [](const auto&, int i) { return 100'000'000 * (i % 10); }
        >
    >
>;
static_assert(SuperClazzOf<sub_type, type>);

// template<class F>
// void for_each(Clazz&& )


using Cat = clazz <
    fun eat <int(), [] { return 1; }>
>;
using Dog = clazz <
    fun eat <int(), [] { return 2; }>
>;
using Cow = clazz <
    fun eat <int(), [] { return 3; }>
>;

using Animals = clazz <
    var cats <std::array<Cat, 1>>,
    var dogs <std::array<Dog, 1>>,
    var cows <std::array<Cow, 1>>
    //, def operator_pointer<[](auto& self) { return &self; }> 
>;

template<class T, class... Ds>
concept ContainerImplements = ((Declaration<Ds> && ...) && Implements<typename std::decay_t<T>::value_type, Ds...>);

long retT(const clazz<var _1<std::string>>& in) {
    return in._1.length();
}

using ATest = decltype(arg _1 = 1);
static_assert(std::is_same_v<typename ATest::var_t, var _1<int>>);

static bool trivial_tuple_test() {
    auto c = clazz{arg _1 = 1, arg _2 = 2.0};
    auto [_1, _2] = c->tuple;
    auto& [__1, __2] = c;
    static_assert(std::is_same_v<decltype(_1), int>);
    static_assert(std::is_same_v<decltype(__1), int>);
    static_assert(std::is_same_v<decltype(_2), double>);
    static_assert(std::is_same_v<decltype(__2), double>);
    return _1 == c._1 && _2 == c._2 && __1 == _1 && __2 == _2;
}

using testComp = clazz <
    var _1<std::string>,
    var _2<std::string, []{return "yo";}>
>;

auto ttc = [](const testComp& t) {
    return t._2.length();
};

struct ttc_in {
    const char* _1 = "";
    const char* _2 = "";
};

// namespace detail {
//     template<typename T>
//     struct type_id {
//         static const unsigned char id;
//     };

//     template<typename T>
//     const unsigned char type_id<T>::id = 0;
// } 

// template <typename T>
// constexpr const void* type_id = &detail::type_id<T>::id;

using var1 = clazz <
    var _1<int>,
    var _2<int>,
    var _3<int>,
    def _10<[](auto& self) { 
        return self._1 + self._2 + self._3;
    }>
>;

using var2 = clazz <
    var _1<int>,
    var _2<int>,
    var _3<int>,
    var _4<int>,
    var _5<int>,
    def _10<[](auto& self) { 
        return self._1 + self._2 + self._3 + self._4 + self._5;
    }>
>;

using var3 = clazz <
    var _1<int>,
    var _2<int>,
    var _3<int>,
    var _4<int>,
    var _5<int>,
    var _6<int>,
    def _10<[](auto& self) { 
        return self._1 + self._2 + self._3 + self._4 + self._5 + self._6;
    }>
>;

using n_trait = trait<dec _10<int()>>;

struct tracker { 
    int i; 
    bool operator<(const tracker& t) const {
        return i < t.i;
    }
    tracker() : i(1) {
        std::cout << "tracker default ctor\n";
    }
    ~tracker() {
        std::cout << "tracker dtor\n";
    }
    tracker(int n) : i(n) {
        std::cout << "tracker int ctor\n";
    }
    tracker(const tracker& t) : i(t.i) { 
        std::cout << "tracker copied\n";
    } 
    tracker(tracker&& t) : i(t.i) { 
        std::cout << "tracker move ctr\n";
    } 
    tracker& operator=(const tracker& t) {
        i = t.i;
        std::cout << "tracker copy assign\n";
        return *this;
    }
    tracker& operator=(tracker&& t) {
        i = t.i;
        std::cout << "tracker move assign\n";
        return *this;
    }
};

using cadef = def operator_assign<[]<class T>(T& self, T&&) -> T& {
    std::cout << "custom move assigned\n";
    return self;
}>;
using cus_ass = clazz <
   cadef 
>;

using casym = symbol_info<cadef>::struple_element_t<cus_ass>;
static_assert(symbol_info<casym>::template has_wider_dec<dec operator_assign<void(cus_ass&&)>>);

template<class T>
void printClazzEl(const T& el);

template<Symbol... S>
auto& printClazz(const clazz<S...>& c);

template<class T>
void printClazzEl(const std::vector<T>& c);

template<class T, size_t N>
void printClazzEl(const std::array<T, N>& c);

template<Symbol... S>
void printClazzEl(const clazz<S...>& c);

template<class T>
void printClazzEl(const T& el) {
    std::cout << el;
}

template<Symbol... S>
void printClazzEl(const clazz<S...>& c) {
    printClazz(c);
}

template<class T>
void printClazzEl(const std::vector<T>& c) {
    std::cout << "[";
    for (bool first = true; auto& el : c) {
        if (first) first = false;
        else std::cout << ", ";
        printClazzEl(el);
    }
    std::cout << "]";
}

template<class T, size_t N>
void printClazzEl(const std::array<T, N>& c) {
    std::cout << "[";
    for (bool first = true; auto& el : c) {
        if (first) first = false;
        else std::cout << ", ";
        printClazzEl(el);
    }
    std::cout << "]";
}

template<Symbol... S>
auto& printClazz(const clazz<S...>& c) {
    std::cout << "clazz{";
    bool first = true;
    ([&] {
        if (first) first = false;
        else std::cout << ", ";
        std::cout << symbol_tag_info<S>::name << ": ";
        printClazzEl(get<symbol_tag_t<S>>(c));
    }(), ...);
    return std::cout << "}";
}

using namespace std::string_literals;

int foo(int argc, char** argv) {
    if (!trivial_tuple_test()) {
        std::terminate();
        return 0;
    }

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

    printClazz(stuff);

    std::cout << "clazz size: " << sizeof(stuff) << '\n'
            << "buffer size: " << buffer.size() << '\n'
            << buffer << '\n';

    // Can regenerate the same data with the buffer, even though the type has fields in a different order
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

    // Can construct any sub-type from the serialised data
    auto deser_sub_from_full = deser<sub_t>(buffer.c_str());
    assert(less_stuff == deser_sub_from_full);

    // std::cout << std::bitset<64>(hash(c2enc)) << '\n';
    // std::cout << std::bitset<64>(hash(c2dec)) << '\n';
    // std::cout << std::bitset<64>(hash(c2dec2)) << '\n';

    constexpr auto hash_test1 = clazz{arg _1 = 2, arg _2 = 3.0};
    constexpr auto hash_test2 = clazz{arg _2 = 3.0, arg _1 = 2};
    constexpr auto hash_test3 = clazz{arg _2 = 3, arg _1 = 2};
    static_assert(hash(hash_test1) == hash(hash_test2));
    static_assert(hash(hash_test1) != hash(hash_test3));

    size_t ids[] = {
        clazz_id<decltype(clazz{arg x = 0})>,
        clazz_id<decltype(clazz{arg y = 0})>,
        clazz_id<decltype(clazz{arg z = 0})>
    };
    size_t hashes[] = {
        hash(clazz{arg x = 0}),
        hash(clazz{arg x = 1}),
        hash(clazz{arg x = 2}),
        hash(clazz{arg y = 0}),
        hash(clazz{arg y = 1}),
        hash(clazz{arg y = 2}),
        hash(clazz{arg z = 0}),
        hash(clazz{arg z = 1}),
        hash(clazz{arg z = 2})
    };

    std::cout << "Clazz x: " << std::bitset<64>(ids[0])    << " popcount is " << std::popcount(ids[0])    << " diff is " << std::popcount(ids[0]^hashes[0]) << '\n';
    std::cout << "Hash x0: " << std::bitset<64>(hashes[0]) << " popcount is " << std::popcount(hashes[0]) << " diff is " << std::popcount(hashes[0]^hashes[1]) << '\n';
    std::cout << "Hash x1: " << std::bitset<64>(hashes[1]) << " popcount is " << std::popcount(hashes[1]) << " diff is " << std::popcount(hashes[1]^hashes[2]) << '\n';
    std::cout << "Hash x2: " << std::bitset<64>(hashes[2]) << " popcount is " << std::popcount(hashes[2]) << " diff is " << std::popcount(hashes[2]^hashes[0]) << '\n';
    std::cout << "Clazz y: " << std::bitset<64>(ids[1])    << " popcount is " << std::popcount(ids[1])    << " diff is " << std::popcount(ids[1]^hashes[3]) << '\n';
    std::cout << "Hash y0: " << std::bitset<64>(hashes[3]) << " popcount is " << std::popcount(hashes[3]) << " diff is " << std::popcount(hashes[3]^hashes[4]) << '\n';
    std::cout << "Hash y1: " << std::bitset<64>(hashes[4]) << " popcount is " << std::popcount(hashes[4]) << " diff is " << std::popcount(hashes[4]^hashes[5]) << '\n';
    std::cout << "Hash y2: " << std::bitset<64>(hashes[5]) << " popcount is " << std::popcount(hashes[5]) << " diff is " << std::popcount(hashes[5]^hashes[3]) << '\n';
    std::cout << "Clazz z: " << std::bitset<64>(ids[2])    << " popcount is " << std::popcount(ids[2])    << " diff is " << std::popcount(ids[2]^hashes[6]) << '\n';
    std::cout << "Hash z0: " << std::bitset<64>(hashes[6]) << " popcount is " << std::popcount(hashes[6]) << " diff is " << std::popcount(hashes[6]^hashes[7]) << '\n';
    std::cout << "Hash z1: " << std::bitset<64>(hashes[7]) << " popcount is " << std::popcount(hashes[7]) << " diff is " << std::popcount(hashes[7]^hashes[8]) << '\n';
    std::cout << "Hash z2: " << std::bitset<64>(hashes[8]) << " popcount is " << std::popcount(hashes[8]) << " diff is " << std::popcount(hashes[8]^hashes[6]) << '\n';

    std::cout << type_name_v<clazz_asc <
        var _1<int>,
        var _2<char>,
        var _3<int>,
        var _4<char>,
        var _5<int>,
        var _6<char>,
        var _7<char>,
        var _8<char>,
        var _9<char>,
        var _10<int>,
        var _11<int>,
        var _12<int>
    >> << '\n';

    cus_ass ca1;
    ca1 = cus_ass{};
    std::cout << "did a custom move assignment\n";

    using multi = clazz<var _1<int>, var _2<int>>;
    auto m1 = multi(args<tag _1, tag _2> = 42);
    auto m2 = make_clazz(args<tag _1, tag _2> = 42);
    static_assert(std::is_same_v<decltype(m1), decltype(m2)>);
    std::cout << "m1._1 is " << m1._1 << " and m2._2 is " << m1._2 << '\n';
    std::cout << "m1 == m2 is " << (m1 == m2 ? "true" : "false") << '\n';

    auto c1 = clazz{arg _1 = 1, arg _2 = 2};
    auto c2 = clazz{arg _2 = 2, arg _1 = 1};
    using std::swap;
    swap(c1, c2);

    auto fwd_c = map(clazz{arg _1 = 1}, [](auto&& v) -> auto&& { return std::move(v); });
    static_assert(meta_clazz_t<decltype(fwd_c)>::has_dec<dec _1<int&&>>);

    using view_test = clazz<var _1<int>>;
    view_test v1 = {1};
    view_t<view_test> v2 = v1;
    view_t<view_test> v3 = v2;
    view_t<view_test> v4 = std::move(v2);

    std::swap(v2, v3);

    int n;
    std::string s;
    auto t1 = clazz<var _1<int&>, var _2<std::string&>>(n, s);
    auto t2 = clazz<var _1<int&>, var _2<std::string&>>(n, s);
    std::cout << "std::swap\n";
    std::swap(t1, t2);

    int soa_count = 0;
    using soa_el = clazz<
        var _1<int>, 
        var _2<std::string, []{return "yo";}>, 
        var _3<tracker, []{return tracker(1);}>,
        def print<[](auto& self) {
            std::cout << "{" << self._1 << "," << self._2 << "," << self._3.i << "}\n";
        }>
    >;
    cvector<soa_el> soa;
    auto soa2 = soa;

    using real_cap_test = cvector<clazz<
        var _1<char>,
        var _2<char>
    >>;

    real_cap_test captest;
    std::cout << "captest capacity is: " << captest.capacity() << '\n';
    captest.reserve(65);
    std::cout << "captest capacity is: " << captest.capacity() << '\n';

    std::cout << type_name_v<decltype(soa.data())> << '\n';

    std::cout << "push (3,no)...\n";
    soa.push_back({3, "no"});
    std::cout << "push (2,po)...\n";
    soa.push_back({2, "po"});
    std::cout << "iter_swap...\n";
    auto b1 = soa.begin();
    std::iter_swap(b1, b1);
    std::cout << "iter deref assign...\n";
    *b1 = *b1;
    std::cout << "shrink...\n";
    soa.shrink_to_fit();
    std::cout << "push (6,yo)...\n";
    soa.push_back(6);
    std::cout << "push (6,ho)...\n";
    soa.push_back(6, "ho");
    std::cout << "emplace (4, ppp) ...\n";
    soa.emplace_back(arg _1 = 4, arg _2(3,'p'), arg _3 = []{ return 6; });
    std::cout << "emplace (4, ppp) ...\n";
    soa.emplace_back(arg _1 = 4, arg _2(3,'p'), arg _3(5));

    std::cout << "assigning lots ...\n";
    soa.append(20, clazz {
        arg _1 = [](size_t i) {
            return 20 - i;
        }, 
        arg _2 = [](size_t i) {
            return std::to_string(20 - i);
        },
        arg _3 = [](size_t i) {
            return tracker(20 - i);
        }
    });

    soa.append(20, clazz {
        arg _1 = [](size_t i) {
            return 20 - i;
        }, 
        arg _2 = [](size_t i) {
            return std::to_string(20 - i);
        },
        arg _3 = [](size_t i) {
            return tracker(20 - i);
        }
    });

    soa.append(20, clazz {
        arg _1 = [](size_t i) {
            return 20 - i;
        }, 
        arg _2 = [](size_t i) {
            return std::to_string(20 - i);
        },
        arg _3 = [](size_t i) {
            return tracker(20 - i);
        }
    });

    std::cout << "sorting...\n";
    // TODO: Fix naive sort
    // std::sort(soa.begin(), soa.end());
    // soa.sort();
    soa.sort([](Clazz auto l, Clazz auto r) {
        return clazz_comparator<tag _3, tag _1, tag _2>::strong_less_than(l, r);   
    });
    std::cout << "removing where _1 is 20 or between 15 and 18 ...\n";
    std::erase_if(soa, [](Clazz auto i) { return (i._1 >= 15 && i._1 <= 18) || i._1 == 20; });
    // std::erase_if(soa, [_1 = soa->_1](size_t i) {
    //     return (_1[i] >= 15 && _1[i] <= 18) || _1[i] == 20;
    // });
    std::cout << type_name_v<decltype(soa)::reference> << '\n';
    std::cout << type_name_v<decltype(soa[0])> << '\n';
    // std::cout << "append1... \n";
    // soa.append(soa);
    // std::cout << "append2... \n";
    // soa.reserve(4*soa.size()); // Ensure soa.arrays() doesn't become old
    // soa.append(soa.size(), soa.arrays());
    // std::cout << "append3... \n";
    // soa.append(soa.size(), clazz{arg _1 = [&](size_t i) {
    //     return soa[i]._1;
    // }, arg _2 = [&](size_t i) {
    //     return soa[i]._2;
    // }, arg _3 = [&](size_t i) {
    //     return soa[i]._3;
    // }});
    for (auto el : soa) {
        soa_count += el._1 + el._2.length();
        el.print();
    }

    // soa_count = 0;
    // for (int i = 0; i < soa.size(); ++i) {
    //     auto el = soa[i];
    //     soa_count += el._1;
    // }
    // for (auto el : soa) {
    //     soa_count += el._1;
    // }
    // for (auto it = soa.begin(), 
    //     end = soa.end();
    //     it != end; 
    //     ++it) {
    //     soa_count += it->_1;
    // }
    // for (int i = 0; i < soa.size(); ++i) {
    //     soa_count += soa->_1[i];
    // }
    // for (auto& el : soa) {
    //     soa_count += el._1;
    // }

    auto data = soa.arrays_with_size();
    for (size_t i = 0; i < data._.size; ++i) {
        std::cout << "first " << data._1[i] << '\n';
    }

    auto agg = xmap(data, 
        args<tag _1, tag _> = [](int* array, auto& s) {
            return std::accumulate(array, array + s.size, 0) / s.size;
        },
        args<tag _2, tag _> = [](std::string* array, auto& s) {
            return std::accumulate(array, array + s.size, 0, [](int i, const std::string& s) {
                return i + s.length();
            }) / s.size;
        });

    std::cout << "avg _1 is " << agg._1 << ", and avg _2.length() is " << agg._2 << ", count is " << soa_count << '\n';

    auto data2 = soa.data();
    auto agg2 = xmap(data2, 
        arg _1 = [](auto span) {
            return std::accumulate(span.begin(), span.end(), 0) / span.size();
        },
        arg _2 = [](auto span) {
            return std::accumulate(span.begin(), span.end(), 0, [](int i, const std::string& s) {
                return i + s.length();
            }) / span.size();
        });

    std::cout << "avg2 _1 is " << agg2._1 << ", and avg2 _2.length() is " << agg2._2 << ", count is " << soa_count << '\n';

    return soa_count;

    // return hash(clazz{arg _1 = 1});

    // auto tup = std::tuple{1, 2};
    // auto nup = nuple(std::tuple{1,2});
    // using nupc_t = clazz<var _2<int>, var _3<int>>;
    // auto nupc = nupc_t(tup);
    // view_t<nupc_t> nupcv = nupc;
    // return nupcv._2 + nupcv._3;
    // auto& nupc2 = as_named_tuple<nuple>(tup);
    // return nupc2._1 + nupc2._2;

    // auto c = clazz<var _1<int, 1>, val::_2<int, 20>, val::_3<int, 300>>{};
    // auto t = tie<tag _1, tag _2>(c);
    // int x,y;
    // auto tie1 = std::tie(y,x);
    // auto tie2 = std::tie(x,y);
    // std::swap(tie1, tie2);
    // auto&& [first, second] = std::tie(x,y);
    // static_assert(std::is_lvalue_reference_v<decltype(first)>);
    // return first + second;
    
    auto pv = hvector<n_trait, sort_asc<var1>, sort_asc<var2>>();
    pv.emplace_back<var1>(arg _1 = 1, arg _2 = 20, arg _3 = 300);
    pv.emplace_back<var1>(arg _1 = 1, arg _2 = 20, arg _3 = 300);
    pv.pop_back();
    pv.emplace_back<var2>(arg _1 = 1, arg _2 = 20, arg _3 = 300, arg _4 = 4000, arg _5 = 50000);
    pv.reserve(200);
    int countv = 0;
    for (auto& i : pv) {
        countv += i._10();
    }
    std::cout << "countv is " << countv << '\n';

    auto pv2 = pv.transpose();
    int countv2 = 0;
    pv2->for_each_var([&](auto& v) {
        for (auto& i : v) {
            countv2 += i._10();
        }
    });
    std::cout << "countv2 is " << countv2 << '\n';
    return countv2;
    
    // auto v2 = var2(2, 30, 400, 5000, 60000);
    // auto v2 = var2(argv[0][0], argv[1][0], argv[2][0], argv[3][0], argv[4][0]);
    // auto v1 = var1(v2);
    
    // auto c1 = variant<var1>{argv[0][0], v1};
    // return reinterpret_cast<var_clazz<var1, var2, var3>&>(c1)._10();
    
    // constexpr bool compare = clazz{arg _1 = 0.9, arg _2 = 2, arg _4 = 0} < clazz<var _2<int, 2>, val::_1<int, 1>>{};
    // return compare;

    // auto testCompInst = testComp{arg _1 ="", arg _2(2, 'h')};
    // return testCompInst._2.length();
    // return testCompInst->call(ttc);
    // struct{const char* _1 = ""; std::string _2 = std::string(2, 'h');} in;
    // return ttc(in);
    // return ttc(ttc_in{._1 = "hi", ._2 = "no"});

    struct {
        int _3 = 12;
        int _1 = 9;
        int _2 = 10;
    } hi;
 
    auto inst = make_clazz<tag _1, tag _2, tag _3>(hi);
    auto inst2 = clazz<var _3<int>, var _1<int>, var _4<int, 3>>(hi);
    auto inst3 = meta_clazz<var _3<int>, var _1<int>, var _4<int, 3>>::import_with_defaults(hi);
    // return inst3._3 + inst3._1 + inst3._4;

    // return retT(clazz<var _1<const char*>>{"sashi"});
    // return retT(clazz{arg _1 = "sashi"});
    // return retT({arg _1 = "sashi"});

    Animals a {
        std::array<Cat, 1>{},
        std::array<Dog, 1>{},
        std::array<Cow, 1>{}
    };
    int count = 0;
    a->for_each_var([&](ContainerImplements<dec eat<int()const, true>> auto&& vec) {
        for (auto& animal : vec) {
            count += animal.eat();
        }
    });
    //return count;
    auto F = testF({arg _2 = 20.0});
    F._1 = 1;
    F._2 = 20;
    int i = 0;
    F->for_each_as_var(try_first {
        [&](var _1<int>& in) {
            i += in._1;
        }, 
        [&](var _2<double>& in) {
            i += in._2;
        }
    });
    return i;
    //F._8(std::string(""));
    return F._1 + F._2 + F._3 + F._4() + F._5(2) + F._6(7) + F._7() + F._8(3) 
    // TODO: Add back
    // + F._9() 
    + F._9(2)
    ;
}

int main(int argc, char** argv) {
    return foo(argc, argv);
    // return test_vectorisation();
}

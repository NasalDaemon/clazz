// GPLv3 @ github.com/MarkMitch/clazz

#include <algorithm>
#include <tuple>
#include <type_traits>
#include <functional>
#include <variant>
#include <string>
#include <iostream>
#include <memory>
#include <numeric>

#ifdef _MSC_VER
#define EBO_MSVC __declspec(empty_bases)
#else
#define EBO_MSVC
#endif

#define CLAZZ_NS clz

namespace CLAZZ_NS {
template<class = void>
constexpr bool false_v = false;

template<class To, class From>
requires sizeof(std::decay_t<To>) == sizeof(std::decay_t<From>)
constexpr To union_cast(From&& in) noexcept {
    return reinterpret_cast<To>(in);
}

// https://stackoverflow.com/questions/35941045/can-i-obtain-c-type-names-in-a-constexpr-way/35943472#35943472
template<class T>
constexpr auto type_name() {
    if (!std::is_constant_evaluated())
        throw "Must be called in constexpr context";

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
}

template<class T>
static constexpr std::string_view type_name_v = type_name<T>();

enum class symbol_type {
    invalid,
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

template<class Top, class T>
using symbol_element_t = typename symbol_info<T>::template struple_element_t<Top>;

template<class T>
concept bool Symbol = symbol_info<T>::type != symbol_type::invalid && sizeof(T) == sizeof(symbol_tuple_t<T>);

template<class T>
concept bool EmptySymbol = Symbol<T> && std::is_same_v<symbol_tuple_t<T>, std::tuple<>>;

template<class T>
concept bool DataSymbol = Symbol<T> && !EmptySymbol<T>;

template<class T>
concept bool Variable = DataSymbol<T> && symbol_info<T>::type == symbol_type::var;

template<class T>
concept bool StaticValue = EmptySymbol<T> && symbol_info<T>::type == symbol_type::val;

template<class T>
concept bool Value = Variable<T> || StaticValue<T>;

template<class T>
concept bool Definition = EmptySymbol<T> && symbol_info<T>::type == symbol_type::def;

template<class T>
concept bool DefinitionTemplate = Definition<T> && symbol_info<T>::is_tmpl_def;

template<class T>
concept bool Function = EmptySymbol<T> && symbol_info<T>::type == symbol_type::fun;

template<class T>
concept bool Overload = EmptySymbol<T> && symbol_info<T>::type == symbol_type::ovl;

template<class T>
concept bool FunctionOverload = Overload<T> && symbol_info<T>::is_all_funs;

template<class T>
concept bool DefinitionOverload = Overload<T> && symbol_info<T>::is_all_defs;

template<class T>
concept bool Method = Definition<T> || Function<T> || Overload<T>;

template<class T>
concept bool StaticMethod = Function<T> || FunctionOverload<T>;

template<class T>
concept bool StaticSymbol = StaticValue<T> || StaticMethod<T>;

template<class>
struct dec_info {
    static constexpr bool is_dec = false;
};

template<class T>
concept bool Declaration = dec_info<T>::is_dec;

template<class>
struct tag_info {
    static constexpr bool is_tag = false;
};

template<Symbol S>
using symbol_tag_info = tag_info<symbol_tag_t<S>>;

template<class T>
concept bool Tag = tag_info<T>::is_tag;

template<Tag... Tags>
struct tags_info {
    static constexpr size_t size = sizeof...(Tags);
    static constexpr bool empty = size == 0;

    // TODO, use array instead of tuple, when more std::array operations are constexpr P0202R3
    static constexpr auto names_array = std::tuple{tag_info<Tags>::name...};
    static constexpr auto names_array_array = std::array{tag_info<Tags>::name...};
};

template<class S, class N>
concept bool SymbolNamed = Symbol<S> && Tag<N> && symbol_info<S>::template has_name<N>;

template<class M, class N>
concept bool DefinitionNamed = Definition<M> && SymbolNamed<M, N>;

template<class M, class N>
concept bool FunctionNamed = Function<M> && SymbolNamed<M, N>;

template<class M, class N>
concept bool MethodNamed = Method<M> && SymbolNamed<M, N>;

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

template<class T>
struct is_clazz : std::false_type{};

template<Symbol... Ts>
struct is_clazz<clazz<Ts...>> : std::true_type {};

template<class T>
concept bool Clazz = is_clazz<std::decay_t<T>>::value;

template<Clazz C>
struct clazz_info : clazz_info<std::decay_t<C>> {};

template<class>
struct is_tuple : std::false_type {};

template<class... Ts>
struct is_tuple<std::tuple<Ts...>> : std::true_type {};

template<class T>
concept bool Tuple = is_tuple<std::decay_t<T>>::value;

template<class>
struct is_named_tuple : std::false_type {};

template<template<class> class>
struct is_named_tuple_wrapper : std::false_type {};

template<class T>
concept bool NamedTuple = is_named_tuple<std::decay_t<T>>::value;

template<template<class> class W>
concept bool NamedTupleWrapper = is_named_tuple_wrapper<W>::value;

template<Clazz T>
using meta_clazz_t = typename std::decay_t<T>::meta_clazz_t;

template<Clazz T>
using meta_values_t = typename meta_clazz_t<T>::meta_values_t;

template<class T>
concept bool Pod = Clazz<T> && clazz_info<std::decay_t<T>>::is_pod;

template<class T>
struct is_meta_clazz : std::false_type{};

template<Symbol... Ts>
struct is_meta_clazz<meta_clazz<Ts...>> : std::true_type {};

template<class T>
concept bool MetaClazz = is_meta_clazz<std::decay_t<T>>::value;

template<class T>
concept bool MetaPod = MetaClazz<T> && std::decay_t<T>::is_pod;

template<Clazz, Symbol...>
struct struple;

template<class T, class... Decs>
concept bool Implements = Clazz<T> && clazz_info<T>::template implements<Decs...>;

template<class T, class... Decs>
concept bool CoImplements = Clazz<T> && clazz_info<T>::template co_implements<Decs...>;

template<Declaration... Ds>
struct trait;

template<class>
struct is_trait : std::false_type {};

template<class... Ds>
struct is_trait<trait<Ds...>> : std::true_type {};

template<class T>
concept bool Trait = is_trait<std::decay_t<T>>::value;

template<class C, class T>
concept bool ImplementsTrait = Clazz<C> && Trait<T> && std::decay_t<T>::template co_implementor<C>;

template<Clazz SubClazz, Clazz SuperClazz>
struct clazz_is_subclazz_of : std::false_type {};

template<Clazz SubClazz, Symbol... Sups>
struct clazz_is_subclazz_of<SubClazz, clazz<Sups...>> {
    static constexpr bool value = clazz_info<SubClazz>::template implements_of<Sups...>;
};

template<class SubClazz, class SuperClazz>
concept bool SubClazzOf = clazz_is_subclazz_of<SubClazz, SuperClazz>::value;

template<class SubClz, class... Symbols>
concept bool SubClazz = SubClazzOf<SubClz, clazz<Symbols...>>;

template<class SuperClazz, class SubClazz>
concept bool SuperClazzOf = SubClazzOf<SubClazz, SuperClazz>;

template<class SupClz, class... Symbols>
concept bool SuperClazz = SuperClazzOf<SupClz, clazz<Symbols...>>;

template<class C1, class C2>
concept bool ClazzOf = SuperClazzOf<C1, C2> && SubClazzOf<C1, C2>;

template<class T, class... Symbols>
concept bool ClazzEq = SubClazz<T, Symbols...> && SuperClazz<T, Symbols...>;

template<Clazz SubClazz, Clazz SuperClazz>
struct clazz_is_co_subclazz_of : std::false_type {};

template<Clazz SubClazz, Symbol... Sups>
struct clazz_is_co_subclazz_of<SubClazz, clazz<Sups...>> {
    static constexpr bool value = clazz_info<SubClazz>::template co_implements_of<Sups...>;
};

template<class SubClazz, class SuperClazz>
concept bool CoSubClazzOf = clazz_is_co_subclazz_of<SubClazz, SuperClazz>::value;

template<class SubClazz, class... Symbols>
concept bool CoSubClazz = CoSubClazzOf<SubClazz, clazz<Symbols...>>;

template<class SuperClazz, class SubClazz>
concept bool CoSuperClazzOf = CoSubClazzOf<SubClazz, SuperClazz>;

template<class T, class... Symbols>
concept bool CoSuperClazz = CoSuperClazzOf<T, clazz<Symbols...>>;

template<class C1, class C2>
concept bool CoClazzOf = CoSuperClazzOf<C1, C2> && CoSubClazzOf<C1, C2>;

template<class T, class... Symbols>
concept bool CoClazz = CoSubClazz<T, Symbols...> && CoSuperClazz<T, Symbols...>;

template<Clazz SubClazz, Clazz SuperClazz>
struct clazz_is_contra_subclazz_of : std::false_type {};

template<Clazz SubClazz, Symbol... Sups>
struct clazz_is_contra_subclazz_of<SubClazz, clazz<Sups...>> {
    static constexpr bool value = clazz_info<SubClazz>::template contra_implements_of<Sups...>;
};

template<class SubClazz, class SuperClazz>
concept bool ContraSubClazzOf = clazz_is_contra_subclazz_of<SubClazz, SuperClazz>::value;

template<class SubClazz, class... Symbols>
concept bool ContraSubClazz = ContraSubClazzOf<SubClazz, clazz<Symbols...>>;

template<class SuperClazz, class SubClazz>
concept bool ContraSuperClazzOf = ContraSubClazzOf<SubClazz, SuperClazz>;

template<class T, class... Symbols>
concept bool ContraSuperClazz = ContraSuperClazzOf<T, clazz<Symbols...>>;

template<class C1, class C2>
concept bool ContraClazzOf = ContraSuperClazzOf<C1, C2> && ContraSubClazzOf<C1, C2>;

template<class T, class... Symbols>
concept bool ContraClazz = ContraSubClazz<T, Symbols...> && ContraSuperClazz<T, Symbols...>;

template<class T>
struct call_signature {
    using return_t = T;
    static constexpr bool is_call = false;
    static constexpr bool is_const = std::is_const_v<T>;
    using as_const = std::add_const_t<T>;
    template<class F, class... Ts>
    static constexpr bool is_partial_application_of = false;
    template<class F, class... Ts>
    static constexpr bool is_exact_partial_application_of = false;
};

template<class R, class... Args>
struct call_signature<R(Args...)> {
    using return_t = R;
    static constexpr bool is_call = true;
    static constexpr bool is_const = false;
    using as_const = R(Args...) const;
    template<class F, class... Ts>
    static constexpr bool is_partial_application_of = std::is_invocable_r_v<R, F, Ts..., Args...>;
    template<class F, class... Ts>
    static constexpr bool is_exact_partial_application_of = std::is_invocable_v<F, Ts..., Args...> 
                                                         && std::is_same_v<R, std::invoke_result_t<F, Ts..., Args...>>;
};

template<class R, class... Args>
struct call_signature<R(Args...) const> {
    using return_t = R;
    static constexpr bool is_call = true;
    static constexpr bool is_const = true;
    static constexpr bool is_getter = sizeof...(Args) == 0;
    using as_const = R(Args...) const;
    template<class F, class... Ts>
    static constexpr bool is_partial_application_of = std::is_invocable_r_v<R, F, Ts..., Args...>;
    template<class F, class... Ts>
    static constexpr bool is_exact_partial_application_of = std::is_invocable_v<F, Ts..., Args...> 
                                                         && std::is_same_v<R, std::invoke_result_t<F, Ts..., Args...>>;
};

template<class T>
concept bool CallSignature = call_signature<T>::is_call;

template<class T>
concept bool CallSignatureConst = CallSignature<T> && call_signature<T>::is_const;

template<class T>
concept bool CallSignatureGetter = CallSignatureConst<T> && call_signature<T>::is_getter;

template<class From, class To>
struct is_convertible : std::is_convertible<From, To> {};

// Non-const call sigs are convertible to const call sigs
template<CallSignature From, class ToReturn, class... ToParams>
struct is_convertible<From, ToReturn(ToParams...)> : std::is_invocable_r<ToReturn, From, ToParams...> {};

// Only const call sigs are convertible to const call sigs
template<CallSignatureConst From, class ToReturn, class... ToParams>
struct is_convertible<From, ToReturn(ToParams...) const> : std::is_invocable_r<ToReturn, From, ToParams...> {};

template<class T>
struct is_struple : std::false_type {};

template<class... Ts>
struct is_struple<struple<Ts...>> : std::true_type {};

template<class T>
concept bool Struple = is_struple<std::decay_t<T>>::value;

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

template<>
struct flatten_tuples<> {
    using type = std::tuple<>;
};

template<class... Ts>
struct flatten_tuples<std::tuple<Ts...>> {
    using type = std::tuple<Ts...>;
};

template<class... T1, class... T2, class... Rest>
struct flatten_tuples<std::tuple<T1...>, std::tuple<T2...>, Rest...> : flatten_tuples<std::tuple<T1..., T2...>, Rest...> {};

template<class... Ts>
using flatten_tuples_t = typename flatten_tuples<Ts...>::type;

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

namespace detail::set {
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

    template<Tag Name, class Target, class Tuple>
    // void Target means Target is yet to be determined by the clazz to be constructed
    // When void, Target cannot be used for template argument deduction of clazz
    struct placeholder : Tuple {
        using tuple_t = Tuple;
        using tuple_t::tuple;

        template<class VarT>
        constexpr inline decltype(auto) make() && noexcept {
            return std::make_from_tuple<VarT>(static_cast<tuple_t&&>(*this));
        }

        constexpr inline tuple_t&& as_tuple() && noexcept {
            return static_cast<tuple_t&&>(*this);
        }

        static constexpr bool is_undetermined = is_undetermined<Target>::value;

        template<Tag T>
        static constexpr bool has_tag = std::is_same_v<T, Name>;

        template<Symbol S>
        static constexpr bool has_name = symbol_info<S>::template has_name<Name>;

        template<Symbol... S>
        static constexpr bool shares_name = meta_clazz<S...>::template has_tag<Name>;
    };
}

template<class P>
struct is_set_placeholder : std::false_type {};

template<class Name, class Target, class T>
struct is_set_placeholder<detail::set::placeholder<Name, Target, T>> : std::true_type {};

template<class P>
concept bool SetPlaceholder = is_set_placeholder<std::decay_t<P>>::value;

namespace detail::set {
    template<SetPlaceholder... Ps>
    struct placeholder_collection : std::tuple<Ps&&...> {
        using tuple_t = std::tuple<Ps&&...>;

        using tuple_t::tuple;

        template<Symbol S>
        static constexpr bool has_name_of = (Ps::template has_name<S> || ...);
        
        template<Tag T>
        static constexpr bool has_tag = (Ps::template has_tag<T> || ...);

        template<DataSymbol S>
        requires has_name_of<S>
        constexpr inline S make_symbol() && {
            constexpr size_t idx = find_ph_for_symbol<0, S>();
            return std::get<idx>(std::move(*this)).template make<S>();
        }

        template<Tag T>
        requires has_tag<T>
        constexpr inline decltype(auto) get_tuple() && {
            constexpr size_t idx = find_ph_for_tag<0, T>();
            return std::get<idx>(std::move(*this)).as_tuple();
        }

    private:
        template<size_t I, DataSymbol S>
        static constexpr inline size_t find_ph_for_symbol() {
            using placeholder_t = std::decay_t<std::tuple_element_t<I, tuple_t>>;
            if constexpr(placeholder_t::template has_name<S>) {
                return I;
            } else {
                return find_ph_for_symbol<I+1, S>();
            }
        }

        template<size_t I, Tag T>
        static constexpr inline size_t find_ph_for_tag() {
            using placeholder_t = std::decay_t<std::tuple_element_t<I, tuple_t>>;
            if constexpr(placeholder_t::template has_tag<T>) {
                return I;
            } else {
                return find_ph_for_tag<I+1, T>();
            }
        }
    };

    template<SetPlaceholder... Ps>
    placeholder_collection(Ps&&...) -> placeholder_collection<Ps...>;
}

template<class P>
struct is_set_placeholder_collection : std::false_type {};

template<class... T>
struct is_set_placeholder_collection<detail::set::placeholder_collection<T...>> : std::true_type {};

template<class P>
concept bool SetPlaceholderCollection = is_set_placeholder_collection<std::decay_t<P>>::value;

template<class T>
struct index_of<T> {
    static constexpr int value = -1;
};

template<DataSymbol S, Tag N, Tag... Ns>
requires symbol_info<S>::template has_name<N>
struct index_of<S, N, Ns...> {
    static constexpr int value = 0;
};

template<DataSymbol S, Tag N, Tag... Ns>
requires !symbol_info<S>::template has_name<N>
struct index_of<S, N, Ns...> {
    static constexpr int value = sizeof...(Ns) == 0
                                     ? -1
                                     : 1 + index_of<S, Ns...>::value;
};

template<DataSymbol S1, DataSymbol S2, Symbol... Ss>
requires symbol_info<S1>::template shares_name<S2>
struct index_of<S1, S2, Ss...> {
    static constexpr int value = 0;
};

template<DataSymbol S1, DataSymbol S2, Symbol... Ss>
requires !symbol_info<S1>::template shares_name<S2>
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
requires symbol_info<S>::template has_name<T>
struct index_of<T, S, Ss...> {
    static constexpr int value = 0;
};

template<Tag T, Symbol S, Symbol... Ss>
requires !symbol_info<S>::template has_name<T>
struct index_of<T, S, Ss...> {
    static constexpr int value = sizeof...(Ss) == 0 
                                     ? -1 
                                     : 1 + index_of<T, Ss...>::value;
};

template<Tag T, SetPlaceholder P, SetPlaceholder... Ps>
requires P::template has_name<T>
struct index_of<T, P, Ps...> {
    static constexpr int value = 0;
};

template<Tag T, SetPlaceholder P, SetPlaceholder... Ps>
requires !P::template has_name<T>
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

template<Clazz Top, class... Xs>
struct struple<Top, void, Xs...> : struple<Top, Xs...> {
protected:
    // Skip void elements
    template<class... Ts>
    constexpr struple(Ts&&... ins) noexcept 
        : struple<Top, Xs...>{std::forward<Ts>(ins)...} 
    {
    }
};

template<Clazz Top, class X, class... Xs>
struct struple<Top, X, Xs...> : struple<Top, Xs...>, symbol_element_t<Top, X> {
protected:
    using this_t = struple<Top, X, Xs...>;
    using base_t = struple<Top, Xs...>;
    using element_t = symbol_element_t<Top, X>;
    using tag_t = symbol_tag_t<X>;
    static_assert(sizeof(base_t) == sizeof(flatten_tuples_t<symbol_tuple_t<Xs>...>));

    constexpr struple() noexcept : base_t{}, element_t{} {}

    template<class... Ts>
    requires EmptySymbol<X>
    constexpr struple(Ts&&... ins) noexcept
        : base_t(std::forward<Ts>(ins)...)
        , element_t{}
    {
    }

    template<class T, class ... Ts>
    requires DataSymbol<X>
    constexpr struple(T&& in, Ts&&... ins) noexcept 
        : base_t(std::forward<Ts>(ins)...)
        , element_t{std::forward<T>(in)}
    {
    }

    template<SetPlaceholderCollection T>
    requires DataSymbol<X> && T::template has_name_of<X>
    constexpr struple(T&& setter_collection) noexcept
        : base_t{std::move(setter_collection)}
        , element_t{std::move(setter_collection).template make_symbol<X>()}
    {
    }

    template<SetPlaceholderCollection T>
    requires DataSymbol<X> && !T::template has_name_of<X> && symbol_info<X>::has_default_ctor
    constexpr struple(T&& setter_collection) noexcept
        : base_t{std::move(setter_collection)}
        , element_t{}
    {
    }

    template<Clazz Clz>
    requires DataSymbol<X> 
          && clazz_info<Clz>::template has_tag<tag_t> 
          && Value<typename clazz_info<Clz>::template tag_symbol_t<tag_t>>
    constexpr struple(Clz&& clz) noexcept
        : base_t{std::forward<Clz>(clz)}
        , element_t{get<tag_t>(std::forward<Clz>(clz))}
    {
    }

    template<Clazz Clz>
    requires DataSymbol<X> 
          && !clazz_info<Clz>::template has_tag<tag_t> 
          && symbol_info<X>::has_default_ctor
    constexpr struple(Clz&& clz) noexcept
        : base_t{std::forward<Clz>(clz)}
        , element_t{}
    {
    }
};

template<class... Names, class... Targets, class... Ts>
requires (!detail::set::placeholder<Names, Targets, Ts>::is_undetermined && ...)
clazz(detail::set::placeholder<Names, Targets, Ts>&&...) -> pod<typename tag_info<Names>::template var_t<Targets>...>;

template<class... Names, class... Targets, class... Ts>
meta_clazz(detail::set::placeholder<Names, Targets, Ts>&&...) -> meta_pod<typename tag_info<Names>::template var_t<Targets>...>;

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

template<class... F>
struct overload : F... {
    using F::operator()...;
};

template<class... T>
overload(T...) -> overload<T...>;

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
    DECLARE_NAMED_TUPLE_VAR_WRAPPER(tuple_name);\
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
template<template<class> class DecWrapper, class DecType, Tag TagName>
struct dec_info_impl {
    template<Declaration To>
    struct is_convertible_to_dec : std::false_type {};
    template<class To>
    struct is_convertible_to_dec<DecWrapper<To>> : is_convertible<DecType, To> {};
    static constexpr bool is_callable = CallSignature<DecType>;
    static constexpr bool is_const_callable = CallSignatureConst<DecType>;
    static constexpr bool is_mutable_callable = is_callable && !CallSignatureConst<DecType>;
    template<Tag Name>\
    static constexpr bool has_tag = std::is_same_v<Name, TagName>;\
    template<Tag Name, class Top, class F>
    static constexpr bool is_applicable_for_def = std::is_same_v<Name, TagName> &&
            ((is_mutable_callable && call_signature<DecType>::template is_partial_application_of<F, Top&>)
        || (is_const_callable && call_signature<DecType>::template is_partial_application_of<F, const Top&>));
    template<Tag Name, class Top, class F>
    static constexpr bool is_match_for_def = std::is_same_v<Name, TagName> &&
            ((is_mutable_callable && call_signature<DecType>::template is_exact_partial_application_of<F, Top&>)
        || (is_const_callable && call_signature<DecType>::template is_exact_partial_application_of<F, const Top&>));
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
        template<class DecType>\
        struct tag_name;\
    }

#define DECLARE_STRUPLE_DEC(tag_name) \
    namespace CLAZZ_NS {\
        template<class DecType>\
        struct dec_info<dec::tag_name<DecType>> : dec_info_impl<dec::tag_name, DecType, ::CLAZZ_NS::tag::tag_name> {\
            using return_t = typename call_signature<DecType>::return_t;\
            static constexpr bool is_dec = true;\
            template<class... Variants>\
            struct variant_def {\
                using type = detail::def::tag_name ## _impl_<int, DecType, decltype([]<class... Os>(auto& self, Os&&... args) {\
                    return detail::visit_clazz_variant<return_t, Variants...>(\
                        std::index_sequence_for<Variants...>{},\
                        self._.index, &self,\
                        []<class C, class... Ts>(C&& c, Ts&&... ins) -> decltype(auto) {\
                            return std::forward<C>(c).tag_name(std::forward<Ts>(ins)...);\
                        },\
                        std::forward<Os>(args)...);\
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
            concept bool HasMemVar = requires (Struct s) { {s.tag_name}; };\
        }\
        template<>\
        struct tag_info<tag::tag_name> {\
            static constexpr bool is_tag = true;\
            STRUPLE_ALIAS_VAR(var_t, tag_name);\
            STRUPLE_ALIAS_VAL(val_t, tag_name);\
            STRUPLE_ALIAS_TPE(tpe_t, tag_name);\
            STRUPLE_ALIAS_DEF(def_t, tag_name);\
            STRUPLE_ALIAS_FUN(fun_t, tag_name);\
            STRUPLE_ALIAS_OVL(ovl_t, tag_name);\
            static constexpr auto& set = set::tag_name;\
            static constexpr auto name = std::string_view(#tag_name);\
            template<Symbol S>\
            struct assert_unique_name {\
                static constexpr bool value = !symbol_info<S>::template has_name<tag::tag_name>;\
                static_assert(value, "Field "#tag_name" is not unique.\n"\
                                     "If defining multiple overloads of def::"#tag_name", use ovl::"#tag_name);\
            };\
            template<class Struct>\
            static constexpr bool has_mem_var = detail::tag::concepts::tag_name::HasMemVar<Struct>;\
            template<class Struct>\
            using mem_var_t = var_t<typename class_data_member_pointer_info<decltype(&Struct::tag_name)>::member_t>;\
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

namespace detail::set {
    template<class Tg, template<bool> class Assert>
    struct setter {
        template<class T>
        constexpr auto operator=(T&& in) const {
            return detail::set::placeholder<Tg, std::decay_t<T>, std::tuple<T&&>>{std::forward<T>(in)};
        }
        template<class T>
        constexpr auto operator()(T&& in) const {
            return detail::set::placeholder<Tg, std::decay_t<T>, std::tuple<T&&>>{std::forward<T>(in)};
        }
        template<class... Ts>
        constexpr auto operator()(Ts&&... ins) const {
            return detail::set::placeholder<Tg, detail::set::undetermined<Assert>, std::tuple<Ts&&...>>{std::forward<Ts>(ins)...};
        }
        template<class Target, class... Ts>
        constexpr auto as(Ts&&... ins) const {
            return detail::set::placeholder<Tg, Target, std::tuple<Ts&&...>>{std::forward<Ts>(ins)...};
        }
        template<class T>
        constexpr auto fwd(T&& in) const {
            return detail::set::placeholder<Tg, T&&, std::tuple<T&&>>{std::forward<T>(in)};
        }
    };
}
}

#define DECLARE_STRUPLE_SET(tag_name) \
    namespace CLAZZ_NS {\
        namespace detail::set::assert_targeted {\
            template<bool E>\
            struct tag_name {\
                static_assert("clazz<...> template deduction failure" && false_v<>,\
                    "set::"#tag_name"(args...):\nCannot deduce template arguments for clazz<...> with set::"#tag_name"(args...), "\
                    "use set::"#tag_name".as<TargetType>(args...) to set a type for deduction when "\
                    "calling a constructor of a clazz member with multiple parameters.");\
            };\
        }\
        namespace set {\
            constexpr detail::set::setter<tag::tag_name, detail::set::assert_targeted::tag_name> tag_name;\
        }\
    }

namespace CLAZZ_NS {
template<Tag TagName>
struct tag_queries {
    using tag_t = TagName;
    template<Tag Name>
    static constexpr bool has_name = std::is_same_v<Name, tag_t>;
    template<Symbol S>
    static constexpr bool shares_name = symbol_info<S>::template has_name<tag_t>;
    template<Symbol S>
    struct assert_unique_name : tag_info<tag_t>::template assert_unique_name<S> {};
};

template<Declaration Dec>
struct dec_queries {
    template<Declaration D>
    static constexpr bool has_dec = std::is_same_v<D, Dec>;
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
    static constexpr bool has_dec = //dec_info<Dec>::template has_tag<TagName>;
        dec_info<Dec>::template is_match_for_def<TagName, Top, F>;
    template<Symbol S>
    static constexpr bool shares_dec = tag_queries<TagName>::template shares_name<S> && 
        ((!DefinitionTemplate<S> && symbol_info<S>::template shares_dec<Sym>)
       || (DefinitionTemplate<S> && std::is_same_v<Sym, S>));
    template<Declaration Dec>
    static constexpr bool has_wider_dec = //dec_info<Dec>::template has_tag<TagName>; 
        dec_info<Dec>::template is_applicable_for_def<TagName, Top, F>;
    template<Declaration D>
    static constexpr bool has_narrower_dec = false; // TODO
    template<Symbol S>
    static constexpr bool shares_wider_dec = tag_queries<TagName>::template shares_name<S> && 
        ((!DefinitionTemplate<S> && symbol_info<S>::template shares_narrower_dec<Sym>)
       || (DefinitionTemplate<S> && std::is_same_v<Sym, S>));
    template<Symbol S>
    static constexpr bool shares_narrower_dec = false; // TODO
};

template<Tag TagName, Declaration Dec>
struct symbol_queries : tag_queries<TagName>, dec_queries<Dec> {};
}

#define FWD_DECLARE_STRUPLE_VAR(tag_name) \
    namespace CLAZZ_NS::detail::var {\
        template<class T, class... DefaultValue>\
        requires sizeof...(DefaultValue) <= 1 && (std::is_invocable_r_v<T, DefaultValue> && ...)\
        struct tag_name ## _impl_;\
    }

#define STRUPLE_ALIAS_VAR(alias_name, tag_name) \
    template<class T, auto... DefaultValue>\
    using alias_name = detail::var::tag_name ## _impl_<T, std::conditional_t<std::is_invocable_v<decltype(DefaultValue)>,\
                                                                                decltype(DefaultValue),\
                                                                                decltype([]{ return DefaultValue; })>...>

#define DECLARE_STRUPLE_VAR(tag_name) \
    namespace CLAZZ_NS {\
        namespace detail::var {\
            template<class T>\
            struct tag_name ## _impl_<T> {\
                T tag_name;\
                template<class... Ts>\
                constexpr tag_name ## _impl_(Ts&&... ins) noexcept : tag_name(std::forward<Ts>(ins)...) {}\
            };\
            template<class T, class DefaultValue>\
            struct tag_name ## _impl_<T, DefaultValue> {\
                T tag_name;\
                template<class... Ts>\
                constexpr tag_name ## _impl_(Ts&&... ins) noexcept : tag_name(std::forward<Ts>(ins)...) {}\
                constexpr tag_name ## _impl_() noexcept : tag_name(DefaultValue{}()) {}\
            };\
        }\
        namespace var {\
            STRUPLE_ALIAS_VAR(tag_name, tag_name);\
        }\
        template<class T, class... DefaultValue>\
        struct symbol_info<detail::var::tag_name ## _impl_<T, DefaultValue...>> : symbol_queries<tag::tag_name, dec::tag_name<T>> {\
            using value_t = T;\
            using symbol_t = detail::var::tag_name ## _impl_<T, DefaultValue...>;\
            static constexpr symbol_type type = symbol_type::var;\
            using tuple_t = std::tuple<T>;\
            template<class>\
            using struple_element_t = symbol_t;\
            static constexpr bool has_default_ctor = sizeof...(DefaultValue) == 1;\
            static constexpr decltype(auto) default_value() requires has_default_ctor { return (DefaultValue{}(), ...); }\
            template<Clazz C>\
            requires clazz_info<C>::template has_symbol<detail::var::tag_name ## _impl_<T, DefaultValue...>>\
            static constexpr decltype(auto) invoke(C&& c) {\
                if constexpr(std::is_rvalue_reference_v<C&&>) return static_cast<T&&>(c.tag_name);\
                else return (c.tag_name);\
            }\
            template<class Test>\
            static constexpr bool has_compatible_field = requires(Test test) { {test.tag_name} -> T };\
            template<class Struct>\
            requires has_compatible_field<std::decay_t<Struct>>\
            static constexpr decltype(auto) extract_compatible_value(Struct&& s) {\
                if constexpr(requires(Struct s) { {s.tag_name} -> T}) {\
                    if constexpr (std::is_lvalue_reference_v<Struct>) return (s.tag_name);\
                    else return std::move(s.tag_name);\
                }\
                else return s.tag_name();\
            }\
        };\
    }
    
#define FWD_DECLARE_STRUPLE_VAL(tag_name) \
    namespace CLAZZ_NS::detail::val {\
        template<class T, class Value>\
        requires std::is_invocable_r_v<T, Value>\
        struct tag_name ## _impl_;\
    }

#define STRUPLE_ALIAS_VAL(alias_name, tag_name) \
    template<class T, auto Value>\
    using alias_name = detail::val::tag_name ## _impl_<T, std::conditional_t<std::is_invocable_v<decltype(Value)>,\
                                                                             decltype(Value),\
                                                                             decltype([]{ return Value; })>>

#define DECLARE_STRUPLE_VAL(tag_name) \
    namespace CLAZZ_NS {\
        namespace detail::val {\
            template<class T, class Value>\
            requires std::is_invocable_r_v<T, Value>\
            struct tag_name ## _impl_ {\
                static constexpr T tag_name = Value{}();\
            };\
        }\
        namespace val {\
            STRUPLE_ALIAS_VAL(tag_name, tag_name);\
        }\
        template<class T, class Value>\
        struct symbol_info<detail::val::tag_name ## _impl_<T, Value>> : symbol_queries<tag::tag_name, dec::tag_name<T>> {\
            using value_t = T;\
            using symbol_t = detail::val::tag_name ## _impl_<T, Value>;\
            static constexpr symbol_type type = symbol_type::val;\
            using tuple_t = std::tuple<>;\
            template<class>\
            using struple_element_t = symbol_t;\
            static constexpr bool has_default_ctor = true;\
            static constexpr T invoke() { return Value{}(); }\
            template<class Test>\
            static constexpr bool has_compatible_field = requires { {Test::tag_name} -> T };\
            template<class Struct>\
            requires has_compatible_field<std::decay_t<Struct>>\
            static constexpr decltype(auto) extract_compatible_value(Struct&& s) {\
                if constexpr(requires { {Struct::tag_name} -> T}) return (s.tag_name);\
                else return s.tag_name();\
            }\
        };\
    }

#define STRUPLE_ALIAS_TPE(alias_name, tag_name) \
    template<class T>\
    using alias_name = detail::tpe::tag_name ## _impl_<T>

#define FWD_DECLARE_STRUPLE_TPE(tag_name) \
    namespace CLAZZ_NS::detail::tpe {\
        template<class T>\
        struct tag_name ## _impl_;\
    }

#define DECLARE_STRUPLE_TPE(tag_name) \
    namespace CLAZZ_NS {\
        namespace detail::tpe {\
            template<class T>\
            struct tag_name ## _impl_ {\
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
        struct tag_name ## _impl_;\
    }

#define STRUPLE_ALIAS_DEF(alias_name, tag_name) \
    template<class Sig, auto... F>\
    requires (CallSignature<Sig> && sizeof...(F) > 0)\
            || (std::is_same_v<const Sig, const void> && sizeof...(F) == 1)\
    using alias_name = detail::def::tag_name ## _impl_<int, Sig, decltype(F)...>

#define DECLARE_STRUPLE_DEF(tag_name) \
    namespace CLAZZ_NS {\
        namespace detail::def {\
            template<class Top, class R, class... Args, class... F>\
            struct tag_name ## _impl_<Top, R(Args...) const, F...> : protected F... {\
                constexpr tag_name ## _impl_() {\
                    static_assert((std::is_invocable_r_v<R, F, const Top&, Args...> || ...),\
                                  "No implementations of def::"#tag_name" are reachable given the call signature.");\
                }\
                template<class... Ts>\
                requires std::is_invocable_v<R(Args...), Ts&&...> && (std::is_invocable_r_v<R, F, const Top&, Ts&&...> || ...)\
                inline constexpr R tag_name(Ts&&... in) const {\
                    return (*this)(static_cast<const Top&>(*this), std::forward<Ts>(in)...);\
                }\
            protected:\
                using F::operator()...;\
                using top_t = Top;\
            };\
            template<class Top, class R, class... Args, class... F>\
            struct tag_name ## _impl_<Top, R(Args...), F...> : protected F... {\
                constexpr tag_name ## _impl_() {\
                    static_assert((std::is_invocable_r_v<R, F, Top&, Args...> || ...),\
                                  "No implementations of def::"#tag_name" are reachable given the call signature.");\
                }\
                template<class... Ts>\
                requires std::is_invocable_v<R(Args...), Ts&&...> && (std::is_invocable_r_v<R, F, Top&, Ts&&...> || ...)\
                inline constexpr R tag_name(Ts&&... in) {\
                    return (*this)(static_cast<Top&>(*this), std::forward<Ts>(in)...);\
                }\
            protected:\
                using F::operator()...;\
                using top_t = Top;\
            };\
            template<class Top, class F>\
            struct tag_name ## _impl_<Top, void, F> : protected F {\
                template<class... Ts>\
                requires std::is_invocable_v<F, Top&, Ts&&...>\
                inline constexpr decltype(auto) tag_name(Ts&&... in) {\
                    return (*this)(static_cast<Top&>(*this), std::forward<Ts>(in)...);\
                }\
            protected:\
                using F::operator();\
                using top_t = Top;\
            };\
            template<class Top, class F>\
            struct tag_name ## _impl_<Top, const void, F> : protected F {\
                template<class... Ts>\
                requires std::is_invocable_v<F, const Top&, Ts&&...>\
                inline constexpr decltype(auto) tag_name(Ts&&... in) const {\
                    return (*this)(static_cast<const Top&>(*this), std::forward<Ts>(in)...);\
                }\
            protected:\
                using F::operator();\
                using top_t = Top;\
            };\
        }\
        namespace def {\
            STRUPLE_ALIAS_DEF(tag_name, tag_name);\
        }\
        template<class Top, class Void, class F>\
        requires !CallSignature<Void> && std::is_same_v<const Void, const void>\
        struct symbol_info<detail::def::tag_name ## _impl_<Top, Void, F>> \
            : tag_queries<tag::tag_name>\
            , tmpl_dec_queries<detail::def::tag_name ## _impl_<Top, Void, F>, tag::tag_name, Top, F> {\
            using value_t = void;\
            using symbol_t = detail::def::tag_name ## _impl_<Top, Void, F>;\
            static constexpr symbol_type type = symbol_type::def;\
            static constexpr bool is_tmpl_def = true;\
            using tuple_t = std::tuple<>;\
            template<Clazz TTop>\
            using struple_element_t = detail::def::tag_name ## _impl_<TTop, Void, F>;\
            static constexpr bool has_default_ctor = true;\
            template<class Test>\
            static constexpr bool has_compatible_field = false;\
            template<Clazz C, class... Ts>\
            requires clazz_info<C>::template has_symbol<symbol_t>\
            static constexpr decltype(auto) invoke(C&& c, Ts&&... ins) {\
                return c.tag_name(std::forward<Ts...>(ins)...);\
            }\
        };\
        template<class Top, CallSignature Sig, class... F>\
        struct symbol_info<detail::def::tag_name ## _impl_<Top, Sig, F...>> : symbol_queries<tag::tag_name, dec::tag_name<Sig>> {\
            using value_t = void;\
            using symbol_t = detail::def::tag_name ## _impl_<Top, Sig, F...>;\
            static constexpr symbol_type type = symbol_type::def;\
            using tuple_t = std::tuple<>;\
            template<Clazz TTop>\
            using struple_element_t = detail::def::tag_name ## _impl_<TTop, Sig, F...>;\
            static constexpr bool has_default_ctor = true;\
            template<class Test>\
            static constexpr bool has_compatible_field = false;\
            template<Clazz C, class... Ts>\
            requires clazz_info<C>::template has_symbol<symbol_t>\
            static constexpr decltype(auto) invoke(C&& c, Ts&&... ins) {\
                return c.tag_name(std::forward<Ts...>(ins)...);\
            }\
        };\
    }

#define FWD_DECLARE_STRUPLE_FUN(tag_name) \
    namespace CLAZZ_NS::detail::fun {\
        template<CallSignatureConst Sig, class... F>\
        struct tag_name ## _impl_;\
    }

#define STRUPLE_ALIAS_FUN(alias_name, tag_name) \
    template<CallSignature Sig, auto... F>\
    using alias_name = detail::fun::tag_name ## _impl_<typename call_signature<Sig>::as_const, decltype(F)...>

#define DECLARE_STRUPLE_FUN(tag_name) \
    namespace CLAZZ_NS {\
        namespace detail::fun {\
            template<class R, class... Args, class... F>\
            struct tag_name ## _impl_<R(Args...) const, F...> {\
                constexpr tag_name ## _impl_() {\
                    static_assert((std::is_invocable_r_v<R, F, Args...> || ...),\
                                  "No implementations of fun::"#tag_name" are reachable given the call signature.");\
                }\
                template<class... Ts>\
                requires std::is_invocable_v<R(Args...), Ts&&...> && (std::is_invocable_r_v<R, F, Ts&&...> || ...)\
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
        struct symbol_info<detail::fun::tag_name ## _impl_<Sig, F...>> : symbol_queries<tag::tag_name, dec::tag_name<Sig>> {\
            using value_t = void;\
            using symbol_t = detail::fun::tag_name ## _impl_<Sig, F...>;\
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
    using alias_name = detail::ovl::tag_name ## _impl_<methods...>

#define FWD_DECLARE_STRUPLE_OVL(tag_name) \
    namespace CLAZZ_NS::detail::ovl {\
        template<MethodNamed<::CLAZZ_NS::tag::tag_name>... F>\
        struct tag_name ## _impl_;\
    }

#define DECLARE_STRUPLE_OVL(tag_name) \
    namespace CLAZZ_NS {\
        namespace detail::ovl {\
            template<MethodNamed<::CLAZZ_NS::tag::tag_name>... F>\
            struct tag_name ## _impl_ : F... {\
                using F::tag_name...;\
            };\
            template<DefinitionNamed<::CLAZZ_NS::tag::tag_name>... Defs>\
            struct tag_name ## _impl_<Defs...> : protected Defs...{\
            protected:\
                using top_t = typename std::tuple_element_t<0, std::tuple<Defs...>>::top_t;\
                using Defs::operator()...;\
            public:\
                template<class... Ts>\
                requires (std::is_invocable_v<Defs, top_t&, Ts&&...> || ...)\
                inline constexpr decltype(auto) tag_name(Ts&&... in) {\
                    static_assert((std::is_same_v<top_t, typename Defs::top_t> && ...));\
                    return (*this)(static_cast<top_t&>(*this), std::forward<Ts>(in)...);\
                }\
                template<class... Ts>\
                requires (std::is_invocable_v<Defs, const top_t&, Ts&&...> || ...)\
                inline constexpr decltype(auto) tag_name(Ts&&... in) const {\
                    static_assert((std::is_same_v<top_t, typename Defs::top_t> && ...));\
                    return (*this)(static_cast<const top_t&>(*this), std::forward<Ts>(in)...);\
                }\
            };\
        }\
        namespace ovl {\
            STRUPLE_ALIAS_OVL(tag_name, tag_name);\
        }\
        template<MethodNamed<tag::tag_name>... F>\
        struct symbol_info<detail::ovl::tag_name ## _impl_<F...>> : tag_queries<tag::tag_name> {\
            using value_t = void;\
            using symbol_t = detail::ovl::tag_name ## _impl_<F...>;\
            static constexpr symbol_type type = symbol_type::ovl;\
            using tuple_t = std::tuple<>;\
            template<class Top>\
            using struple_element_t = detail::ovl::tag_name ## _impl_<symbol_element_t<Top, F>...>;\
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
            requires clazz_info<C>::template has_symbol<symbol_t>\
            static constexpr decltype(auto) invoke(C&& c, Ts&&... ins) {\
                return c.tag_name(std::forward<Ts...>(ins)...);\
            }\
        };\
    }

#define FWD_DECLARE_STRUPLE_SYMBOL(tag_name) \
    FWD_DECLARE_STRUPLE_TAG(tag_name);\
    FWD_DECLARE_STRUPLE_DEC(tag_name);\
    FWD_DECLARE_STRUPLE_VAR(tag_name);\
    FWD_DECLARE_STRUPLE_VAL(tag_name);\
    FWD_DECLARE_STRUPLE_TPE(tag_name);\
    FWD_DECLARE_STRUPLE_DEF(tag_name);\
    FWD_DECLARE_STRUPLE_FUN(tag_name);\
    FWD_DECLARE_STRUPLE_OVL(tag_name);

#define DECLARE_STRUPLE_SYMBOL(tag_name) \
    FWD_DECLARE_STRUPLE_SYMBOL(tag_name);\
    DECLARE_STRUPLE_SET(tag_name);\
    DECLARE_STRUPLE_TAG(tag_name);\
    DECLARE_STRUPLE_DEC(tag_name);\
    DECLARE_STRUPLE_VAR(tag_name);\
    DECLARE_STRUPLE_VAL(tag_name);\
    DECLARE_STRUPLE_TPE(tag_name);\
    DECLARE_STRUPLE_DEF(tag_name);\
    DECLARE_STRUPLE_FUN(tag_name);\
    DECLARE_STRUPLE_OVL(tag_name);

DECLARE_STRUPLE_SYMBOL(_1);
DECLARE_STRUPLE_SYMBOL(_2);
DECLARE_STRUPLE_SYMBOL(_3);
DECLARE_STRUPLE_SYMBOL(_4);
DECLARE_STRUPLE_SYMBOL(_5);
DECLARE_STRUPLE_SYMBOL(_6);
DECLARE_STRUPLE_SYMBOL(_7);
DECLARE_STRUPLE_SYMBOL(_8);
DECLARE_STRUPLE_SYMBOL(_9);
DECLARE_STRUPLE_SYMBOL(_10);
DECLARE_STRUPLE_SYMBOL(_11);
DECLARE_STRUPLE_SYMBOL(_12);
DECLARE_STRUPLE_SYMBOL(_13);
DECLARE_STRUPLE_SYMBOL(_14);
DECLARE_STRUPLE_SYMBOL(_15);
DECLARE_STRUPLE_SYMBOL(_16);
DECLARE_STRUPLE_SYMBOL(_17);
DECLARE_STRUPLE_SYMBOL(_18);
DECLARE_STRUPLE_SYMBOL(_19);
DECLARE_STRUPLE_SYMBOL(_20);
DECLARE_STRUPLE_SYMBOL(_21);
DECLARE_STRUPLE_SYMBOL(_22);

DECLARE_NAMED_TUPLE(nuple);
DECLARE_NAMED_TUPLE_VAR(nuple, 0, _1);
DECLARE_NAMED_TUPLE_VAR(nuple, 1, _2);
DECLARE_NAMED_TUPLE_VAR(nuple, 2, _3);
DECLARE_NAMED_TUPLE_VAR(nuple, 3, _4);
DECLARE_NAMED_TUPLE_VAR(nuple, 4, _5);
DECLARE_NAMED_TUPLE_VAR(nuple, 5, _6);
DECLARE_NAMED_TUPLE_VAR(nuple, 6, _7);
DECLARE_NAMED_TUPLE_VAR(nuple, 7, _8);
DECLARE_NAMED_TUPLE_VAR(nuple, 8, _9);
DECLARE_NAMED_TUPLE_VAR(nuple, 9, _10);
DECLARE_NAMED_TUPLE_VAR(nuple, 10, _11);
DECLARE_NAMED_TUPLE_VAR(nuple, 11, _12);
DECLARE_NAMED_TUPLE_VAR(nuple, 12, _13);
DECLARE_NAMED_TUPLE_VAR(nuple, 13, _14);
DECLARE_NAMED_TUPLE_VAR(nuple, 14, _15);
DECLARE_NAMED_TUPLE_VAR(nuple, 15, _16);
DECLARE_NAMED_TUPLE_VAR(nuple, 16, _17);
DECLARE_NAMED_TUPLE_VAR(nuple, 17, _18);
DECLARE_NAMED_TUPLE_VAR(nuple, 18, _19);
DECLARE_NAMED_TUPLE_VAR(nuple, 19, _20);
DECLARE_NAMED_TUPLE_VAR(nuple, 20, _21);
DECLARE_NAMED_TUPLE_VAR(nuple, 21, _22);

// Increment and decrement
DECLARE_STRUPLE_SYMBOL(operator_increment); // ++a, a++
DECLARE_STRUPLE_SYMBOL(operator_decrement); // --a, a--

// Member access
DECLARE_STRUPLE_SYMBOL(operator_pointer); // a->b
DECLARE_STRUPLE_SYMBOL(operator_deref); // *a
DECLARE_STRUPLE_SYMBOL(operator_address_of); // &a
DECLARE_STRUPLE_SYMBOL(operator_subscript); // a[b]

// Arithemtic
DECLARE_STRUPLE_SYMBOL(operator_plus); // a + b
DECLARE_STRUPLE_SYMBOL(operator_minus); // a - b
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
DECLARE_STRUPLE_SYMBOL(operator_type); // (type)a
DECLARE_STRUPLE_SYMBOL(operator_paren); // a(b)
DECLARE_STRUPLE_SYMBOL(operator_comma); // a,b
DECLARE_STRUPLE_SYMBOL(operator_ctor); // A()
DECLARE_STRUPLE_SYMBOL(operator_dtor); // a.~A()
DECLARE_STRUPLE_SYMBOL(operator_swap); // swap(a, b)

// Reserved keywords
DECLARE_STRUPLE_SYMBOL(_); // Reserved member name for library generated clazzes
DECLARE_STRUPLE_SYMBOL(index);
DECLARE_STRUPLE_SYMBOL(size);

namespace CLAZZ_NS {
// Get by index
template<size_t I, Clazz C>
requires Variable<typename meta_values_t<C>::template index_symbol_t<I>>
constexpr inline decltype(auto) get(C&& c) noexcept {
    using symbol_t = typename meta_values_t<C>::template index_symbol_t<I>;
    return symbol_info<symbol_t>::invoke(std::forward<C>(c));
}

template<size_t I, Clazz C>
requires StaticValue<typename meta_values_t<C>::template index_symbol_t<I>>
constexpr inline decltype(auto) get(C&& c) noexcept {
    using symbol_t = typename clazz_info<C>::template index_symbol_t<I>;
    return symbol_info<symbol_t>::invoke();
}

// Get variable by tag
template<Tag tag, Clazz C>
requires clazz_info<C>::template has_tag<tag> 
      && Variable<typename clazz_info<C>::template tag_symbol_t<tag>>
constexpr inline decltype(auto) get(C&& c) noexcept {
    using symbol_t = typename clazz_info<C>::template tag_symbol_t<tag>;
    return symbol_info<symbol_t>::invoke(std::forward<C>(c));
}

// Get static value by tag
template<Tag tag, Clazz C>
requires clazz_info<C>::template has_tag<tag> 
      && StaticValue<typename clazz_info<C>::template tag_symbol_t<tag>>
constexpr inline auto get(C&& c) noexcept {
    using symbol_t = typename clazz_info<C>::template tag_symbol_t<tag>;
    return symbol_info<symbol_t>::invoke();
}

// Get a super clazz by indices
template<size_t... I, Clazz C>
requires sizeof...(I) > 1
constexpr inline auto get(C&& c) noexcept {
    using clazz_t = clazz<typename meta_values_t<C>::template index_symbol_t<I>...>;
    return clazz_t(std::forward<C>(c));
}

// Get a super clazz by Tags
template<Tag... Tags, Clazz C>
requires sizeof...(Tags) > 1
constexpr inline auto get(C&& c) noexcept {
    using clazz_t = clazz<typename meta_values_t<C>::template tag_symbol_t<Tags>...>;
    return clazz_t(std::forward<C>(c));
}

// Tie a super clazz by indices
template<size_t... I, Clazz C>
requires sizeof...(I) > 1
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
requires sizeof...(Tags) > 1 && (clazz_info<C>::template has_tag<Tags> && ...)
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
requires !clazz_info<C>::template has_tag<tag> 
constexpr inline auto get_or(C&&, T&& in) noexcept {
    return std::forward<T>(in);
}

template<Tag tag, Clazz C, class T>
requires clazz_info<C>::template has_tag<tag> 
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
    template<Value... X>
    struct clazz_map_helper {
        template<class... T>
        using invokes_apply_t = clazz <
            std::conditional_t <
                std::is_same_v<T, void>,
                std::tuple<>,
                typename symbol_tag_info<X>::template var_t<T>
            >...
        >;
    };
}

template<Value... X, class... F>
requires sizeof...(X) == sizeof...(F)
constexpr inline auto map(const clazz<X...>& c, F&&... f) {
    using invokes_t = std::tuple<std::invoke_result_t<F, decltype(get<symbol_tag_t<X>>(c))>...>;
    // using clazz_t = apply_tuple_t<invokes_t, typename detail::clazz_map_helper<X...>::template invokes_apply_t>;
    using clazz_t = clazz <
        std::conditional_t <
            std::is_same_v<std::invoke_result_t<F, decltype(get<symbol_tag_t<X>>(c))>, void>,
            std::tuple<>,
            typename symbol_tag_info<X>::template var_t<std::invoke_result_t<F, decltype(get<symbol_tag_t<X>>(c))>>
        >...
    >;

    return [&]<Value... V, SetPlaceholder... Fs>(clazz_info<clazz<V...>>, 
                                                 detail::set::placeholder_collection<Fs...>&& fs) {
        return clazz_t{std::invoke(
            std::get<0>(std::move(fs).template get_tuple<symbol_tag_t<V>>()), 
            get<symbol_tag_t<V>>(c))...};
    }(clazz_info<clazz_t>{}, detail::set::placeholder_collection{symbol_tag_info<X>::set(std::forward<F>(f))...});
}

template<Tag... Tags, Clazz C, class... F>
requires sizeof...(Tags) == sizeof...(F) 
      && (clazz_info<C>::template has_tag<Tags> && ...)
constexpr inline auto xmap(C&& c, F&&... f) {
    using invokes_t = std::tuple<std::invoke_result_t<F, decltype(get<Tags>(std::forward<C>(c)))>...>;
    // using clazz_t = apply_tuple_t<invokes_t, typename detail::clazz_map_helper<X...>::template invokes_apply_t>;
    using clazz_t = clazz <
        std::conditional_t <
            std::is_same_v<std::invoke_result_t<F, decltype(get<Tags>(std::forward<C>(c)))>, void>,
            std::tuple<>,
            typename tag_info<Tags>::template var_t<std::invoke_result_t<F, decltype(get<Tags>(std::forward<C>(c)))>>
        >...
    >;

    return [&]<Value... V, SetPlaceholder... Fs>(clazz_info<clazz<V...>>, 
                                                 detail::set::placeholder_collection<Fs...>&& fs) {
        return clazz_t{std::invoke(
            std::get<0>(std::move(fs).template get_tuple<symbol_tag_t<V>>()), 
            get<symbol_tag_t<V>>(c))...};
    }(clazz_info<clazz_t>{}, detail::set::placeholder_collection{tag_info<Tags>::set(std::forward<F>(f))...});
}

template<Symbol... X>
struct clazz_info<clazz<X...>> {
    template<Symbol S>
    // using info = symbol_info<symbol_element_t<clazz<X...>, S>>;
    using info = symbol_info<S>;

    using clazz_t = clazz<X...>;
    using meta_clazz_t = meta_clazz<X...>;

    static constexpr size_t size = sizeof...(X);

    template<Symbol S>
    static constexpr bool has_symbol = (info<X>::template shares_dec<S> || ...);
    
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
    using tag_symbol_t = std::tuple_element_t<index_of<tag, X...>::value, std::tuple<X...>>;

    template<size_t I>
    using index_symbol_t = std::tuple_element_t<I, std::tuple<X...>>;

    template<size_t I>
    using index_element_t = typename info<index_symbol_t<I>>::value_t;

    template<size_t I>
    using index_tag_t = symbol_tag_t<index_symbol_t<I>>;

    template<Tag tag>
    static constexpr size_t tag_index = index_of<tag, X...>::value;

    using tags_info_t = tags_info<symbol_tag_t<X>...>;

    template<class... Tags>
    using super_meta_clazz_t = meta_clazz<tag_symbol_t<Tags>...>;

    template<Symbol... Ts>
    using clazz_info_wrapper = clazz_info<clazz<Ts...>>;

    using pod_info_t = bind_to_t<clazz_info_wrapper, flatten_tuples_t<assign_tuple_t<symbol_tuple_t<X>, X>...>>;
    static constexpr bool is_pod = (DataSymbol<X> && ...);

    using values_info_t = bind_to_t<clazz_info_wrapper, flatten_tuples_t<std::conditional_t<Value<X>, std::tuple<X>, std::tuple<>>...>>;
    static constexpr bool is_values = (Value<X> && ...);
    
    using variables_info_t = bind_to_t<clazz_info_wrapper, flatten_tuples_t<std::conditional_t<Variable<X>, std::tuple<X>, std::tuple<>>...>>;
    static constexpr bool is_variables = (Variable<X> && ...);

    static constexpr bool is_default_assignable = ((!Variable<X> || 
        std::is_assignable_v<std::add_lvalue_reference_t<typename symbol_info<X>::value_t>, 
                             std::add_lvalue_reference_t<typename symbol_info<X>::value_t>>) && ...);

    template<Tag tag>
    static constexpr bool has_tag = (symbol_info<X>::template has_name<tag> || ...);

    template<Tag tag>
    static constexpr bool is_clazz_var = Variable<tag_symbol_t<tag>> && Clazz<std::tuple_element_t<0, symbol_tuple_t<tag_symbol_t<tag>>>>;
};

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

    union {
        clazz_t fields;
        tuple_t tuple;
    };
    
    ~meta_clazz() noexcept {
        fields.~clazz_t();
    }
    
    template<class... Ts>
    constexpr meta_clazz(Ts&&... in) noexcept : fields(std::forward<Ts>(in)...) {
        static_assert(sizeof(tuple_t) == sizeof(meta_clazz_t));
    }

    template<class T>
    requires std::is_same_v<tuple_t, std::decay_t<T>>
    constexpr meta_clazz(T&& tup) noexcept : tuple(std::forward<T>(tup)) {
        static_assert(sizeof(tuple_t) == sizeof(meta_clazz_t));
    }

    template<class Struct>
    requires !Clazz<Struct> && info_v importable<Struct>
    static constexpr clazz_t import(Struct&& in) noexcept {
        return make_clazz(indices_of_vars_t<X...>{}, std::forward<Struct>(in));
    }

    template<class Struct>
    requires !Clazz<Struct> && info_v partially_importable<Struct>
    static constexpr clazz_t import_with_defaults(Struct&& in) noexcept { 
        return make_clazz_with_defaults(indices_of_vars_t<X...>{}, std::forward<Struct>(in));
    }

    template<class Struct>
    requires !Clazz<Struct>
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
        using extractible_tuple_t = flatten_tuples_t<std::conditional_t<tag_info<info_t index_tag_t<I>>::template has_mem_var<std::decay_t<Struct>>, std::tuple<std::index_sequence<I>>, std::tuple<>>...>;
        using extractible_indices_t = bind_to_t<flatten_indices_t, extractible_tuple_t>;
        return make_clazz_with_defaults_2(extractible_indices_t{}, std::forward<Struct>(in));
    }

    template<size_t... I, class Struct>
    static constexpr auto make_clazz_with_defaults_2(std::index_sequence<I...>, Struct&& in) noexcept {
        return clazz_t(tag_info<info_t index_tag_t<I>>::set = symbol_info<info_t index_symbol_t<I>>::extract_compatible_value(std::forward<Struct>(in))...);
    }

    template<size_t... I, class Struct>
    static constexpr auto make_partial_clazz(std::index_sequence<I...>, Struct&& in) noexcept {
        using extractible_symbols_t = flatten_tuples_t<std::conditional_t<tag_info<info_t index_tag_t<I>>::template has_mem_var<std::decay_t<Struct>>, std::tuple<info_t index_symbol_t<I>>, std::tuple<>>...>;
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

    using meta_pod_t = bind_to_t<meta_clazz, flatten_tuples_t<assign_tuple_t<symbol_tuple_t<X>, X>...>>;
    static constexpr bool is_pod = (DataSymbol<X> && ...);

    using meta_values_t = bind_to_t<meta_clazz, flatten_tuples_t<std::conditional_t<Value<X>, std::tuple<X>, std::tuple<>>...>>;
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
    static constexpr bool has_tag = (symbol_info<X>::template has_name<tag> || ...);

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
    requires !info_v has_tag<tag>
    constexpr inline auto get_or(T&& in) const noexcept {
        return std::forward<T>(in);
    }

    template<Tag tag, class T>
    requires info_v has_tag<tag> && Value<info_t tag_symbol_t<tag>>
    constexpr inline decltype(auto) get_or(T&&) & noexcept {
        return get<tag>();
    }

    template<Tag tag, class T>
    requires info_v has_tag<tag> && Value<info_t tag_symbol_t<tag>>
    constexpr inline decltype(auto) get_or(T&&) const & noexcept {
        return get<tag>();
    }

    template<Tag tag, class T>
    requires info_v has_tag<tag> && Value<info_t tag_symbol_t<tag>>
    constexpr inline decltype(auto) get_or(T&&) && noexcept {
        return get<tag>();
    }

    template<Tag tag>
    static constexpr bool is_clazz_var = Variable<info_t tag_symbol_t<tag>> 
                                      && Clazz<std::tuple_element_t<0, symbol_tuple_t<info_t tag_symbol_t<tag>>>>;

    // Matching
    template<class F>
    constexpr inline void for_each_as_var(F&& f) {
        ([&] { if constexpr (Variable<X>)
            for_each_as_var<symbol_tag_t<X>>(std::forward<F>(f));
        }(), ...);
    }

    template<class... Tags, class F>
    requires sizeof...(Tags) > 0 && (Variable<info_t tag_symbol_t<Tags>> && ...)
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
    requires sizeof...(Tags) > 0 && (Value<info_t tag_symbol_t<Tags>> && ...)
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
    requires std::is_invocable_v<F, clazz_t, Ts...>
    constexpr inline decltype(auto) call(F&& f, Ts&&... params) {
        return std::invoke(std::forward<F>(f), fields, std::forward<Ts>(params)...);
    }

    template<class F, class... Ts>
    requires std::is_invocable_v<F, const clazz_t, Ts...>
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
                std::tuple<>, 
                std::tuple<Ts>
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
    requires (meta_values_t<L>::template has_tag<Tags> && ...) 
          && (meta_values_t<R>::template has_tag<Tags> && ...)
    static constexpr bool weak_less_than(const L& l, const R& r) {
        return std::forward_as_tuple(get<Tags>(l)..., meta_values_t<L>::size) 
             < std::forward_as_tuple(get<Tags>(r)..., meta_values_t<R>::size);
    }
    
    template<Clazz L, Clazz R>
    requires (meta_values_t<L>::template has_tag<Tags> && ...) 
          && (meta_values_t<R>::template has_tag<Tags> && ...)
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
    requires (meta_values_t<L>::template has_tag<Tags> && ...) 
          && (meta_values_t<R>::template has_tag<Tags> && ...)
    static constexpr bool weak_eqivalent(const L& l, const R& r) {
        // Can only be equivalent if L and R have the same number of value fields
        if constexpr (meta_values_t<L>::size == meta_values_t<R>::size)
            return std::forward_as_tuple(get<Tags>(l)...) == std::forward_as_tuple(get<Tags>(r)...);
        else
            return false;
    }
    
    template<Clazz L, Clazz R>
    requires (meta_values_t<L>::template has_tag<Tags> && ...) 
          && (meta_values_t<R>::template has_tag<Tags> && ...)
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
    static constexpr bool r_has_tag_of = R::template has_tag<symbol_tag_t<S>>;
    
    template<Symbol S>
    using r_symbol_of = typename R::template tag_symbol_t<symbol_tag_t<S>>;

    template<Symbol S>
    static constexpr bool l_value_shared_by_r = Value<S> && r_has_tag_of<S> && Value<r_symbol_of<S>>;
        
    using l_shared_symbols_tuple_t = flatten_tuples_t<
        std::conditional_t<l_value_shared_by_r<Ls>,
            std::tuple<Ls>,
            std::tuple<>
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
    constexpr auto rsize = clazz_info<L>::values_info_t::size;
    if constexpr (lsize < rsize)
        return l_clazz_comparator_t<L, R>::strong_less_than(l, r);
    else if constexpr (lsize > rsize)
        return l_clazz_comparator_t<R, L>::strong_less_than(l, r);
    else {
        using lshared = typename shared_vars<L, R>::l_shared_clazz_t;
        using rshared = typename shared_vars<R, L>::l_shared_clazz_t;
        if constexpr (meta_values_t<lshared>::tags_info_t::names_array <= meta_values_t<rshared>::tags_info_t::names_array)
            return l_clazz_comparator_t<L, R>::strong_less_than(l, r);
        else
            return l_clazz_comparator_t<R, L>::strong_less_than(l, r);
    }
}

template<Tag... Tags, Clazz L, Clazz R>
requires sizeof...(Tags) > 0
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
requires sizeof...(Tags) > 0
constexpr bool strong_equals(const L& l, const R& r) {
    return clazz_comparator<Tags...>::strong_equals(l, r);
}

template<MetaClazz M1, MetaClazz M2>
constexpr bool operator==(const M1& l, const M2& r) {
    return strong_equals(l.fields, r.fields);
}

template<Tag... Tags, class Struct>
requires (tag_info<Tags>::template has_mem_var<std::decay_t<Struct>> && ...)
static constexpr inline auto make_clazz(Struct&& s) {
    using meta_clazz_t = meta_clazz<typename tag_info<Tags>::template mem_var_t<std::decay_t<Struct>>...>;
    return meta_clazz_t::import(std::forward<Struct>(s));
}

template <class T>
inline void hash_combine(size_t& seed, T const& v) {
    seed ^= std::hash<T>()(v) + 0x9e3779b9 + (seed<<6) + (seed>>2);
}

namespace detail {
    template<size_t... I>
    void hash_combine_clazz(std::index_sequence<I...>, size_t& seed, const Clazz& c) {
        (hash_combine(seed, get<I>(c)), ...);
    }

    template<size_t... I, size_t N, class T>
    void hash_combine_array(std::index_sequence<I...>, size_t& seed, const std::array<T, N>& a) {
        (hash_combine(seed, get<I>(a)), ...);
    }
}

// This will be be constexpr when P0202R3 is implemented in GCC
template<Clazz C>
auto sorted_values_names_array() {
    auto fields = meta_values_t<C>::tags_info_t::names_array_array;
    std::sort(begin(fields), end(fields));
    return fields;
}

// Hash of value fields oblivious to field order (fields are ordered lexicographically before hashing)
template<Clazz C>
size_t hash(const C& c) {
    // This will be be constexpr when P0202R3 is implemented in GCC
    auto fields = sorted_values_names_array<C>();
    using indicies = std::make_index_sequence<meta_values_t<C>::size>;

    size_t seed = 0;
    // Hash the sorted value field names
    detail::hash_combine_array(indicies{}, seed, fields);
    // Hash their values 
    // TODO: Sort the values by field name before hashing, which needs constexpr sorted field names
    detail::hash_combine_clazz(indicies{}, seed, c);
    return seed;
}

template<Symbol... X>
struct clazz : struple<clazz<X...>, X...> {
    using struple_t = struple<clazz<X...>, X...>;
    using tuple_t = flatten_tuples_t<symbol_tuple_t<X>...>;
    using meta_clazz_t = meta_clazz<X...>;
    
    static_assert(sizeof(struple_t) == sizeof(tuple_t));
    
    static_assert(assert_unique_symbol_names<X...>::value, "Fields names are not unique.");
                
    // Construct with a struct with the necessary fields of the same name
    template<class Struct>
    requires !Clazz<Struct> // Is not a class
          && (symbol_tag_info<X>::template has_mem_var<std::decay_t<Struct>> || ...) // Has at least one matching field
          && ((symbol_tag_info<X>::template has_mem_var<std::decay_t<Struct>>
            || symbol_info<X>::has_default_ctor) && ...) // Has all matching fields were no default ctors are defined
    constexpr clazz(Struct&& s) noexcept : clazz{meta_clazz_t::import_with_defaults(std::forward<Struct>(s))} {}
    
    // Construct with "designated initialiser" style set syntax
    template<class... Ts>
    requires sizeof...(Ts) > 0 && (SetPlaceholder<Ts> && ...) && (std::is_rvalue_reference_v<Ts&&> && ...)
    constexpr clazz(Ts&&... in) noexcept 
        : clazz(detail::set::placeholder_collection<Ts...>(std::forward<Ts>(in)...)) 
    {
        static_assert((Ts::template shares_name<X...> && ...), 
            "Initialising a field which doesn't exist in clazz");
    }

    // Construct from tuple with the same number of fields as we have data fields
    template<Tuple T>
    requires std::tuple_size<std::decay_t<T>>::value == clazz_info<clazz>::pod_info_t::size
    constexpr clazz(T&& tuple) noexcept : clazz{std::make_from_tuple<clazz>(std::forward<T>(tuple))} {}

private:
    struct use_struple_ctor_tag{};

    template<class... Ts>
    constexpr clazz(use_struple_ctor_tag, Ts&&... in) noexcept : struple_t(std::forward<Ts>(in)...) {}

public:
    // Use custom constructor or forward to struple constructor
    template<class... Ts>
    constexpr clazz(Ts&&... ins) noexcept : clazz{[&]() -> clazz {
        if constexpr(clazz_info<clazz>::template has_dec<dec::operator_ctor<clazz(Ts&&...) const>>) {
            return clazz::operator_ctor(std::forward<Ts>(ins)...);
        } else {
            return clazz{use_struple_ctor_tag{}, std::forward<Ts>(ins)...};
        }
    }()} {}

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

    constexpr decltype(auto) operator=(clazz&& other) 
    requires clazz_info<clazz>::is_default_assignable
          || clazz_info<clazz>::template has_co_dec<dec::operator_assign<void(clazz&&)>>
    {
        if constexpr (clazz_info<clazz>::template has_co_dec<dec::operator_assign<void(clazz&&)>>) {
            using return_t = decltype(this->operator_assign(std::move(other)));
            if constexpr (std::is_same_v<void, return_t>) {
                this->operator_assign(std::move(other));
                return *this;
            } else {
                return this->operator_assign(std::move(other));
            }
        } else {
            [this, &other]<Variable... V>(clazz_info<clazz<V...>>) {
                ((get<symbol_tag_t<V>>(*this) =
                    std::forward<typename symbol_info<V>::value_t>(get<symbol_tag_t<V>>(other))), ...);
            }(typename clazz_info<clazz>::variables_info_t{});
            return *this;
        }
    }

    constexpr decltype(auto) operator=(const clazz& other) 
    requires clazz_info<clazz>::is_default_assignable
          || clazz_info<clazz>::template has_co_dec<dec::operator_assign<void(clazz&&)>> 
    {
        if constexpr (clazz_info<clazz>::template has_co_dec<dec::operator_assign<void(const clazz&)>>) {
            using return_t = decltype(this->operator_assign(other));
            if constexpr (std::is_same_v<void, return_t>) {
                this->operator_assign(other);
                return *this;
            } else {
                return this->operator_assign(other);
            }
        } else {
            [this, &other]<Variable... V>(clazz_info<clazz<V...>>) {
                ((get<symbol_tag_t<V>>(*this) = get<symbol_tag_t<V>>(other)), ...);
            }(typename clazz_info<clazz>::variables_info_t{});
            return *this;
        }
    }

    // Custom clazz swap for same instance
    friend void swap(clazz& l, clazz& r) {
        if constexpr (clazz_info<clazz>::template has_dec<dec::operator_swap<void(clazz&)>>) {
            l.operator_swap(r);
        } else {
            [&]<Variable... V>(clazz_info<clazz<V...>>) {
                using std::swap;
                (swap(get<symbol_tag_t<V>>(l), get<symbol_tag_t<V>>(r)), ...);
            }(typename clazz_info<clazz>::variables_info_t{});
            std::cout << "inner friend swapped\n";
        }
    }

    // Custom clazz swap for clazzes with the same variables
    template<Clazz R>
    requires ClazzOf<typename clazz_info<clazz>::variables_info_t::clazz_t, 
                     typename clazz_info<R>::variables_info_t::clazz_t>
    friend void swap(clazz& l, R& r) {
        if constexpr (clazz_info<clazz>::template has_dec<dec::operator_swap<void(R&)>>) {
            l.operator_swap(r);
        } else {
            [&]<Variable... V>(clazz_info<clazz<V...>>) {
                using std::swap;
                (swap(get<symbol_tag_t<V>>(l), get<symbol_tag_t<V>>(r)), ...);
            }(typename clazz_info<clazz>::variables_info_t{});
            std::cout << "general inner friend swapped\n";
        }
    }

    // inline clazz& operator=(clazz&& other) {
    //     meta_clazz_of(*this).tuple = std::move(meta_clazz_of(other).tuple);
    //     return *this;
    // }

    // inline clazz& operator=(const clazz& other) {
    //     meta_clazz_of(*this).tuple = meta_clazz_of(other).tuple;
    //     return *this;
    // }

    // TODO: Find way of making destructor conditionally non-trival for constexpr purposes
    // ~clazz() noexcept {
    //     if constexpr (clazz_info<clazz>::template has_dec<dec::operator_dtor<void()>>)
    //         this->operator_dtor();
    // }

    template<Clazz T>
    constexpr inline decltype(auto) operator==(const T& r) const {
        if constexpr (clazz_info<clazz>::template has_co_dec<dec::operator_eq<void(const T&)>>)
            return this->operator_eq(r);
        else
            return strong_equals(*this, r);
    }

    template<Clazz T>
    constexpr inline decltype(auto) operator!=(const T& r) const {
        if constexpr (clazz_info<clazz>::template has_co_dec<dec::operator_not_eq<void(const T&)>>)
            return this->operator_not_eq(r);
        else
            return !strong_equals(*this, r);
    }

    template<Clazz T>
    constexpr inline decltype(auto) operator<(const T& r) const {
        if constexpr (clazz_info<clazz>::template has_co_dec<dec::operator_less<void(const T&)>>)
            return this->operator_less(r);
        else
            return strong_less_than(*this, r);
    }

    template<Clazz T>
    constexpr inline decltype(auto) operator>=(const T& r) const {
        if constexpr (clazz_info<clazz>::template has_co_dec<dec::operator_greater_eq<void(const T&)>>)
            return this->operator_greater_eq(r);
        else
            return !strong_less_than(*this, r);
    }

    template<Clazz T>
    constexpr inline decltype(auto) operator>(const T& r) const {
        if constexpr (clazz_info<clazz>::template has_co_dec<dec::operator_greater<void(const T&)>>)
            return this->operator_greater(r);
        else
            return strong_less_than(r, *this);
    }

    template<Clazz T>
    constexpr inline decltype(auto) operator<=(const T& r) const {
        if constexpr (clazz_info<clazz>::template has_co_dec<dec::operator_less_eq<void(const T&)>>)
            return this->operator_less_eq(r);
        else
            return !strong_less_than(r, *this);
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

template<Clazz L, ClazzOf<L> R>
void swap(L& l, R& r) {
    using std::swap;
    swap(l->tuple, r->tuple);
    std::cout << "outer friend swapped\n";
}

template<Clazz C, Tuple T>
requires std::is_same_v<T, typename meta_clazz_t<C>::tuple_t>
const C& as_clazz(const T& tuple) {
    return union_cast<const C&>(tuple);
}

template<Clazz C, Tuple T>
requires std::is_same_v<T, typename meta_clazz_t<C>::tuple_t>
C& as_clazz(T& tuple) {
    return union_cast<C&>(tuple);
}

template<Clazz C>
const auto& as_tuple(const C& c) {
    return meta_clazz_of(c).tuple;
}

template<Clazz C>
requires std::is_rvalue_reference_v<C&&>
const auto&& as_tuple(C&& c) {
    return std::move(meta_clazz_of(c).tuple);
}

template<NamedTupleWrapper N, Tuple T>
auto& as_named_tuple(const T& tuple) {
    using nup_t = bind_to_t<N, T>;
    return union_cast<const nup_t&>(tuple);
}

template<NamedTupleWrapper N, Tuple T>
auto& as_named_tuple(T& tuple) {
    using nup_t = bind_to_t<N, T>;
    return union_cast<nup_t&>(tuple);
}

template<NamedTupleWrapper N, Tuple T>
auto& as_nuple(const T& tuple) {
    return as_named_tuple<nuple>(tuple);
}

template<NamedTupleWrapper N, Tuple T>
auto& as_nuple(T& tuple) {
    return as_named_tuple<nuple>(tuple);
}

template<class, bool Const, Tag... Tags>
struct view;

template<class... X, bool Const, Tag... Tags>
requires sizeof...(Tags) > 0
struct view<clazz<X...>, Const, Tags...> 
     : view<typename meta_clazz<X...>::template super_meta_clazz_t<Tags...>::clazz_t, Const> {};

template<class... X, bool Const>
struct view<clazz<X...>, Const> {
    template<class T>
    using const_t = std::conditional_t<Const, std::add_const_t<T>, T>;

    using type = clazz <
        std::conditional_t <
            Variable<X>, 
            typename symbol_tag_info<X>::template var_t<std::add_lvalue_reference_t<const_t<typename symbol_info<X>::value_t>>>,
            X
        >...
    >;
};

template<NamedTupleWrapper N, class... X, bool Const, Tag... Tags>
requires sizeof...(Tags) > 0
struct view<N<X...>, Const, Tags...> 
     : view<typename N<X...>::type::template super_meta_clazz_t<Tags...>::clazz_t, Const> {};

template<NamedTupleWrapper N, class... X, bool Const>
struct view<N<X...>, Const> {
    template<class T>
    using const_t = std::conditional_t<Const, std::add_const_t<T>, T>;
    using type = N<std::add_lvalue_reference_t<const_t<typename symbol_info<X>::value_t>>...>;
};

template<class T, Tag... Tags>
using view_t = typename view<T, false, Tags...>::type;

template<class T, Tag... Tags>
using const_view_t = typename view<T, true, Tags...>::type;

template<Declaration... Ds>
struct trait {
    template<Clazz C>
    static constexpr bool co_implementor = clazz_info<C>::template co_implements<Ds...>;

    template<CoImplements<Ds...>... Variants>
    using variant_clazz_t = clazz < 
        // Variant index stored under object._.size
        var :: _ < clazz < var::index<unsigned char const> > >,
        // All trait defs callable with object.trait_def(...)
        typename dec_info<Ds>::template variant_def_t<Variants...>...
    >;
};

template<class>
struct variant_ebo {};

template<class V>
struct variant : variant_ebo<variant<V>> {
    const unsigned char index;
    V v;
    template<class... Ts>
    constexpr variant(unsigned char i, Ts&&... in) noexcept : index(i), v(std::forward<Ts>(in)...) {}
};

template<class T>
constexpr decltype(auto) vmax(T&& val) {
    return std::forward<T>(val);
}

template<class T0, class T1, class... Ts>
constexpr decltype(auto) vmax(T0&& val1, T1&& val2, Ts&&... vs) {
    return (val1 > val2) ?
      vmax(val1, std::forward<Ts>(vs)...) :
      vmax(val2, std::forward<Ts>(vs)...);
}


template<Trait Trt, ImplementsTrait<Trt>... Variants>
struct vvector {
    using size_type = unsigned int;
    using value_type = typename Trt::template variant_clazz_t<Variants...>;
    using reference = value_type&;
    using const_reference = const value_type&;

private:
    size_type write_head = 0;
    std::vector<char> buffer;
    std::vector<size_type> positions;

public:
    template<class T>
    void push_back(T&& value) {
        void(emplace_back<std::decay_t<T>>(std::forward<T>(value)));
    }
    
    template<class T, class... Ts>
    requires is_one_of<T, Variants...>::value
    reference emplace_back(Ts&&... args) {
        using variant_t = variant<T>;
        constexpr size_type value_size = sizeof(variant_t);
        constexpr size_t index = index_of_type<T, Variants...>::value;

        positions.push_back(write_head);
        buffer.reserve(write_head + value_size);
        new (&buffer[write_head]) variant_t(index, std::forward<Ts>(args)...);
        write_head += value_size;
        return back();
    }
    
    reference at(size_type pos) {
        return reinterpret_cast<reference>(buffer[positions.at(pos)]);
    }
    const_reference at(size_type pos) const {
        return reinterpret_cast<const_reference>(buffer[positions.at(pos)]);
    }
    reference operator[](size_type pos) {
        return reinterpret_cast<reference>(buffer[positions[pos]]);
    }
    const_reference operator[](size_type pos) const {
        return reinterpret_cast<const_reference>(buffer[positions[pos]]);
    }
    reference front() {
        return reinterpret_cast<reference>(buffer[0]);
    }
    const_reference front() const {
        return reinterpret_cast<const_reference>(buffer[0]);
    }
    reference back() {
        return reinterpret_cast<reference>(buffer[positions.back()]);
    }
    const_reference back() const {
        return reinterpret_cast<const_reference>(buffer[positions.back()]);
    }
    size_type size() const noexcept {
        return positions.size();
    }
    size_t data_size() const noexcept {
        return buffer.size();
    }
    bool empty() const noexcept {
        return positions.empty();
    }
    void pop_back() {
        auto& back = buffer[positions.back()];

        // Destruct back element
        detail::visit_clazz_variant<void, Variants...>(
            std::index_sequence_for<Variants...>{},
            back, &back, []<class T>(T& self) { self.~T(); });

        // Remove position and truncate buffer
        positions.pop_back();
        buffer.resize(back);
    }

private:
    static constexpr size_type var_sizes[] = { sizeof(variant<Variants>)... };

    template<bool Const>
    class _iterator {
        using value_type = vvector::value_type;
        using reference = value_type&;
        using pointer = value_type*;

        using container_t = std::conditional_t<Const, const vvector, vvector>;
        using value_t = std::conditional_t<Const, const value_type, value_type>;

        friend container_t;

        size_type pos = 0;
        container_t& underlying;        

    public:                
        auto& operator++() {
            pos += var_sizes[underlying.buffer[pos]];
            return *this;
        }
        
        inline value_t& operator*() const {
            return reinterpret_cast<value_t&>(underlying.buffer[pos]);
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
    inline const_iterator cbegin() const {
        return {0, *this};
    }
    inline iterator end() {
        return {write_head, *this};
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
    auto transpose_copy(std::index_sequence<I...>) const & {
        using visitor = overload<decltype([](const Variants& self, auto& out) { get<I>(out).emplace_back(self); })...>;
        return transpose_visit(*this, visitor{});
    }

    template<size_t... I>
    auto transpose_move(std::index_sequence<I...>) && {
        using visitor = overload<decltype([](Variants& self, auto& out) { get<I>(out).emplace_back(std::move(self)); })...>;
        return transpose_visit(*this, visitor{});
    }

    static auto transpose_visit(auto& self, auto&& visitor) {
        transpose_t out;
        for (auto& i : self) {
            detail::visit_clazz_variant<void, Variants...>(
                std::index_sequence_for<Variants...>{},
                i._.index, &i, visitor, out);
        }
        return out; 
    }

public:
    void reserve(size_type elements) {
        constexpr auto max = vmax(0, sizeof(variant<Variants>)...);
        buffer.reserve(max * elements);
        positions.reserve(elements);
    }

    vvector() {
        reserve(14);
    }

private:
    void destruct_all() {
        for (auto& i : *this)
            detail::visit_clazz_variant<void, Variants...>(
                std::index_sequence_for<Variants...>{},
                i._.index, &i,
                []<class T>(T& self) { self.~T(); });
    }

public:
    void clear() {
        destruct_all();
        buffer.clear();
        positions.clear();
    }

    ~vvector() {
        destruct_all();
    }
};

// Default alignment to cache-line
template<Pod, size_t Align = 64>
struct soa;

template<Variable... X, size_t Align>
struct soa<clazz<X...>, Align> {
    template<class, size_t>
    friend struct soa;

    template<template<class...> class Wrapper, Variable S>
    using mapped_symbol_t = typename symbol_tag_info<S>::template var_t<Wrapper<typename symbol_info<S>::value_t>>;

    // template<class... T>
    // using container_wrapper = std::vector<T...>;

    // TODO: Do not expose underlying vectors, only std::span (when GCC supports)
    // using arrays_t = clazz<mapped_symbol_t<container_wrapper, X>...>;
    // arrays_t arrays;

    using size_type = size_t;
    using clazz_t = clazz<X...>;

    using value_type = view_t<clazz_t>;
    using reference = view_t<clazz_t>;
    using const_reference = const_view_t<clazz_t>;

private:
    static constexpr size_t _unit_size = (sizeof(typename symbol_info<X>::value_t) + ...);

    using value_types = std::tuple<typename symbol_info<X>::value_t...>;

    template<size_t I>
    using value_t = std::tuple_element_t<I, value_types>;

    static constexpr auto sizes = []<size_t... I>(std::index_sequence<I...>) {
        return std::array{sizeof(value_t<I>)...};
    }(std::index_sequence_for<X...>{});

    using offsets_t = std::array<size_type, sizeof...(X)>;
    using arrays_t = clazz<mapped_symbol_t<std::add_pointer_t, X>...>;

    template<class T>
    using data_wrapper_t = std::add_const_t<std::add_pointer_t<T>>;

    using data_t = clazz<
        var::_<clazz<var::size<const size_type>>>,
        mapped_symbol_t<data_wrapper_t, X>...
    >;

    size_type _capacity;
    size_type _size;

    struct buffer_deleter {
        void operator()(char* p) { free(p); }
    };

    using buffer_t = std::unique_ptr<char, buffer_deleter>;
    buffer_t _buffer;
    arrays_t _arrays;

    static constexpr size_type default_capacity = 12;
    static constexpr size_type min_size = 4;

    soa(const offsets_t& offsets, size_type capacity, size_type size)
        : _capacity{capacity}
        , _size{size}
        , _buffer{_create_buffer(offsets, default_capacity)}
        , _arrays{_create_arrays(_buffer.get(), offsets)}
    {}

public:
    soa() : soa{_create_offsets(default_capacity), default_capacity, 0} {}

    soa(const soa& other) 
        : soa(_create_offsets(other._capacity), other._capacity, other._size)
    {
        _for_each(other._buffer.get(), _capacity, [this]<class T>(T* our_buffer, T* other_buffer) {
            for(size_t i = 0; i < _size; ++i) {
                new (our_buffer + i) T(other_buffer[i]);
            }
        });
    }

    soa(soa&& other) : soa() {
        swap(other);
    }

    void swap(soa& other) {
        using std::swap;
        swap(_capacity, other._capacity);
        swap(_size, other._size);
        swap(_buffer, other._buffer);
        swap(_arrays, other._arrays);
    }

    friend void swap(soa& l, soa& r) {
        l.swap(r);
    }

    void reserve(size_type n) {
        if (n > _capacity) {
            _resize_buffer(n);
        }
    }

    void resize(size_type n) {
        if (n > _capacity) {
            _resize_buffer(n);
        } else if (n < _size) {
            _for_each([n, this]<class T>(T* array) {
                if constexpr (!std::is_trivially_destructible_v<T>) {
                    for(size_type i = n; i < _size; ++i) {
                        array[i].~T();
                    }
                }
            });
            _size = n;
        }
    }

    void shrink_to_fit() {
        if (_size < _capacity) {
            _resize_buffer(std::max(_size, min_size));
        }
    }

private:
    void _grow_if_full() {
        if (_capacity == _size)
            _grow_buffer();
    }
    void _grow_buffer() {
        _resize_buffer(_size * 2);
    }

    void _resize_buffer(size_type capacity) {
        auto offsets = _create_offsets(capacity);
        auto new_buffer = _create_buffer(offsets, capacity);

        if (!empty()) {
            _for_each(new_buffer.get(), offsets, [this]<class T>(T* old_buffer, T* new_buffer) {
                for (size_t i = 0; i < _size; ++i) {
                    new (new_buffer + i) T(std::move(old_buffer[i]));
                    old_buffer[i].~T();
                }
            });
        }

        _buffer = std::move(new_buffer);
        _arrays = _create_arrays(_buffer.get(), offsets);
        _capacity = capacity;
    }

    constexpr offsets_t _create_offsets(size_type capacity) {
        offsets_t offsets = {0};
        size_type offset = 0;
        for (int i = 1; i < sizeof...(X); ++i) {
            offset += capacity * sizes[i-1];
            // Add padding if we are aligning
            if constexpr (Align != 0)
                offset = (offset + (Align - 1)) & -Align;
            offsets[i] = offset;
        }
        return offsets;
    }

    buffer_t _create_buffer(const offsets_t& offsets, size_type capacity) {
        size_type size = offsets.back() + capacity * sizes.back();
        if constexpr (Align != 0) {
            char* buffer = (char*)aligned_alloc(Align, (size + (Align - 1)) & -Align);
            return buffer_t(buffer);
        } else {
            char* buffer = (char*)malloc(size);
            return buffer_t(buffer);
        }
    }

    auto _create_arrays(char* buffer, const offsets_t& offsets) {
        return _create_arrays(std::index_sequence_for<X...>{}, buffer, offsets);
    }

    template<size_t... I>
    auto _create_arrays(std::index_sequence<I...>, char* buffer, const offsets_t& offsets) {
        return arrays_t{_get_array<I>(buffer, offsets)...};
    }

    template<size_t... I>
    void _copy_from_buffer(std::index_sequence<I...>, char* other_buffer) {
        (_copy_from_buffer<I>(
            _get_array<I>(),
            _get_array<I>(other_buffer, _capacity)
            ), ...);
    }
    template<size_t I, class T>
    void _copy_from_buffer(T* our_buffer, T* other_buffer) {
        for(size_t i = 0; i < _size; ++i) {
            new (our_buffer + i) T(other_buffer[i]);
        }
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
    void _for_each(char* other_buffer, const offsets_t& other_offsets, F&& f) {
        _for_each(std::index_sequence_for<X...>{}, other_buffer, other_offsets, std::forward<F>(f));
    }
    template<size_t... I, class F>
    void _for_each(std::index_sequence<I...>, char* other_buffer, const offsets_t& other_offsets, F&& f) {
        (std::invoke(std::forward<F>(f), _get_array<I>(), _get_array<I>(other_buffer, other_offsets)), ...);
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

    template<size_t I>
    inline value_t<I>* _get_array() {
        return get<I>(_arrays);
    }
    template<size_t I>
    inline value_t<I>* _get_array(char* buffer, const offsets_t& offsets) {
        return reinterpret_cast<value_t<I>*>(buffer + offsets[I]);
    }
    template<size_t I>
    const value_t<I>* _get_array() const {
        return get<I>(_arrays);
    }
    template<size_t I>
    const value_t<I>* _get_array(char* buffer, const offsets_t& offsets) const {
        return reinterpret_cast<const value_t<I>*>(buffer + offsets[I]);
    }

public:
    void push_back(const clazz_t& value) {
        _grow_if_full();

        _apply_arrays([&, this]<class... T>(T*... array) {
            (new (array + _size) T(get<symbol_tag_t<X>>(value)), ...);
        });
        ++_size;
    }

    void push_back(clazz_t&& value) {
        _grow_if_full();

        _apply_arrays([&, this]<class... T>(T*... array) {
            (new (array + _size) T(get<symbol_tag_t<X>>(std::move(value))), ...);
        });
        ++_size;
    }

    template<class... Ts>
    void push_back(Ts&&... ins) {
        push_back(clazz_t{std::forward<Ts>(ins)...});
    }

    template<class... Ts>
    requires (SetPlaceholder<Ts> && ...)
    reference emplace_back(Ts&&... ins) {
        _grow_if_full();

        auto phc = detail::set::placeholder_collection<Ts...>(std::forward<Ts>(ins)...);
        _apply_arrays([&, this]<class... T>(T*... array) {
            (new (array + _size) T(std::make_from_tuple<T>(std::move(phc).template get_tuple<symbol_tag_t<X>>())), ...);
        });
        ++_size;

        return back();
    }

    template<class... Ts>
    requires (!SetPlaceholder<Ts> && ...)
    reference emplace_back(Ts&&... ins) {
        _grow_if_full();

        _apply_arrays([&, this]<class... T>(T*... array) {
            (new (array + _size) T(std::forward<Ts>(ins)), ...);
        });
        ++_size;

        return back();
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
        return _apply_arrays([=](auto*... array) {
            return reference{array[pos]...};
        });
    }
    const_reference operator[](size_type pos) const {
        return _apply_arrays([=](auto*... array) {
            return const_reference{array[pos]...};
        });
    }
    reference front() {
        return _apply_arrays([](auto*... array) {
            return reference{array[0]...};
        });
    }
    const_reference front() const {
        return _apply_arrays([](auto*... array) {
            return const_reference{array[0]...};
        });
    }
    reference back() {
        return _apply_arrays([back = _size - 1](auto*... array) {
            return reference{array[back]...};
        });
    }
    const_reference back() const {
        return _apply_arrays([back = _size - 1](auto*... array) {
            return const_reference{array[back]...};
        });
    }
    data_t data() noexcept {
        return {_size, get<symbol_tag_t<X>>(_arrays)...};
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
    ~soa() {
        clear();
    }

private:
    template<bool Const>
    struct _iterator {
        using difference_type = std::ptrdiff_t;
        using iterator_category = std::random_access_iterator_tag;

    private:
        template<class T>
        using pointer_wrapper = std::conditional_t<Const, 
                                                   std::add_pointer_t<std::add_const_t<T>>, 
                                                   std::add_pointer_t<T>>;

        using pointers = clazz <
            typename soa::template mapped_symbol_t<pointer_wrapper, X>...
        >;

        template<class T>
        using reference_wrapper = std::conditional_t<Const, 
                                                     std::add_lvalue_reference_t<std::add_const_t<T>>, 
                                                     std::add_lvalue_reference_t<T>>;

        using references = clazz <
            ovl::operator_assign<
                // Custom move assignment swaps elements (required for efficient sorting)
                def::operator_assign<void, []<class T>(T& self, T&& other) {
                    using std::swap;
                    (swap(get<symbol_tag_t<X>>(self), get<symbol_tag_t<X>>(other)), ...);
                    std::cout << "assign swapped\n";
                }>,
                def::operator_assign<void, []<class T>(T& self, T& other) {
                    ((get<symbol_tag_t<X>>(self) = get<symbol_tag_t<X>>(other)), ...);
                    std::cout << "assign copied\n";
                }>
            >,
            typename soa::template mapped_symbol_t<reference_wrapper, X>...
        >;

        pointers ptrs;

        template<class... T>
        _iterator(T*... ptrs_) : ptrs{ptrs_...} {}

        template<class... Ts>
        _iterator(const _iterator& other) : ptrs{other.ptrs} {}

    public:
        using value_type = references;
        using reference = value_type&;
        using pointer = value_type*;

        _iterator& operator++() {
            (++get<symbol_tag_t<X>>(ptrs), ...);
            return *this;
        }

        _iterator& operator--() {
            (--get<symbol_tag_t<X>>(ptrs), ...);
            return *this;
        }

        _iterator& operator=(const _iterator& other) {
            ptrs = other.ptrs;
            return *this;
        }

        difference_type operator-(const _iterator& other) const {
            return get<0>(ptrs) - get<0>(other.ptrs);
        }

        _iterator operator-(int n) const {
            return _iterator{(get<symbol_tag_t<X>>(ptrs) - n)...};
        }

        _iterator operator+(int n) const {
            return _iterator{(get<symbol_tag_t<X>>(ptrs) + n)...};
        }

        bool operator<(const _iterator& other) const {
            return get<0>(ptrs) < get<0>(other.ptrs);
        }
        
        inline reference operator*() const {
            return union_cast<reference>(const_cast<pointers&>(ptrs));
        }

        inline pointer operator->() const {
            return &**this;
        }

        inline bool operator==(const _iterator& other) const {
            return ptrs == other.ptrs;
        }
        
        inline bool operator!=(const _iterator& other) const {
            return !(*this == other);
        }
    };

public:
    using iterator = _iterator<false>;
    using const_iterator = _iterator<true>;

    inline iterator begin() {
        return _apply_arrays([](auto*... array) {
            return iterator{array...};
        });
    }
    inline const_iterator cbegin() const {
        return _apply_arrays([](const auto*... array) {
            return const_iterator{array...};
        });
    }
    inline iterator end() {
        return _apply_arrays([this](auto*... array) {
            return iterator{(array + _size)...};
        });
    }
    inline const_iterator cend() const {
        return _apply_arrays([this](const auto*... array) {
            return const_iterator{(array + _size)...};
        });
    }

    void sort() {
        return sort([](const reference& l, const reference& r) {
            return l < r;
        });
    }

    // Since most comparisons during sorting are done on one field of each element, find
    // the sorted order first, before sorting all of the elements in memory, field by field.
    // Moving all fields of the elements at once in a single pass sorting process will thrash
    // the cache, as that is a row-wise process, whereas soa is optimised for column-wise processes.
    // Once we have found an order, then move the elements in each field/column one-by-one.
    // Another added benefit will be fewer move operations per element on average, as once we have
    // found the order, the number of moves required to sort in memory will be less.
    template<class Comp>
    requires std::is_invocable_r_v<bool, Comp&&, reference, reference>
    void sort(Comp&& comp) {
        // Allocate all the memory needed in one go
        auto buffer = std::make_unique<size_type[]>(2 * _size);
        // Divide it up into two arrays
        size_type* indices = buffer.get();
        size_type* targets = buffer.get() + _size;

        // Create a list of indices for each element in the soa
        std::iota(indices, indices + _size, 0);
        // Find the resulting locations of the indices after the soa is in sorted order
        std::sort(indices, indices + _size, [&, this](size_type l, size_type r) -> bool {
            return std::invoke(std::forward<Comp>(comp), (*this)[l], (*this)[r]); 
        });
 
        // The set of target locations for the underlying elements
        // is the inverse permutation of the set of sorted indices
        for (size_type i = 0; i < _size; ++i) {
            targets[indices[i]] = i;
        }

        // For each array in this soa, move the elements of into their target positions in memory
        // Re-use the memory of the now useless indices array for the mutable targets array
        _for_each([this, targets0 = (const size_type*)targets, targets = indices](auto* array) {
            // Initialise mutable targets array with the original immutable targets0 array
            std::copy_n(targets0, _size, targets);
            // Put the elements of this array into their target position
            for (size_type i = 0; i < _size; ++i) {
                while (targets[i] != i) {
                    using std::swap;
                    swap(array[i], array[targets[i]]);
                    swap(targets[i], targets[targets[i]]);
                }
            }
        });
    }

    template<class... S>
    bool operator==(const soa<S...>& other) const {
        if constexpr (sizeof...(S) != sizeof...(X)) return false;
        else {
            static_assert((SymbolNamed<S, symbol_tag_t<X>> && ...), "soa's do not share field names");
            return _size == other._size 
                && (std::equal(get<symbol_tag_t<X>>(_arrays), 
                               get<symbol_tag_t<X>>(_arrays) + _size, 
                               get<symbol_tag_t<X>>(other._arrays)) && ...);
        }
    }
};

// TODO: Consider binary search for variant index
namespace detail {
    template<class R, class... Variants, size_t I, size_t... Is, class F, class... Ts>
    constexpr R visit_clazz_variant(std::index_sequence<I, Is...>, const unsigned char index, void* const ptr, F&& f, Ts&&... args) {
        using variant_t = std::tuple_element_t<I, std::tuple<variant<Variants>...>>;
        if (index == I)
            return std::invoke(std::forward<F>(f), reinterpret_cast<variant_t*>(ptr)->v, std::forward<Ts>(args)...);
        else
            return visit_clazz_variant<R, Variants...>(std::index_sequence<Is...>{}, index, ptr, std::forward<F>(f), std::forward<Ts>(args)...);
    }
    
    template<class R, class... Variants, size_t I, size_t... Is, class F, class... Ts>
    constexpr R visit_clazz_variant(std::index_sequence<I, Is...>, const unsigned char index, const void* const ptr, F&& f, Ts&&... args) {
        using variant_t = std::tuple_element_t<I, std::tuple<variant<Variants>...>>;
        if (index == I)
            return std::invoke(std::forward<F>(f), reinterpret_cast<const variant_t*>(ptr)->v, std::forward<Ts>(args)...);
        else
            return visit_clazz_variant<R, Variants...>(std::index_sequence<Is...>{}, index, ptr, std::forward<F>(f), std::forward<Ts>(args)...);
    }

    template<class R, class... Variants, class T, class F, class... Ts>
    inline constexpr R visit_clazz_variant(std::index_sequence<>, unsigned char, T*, F&&, Ts&&...) noexcept {
        // Unrecoverable error if you are calling a variant with invalid index
        std::terminate();
    }
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
        size_t operator()(const C& c) const {
            return CLAZZ_NS::hash(c);
        }
    };

    template<CLAZZ_NS::Clazz L, CLAZZ_NS::Clazz R>
    requires CLAZZ_NS::ClazzOf<typename CLAZZ_NS::clazz_info<L>::variables_info_t::clazz_t, 
                               typename CLAZZ_NS::clazz_info<R>::variables_info_t::clazz_t>
    void swap(L& l, R& r) {
        using namespace CLAZZ_NS;
        [&]<Variable... V>(clazz_info<clazz<V...>>) {
            using std::swap;
            (swap(get<symbol_tag_t<V>>(l), get<symbol_tag_t<V>>(r)), ...);
        }(typename clazz_info<L>::variables_info_t{});
        std::cout << "std swapped\n";
    }

    template<class... X>
    void iter_swap(CLAZZ_NS::clazz<X...>& l, CLAZZ_NS::clazz<X...>& r) {
        swap(l, r);
        std::cout << "std swapped\n";
    }
}

// TEST CODE
using namespace CLAZZ_NS;

DECLARE_STRUPLE_SYMBOL(eat);
DECLARE_STRUPLE_SYMBOL(cats);
DECLARE_STRUPLE_SYMBOL(dogs);
DECLARE_STRUPLE_SYMBOL(cows);
DECLARE_STRUPLE_SYMBOL(print);
DECLARE_STRUPLE_SYMBOL(swap);
DECLARE_STRUPLE_SYMBOL(x);
DECLARE_STRUPLE_SYMBOL(y);
DECLARE_STRUPLE_SYMBOL(z);
DECLARE_STRUPLE_SYMBOL(name);
DECLARE_STRUPLE_SYMBOL(age);

inline clazz<var::_1<int>, var::_2<double>> testf() {
    return {1, 2.0};
}

inline nuple<int, double> testf2() {
    return {1, 2.0};
}

inline auto testfs(clazz<var::_1<int>, var::_2<double>>& in) {
    return in._1;
}

using type = meta_pod <
    var :: _1 <int>,
    var :: _2 <double>
>:: with_symbols <
    val :: _3 <int, 100>,
    fun :: _4 <int(),
        [] { return 1'000; }
    >,
    fun :: _5 <int(int) const,
        [](int in) {
            return 10'000 * (in % 10);
        }
    >,
    def :: _6 <int(int) const,
        [](Implements<dec::_1<int>>& clz, int x) {
            return x + clz._1*100'000;
        }
    >,
    def :: _7 <int(),
        [](auto& self) {
            return 1'000'000 * (self._1 *= 1);
        }
    >,
    def :: _8 <int(int) const,
        [](const auto& self, int in) {
            return 10'000'000 * (in % 10) * self._1;
        }
    >,
    ovl :: _9 <
        fun :: _9 <int() const,
            [] { return 100'000'000; }
        >,
        def :: _9 <int(int) const,
            [](auto&, int i) { return 100'000'000 * (i % 10); }
        >
    >
    ,
    def::operator_pointer<void() const, [](auto& ths) {
        return &ths;
    }>
>::clazz_t;

static_assert(std::is_same_v<var::_1<int>, clazz_info<type>::tag_symbol_t<tag::_1>>);

using type2 = clazz <
    var :: _1 <int>,
    tpe :: _2 <double>
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

static_assert(clazz_info<type>::has_dec<dec::_1<int>>);
static_assert(clazz_info<type>::has_dec<dec::_2<double>>);
static_assert(clazz_info<type>::has_dec<dec::_3<int>>);
static_assert(clazz_info<type>::has_dec<dec::_4<int() const>>);
static_assert(clazz_info<type>::has_dec<dec::_5<int(int) const>>);
static_assert(clazz_info<type>::has_dec<dec::_6<int(int) const>>);
static_assert(clazz_info<type>::has_dec<dec::_7<int()>>);
static_assert(clazz_info<type>::has_dec<dec::_8<int(int) const>>);
static_assert(clazz_info<type>::has_dec<dec::_9<int() const>>);

inline type testF(const clazz<var::_1<int, 1>, var::_2<double>>& clz) {
    //return {set::_1 = 1, set::_2 = 20.0};
    clz->template get_or<tag::_3>(3);
    return clz;
}

using sub_type = clazz <
    def :: _7 < int(), 
        [](auto&){return 1;}
    >,
    ovl :: _9 <
        fun :: _9 <int() const,
            [] { return 100'000'000; }
        >,
        def :: _9 <int(int) const,
            [](auto&, int i) { return 100'000'000 * (i % 10); }
        >
    >
>;
static_assert(SuperClazzOf<sub_type, type>);

// template<class F>
// void for_each(Clazz&& )


using Cat = clazz <
    fun :: eat <int(), [] { return 1; }>
>;
using Dog = clazz <
    fun :: eat <int(), [] { return 2; }>
>;
using Cow = clazz <
    fun :: eat <int(), [] { return 3; }>
>;

using Animals = clazz <
    var :: cats <std::array<Cat, 1>>,
    var :: dogs <std::array<Dog, 1>>,
    var :: cows <std::array<Cow, 1>>
    //, def :: operator_pointer<void, [](auto& self) { return &self; }> 
>;

template<class T, Declaration... Ds>
concept bool ContainerImplements = Implements<typename std::decay_t<T>::value_type, Ds...>;

template<class C1, class C2>
concept bool Covariant = is_convertible<C2, C1>::value;

long retT(const clazz<var::_1<std::string>>& in) {
    return in._1.length();
}

static bool trivial_tuple_test() {
    auto c = clazz{set::_1 = 1, set::_2 = 2.0};
    auto [_1, _2] = c->tuple;
    auto [__1, __2] = c;
    static_assert(std::is_same_v<decltype(_1), int>);
    static_assert(std::is_same_v<decltype(__1), int>);
    static_assert(std::is_same_v<decltype(_2), double>);
    static_assert(std::is_same_v<decltype(__2), double>);
    return _1 == c._1 && _2 == c._2 && __1 == _1 && __2 == _2;
}

using testComp = clazz <
    var::_1<std::string>,
    var::_2<std::string, []{return "yo";}>
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
    var::_1<int>,
    var::_2<int>,
    var::_3<int>,
    def::_10<int(), [](auto& self) { 
        return self._1 + self._2 + self._3;
    }>
>;

using var2 = clazz <
    var::_1<int>,
    var::_2<int>,
    var::_3<int>,
    var::_4<int>,
    var::_5<int>,
    def::_10<int(), [](auto& self) { 
        return self._1 + self._2 + self._3 + self._4 + self._5;
    }>
>;

using var3 = clazz <
    var::_1<int>,
    var::_2<int>,
    var::_3<int>,
    var::_4<int>,
    var::_5<int>,
    var::_6<int>,
    def::_10<int(), [](auto& self) { 
        return self._1 + self._2 + self._3 + self._4 + self._5 + self._6;
    }>
>;

using n_trait = trait<dec::_10<int()>>;

struct tracker { 
    int i; 
    bool operator<(const tracker& t) {
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

int main(int argc, char** argv) {
    if (!trivial_tuple_test()) {
        std::terminate();
        return 0;
    }

    auto c1 = clazz{set::_1 = 1, set::_2 = 2};
    auto c2 = clazz{set::_2 = 2, set::_1 = 1};
    using std::swap;
    swap(c1, c2);

    int one = 1;

    auto fwd_c = map(clazz{set::_1 = 1}, [](auto&& var) -> auto&& { return std::move(var); });
    static_assert(meta_clazz_t<decltype(fwd_c)>::has_dec<dec::_1<int&&>>);

    using view_test = clazz<var::_1<int>>;
    view_test v1 = {1};
    view_t<view_test> v2 = v1;
    view_t<view_test> v3 = v2;
    view_t<view_test> v4 = std::move(v2);

    // std::swap(v2, v3);

    int n;
    std::string s;
    auto t1 = clazz<var::_1<int&>, var::_2<std::string&>>(n, s);
    // std::swap(t1, t1);

    int soa_count = 0;
    soa<clazz<var::_1<int>, var::_2<std::string, []{return "yo";}>, var::_3<tracker, []{return tracker(1);}>>> soa;
    std::cout << "push (3,no)...\n";
    soa.push_back({3, "no"});
    std::cout << "push (2,po)...\n";
    soa.push_back({2, "po"});
    std::cout << "shrink...\n";
    soa.shrink_to_fit();
    std::cout << "push (6,yo)...\n";
    soa.push_back(6);
    std::cout << "push (6,ho)...\n";
    soa.push_back(6, "ho");
    std::cout << "emplace (4, ppp) ...\n";
    soa.emplace_back(set::_1 = 4, set::_2(3,'p'), set::_3(6));
    std::cout << "emplace (4, ppp) ...\n";
    soa.emplace_back(set::_1 = 4, set::_2(3,'p'), set::_3(5));
    std::cout << "sorting...\n";
    // std::sort(soa.begin(), soa.end());
    soa.sort([](const auto& l, const auto& r) {
        return clazz_comparator<tag::_3, tag::_1, tag::_2>::strong_less_than(l, r);   
    });
    for(auto& el : soa) {
        soa_count += el._1 + el._2.length();
        std::cout << el._1 << ',' << el._2 << ',' << el._3.i << '\n';
    }

    auto data = soa.data();
    for (int i = 0; i < data._.size; ++i) {
        std::cout << "first " << data._1[i] << '\n';
    }

    return soa_count;

    // return hash(clazz{set::_1 = 1});

    // auto tup = std::tuple{1, 2};
    // auto nup = nuple(std::tuple{1,2});
    // using nupc_t = clazz<var::_2<int>, var::_3<int>>;
    // auto nupc = nupc_t(tup);
    // view_t<nupc_t> nupcv = nupc;
    // return nupcv._2 + nupcv._3;
    // auto& nupc2 = as_named_tuple<nuple>(tup);
    // return nupc2._1 + nupc2._2;

    // auto c = clazz<var::_1<int, 1>, val::_2<int, 20>, val::_3<int, 300>>{};
    // auto t = tie<tag::_1, tag::_2>(c);
    // int x,y;
    // auto tie1 = std::tie(y,x);
    // auto tie2 = std::tie(x,y);
    // std::swap(tie1, tie2);
    // auto&& [first, second] = std::tie(x,y);
    // static_assert(std::is_lvalue_reference_v<decltype(first)>);
    // return first + second;
    
    // auto pv = vvector<n_trait
    // , var1
    // , var2
    // >();
    // pv.emplace_back<var1>(1, 20, 300);
    // // pv.emplace_back<var1>(1, 20, 300);
    // pv.emplace_back<var2>(1, 20, 300, 4000, 50000);
    // // int countv = 0;
    // // for(auto& i : pv) {
    // //     countv += i._10();
    // // }

    // auto pv2 = pv.transpose();
    // int countv2 = 0;
    // pv2->for_each_var([&](auto& v) {
    //     for (auto& i : v) {
    //         countv2 += i._10();
    //     }
    // });
    // return countv2;
    
    // auto v2 = var2(2, 30, 400, 5000, 60000);
    // auto v2 = var2(argv[0][0], argv[1][0], argv[2][0], argv[3][0], argv[4][0]);
    // auto v1 = var1(v2);
    
    // auto c1 = variant<var1>{argv[0][0], v1};
    // return reinterpret_cast<var_clazz<var1, var2, var3>&>(c1)._10();
    
    // constexpr bool compare = clazz{set::_1 = 0.9, set::_2 = 2, set::_4 = 0} < clazz<var::_2<int, 2>, val::_1<int, 1>>{};
    // return compare;

    // auto testCompInst = testComp{set::_1 ="", set::_2(2, 'h')};
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
 
    auto inst = make_clazz<tag::_1, tag::_2, tag::_3>(hi);
    auto inst2 = clazz<var::_3<int>, var::_1<int>, var::_4<int, 3>>(hi);
    auto inst3 = meta_clazz<var::_3<int>, var::_1<int>, var::_4<int, 3>>::import_with_defaults(hi);
    // return inst3._3 + inst3._1 + inst3._4;

    // return retT(clazz<var::_1<const char*>>{"sashi"});
    // return retT(clazz{set::_1 = "sashi"});
    // return retT({set::_1 = "sashi"});

    Animals a {
        std::array<Cat, 1>{},
        std::array<Dog, 1>{},
        std::array<Cow, 1>{}
    };
    int count = 0;
    a->for_each_var([&](ContainerImplements<dec::eat<int()const>>&& vec) {
        for (auto& animal : vec) {
            count += animal.eat();
        }
    });
    //return count;
    auto F = testF({set::_2 = 20.0});
    F._1 = 1;
    F._2 = 20;
    int i = 0;
    F->for_each_as_var(try_first {
        [&](var::_1<int>& in) {
            i += in._1;
        }, 
        [&](var::_2<double>& in) {
            i += in._2;
        }
    });
    return i;
    //F._8(std::string(""));
    return F._1 + F._2 + F._3 + F._4() + F._5(2) + F._6(7) + F._7() + F._8(3) + F._9() + F._9(2);
}

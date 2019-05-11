#include <algorithm>
#include <tuple>
#include <type_traits>
#include <functional>
#include <variant>
#include <string>

#ifdef _MSC_VER
#define EBO_MSVC __declspec(empty_bases)
#else
#define EBO_MSVC
#endif

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
    char const* p = __PRETTY_FUNCTION__;
    while (*p++ != '=');
    for (; *p == ' '; ++p);
    char const* p2 = p;
    int count = 1;
    for (;;++p2)
    {
        switch (*p2)
        {
        case '[':
            ++count;
            break;
        case ']':
            --count;
            if (!count)
                return std::string_view{p, std::size_t(p2 - p)};
        }
    }
    return std::string_view{};
}

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
concept bool Pod = Clazz<T> && meta_clazz_t<T>::is_pod;

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
concept bool Implements = Clazz<T> && meta_clazz_t<T>::template implements<Decs...>;

template<class T, class... Decs>
concept bool CoImplements = Clazz<T> && meta_clazz_t<T>::template co_implements<Decs...>;

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
    static constexpr bool value = meta_clazz_t<SubClazz>::template implements_of<Sups...>;
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
    static constexpr bool value = meta_clazz_t<SubClazz>::template co_implements_of<Sups...>;
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
    static constexpr bool value = meta_clazz_t<SubClazz>::template contra_implements_of<Sups...>;
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

template<class C>
struct assign_tuple<std::tuple<>, C> {
    using type = std::tuple<>;
};

template<class T, class C>
struct assign_tuple<std::tuple<T>, C> {
    using type = std::tuple<C>;
};

template<class T, class C>
using assign_tuple_t = typename assign_tuple<T, C>::type;

template<class, template<class> class>
struct map_tuple;

template<class... Ts, template<class> class W>
struct map_tuple<std::tuple<Ts...>, W> {
    using type = std::tuple<W<Ts>...>;
};

template<class T, template<class> class W>
using map_tuple_t = typename map_tuple<T, W>::type;

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

    template<Tag Name, class Target, class Tuple>
    // void Target means Target is yet to be determined by the clazz to be constructed
    // When void, Target cannot be used for template argument deduction of clazz
    struct placeholder : Tuple {
        using tuple_t = Tuple;
        using tuple_t::tuple;
                    
        template<class VarT>
        constexpr inline decltype(auto) make() noexcept {
            return std::make_from_tuple<VarT>(static_cast<tuple_t&>(*this));
        }

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

        template<DataSymbol S>
        requires has_name_of<S>
        constexpr inline S make_symbol() {
            constexpr size_t idx = find_ph_for_symbol<0, S>();
            return std::get<idx>(*this).template make<S>();
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
    };
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
    template<class... Names>
    constexpr struple(Names&&...) noexcept {}
};

template<Clazz Top, class X, class... Xs>
struct struple<Top, X, Xs...> : struple<Top, Xs...>, symbol_element_t<Top, X>
{
    using this_t = struple<Top, X, Xs...>;
    using base_t = struple<Top, Xs...>;
    using element_t = symbol_element_t<Top, X>;
    using tag_t = symbol_tag_t<X>;
    static_assert(sizeof(base_t) == sizeof(flatten_tuples_t<symbol_tuple_t<Xs>...>));

protected:
    constexpr struple() noexcept
        :base_t(), element_t{} {}

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
        , element_t{setter_collection.template make_symbol<X>()}
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
          && meta_clazz_t<Clz>::template has_tag<tag_t> 
          && Value<typename meta_clazz_t<Clz>::template tag_symbol_t<tag_t>>
    constexpr struple(Clz&& clz) noexcept
        : base_t{std::forward<Clz>(clz)}
        , element_t{get<tag_t>(std::forward<Clz>(clz))}
    {
    }

    template<Clazz Clz>
    requires DataSymbol<X> 
          && !meta_clazz_t<Clz>::template has_tag<tag_t> 
          && symbol_info<X>::has_default_ctor
    constexpr struple(Clz&& clz) noexcept
        : base_t{std::forward<Clz>(clz)}
        , element_t{}
    {
    }
};

template<class... Names, class... Targets, class... Ts>
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

#define DECLARE_NAMED_TUPLE_VAR_WRAPPER(struple_symbol) \
    namespace struple_symbols { template<size_t I, class T> struct struple_symbol { using type = void; }; }

#define DECLARE_NAMED_TUPLE_OR_META_CLAZZ(tuple_name, clazz_or_meta_clazz) \
    DECLARE_NAMED_TUPLE_VAR_WRAPPER(tuple_name);\
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
    tuple_name(std::tuple<Ts...>) -> tuple_name<Ts...>;
    
#define DECLARE_NAMED_META_CLAZZ(tuple_name) DECLARE_NAMED_TUPLE_OR_META_CLAZZ(tuple_name, meta_clazz)
#define DECLARE_NAMED_TUPLE(tuple_name) DECLARE_NAMED_TUPLE_OR_META_CLAZZ(tuple_name, clazz)

#define DECLARE_NAMED_TUPLE_VAR(tuple_name, index, struple_tag_name) \
    namespace struple_symbols {\
        template<class T>\
        struct tuple_name<index, T> { using type = ::var::struple_tag_name<T>; };\
        static_assert(Symbol<typename tuple_name<std::max(0, index-1), int>::type>,\
                      "Previous index of "#tuple_name" is not valid");\
    }

template<template<class> class DecWrapper, class DecType, Tag TagName>
struct dec_info_impl {
    template<Declaration To>
    struct is_convertible_to_dec : std::false_type {};
    template<class To>
    struct is_convertible_to_dec<DecWrapper<To>> : is_convertible<DecType, To> {};
    static constexpr bool is_callable = CallSignature<DecType>;
    static constexpr bool is_const_callable = CallSignatureConst<DecType>;
    static constexpr bool is_mutable_callable = is_callable && !CallSignatureConst<DecType>;
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

#define DECLARE_STRUPLE_DEC(tag_name) \
    FWD_DECLARE_STRUPLE_VAR(tag_name)\
    FWD_DECLARE_STRUPLE_DEF(tag_name)\
    namespace dec {\
        template<class DecType>\
        struct tag_name {};\
    }\
    template<class DecType>\
    struct dec_info<dec::tag_name<DecType>> : dec_info_impl<dec::tag_name, DecType, ::tag::tag_name> {\
        using return_t = typename call_signature<DecType>::return_t;\
        static constexpr bool is_dec = true;\
        template<class... Variants>\
        struct variant_def {\
            using type = detail::def::tag_name ## _impl_<int, DecType, decltype([]<class... Os>(auto& self, Os&&... args) {\
                return detail::visit_clazz_variant<return_t, Variants...>(\
                    std::index_sequence_for<Variants...>{},\
                    self.variant_index, &self,\
                    []<class C, class... Ts>(C&& c, Ts&&... ins) {\
                        return std::forward<C>(c).tag_name(std::forward<Ts>(ins)...);\
                    },\
                    std::forward<Os>(args)...);\
            })>;\
        };\
        template<class... Variants>\
        using variant_def_t = typename variant_def<Variants...>::type;\
    }

#define FWD_DECLARE_STRUPLE_TAG(tag_name) \
    namespace tag {\
        struct tag_name;\
    }

#define DECLARE_STRUPLE_TAG(tag_name) \
    FWD_DECLARE_STRUPLE_VAR(tag_name)\
    FWD_DECLARE_STRUPLE_VAL(tag_name)\
    namespace tag {\
        struct tag_name {};\
        namespace tag_name ## _concepts_impl_ {\
            template<class Struct>\
            concept bool HasMemVar = requires (Struct s) { {s.tag_name}; };\
        }\
    }\
    template<>\
    struct tag_info<tag::tag_name> {\
        static constexpr bool is_tag = true;\
        template<class T>\
        using var_t = detail::var::tag_name ## _impl_<T>;\
        template<class T, auto Value>\
        using val_t = detail::val::tag_name ## _impl_<T, decltype([]{ return Value; })>;\
        static constexpr auto& set_placeholder = set::tag_name;\
        static constexpr auto name = std::string_view(#tag_name);\
        template<Symbol S>\
        struct assert_unique_name {\
            static constexpr bool value = !symbol_info<S>::template has_name<tag::tag_name>;\
            static_assert(value, "Field "#tag_name" is not unique.");\
        };\
        template<class Struct>\
        static constexpr bool has_mem_var = tag::tag_name ## _concepts_impl_::HasMemVar<Struct>;\
        template<class Struct>\
        using mem_var_t = var_t<typename class_data_member_pointer_info<decltype(&Struct::tag_name)>::member_t>;\
    }

template<class>
struct class_data_member_pointer_info;

template<class Outer, class Member>
struct class_data_member_pointer_info<Member Outer::*> {
    static constexpr bool is_data_member = true;
    using member_t = Member;
};

#define DECLARE_STRUPLE_SET(tag_name) \
    FWD_DECLARE_STRUPLE_TAG(tag_name)\
    namespace set {\
        constexpr struct {\
            template<bool E>\
            struct assert_targeted {\
                static_assert("clazz<...> template deduction failure" && false_v<>,\
                  "set::"#tag_name"(args...):\nCannot deduce template arguments for clazz<...> with set::"#tag_name"(args...), "\
                  "use set::"#tag_name".as<TargetType>(args...) to set a type for deduction when "\
                  "calling a constructor of a clazz member with multiple parameters.");\
            };\
            template<class T>\
            constexpr auto operator=(T&& in) const {\
                return detail::set::placeholder<tag::tag_name, std::decay_t<T>, std::tuple<T&&>>{std::forward<T>(in)};\
            }\
            template<class T>\
            constexpr auto operator()(T&& in) const {\
                return detail::set::placeholder<tag::tag_name, std::decay_t<T>, std::tuple<T&&>>{std::forward<T>(in)};\
            }\
            template<class... Ts>\
            constexpr auto operator()(Ts&&... ins) const {\
                return detail::set::placeholder<tag::tag_name, detail::set::undetermined<assert_targeted>, std::tuple<Ts&&...>>{std::forward<Ts>(ins)...};\
            }\
            template<class Target, class... Ts>\
            constexpr auto as(Ts&&... ins) const {\
                return detail::set::placeholder<tag::tag_name, Target, std::tuple<Ts&&...>>{std::forward<Ts>(ins)...};\
            }\
        } tag_name;\
    }\

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
    static constexpr bool has_dec = dec_info<Dec>::template is_match_for_def<TagName, Top, F>;
    template<Symbol S>
    static constexpr bool shares_dec = tag_queries<TagName>::template shares_name<S> && 
        ((!DefinitionTemplate<S> && symbol_info<S>::template shares_dec<Sym>)
       || (DefinitionTemplate<S> && std::is_same_v<Sym, S>));
    template<Declaration Dec>
    static constexpr bool has_wider_dec = dec_info<Dec>::template is_applicable_for_def<TagName, Top, F>;
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

#define FWD_DECLARE_STRUPLE_VAR(tag_name) \
    namespace detail::var {\
        template<class T, class... DefaultValue>\
        requires sizeof...(DefaultValue) <= 1 && (std::is_invocable_r_v<T, DefaultValue> && ...)\
        struct tag_name ## _impl_;\
    }

#define DECLARE_STRUPLE_VAR(tag_name) \
    FWD_DECLARE_STRUPLE_VAR(tag_name)\
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
        template<class T, auto... DefaultValue>\
        using tag_name = detail::var::tag_name ## _impl_<T, std::conditional_t<std::is_invocable_v<decltype(DefaultValue)>,\
                                                                          decltype(DefaultValue),\
                                                                          decltype([]{ return DefaultValue; })>...>;\
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
        template<Clazz C>\
        requires meta_clazz_t<C>::template has_symbol<detail::var::tag_name ## _impl_<T, DefaultValue...>>\
        static constexpr decltype(auto) invoke(C&& c) {\
            if constexpr (std::is_lvalue_reference_v<C>) return (c.tag_name);\
            else return std::move(c.tag_name);\
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
    };
    
#define FWD_DECLARE_STRUPLE_VAL(tag_name) \
    namespace detail::val {\
        template<class T, class Value>\
        requires std::is_invocable_r_v<T, Value>\
        struct tag_name ## _impl_;\
    }

#define DECLARE_STRUPLE_VAL(tag_name) \
    FWD_DECLARE_STRUPLE_VAL(tag_name)\
    namespace detail::val {\
        template<class T, class Value>\
        requires std::is_invocable_r_v<T, Value>\
        struct tag_name ## _impl_ {\
            static constexpr T tag_name = Value{}();\
        };\
    }\
    namespace val {\
        template<class T, auto Value>\
        using tag_name = detail::val::tag_name ## _impl_<T, std::conditional_t<std::is_invocable_v<decltype(Value)>,\
                                                                               decltype(Value),\
                                                                               decltype([]{ return Value; })>>;\
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
    };

#define DECLARE_STRUPLE_TPE(tag_name) \
    namespace detail::tpe {\
        template<class T>\
        struct tag_name ## _impl_ {\
            using tag_name = T;\
        };\
    }\
    namespace tpe {\
        template<class T>\
        using tag_name = detail::tpe::tag_name ## _impl_<T>;\
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
    };

#define FWD_DECLARE_STRUPLE_DEF(tag_name) \
    namespace detail::def {\
        template<class Top, class Sig, class... F>\
        struct tag_name ## _impl_;\
    }

#define DECLARE_STRUPLE_DEF(tag_name) \
    FWD_DECLARE_STRUPLE_DEF(tag_name)\
    namespace detail::def {\
        template<class Top, class R, class... Args, class... F>\
        struct tag_name ## _impl_<Top, R(Args...) const, F...> : private F... {\
            constexpr tag_name ## _impl_() {\
                static_assert((std::is_invocable_r_v<R, F, const Top&, Args...> || ...),\
                              "No implementations of def::"#tag_name" are reachable given the call signature.");\
            }\
            template<class... Ts>\
            requires std::is_invocable_v<R(Args...), Ts&&...> && (std::is_invocable_r_v<R, F, const Top&, Ts&&...> || ...)\
            inline constexpr R tag_name(Ts&&... in) const {\
                return (*this)(static_cast<const Top&>(*this), std::forward<Ts>(in)...);\
            }\
            private:\
                using F::operator()...;\
        };\
        template<class Top, class R, class... Args, class... F>\
        struct tag_name ## _impl_<Top, R(Args...), F...> : private F... {\
            constexpr tag_name ## _impl_() {\
                static_assert((std::is_invocable_r_v<R, F, Top&, Args...> || ...),\
                              "No implementations of def::"#tag_name" are reachable given the call signature.");\
            }\
            template<class... Ts>\
            requires std::is_invocable_v<R(Args...), Ts&&...> && (std::is_invocable_r_v<R, F, Top&, Ts&&...> || ...)\
            inline constexpr R tag_name(Ts&&... in) {\
                return (*this)(static_cast<Top&>(*this), std::forward<Ts>(in)...);\
            }\
            private:\
                using F::operator()...;\
        };\
        template<class Top, class F>\
        struct tag_name ## _impl_<Top, void, F> : private F {\
            template<class... Ts>\
            inline constexpr decltype(auto) tag_name(Ts&&... in) {\
                return (*this)(static_cast<Top&>(*this), std::forward<Ts>(in)...);\
            }\
            private:\
                using F::operator();\
        };\
        template<class Top, class F>\
        struct tag_name ## _impl_<Top, const void, F> : private F {\
            template<class... Ts>\
            inline constexpr decltype(auto) tag_name(Ts&&... in) const {\
                return (*this)(static_cast<const Top&>(*this), std::forward<Ts>(in)...);\
            }\
            private:\
                using F::operator();\
        };\
    }\
    namespace def {\
        template<class Sig, auto... F>\
        requires (CallSignature<Sig> && sizeof...(F) > 0)\
              || (std::is_same_v<const Sig, const void> && sizeof...(F) == 1)\
        using tag_name = detail::def::tag_name ## _impl_<int, Sig, decltype(F)...>;\
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
        requires meta_clazz_t<C>::template has_symbol<symbol_t>\
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
        requires meta_clazz_t<C>::template has_symbol<symbol_t>\
        static constexpr decltype(auto) invoke(C&& c, Ts&&... ins) {\
            return c.tag_name(std::forward<Ts...>(ins)...);\
        }\
    }

#define DECLARE_STRUPLE_FUN(tag_name) \
    namespace detail::fun {\
        template<CallSignatureConst Sig, class... F>\
        struct tag_name ## _impl_;\
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
        template<CallSignature Sig, auto... F>\
        using tag_name = detail::fun::tag_name ## _impl_<typename call_signature<Sig>::as_const, decltype(F)...>;\
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
    };

#define DECLARE_STRUPLE_OVL(tag_name) \
    namespace detail::ovl {\
        template<MethodNamed<tag::tag_name>... F>\
        struct tag_name ## _impl_ : F... {\
            using F::tag_name...;\
        };\
    }\
    namespace ovl {\
        template<MethodNamed<tag::tag_name>... methods>\
        using tag_name = detail::ovl::tag_name ## _impl_<methods...>;\
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
        requires meta_clazz_t<C>::template has_symbol<symbol_t>\
        static constexpr decltype(auto) invoke(C&& c, Ts&&... ins) {\
            return c.tag_name(std::forward<Ts...>(ins)...);\
        }\
    };

#define DECLARE_STRUPLE_SYMBOL(tag_name) \
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
DECLARE_STRUPLE_SYMBOL(operator_points_to); // a->b
DECLARE_STRUPLE_SYMBOL(operator_deref); // *a
DECLARE_STRUPLE_SYMBOL(operator_address_of); // &a
DECLARE_STRUPLE_SYMBOL(operator_square_brackets); // a[b]

// Arithemtic
DECLARE_STRUPLE_SYMBOL(operator_plus); // a + b
DECLARE_STRUPLE_SYMBOL(operator_minus); // a - b
DECLARE_STRUPLE_SYMBOL(operator_mul) // a * b
DECLARE_STRUPLE_SYMBOL(operator_div) // a / b
DECLARE_STRUPLE_SYMBOL(operator_mod) // a % b
DECLARE_STRUPLE_SYMBOL(operator_bitand) // a & b
DECLARE_STRUPLE_SYMBOL(operator_bitor) // a | b
DECLARE_STRUPLE_SYMBOL(operator_xor) // a ^ b
DECLARE_STRUPLE_SYMBOL(operator_compl) // a ~ b
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
DECLARE_STRUPLE_SYMBOL(operator_spaceship) // a <=> b

// Assignment
DECLARE_STRUPLE_SYMBOL(operator_assign) // a = b
DECLARE_STRUPLE_SYMBOL(operator_minus_assign) // a -= b
DECLARE_STRUPLE_SYMBOL(operator_plus_assign) // a += b
DECLARE_STRUPLE_SYMBOL(operator_mul_assign) // a *= b
DECLARE_STRUPLE_SYMBOL(operator_div_assign) // a /= b
DECLARE_STRUPLE_SYMBOL(operator_mod_assign) // a %= b
DECLARE_STRUPLE_SYMBOL(operator_lshift_assign) // a <<= b
DECLARE_STRUPLE_SYMBOL(operator_rshift_assign) // a >>= b
DECLARE_STRUPLE_SYMBOL(operator_bitand_assign) // a &= b
DECLARE_STRUPLE_SYMBOL(operator_bitor_assign) // a |= b
DECLARE_STRUPLE_SYMBOL(operator_xor_assign) // a ^= b

// Other
DECLARE_STRUPLE_SYMBOL(operator_type); // (type)a
DECLARE_STRUPLE_SYMBOL(operator_parentheses); // a(b)
DECLARE_STRUPLE_SYMBOL(operator_comma); // a,b
DECLARE_STRUPLE_SYMBOL(operator_dtor); // a.~a()

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
    using symbol_t = typename meta_clazz_t<C>::template index_symbol_t<I>;
    return symbol_info<symbol_t>::invoke();
}

// Get variable by tag
template<Tag tag, Clazz C>
requires meta_clazz_t<C>::template has_tag<tag> 
      && Variable<typename meta_clazz_t<C>::template tag_symbol_t<tag>>
constexpr inline decltype(auto) get(C&& c) noexcept {
    using symbol_t = typename meta_clazz_t<C>::template tag_symbol_t<tag>;
    return symbol_info<symbol_t>::invoke(std::forward<C>(c));
}

// Get static value by tag
template<Tag tag, Clazz C>
requires meta_clazz_t<C>::template has_tag<tag> 
      && StaticValue<typename meta_clazz_t<C>::template tag_symbol_t<tag>>
constexpr inline auto get(C&& c) noexcept {
    using symbol_t = typename meta_clazz_t<C>::template tag_symbol_t<tag>;
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
requires sizeof...(Tags) > 1 && (meta_clazz_t<C>::template has_tag<Tags> && ...)
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
requires !meta_clazz_t<C>::template has_tag<tag> 
constexpr inline auto get_or(C&&, T&& in) noexcept {
    return std::forward<T>(in);
}

template<Tag tag, Clazz C, class T>
requires meta_clazz_t<C>::template has_tag<tag> 
      && Value<typename meta_clazz_t<C>::template tag_symbol_t<tag>>
constexpr inline decltype(auto) get_or(C&& c, T&&) noexcept {
    return get<tag>(std::forward<C>(c));
}

template<Symbol... X>
struct symbols_info {
    static constexpr size_t size = sizeof...(X);

    template<Symbol S>
    static constexpr bool has_symbol = (symbol_info<X>::template shares_dec<S> || ...);
    
    template<class Struct>
    static constexpr bool importable = ((EmptySymbol<X> || tag_info<symbol_tag_t<X>>::template has_mem_var<std::decay_t<Struct>>) && ...);

    template<Declaration D>
    static constexpr bool has_dec = (symbol_info<X>::template has_dec<D> || ...);
    
    template<Declaration... Decs>
    static constexpr bool implements = (has_dec<Decs> && ...);
    
    template<Symbol S>
    static constexpr bool has_dec_of = (symbol_info<X>::template shares_dec<S> || ...);

    template<Symbol... S>
    static constexpr bool implements_of = (has_dec_of<S> && ...);

    template<Declaration D>
    static constexpr bool has_co_dec = (symbol_info<X>::template has_wider_dec<D> || ...);
    
    template<Declaration... Decs>
    static constexpr bool co_implements = (has_co_dec<Decs> && ...);

    template<Symbol S>
    static constexpr bool has_co_dec_of = (symbol_info<X>::template shares_wider_dec<S> || ...);

    template<Symbol... S>
    static constexpr bool co_implements_of = (has_co_dec_of<S> && ...);

    template<Declaration D>
    static constexpr bool has_contra_dec = (symbol_info<X>::template has_narrower_dec<D> || ...);
    
    template<Declaration... Decs>
    static constexpr bool contra_implements = (has_contra_dec<Decs> && ...);

    template<Symbol S>
    static constexpr bool has_contra_dec_of = (symbol_info<X>::template shares_narrower_dec<S> || ...);

    template<Symbol... S>
    static constexpr bool contra_implements_of = (has_contra_dec_of<S> && ...);

    template<Tag tag>
    using tag_symbol_t = std::tuple_element_t<index_of<tag, X...>::value, std::tuple<X...>>;

    template<size_t I>
    using index_symbol_t = std::tuple_element_t<I, std::tuple<X...>>;

    template<size_t I>
    using index_element_t = typename symbol_info<index_symbol_t<I>>::value_t;

    template<size_t I>
    using index_tag_t = symbol_tag_t<index_symbol_t<I>>;

    template<Tag tag>
    static constexpr size_t tag_index = index_of<tag, X...>::value;

    using tags_info_t = tags_info<symbol_tag_t<X>...>;

    template<class... Tags>
    using super_meta_clazz_t = meta_clazz<tag_symbol_t<Tags>...>;

    using pod_info_t = bind_to_t<symbols_info, flatten_tuples_t<assign_tuple_t<symbol_tuple_t<X>, X>...>>;
    static constexpr bool is_pod = (DataSymbol<X> && ...);

    using values_info_t = bind_to_t<symbols_info, flatten_tuples_t<std::conditional_t<Value<X>, std::tuple<X>, std::tuple<>>...>>;
    static constexpr bool is_values = (Value<X> && ...);

    template<Tag tag>
    static constexpr bool has_tag = (symbol_info<X>::template has_name<tag> || ...);

    template<Tag tag>
    static constexpr bool is_clazz_var = Variable<tag_symbol_t<tag>> && Clazz<std::tuple_element_t<0, symbol_tuple_t<tag_symbol_t<tag>>>>;
};

template<Symbol... X>
struct meta_clazz : symbols_info<X...> {
    using clazz_t = clazz<X...>;
    using tuple_t = typename clazz_t::tuple_t;
    using meta_clazz_t = meta_clazz;

    static constexpr size_t size = sizeof...(X);
    
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

    template<Symbol S>
    static constexpr bool has_symbol = (symbol_info<X>::template shares_dec<S> || ...);
    
    template<class Struct>
    static constexpr bool importable = ((EmptySymbol<X> || tag_info<symbol_tag_t<X>>::template has_mem_var<std::decay_t<Struct>>) && ...);

    template<class Struct>
    requires !Clazz<Struct> && importable<Struct>
    static constexpr clazz_t import(Struct&& in) noexcept {
        return make_clazz(indices_of_vars_t<X...>{}, std::forward<Struct>(in));
    }

    template<class Struct>
    static constexpr bool partially_importable = ((EmptySymbol<X> || tag_info<symbol_tag_t<X>>::template has_mem_var<std::decay_t<Struct>> || symbol_info<X>::has_default_ctor) && ...);

    template<class Struct>
    requires !Clazz<Struct> && partially_importable<Struct>
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
        return clazz_t(symbol_info<index_symbol_t<I>>::extract_compatible_value(std::forward<Struct>(in))...);
    }

    template<size_t... I, class Struct>
    static constexpr auto make_clazz_with_defaults(std::index_sequence<I...>, Struct&& in) noexcept {
        using extractible_tuple_t = flatten_tuples_t<std::conditional_t<tag_info<index_tag_t<I>>::template has_mem_var<std::decay_t<Struct>>, std::tuple<std::index_sequence<I>>, std::tuple<>>...>;
        using extractible_indices_t = bind_to_t<flatten_indices_t, extractible_tuple_t>;
        return make_clazz_with_defaults_2(extractible_indices_t{}, std::forward<Struct>(in));
    }

    template<size_t... I, class Struct>
    static constexpr auto make_clazz_with_defaults_2(std::index_sequence<I...>, Struct&& in) noexcept {
        return clazz_t(tag_info<index_tag_t<I>>::set_placeholder = symbol_info<index_symbol_t<I>>::extract_compatible_value(std::forward<Struct>(in))...);
    }

    template<size_t... I, class Struct>
    static constexpr auto make_partial_clazz(std::index_sequence<I...>, Struct&& in) noexcept {
        using extractible_symbols_t = flatten_tuples_t<std::conditional_t<tag_info<symbol_tag_t<index_symbol_t<I>>>::template has_mem_var<std::decay_t<Struct>>, std::tuple<index_symbol_t<I>>, std::tuple<>>...>;
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

    template<Declaration D>
    static constexpr bool has_dec = (symbol_info<X>::template has_dec<D> || ...);
    
    template<Declaration... Decs>
    static constexpr bool implements = (has_dec<Decs> && ...);
    
    template<Symbol S>
    static constexpr bool has_dec_of = (symbol_info<X>::template shares_dec<S> || ...);

    template<Symbol... S>
    static constexpr bool implements_of = (has_dec_of<S> && ...);

    template<Declaration D>
    static constexpr bool has_co_dec = (symbol_info<X>::template has_wider_dec<D> || ...);
    
    template<Declaration... Decs>
    static constexpr bool co_implements = (has_co_dec<Decs> && ...);

    template<Symbol S>
    static constexpr bool has_co_dec_of = (symbol_info<X>::template shares_wider_dec<S> || ...);

    template<Symbol... S>
    static constexpr bool co_implements_of = (has_co_dec_of<S> && ...);

    template<Declaration D>
    static constexpr bool has_contra_dec = (symbol_info<X>::template has_narrower_dec<D> || ...);
    
    template<Declaration... Decs>
    static constexpr bool contra_implements = (has_contra_dec<Decs> && ...);

    template<Symbol S>
    static constexpr bool has_contra_dec_of = (symbol_info<X>::template shares_narrower_dec<S> || ...);

    template<Symbol... S>
    static constexpr bool contra_implements_of = (has_contra_dec_of<S> && ...);

    template<Tag tag>
    using tag_symbol_t = std::tuple_element_t<index_of<tag, X...>::value, std::tuple<X...>>;

    template<size_t I>
    using index_symbol_t = std::tuple_element_t<I, std::tuple<X...>>;

    template<size_t I>
    using index_element_t = typename symbol_info<index_symbol_t<I>>::value_t;

    template<size_t I>
    using index_tag_t = symbol_tag_t<index_symbol_t<I>>;

    template<Tag tag>
    static constexpr size_t tag_index = index_of<tag, X...>::value;

    using tags_info_t = tags_info<symbol_tag_t<X>...>;

    template<class... Tags>
    using super_meta_clazz_t = meta_clazz<tag_symbol_t<Tags>...>;

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
        return ::get<I>(fields);
    }

    template<size_t I>
    constexpr inline decltype(auto) get() const & {
        return ::get<I>(fields);
    }

    template<size_t I>
    constexpr inline decltype(auto) get() && {
        return ::get<I>(std::move(fields));
    }

    template<Tag tag>
    static constexpr bool has_tag = (symbol_info<X>::template has_name<tag> || ...);

    // Get variable by tag
    template<Tag tag>
    constexpr inline decltype(auto) get() & noexcept {
        return ::get<tag>(fields);
    }

    template<Tag tag>
    constexpr inline decltype(auto) get() const & noexcept {
        return ::get<tag>(fields);
    }

    template<Tag tag>
    constexpr inline decltype(auto) get() && noexcept {
        return ::get<tag>(std::move(fields));
    }

    // Get static value by tag
    template<Tag tag>
    constexpr inline auto get() const noexcept {
        return ::get<tag>(fields);
    }

    // Try get values by tag
    template<Tag tag, class T>
    requires !has_tag<tag>
    constexpr inline auto get_or(T&& in) const noexcept {
        return std::forward<T>(in);
    }

    template<Tag tag, class T>
    requires has_tag<tag> && Value<tag_symbol_t<tag>>
    constexpr inline decltype(auto) get_or(T&&) & noexcept {
        return get<tag>();
    }

    template<Tag tag, class T>
    requires has_tag<tag> && Value<tag_symbol_t<tag>>
    constexpr inline decltype(auto) get_or(T&&) const & noexcept {
        return get<tag>();
    }

    template<Tag tag, class T>
    requires has_tag<tag> && Value<tag_symbol_t<tag>>
    constexpr inline decltype(auto) get_or(T&&) && noexcept {
        return get<tag>();
    }

    template<Tag tag>
    static constexpr bool is_clazz_var = Variable<tag_symbol_t<tag>> && Clazz<std::tuple_element_t<0, symbol_tuple_t<tag_symbol_t<tag>>>>;

    // Matching
    template<class F>
    constexpr inline void for_each_as_var(F&& f) {
        ([&] { if constexpr (Variable<X>)
            for_each_as_var<symbol_tag_t<X>>(std::forward<F>(f));
        }(), ...);
    }

    template<class... Tags, class F>
    requires sizeof...(Tags) > 0 && (Variable<tag_symbol_t<Tags>> && ...)
    constexpr inline void for_each_as_var(F&& f) {
        (std::invoke(std::forward<F>(f), union_cast<tag_symbol_t<Tags>&>(get<Tags>())), ...);
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
    requires sizeof...(Tags) > 0 && (Value<tag_symbol_t<Tags>> && ...)
    constexpr inline void for_each(F&& f) {
        (std::invoke(std::forward<F>(f), get<Tags>()), ...);
    }

    template<class... tag>
    constexpr inline clazz<tag_symbol_t<tag>...> super_clazz() const & {
        return *this;
    }

    template<class... tag>
    constexpr inline clazz<tag_symbol_t<tag>...> super_clazz() && {
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

/* Objective less than comparison between two clazzes which share at least one value field
 * Field order for the comparison operation is defined by the order of the fields in the clazz:
 * (1) which has less value fields, or else the clazz:
 * (2) which has the names of the shared value fields in a lower lexicographical order
 *
 * When values of the respective shared value fields are equal between two instances, the instance
 * of the clazz with fewer value fields is seen as less than the other.
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
 * For operation between two clazzes, LHS and RHS, with the value fields named: LHS{e, b, a, d} and RHS{e, a, b, c} 
 * (2) lhs < rhs => RHS defines order of comparison, i.e. field e compared first then a, then b
 *                  c and d are disregarded, as it is mutually exclusive between two clazzes
 * Why: LHS and RHS have the same number of value fields
 *  BUT LHS and RHS share fields named {a, b, e}
 *  AND the order of shared value fields on RHS are lexicographically less than the order of shared value fields on RHS
 *  i.e. ["e", "a", "b"] < ["e", "b", "a"]
 * If lhs.e == rhs.e && lhs.a == rhs.a && lhs.b == rhs.b, then lhs < rhs returns false 
 * Why: LHS and RHS have the same number of value fields (in this case, lhs == rhs)
 */
template<Clazz L, Clazz R>
constexpr bool strong_less_than(const L& l, const R& r) {
    constexpr auto lsize = meta_clazz_t<L>::meta_values_t::size;
    constexpr auto rsize = meta_clazz_t<L>::meta_values_t::size;
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

namespace std {
    template<size_t I, Clazz C>
    constexpr inline decltype(auto) get(C&& c) {
        return ::get<I>(std::forward<C>(c));
    }

    template<Clazz C>
    struct tuple_size<C> : std::integral_constant<size_t, meta_values_t<C>::size> {};

    template<size_t N, Clazz C>
    struct tuple_element<N, C> {
        using type = typename meta_values_t<C>::template index_element_t<N>;
    };

    template<Clazz C>
    struct hash<C> {
        size_t operator()(const C& c) const {
            return ::hash(c);
        }
    };
}

template<Symbol... X>
struct clazz : struple<clazz<X...>, X...> {
    using struple_t = struple<clazz<X...>, X...>;
    using tuple_t = flatten_tuples_t<symbol_tuple_t<X>...>;
    using meta_clazz_t = meta_clazz<X...>;
    
    static_assert(sizeof(struple_t) == sizeof(tuple_t));
    
    static_assert(assert_unique_symbol_names<X...>::value, "Fields names are not unique.");
    
    // Listing variables
    template<class... Ts>
    constexpr clazz(Ts&&... in) noexcept : struple_t(std::forward<Ts>(in)...) {}
    
    // Construct with a struct with the necessary fields of the same name
    template<class Struct>
    requires !Clazz<Struct> // Is not a class
          && (tag_info<symbol_tag_t<X>>::template has_mem_var<std::decay_t<Struct>> || ...) // Has at least one matching field
          && ((tag_info<symbol_tag_t<X>>::template has_mem_var<std::decay_t<Struct>>
            || symbol_info<X>::has_default_ctor) && ...) // Has matching fields were no default ctors are defined
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
    requires std::tuple_size<std::decay_t<T>>::value == symbols_info<X...>::pod_info_t::size
    constexpr clazz(T&& tuple) noexcept : clazz{std::make_from_tuple<clazz>(std::forward<T>(tuple))} {}

    constexpr inline auto operator->() {
        if constexpr (meta_clazz_t::template has_co_dec<dec::operator_points_to<void()>>)
            return this->operator_points_to();
        else 
            return &meta_clazz_of(*this);
    }

    constexpr inline auto operator->() const {
        if constexpr (meta_clazz_t::template has_co_dec<dec::operator_points_to<void() const>>)
            return this->operator_points_to();
        else
            return &meta_clazz_of(*this);
    }

    // TODO: Find way of making destructor conditionally non-trival for constexpr purposes
    // ~clazz() noexcept {
    //     if constexpr (meta_clazz_t::template has_dec<dec::operator_dtor<void()>>)
    //         this->operator_dtor();
    // }

    template<Clazz T>
    constexpr inline auto operator==(const T& r) const {
        if constexpr (meta_clazz_t::template has_co_dec<dec::operator_eq<void(const T&)>>)
            return this->operator_eq(r);
        else
            return strong_equals(*this, r);
    }

    template<Clazz T>
    constexpr inline auto operator!=(const T& r) const {
        if constexpr (meta_clazz_t::template has_co_dec<dec::operator_not_eq<void(const T&)>>)
            return this->operator_not_eq(r);
        else
            return !strong_equals(*this, r);
    }

    template<Clazz T>
    constexpr inline auto operator<(const T& r) const {
        if constexpr (meta_clazz_t::template has_co_dec<dec::operator_less<void(const T&)>>)
            return this->operator_less(r);
        else
            return strong_less_than(*this, r);
    }

    template<Clazz T>
    constexpr inline auto operator>=(const T& r) const {
        if constexpr (meta_clazz_t::template has_co_dec<dec::operator_greater_eq<void(const T&)>>)
            return this->operator_greater_eq(r);
        else
            return !strong_less_than(*this, r);
    }

    template<Clazz T>
    constexpr inline auto operator>(const T& r) const {
        if constexpr (meta_clazz_t::template has_co_dec<dec::operator_greater<void(const T&)>>)
            return this->operator_greater(r);
        else
            return strong_less_than(r, *this);
    }

    template<Clazz T>
    constexpr inline auto operator<=(const T& r) const {
        if constexpr (meta_clazz_t::template has_co_dec<dec::operator_less_eq<void(const T&)>>)
            return this->operator_less_eq(r);
        else
            return !strong_less_than(r, *this);
    }

// Have to use macros instead of template variables unfortunately
// as we don't want to pollute top-level clazz interface
#define HAS_CO_DEC(dec) (symbol_info<X>::template has_wider_dec<dec> || ...)

#undef HAS_CO_DEC

};

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
            typename tag_info<symbol_tag_t<X>>::template var_t<std::add_lvalue_reference_t<const_t<typename symbol_info<X>::value_t>>>,
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

DECLARE_STRUPLE_SYMBOL(variant_index);

template<Declaration... Ds>
struct trait {
    template<Clazz C>
    static constexpr bool co_implementor = meta_clazz_t<C>::template co_implements<Ds...>;

    template<CoImplements<Ds...>... Variants>
    using variant_clazz_t = clazz < 
        var :: variant_index <unsigned char const>,
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
class vvector {
    size_t write_head = 0;
    std::vector<char> buffer;
    std::vector<size_t> positions;

public:
    template<class T>
    void push_back(T&& value) {
        emplace_back<std::decay_t<T>>(std::forward<T>(value));
    }
    
    template<class T, class... Ts>
    requires is_one_of<T, Variants...>::value
    void emplace_back(Ts&&... args) {
        using variant_t = variant<T>;
        constexpr size_t value_size = sizeof(variant_t);
        constexpr size_t index = index_of_type<T, Variants...>::value;

        positions.push_back(write_head);
        buffer.reserve(write_head + value_size);
        new (&buffer[write_head]) variant_t(index, std::forward<Ts>(args)...);
        write_head += value_size;
    }
    
    using value_type = typename Trt::template variant_clazz_t<Variants...>;
    using reference = value_type&;
    using const_reference = const value_type&;

    reference at(size_t pos) {
        return reinterpret_cast<reference>(buffer[positions.at(pos)]);
    }
    const_reference at(size_t pos) const {
        return reinterpret_cast<const_reference>(buffer[positions.at(pos)]);
    }
    reference operator[](size_t pos) {
        return reinterpret_cast<reference>(buffer[positions[pos]]);
    }
    const_reference operator[](size_t pos) const {
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
    size_t size() const noexcept {
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
    static constexpr size_t var_sizes[] = { sizeof(variant<Variants>)... };

    template<bool Const>
    class _iterator {
        using value_type = vvector::value_type;
        using reference = value_type&;
        using pointer = value_type*;

        using container_t = std::conditional_t<Const, const vvector, vvector>;
        using value_t = std::conditional_t<Const, const value_type, value_type>;

        friend container_t;

        size_t pos = 0;
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
    using const_interator = _iterator<true>;

    inline iterator begin() {
        return {0, *this};
    }
    inline const_interator cbegin() const {
        return {0, *this};
    }
    inline iterator end() {
        return {write_head, *this};
    }
    inline const_interator cend() const {
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
                i.variant_index, &i, visitor, out);
        }
        return out; 
    }

public:
    void reserve(size_t elements) {
        constexpr auto max = vmax(0, sizeof(variant<Variants>)...);
        buffer.reserve(max * elements);
        positions.reserve(elements);
    }

    vvector() {
        reserve(16);
    }

    void clear() {
        // Destruct all variants
        for (auto& i : *this)
            detail::visit_clazz_variant<void, Variants...>(
                std::index_sequence_for<Variants...>{},
                i.variant_index, &i,
                []<class T>(T& self) { self.~T(); });

        buffer.clear();
        positions.clear();
    }

    ~vvector() {
        clear();
    }
};

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

DECLARE_STRUPLE_SYMBOL(eat);
DECLARE_STRUPLE_SYMBOL(cats);
DECLARE_STRUPLE_SYMBOL(dogs);
DECLARE_STRUPLE_SYMBOL(cows);


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
    def::operator_points_to<void() const, [](auto& ths) {
        return &ths;
    }>
>::clazz_t;

static_assert(std::is_same_v<var::_1<int>, meta_clazz_t<type>::tag_symbol_t<tag::_1>>);

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

static_assert(meta_clazz_t<type>::has_dec<dec::_1<int>>);
static_assert(meta_clazz_t<type>::has_dec<dec::_2<double>>);
static_assert(meta_clazz_t<type>::has_dec<dec::_3<int>>);
static_assert(meta_clazz_t<type>::has_dec<dec::_4<int() const>>);
static_assert(meta_clazz_t<type>::has_dec<dec::_5<int(int) const>>);
static_assert(meta_clazz_t<type>::has_dec<dec::_6<int(int) const>>);
static_assert(meta_clazz_t<type>::has_dec<dec::_7<int()>>);
static_assert(meta_clazz_t<type>::has_dec<dec::_8<int(int) const>>);
static_assert(meta_clazz_t<type>::has_dec<dec::_9<int() const>>);

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
    //, def :: operator_points_to<void, [](auto& self) { return &self; }> 
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

DECLARE_STRUPLE_SYMBOL(print);
DECLARE_STRUPLE_SYMBOL(x);
DECLARE_STRUPLE_SYMBOL(y);
DECLARE_STRUPLE_SYMBOL(z);
DECLARE_STRUPLE_SYMBOL(name);
DECLARE_STRUPLE_SYMBOL(age);


using n_trait = trait<dec::_10<int()>>;

int main(int argc, char** argv) {
    if (!trivial_tuple_test()) {
        std::terminate();
        return 0;
    }

    // return hash(clazz{set::_1 = 1});

    auto tup = std::tuple{1, 2};
    auto nup = nuple(std::tuple{1,2});
    using nupc_t = clazz<var::_2<int>, var::_3<int>>;
    auto nupc = nupc_t(tup);
    view_t<nupc_t> nupcv = nupc;
    return nupcv._2 + nupcv._3;
    auto& nupc2 = as_named_tuple<nuple>(tup);
    return nupc2._1 + nupc2._2;

    auto c = clazz<var::_1<int, 1>, val::_2<int, 20>, val::_3<int, 300>>{};
    auto t = tie<tag::_1, tag::_2>(c);
    auto& [first, second] = t;
    return first + second;
    
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
    
    constexpr bool compare = clazz{set::_1 = 0.9, set::_2 = 2, set::_4 = 0} < clazz<var::_2<int, 2>, val::_1<int, 1>>{};
    return compare;

    auto testCompInst = testComp{set::_1 ="", set::_2(2, 'h')};
    return testCompInst._2.length();
    return testCompInst->call(ttc);
    struct{const char* _1 = ""; std::string _2 = std::string(2, 'h');} in;
    return ttc(in);
    return ttc(ttc_in{._1 = "hi", ._2 = "no"});

    struct {
        int _3 = 12;
        int _1 = 9;
        int _2 = 10;
    } hi;
 
    auto inst = make_clazz<tag::_1, tag::_2, tag::_3>(hi);
    auto inst2 = clazz<var::_3<int>, var::_1<int>, var::_4<int, 3>>(hi);
    auto inst3 = meta_clazz<var::_3<int>, var::_1<int>, var::_4<int, 3>>::import_with_defaults(hi);
    return inst3._3 + inst3._1 + inst3._4;

    return retT(clazz<var::_1<const char*>>{"sashi"});
    return retT(clazz{set::_1 = "sashi"});
    return retT({set::_1 = "sashi"});

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
    F._2 = 10;
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

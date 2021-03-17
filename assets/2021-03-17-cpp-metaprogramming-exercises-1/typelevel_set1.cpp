// https://www.slamecka.cz/posts/2021-03-17-cpp-metaprogramming-exercises-1/
//
// This is a set of exercises for type level programming in C++, suitable for
// beginners or as a refresher for intermediate programmers.
//
// Scope:
// - Most exercises in this set are confined to vectors of integers for simplicity.
// - Exercises are centered around basic functional techniques since they are easy to express with template metaprogramming.
//
// How to solve it:
// - You should be able to compile with gcc version 10.2, g++ --std=c++20 typelevel_set1.cpp
// - You should understand the requested behaviours from static_assert-s,
//   except for print, which comes with a test in main().
// - Tests are commented-out and you should uncomment them as you go.
// - You might want to read some introductory material before attempting these:
//
//   https://en.cppreference.com/w/cpp/language/templates
//
//   https://en.cppreference.com/w/cpp/language/parameter_pack
//
//   https://en.cppreference.com/w/cpp/language/template_parameters
//      in this set we'll only see non-type template parameters and type template parameters
//
//   https://en.cppreference.com/w/cpp/header/type_traits
//
//   https://en.cppreference.com/w/cpp/language/partial_specialization
//      also see https://stackoverflow.com/questions/17005985/what-is-the-partial-ordering-procedure-in-template-deduction
//

#include <iostream>
#include <type_traits>

namespace {

/**
 * 1. Define Vector, a template level list of integers.
 * Hint: Use non-type template parameter pack.
 */

// Your code goes here:
template <int...>
struct Vector
{};
// ^ Your code goes here

static_assert(std::is_same_v<Vector<1,2>, Vector<1,2>>);


/**
 * 2. Define function print() that prints Vector-s.
 * Example: print(Vector<1,2,3>{}); // prints "1 2 3" (newline)
 * See main() below.
 */

// Your code goes here:
template <int... T>
void print(Vector<T...>) // empty vector case
{
    std::cout << std::endl;
}

template <int H>
void print(Vector<H>)
{
    // this case exists to avoid the extra space at the end
    std::cout << H << std::endl;
}

template <int H, int ...T>
void print(Vector<H, T...>)
{
    std::cout << H << " ";
    print(Vector<T...>{});
}
// ^ Your code goes here


/**
 * 3. Define Prepend.
 * Hint: Use `using type = ...` inside a struct that has both non-type and type template parameters.
 */

// Your code goes here:
template<int H, typename V>
struct Prepend;

template<int H, int ...T>
struct Prepend<H, Vector<T...>>
{
    using type = Vector<H, T...>;
};
// ^ Your code goes here

static_assert(std::is_same_v<Prepend<1, Vector<2,3>>::type, Vector<1,2,3>>);


/**
 * 4. Define PrependT that can be used without ::type.
 * Hint: See how enable_if_t is defined in terms of enable_if.
 *
 * This technique is not used further to reduce boilerplate.
 */

// Your code goes here:
template<int H, typename V>
using PrependT = Prepend<H, V>::type;
// ^ Your code goes here

static_assert(std::is_same_v<PrependT<1, Vector<2,3>>, Vector<1,2,3>>);


/**
 * 5. Define Append.
 */

// Your code goes here:
template <int, typename>
struct Append;

template <int E, int H, int... T>
struct Append<E, Vector<H, T...>>
{
    using type = typename Prepend<H, typename Append<E, Vector<T...>>::type>::type;
};

template <int E, int... T>
struct Append<E, Vector<T...>>
{
    using type = Vector<E>;
};
// ^ Your code goes here

static_assert(std::is_same_v< Append<4, Vector<1,2,3>>::type , Vector<1,2,3,4> >);


/**
 * 6. Define PopBack.
 */

// Your code goes here:
template<typename>
struct PopBack;

template <int H, int HH, int... T>
struct PopBack<Vector<H, HH, T...>>
{
    using type = Prepend<H, typename PopBack<Vector<HH, T...>>::type>::type;
};

template <int H, int... T>
struct PopBack<Vector<H, T...>>
{
    using type = Vector<T...>;
};
// ^ Your code goes here

static_assert(std::is_same_v< PopBack<Vector<1,2,3,4>>::type , Vector<1,2,3> >);


/**
 * 7. Define RemoveFirst, that removes the first occurence of element R from vector V.
 */

// Your code goes here:
template<int R, typename V>
struct RemoveFirst;

template<int R, int... T>
struct RemoveFirst<R, Vector<R, T...>> // head is matching
{
    using type = Vector<T...>;
};

template<int R, int H, int... T>
struct RemoveFirst<R, Vector<H, T...>> // head is not matching
{
    using type = typename Prepend<H, typename RemoveFirst<R, Vector<T...>>::type>::type;
};

template<int R, int... T>
struct RemoveFirst<R, Vector<T...>> // handle empty case
{
    using type = Vector<T...>;
};
// ^ Your code goes here

static_assert(std::is_same_v<RemoveFirst<1, Vector<1,1,2>>::type, Vector<1,2>>);


/**
 * 8. Define RemoveAll, that removes all occurences of element R from vector V.
 */

// Your code goes here:
template<int R, typename V>
struct RemoveAll;

template<int R, int... T>
struct RemoveAll<R, Vector<R, T...>> // head is matching, remove in the rest
{
    using type = typename RemoveAll<R, Vector<T...>>::type;
};

template<int R, int H, int... T>
struct RemoveAll<R, Vector<H, T...>> // head is not matching
{
    using type = typename Prepend<H, typename RemoveAll<R, Vector<T...>>::type>::type;
};

template<int R, int... T>
struct RemoveAll<R, Vector<T...>> // handle empty case
{
    using type = Vector<T...>;
};
// ^ Your code goes here

static_assert(std::is_same_v<RemoveAll<9, Vector<1,9,2,9,3,9>>::type, Vector<1,2,3>>);


/**
 * 9. Define Length.
 * Hint: Use `static constexpr int value = ...` inside the struct.
 */

// Your code goes here:
template <typename V>
struct Length;

template<int H, int... T>
struct Length<Vector<H, T...>>
{
    static constexpr int value = 1 + Length<Vector<T...>>::value;
};

template<int... T>
struct Length<Vector<T...>>
{
    static constexpr int value = 0;
};
// ^ Your code goes here

static_assert(Length<Vector<1,2,3>>::value == 3);


/**
 * 10. Define length, which works like Length<V>::value.
 * Hint: See how is_same_v is defined in terms of is_same.
 */

// Your code goes here:
template<typename V>
inline constexpr std::size_t length = Length<V>::value;
// ^ Your code goes here

static_assert(length<Vector<>> == 0);
static_assert(length<Vector<1,2,3>> == 3);


/**
 * 11. Define Min, that stores the minimum of a vector in its property `value`.
 */

// Your code goes here:
template<int M, typename V>
struct _Min;

template<int M, int H, int... T>
struct _Min<M, Vector<H, T...>>
{
    static constexpr int value = _Min<M < H ? M : H, Vector<T...>>::value;
};

template<int M, int... T>
struct _Min<M, Vector<T...>>
{
    static constexpr int value = M;
};

template<typename V>
struct Min;

template<int H, int... T>
struct Min<Vector<H, T...>>
{
    static constexpr int value = _Min<H, Vector<T...>>::value;
};
// ^ Your code goes here

static_assert(Min<Vector<3,1,2>>::value == 1);
static_assert(Min<Vector<1,2,3>>::value == 1);
static_assert(Min<Vector<3,2,1>>::value == 1);


/**
 * 12. Define Sort.
 */

// Your code goes here:
template<typename Prefix, typename Rest>
struct _Sort;

template<int... Prefix, int H, int... T>
struct _Sort<Vector<Prefix...>, Vector<H, T...>>
{
    static constexpr int m = Min<Vector<H, T...>>::value;
    using rest = RemoveFirst<m, Vector<H, T...>>::type;
    using type = _Sort< typename Append<m, Vector<Prefix...>>::type , rest >::type;
};

template<int... Prefix, int... T>
struct _Sort<Vector<Prefix...>, Vector<T...>>
{
    using type = Vector<Prefix...>;
};

template<typename>
struct Sort;

template<int... T>
struct Sort<Vector<T...>>
{
    using type = _Sort<Vector<>, Vector<T...>>::type;
};
// ^ Your code goes here

static_assert(std::is_same_v<Sort<Vector<4,1,2,5,6,3>>::type, Vector<1,2,3,4,5,6>>);
static_assert(std::is_same_v<Sort<Vector<3,3,1,1,2,2>>::type, Vector<1,1,2,2,3,3>>);
static_assert(std::is_same_v<Sort<Vector<2,2,1,1,3,3>>::type, Vector<1,1,2,2,3,3>>);


/**
 * 13. Define Uniq.
 */

// Your code goes here:
template<typename V>
struct Uniq;

template <int H, int... T>
struct Uniq<Vector<H, H, T...>> // remove duplicates
{
    using type = typename Uniq<Vector<H, T...>>::type;
};

template <int H, int... T>
struct Uniq<Vector<H, T...>>
{
    using type = typename Prepend<H, typename Uniq<Vector<T...>>::type>::type;
};

template <int... T>
struct Uniq<Vector<T...>>
{
    using type = Vector<T...>;
};
// ^ Your code goes here

static_assert(std::is_same_v<Uniq<Vector<1,1,2,2,1,1>>::type, Vector<1,2,1>>);


/**
 * 14. Define type Set.
 */

// Your code goes here:
template<int... T>
struct Set
{
    using type = typename Uniq<typename Sort<Vector<T...>>::type>::type;
};
// ^ Your code goes here

static_assert(std::is_same_v<Set<2,1,3,1,2,3>::type, Set<1,2,3>::type>);


/**
 * 15. Define SetFrom.
 */

// Your code goes here:
template<typename V>
struct SetFrom;

template<int... T>
struct SetFrom<Vector<T...>>
{
    using type = typename Set<T...>::type;
};
// ^ Your code goes here

static_assert(std::is_same_v<SetFrom<Vector<2,1,3,1,2,3>>::type, Set<1,2,3>::type>);


/**
 * 16. Define Get for Vector.
 * Provide an improved error message when accessing outside of Vector bounds.
 */

// Your code goes here:
template<int N, typename V>
struct Get;

template<int H, int... T>
struct Get<0, Vector<H, T...>>
{
    static constexpr int value = H;
};

template<int N, int H, int... T>
struct Get<N, Vector<H, T...>>
{
    static_assert(N > 0, "Access out of bounds.");
    static constexpr int value = Get<N-1, Vector<T...>>::value;
};

template<int N, int... T>
struct Get<N, Vector<T...>>
{
    static_assert(sizeof...(T) != 0, "Access out of bounds.");
};
// ^ Your code goes here

static_assert(Get<0, Vector<0,1,2>>::value == 0);
static_assert(Get<1, Vector<0,1,2>>::value == 1);
static_assert(Get<2, Vector<0,1,2>>::value == 2);
// static_assert(Get<9, Vector<0,1,2>>::value == 2); // How good is your error message?


/**
 * 17. Define BisectLeft for Vector.
 * Given n and arr, return the first index i such that arr[i] >= n.
 * If it doesn't exist, return the length of the array.
 *
 * Don't worry about complexity but aim for the binary search pattern.
 *
 * Hint: You might find it convenient to define a constexpr helper function.
 */

// Your code goes here:
template<int L, int R, int N, int... T>
static constexpr int bisectLeftLR(Vector<T...> haystack)
{
    if constexpr (L >= R)
    {
        return L;
    }
    else
    {
        constexpr int Mid = (L + R) / 2;
        if constexpr (Get<Mid, decltype(haystack)>::value < N)
        {
            return bisectLeftLR<Mid+1, R, N>(haystack);
        }
        else
        {
            return bisectLeftLR<L, Mid, N>(haystack);
        }
    }
};

template<int N, typename V>
struct BisectLeft;

template<int N, int... T>
struct BisectLeft<N, Vector<T...>>
{
    static constexpr int value = bisectLeftLR<0, sizeof...(T), N>(Vector<T...>{});
};
// ^ Your code goes here

static_assert(BisectLeft<  3, Vector<0,1,2,3,4>>::value == 3);
static_assert(BisectLeft<  3, Vector<0,1,2,4,5>>::value == 3);
static_assert(BisectLeft<  9, Vector<0,1,2,4,5>>::value == 5);
static_assert(BisectLeft< -1, Vector<0,1,2,4,5>>::value == 0);
static_assert(BisectLeft<  2, Vector<0,2,2,2,2,2>>::value == 1);


/**
 * 18. Define Insert for Vector, it should take position, value and vector.
 * Don't worry about bounds.
 * Hint: use the SFINAE Enable trick, e.g.
 *   template<typename X, typename Enable = void> struct Foo;
 *   template<typename X> struct<std::enable_if_t<..something      about X..>> Foo {...};
 *   template<typename X> struct<std::enable_if_t<..something else about X..>> Foo {...};
 */

// Your code goes here:
template<int Pos, int Val, typename V, typename Enable = void>
struct Insert;

template<int Pos, int Val, int... T>
struct Insert<Pos, Val, Vector<T...>, typename std::enable_if_t<Pos == 0>>
{
    using type = Vector<Val, T...>;
};

template<int Pos, int Val, int H, int... T>
struct Insert<Pos, Val, Vector<H, T...>, typename std::enable_if_t<(Pos > 0)>>
{
    using type = typename Prepend<H, typename Insert<Pos-1, Val, Vector<T...>>::type>::type;
};
// ^ Your code goes here

static_assert(std::is_same_v<Insert<0, 3, Vector<4,5,6>>::type, Vector<3,4,5,6>>);
static_assert(std::is_same_v<Insert<1, 3, Vector<4,5,6>>::type, Vector<4,3,5,6>>);
static_assert(std::is_same_v<Insert<2, 3, Vector<4,5,6>>::type, Vector<4,5,3,6>>);
static_assert(std::is_same_v<Insert<3, 3, Vector<4,5,6>>::type, Vector<4,5,6,3>>);

}

int main()
{
    print(Vector<>{});
    print(Vector<1>{});
    print(Vector<1,2,3,4,5,6>{});
    std::cout << typeid(Vector<1,2,3,4,5,6>{}).name() << '\n';
}

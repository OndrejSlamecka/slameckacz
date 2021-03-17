---
title: C++ Metaprogramming Exercises Vol. I
---

Below is a set of exercises for type level programming in C++, suitable for newcomers to the topic but hopefully
entertaining even for an intermediate template programmer. I made this when I decided to dedicate an evening to
improving my fluency in the use of templates but couldn't easily find such exercises elsewhere.


.. code-block:: c++

    // https://www.slamecka.cz/posts/2021-03-17-cpp-metaprogramming-exercises-1/
    //
    // This is a set of exercises for type level programming in C++, suitable for
    // newcomers to the topic but hopefully entertaining even for an intermediate
    // template programmer.
    //
    // Scope:
    // - For simplicity most exercises in this set are confined to vectors of integers.
    // - Exercises are centered around basic topics from functional programming.
    //
    // How to solve it:
    // - You should be able to compile with gcc version 10.2, g++ --std=c++20 typelevel_set1.cpp
    // - You should understand the requested behaviours from static_assert-s,
    //   except for print, which comes with a test in main().
    // - Tests are commented-out and you should uncomment them as you go.
    // - You might want to read some introductory material before attempting these
    //   but the key complicated part is in the order of template deduction which is
    //   explained well in this StackOverflow answer
    //   https://stackoverflow.com/questions/17005985/what-is-the-partial-ordering-procedure-in-template-deduction
    //   and a lot of knowledge is in the cppreference.com pages on type_traits
    //   https://en.cppreference.com/w/cpp/header/type_traits

    #include <iostream>
    #include <type_traits>

    namespace {

    /**
     * 1. Define Vector, a template level list of integers.
     */

    // Your code goes here:
    // ^ Your code goes here

    // static_assert(std::is_same_v<Vector<1,2>, Vector<1,2>>);


    /**
     * 2. Define function print() that prints Vector-s.
     * Example: print(Vector<1,2,3>{}); // prints "1 2 3" (newline)
     * See main() below.
     */

    // Your code goes here:
    // ^ Your code goes here


    /**
     * 3. Define Prepend.
     * Hint: Use `using type = ...` inside a templated struct.
     */

    // Your code goes here:
    // ^ Your code goes here

    // static_assert(std::is_same_v<Prepend<1, Vector<2,3>>::type, Vector<1,2,3>>);


    /**
     * 4. Define PrependT that can be used without ::type.
     * Hint: See how enable_if_t is defined in terms of enable_if.
     *
     * This technique is not used further to reduce boilerplate.
     */

    // Your code goes here:
    // ^ Your code goes here

    // static_assert(std::is_same_v<PrependT<1, Vector<2,3>>, Vector<1,2,3>>);


    /**
     * 5. Define Append.
     */

    // Your code goes here:
    // ^ Your code goes here

    // static_assert(std::is_same_v< Append<4, Vector<1,2,3>>::type , Vector<1,2,3,4> >);


    /**
     * 6. Define PopBack.
     */

    // Your code goes here:
    // ^ Your code goes here

    // static_assert(std::is_same_v< PopBack<Vector<1,2,3,4>>::type , Vector<1,2,3> >);


    /**
     * 7. Define RemoveFirst, that removes the first occurence of element R from vector V.
     */

    // Your code goes here:
    // ^ Your code goes here

    // static_assert(std::is_same_v<RemoveFirst<1, Vector<1,1,2>>::type, Vector<1,2>>);


    /**
     * 8. Define RemoveAll, that removes all occurences of element R from vector V.
     */

    // Your code goes here:
    // ^ Your code goes here

    // static_assert(std::is_same_v<RemoveAll<9, Vector<1,9,2,9,3,9>>::type, Vector<1,2,3>>);


    /**
     * 9. Define Length.
     * Hint: Use `static constexpr int value = ...` inside the struct.
     */

    // Your code goes here:
    // ^ Your code goes here

    // static_assert(Length<Vector<1,2,3>>::value == 3);


    /**
     * 10. Define length, which works like Length<V>::value.
     * Hint: See how is_same_v is defined in terms of is_same.
     */

    // Your code goes here:
    // ^ Your code goes here

    // static_assert(length<Vector<>> == 0);
    // static_assert(length<Vector<1,2,3>> == 3);


    /**
     * 11. Define Min, that stores the minimum of a vector in its property `value`.
     */

    // Your code goes here:
    // ^ Your code goes here

    // static_assert(Min<Vector<3,1,2>>::value == 1);
    // static_assert(Min<Vector<1,2,3>>::value == 1);
    // static_assert(Min<Vector<3,2,1>>::value == 1);


    /**
     * 12. Define Sort.
     */

    // Your code goes here:
    // ^ Your code goes here

    // static_assert(std::is_same_v<Sort<Vector<4,1,2,5,6,3>>::type, Vector<1,2,3,4,5,6>>);
    // static_assert(std::is_same_v<Sort<Vector<3,3,1,1,2,2>>::type, Vector<1,1,2,2,3,3>>);
    // static_assert(std::is_same_v<Sort<Vector<2,2,1,1,3,3>>::type, Vector<1,1,2,2,3,3>>);


    /**
     * 13. Define Uniq.
     */

    // Your code goes here:
    // ^ Your code goes here

    // static_assert(std::is_same_v<Uniq<Vector<1,1,2,2,1,1>>::type, Vector<1,2,1>>);


    /**
     * 14. Define type Set.
     */

    // Your code goes here:
    // ^ Your code goes here

    // static_assert(std::is_same_v<Set<2,1,3,1,2,3>::type, Set<1,2,3>::type>);


    /**
     * 15. Define SetFrom.
     */

    // Your code goes here:
    // ^ Your code goes here

    // static_assert(std::is_same_v<SetFrom<Vector<2,1,3,1,2,3>>::type, Set<1,2,3>::type>);


    /**
     * 16. Define Get for Vector.
     * Provide an improved error message when accessing outside of Vector bounds.
     */

    // Your code goes here:
    // ^ Your code goes here

    // static_assert(Get<0, Vector<0,1,2>>::value == 0);
    // static_assert(Get<1, Vector<0,1,2>>::value == 1);
    // static_assert(Get<2, Vector<0,1,2>>::value == 2);
    // static_assert(Get<9, Vector<0,1,2>>::value == 2); // How good is your error message?


    /**
     * 17. Define BisectLeft for Vector.
     * Given n and arr, return the first index i such that arr[i] >= n.
     * If it doesn't exist, return the length of the array.
     *
     * This implementation should have O(n * log n) complexity due to calling Get
     * O(log n) times. Writing a linear time Find would be better in practice, this
     * is just for exercise.
     *
     * Hint: You might find it convenient to define a constexpr helper function.
     */

    // Your code goes here:
    // ^ Your code goes here

    // static_assert(BisectLeft<  3, Vector<0,1,2,3,4>>::value == 3);
    // static_assert(BisectLeft<  3, Vector<0,1,2,4,5>>::value == 3);
    // static_assert(BisectLeft<  9, Vector<0,1,2,4,5>>::value == 5);
    // static_assert(BisectLeft< -1, Vector<0,1,2,4,5>>::value == 0);
    // static_assert(BisectLeft<  2, Vector<0,2,2,2,2,2>>::value == 1);


    /**
     * 18. Define Insert for Vector, it should take position, value and vector.
     * Don't worry about bounds.
     * Hint: use the SFINAE Enable trick, e.g.
     *   template<typename X, typename Enable = void> struct Foo;
     *   template<typename X> struct<std::enable_if_t<..something      about X..>> Foo {...};
     *   template<typename X> struct<std::enable_if_t<..something else about X..>> Foo {...};
     */

    // Your code goes here:
    // ^ Your code goes here

    // static_assert(std::is_same_v<Insert<0, 3, Vector<4,5,6>>::type, Vector<3,4,5,6>>);
    // static_assert(std::is_same_v<Insert<1, 3, Vector<4,5,6>>::type, Vector<4,3,5,6>>);
    // static_assert(std::is_same_v<Insert<2, 3, Vector<4,5,6>>::type, Vector<4,5,3,6>>);
    // static_assert(std::is_same_v<Insert<3, 3, Vector<4,5,6>>::type, Vector<4,5,6,3>>);


    /**
     * 19. Provide a Vector instance of the below defined Fmap.
     */

    template<template <int> typename F, typename C>
    struct Fmap;

    template<int X>
    struct PlusOne
    {
        static constexpr int value = X + 1;
    };

    // Your code goes here:
    // ^ Your code goes here

    // static_assert(std::is_same_v<Fmap<PlusOne, Vector<1,2,3>>::type, Vector<2,3,4>>);


    /**
     * 20. Provide an instance of Fmap for Nothing & Just defined below.
     */

    struct Nothing
    {};

    template<int V>
    struct Just
    {};

    // Your code goes here:
    // ^ Your code goes here

    // static_assert(std::is_same_v<Fmap<PlusOne, Just<1>>::type, Just<2>>);
    // static_assert(std::is_same_v<Fmap<PlusOne, Nothing>::type, Nothing>);


    /**
     * 21. Define type Pair.
     */

    // Your code goes here:
    // ^ Your code goes here

    // static_assert(std::is_same_v<Pair<1,2>, Pair<1,2>>);


    /**
     * 22. Define type PairVector.
     * Hint: Take arbitrary type as its template parameter.
     */

    // Your code goes here:
    // ^ Your code goes here

    // static_assert(std::is_same_v<PairVector<Pair<1,2>>, PairVector<Pair<1,2>>>);


    /**
     * 23. Define PairVectorPrepend.
     */

    // Your code goes here:
    // ^ Your code goes here

    // static_assert(std::is_same_v< PairVectorPrepend<Pair<1,2>, PairVector<Pair<3,4>>>::type, PairVector<Pair<1,2>, Pair<3,4>> >);


    /**
     * 24. Define Zip.
     */

    // Your code goes here:
    // ^ Your code goes here

    // static_assert(std::is_same_v< Zip<Vector<1,3,5>, Vector<2,4,6>>::type, PairVector<Pair<1,2>, Pair<3,4>, Pair<5,6>> >);


    // Zip concludes this set of introductory exercises. You'll notice that the in the last few exercises the
    // integer constraint has been lifted and we could use any type. What we called PairVector is actually
    // a container suitable for any types. This allows for heterogenous containers, much more useful Fmap and
    // new, more complicated but useful functions. Perhaps a topic of a future, more advanced set of exercises.
    }

    int main()
    {
    //     print(Vector<>{});
    //     print(Vector<1>{});
    //     print(Vector<1,2,3,4,5,6>{});
    //     std::cout << typeid(Vector<1,2,3,4,5,6>{}).name() << '\n';
    }


You can have a look at `my solution</assets/2021-03-17-cpp-metaprogramming-exercises-1/typelevel_set1.cpp>`_,
which is provided with no guarantees.

In order to generate the exercise file from the solved file I used the script below.

.. code-block:: sh

    cat typelevel_set1.cpp | awk -v incode=1 '{ if ($0 ~ "// \\\^?.?Your"){ incode = !incode; if (!incode) { print $0 } }; if (incode) { print $0 } }' | awk -v inmain=0 '{ if (inmain == 1 && $0 != "{" && $0 != "}") { printf "// %s\n",$0; } else { print $0 }; if ($0 == "int main()") { inmain = 1 }; }' | awk '{ if ($0 ~ "^static_assert") { printf "// %s\n",$0; } else { print $0; } }'

//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef _LIBCPP___ALGORITHM_SHUFFLE_H
#define _LIBCPP___ALGORITHM_SHUFFLE_H

#include <__config>
#include <__iterator/iterator_traits.h>
#include <__random/uniform_int_distribution.h>
#include <__utility/swap.h>
#include <cstddef>
#include <cstdint>

#if !defined(_LIBCPP_HAS_NO_PRAGMA_SYSTEM_HEADER)
#pragma GCC system_header
#endif

_LIBCPP_PUSH_MACROS
#include <__undef_macros>

_LIBCPP_BEGIN_NAMESPACE_STD

class _LIBCPP_TYPE_VIS __libcpp_debug_randomizer {
public:
  __libcpp_debug_randomizer() {
    __state = __seed();
    __inc = __state + 0xda3e39cb94b95bdbULL;
    __inc = (__inc << 1) | 1;
  }
  typedef uint_fast32_t result_type;

  static const result_type _Min = 0;
  static const result_type _Max = 0xFFFFFFFF;

  _LIBCPP_HIDE_FROM_ABI result_type operator()() {
    uint_fast64_t __oldstate = __state;
    __state = __oldstate * 6364136223846793005ULL + __inc;
    return __oldstate >> 32;
  }

  static _LIBCPP_HIDE_FROM_ABI _LIBCPP_CONSTEXPR result_type min() { return _Min; }
  static _LIBCPP_HIDE_FROM_ABI _LIBCPP_CONSTEXPR result_type max() { return _Max; }

private:
  uint_fast64_t __state;
  uint_fast64_t __inc;
#if !defined(_LIBCPP_DEBUG_RANDOMIZE_UNSPECIFIED_STABILITY_SEED)
  static constant char __x;
#endif
  _LIBCPP_HIDE_FROM_ABI static uint_fast64_t __seed() {
#ifdef _LIBCPP_DEBUG_RANDOMIZE_UNSPECIFIED_STABILITY_SEED
    return _LIBCPP_DEBUG_RANDOMIZE_UNSPECIFIED_STABILITY_SEED;
#else
    return reinterpret_cast<uintptr_t>(&__x);
#endif
  }
};

#if 0 // this is not supported
#if _LIBCPP_STD_VER <= 14 || defined(_LIBCPP_ENABLE_CXX17_REMOVED_RANDOM_SHUFFLE) \
  || defined(_LIBCPP_BUILDING_LIBRARY)
class _LIBCPP_TYPE_VIS __rs_default;

_LIBCPP_FUNC_VIS __rs_default __rs_get();

class _LIBCPP_TYPE_VIS __rs_default
{
    static unsigned __c_;

    __rs_default();
public:
    typedef uint_fast32_t result_type;

    static const result_type _Min = 0;
    static const result_type _Max = 0xFFFFFFFF;

    __rs_default(const __rs_default&);
    ~__rs_default();

    result_type operator()();

    static _LIBCPP_CONSTEXPR result_type min() {return _Min;}
    static _LIBCPP_CONSTEXPR result_type max() {return _Max;}

    friend _LIBCPP_FUNC_VIS __rs_default __rs_get();
};

_LIBCPP_FUNC_VIS __rs_default __rs_get();

template <class _RandomAccessIterator>
_LIBCPP_DEPRECATED_IN_CXX14 void
random_shuffle(_RandomAccessIterator __first, _RandomAccessIterator __last)
{
    typedef typename iterator_traits<_RandomAccessIterator>::difference_type difference_type;
    typedef uniform_int_distribution<ptrdiff_t> _Dp;
    typedef typename _Dp::param_type _Pp;
    difference_type __d = __last - __first;
    if (__d > 1)
    {
        _Dp __uid;
        __rs_default __g = __rs_get();
        for (--__last, (void) --__d; __first < __last; ++__first, (void) --__d)
        {
            difference_type __i = __uid(__g, _Pp(0, __d));
            if (__i != difference_type(0))
                swap(*__first, *(__first + __i));
        }
    }
}

template <class _RandomAccessIterator, class _RandomNumberGenerator>
_LIBCPP_DEPRECATED_IN_CXX14 void
random_shuffle(_RandomAccessIterator __first, _RandomAccessIterator __last,
#ifndef _LIBCPP_CXX03_LANG
               _RandomNumberGenerator&& __rand)
#else
               _RandomNumberGenerator& __rand)
#endif
{
    typedef typename iterator_traits<_RandomAccessIterator>::difference_type difference_type;
    difference_type __d = __last - __first;
    if (__d > 1)
    {
        for (--__last; __first < __last; ++__first, (void) --__d)
        {
            difference_type __i = __rand(__d);
            if (__i != difference_type(0))
              swap(*__first, *(__first + __i));
        }
    }
}
#endif
#endif

template<class _RandomAccessIterator, class _UniformRandomNumberGenerator>
    void shuffle(_RandomAccessIterator __first, _RandomAccessIterator __last,
                 _UniformRandomNumberGenerator&& __g)
{
    typedef typename iterator_traits<_RandomAccessIterator>::difference_type difference_type;
    typedef uniform_int_distribution<ptrdiff_t> _Dp;
    typedef typename _Dp::param_type _Pp;
    difference_type __d = __last - __first;
    if (__d > 1)
    {
        _Dp __uid;
        for (--__last, (void) --__d; __first < __last; ++__first, (void) --__d)
        {
            difference_type __i = __uid(__g, _Pp(0, __d));
            if (__i != difference_type(0))
                swap(*__first, *(__first + __i));
        }
    }
}

_LIBCPP_END_NAMESPACE_STD

_LIBCPP_POP_MACROS

#endif // _LIBCPP___ALGORITHM_SHUFFLE_H

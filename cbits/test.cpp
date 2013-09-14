/*
 * =====================================================================================
 *
 *       Filename:  test.cpp
 *
 *    Description:  
 *
 *        Version:  1.0
 *        Created:  09/13/13 14:41:18
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  YOUR NAME (), 
 *   Organization:  
 *
 * =====================================================================================
 */
extern "C" {
#include <stdlib.h>
#include "test.hpp"

int fib(int i) {

    return i <= 1 ? i : fib(i - 1) + fib (i - 2);

}

}

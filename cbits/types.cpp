/*
 * =====================================================================================
 *
 *       Filename:  types.hpp
 *
 *    Description:  Interface file for types not automatically wrapped
 *
 *        Version:  1.0
 *        Created:  09/24/13 11:54:11
 *       Revision:  none
 *       Compiler:  g++
 *
 *         Author:  Arjun Comar 
 *
 * =====================================================================================
 */

#include <opencv_generated.hpp>
#include "types.hpp"

extern "C" {
string* std_create_string(char* s, int len) {

    return new string(*s, len);

}

vector_int* std_create_vector_int(int* is, size_t len) {

    return new vector_int(is, is + len);

}

Mat* cv_create_Mat() {

   return new Mat(); 

}
}

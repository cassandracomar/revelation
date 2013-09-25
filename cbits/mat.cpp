/*
 * =====================================================================================
 *
 *       Filename:  mat.cpp
 *
 *    Description:  
 *
 *        Version:  1.0
 *        Created:  09/24/13 20:12:17
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  YOUR NAME (), 
 *   Organization:  
 *
 * =====================================================================================
 */
#include <opencv_generated.hpp>
#include <mat.hpp>

extern "C" {
Mat* cv_create_Mat() {

    return new Mat();

}
}

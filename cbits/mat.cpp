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

Mat* cv_Mat_assign(Mat* self, Mat* m) {
    *self = *m;
    return self;
}

Mat* cv_Mat_assignVal(Mat* self, Scalar* s) {
    *self = *s;
    return self;
}

Mat* cv_Mat_getRow(Mat* self, int y) {
    return new Mat(self->row(y));
}

Mat* cv_Mat_getCol(Mat* self, int x) {
    return new Mat(self->col(x));
}

Mat* cv_Mat_getRowRange(Mat* self, int startrow, int endrow) {
    return new Mat(self->rowRange(startrow, endrow));
}
Mat* cv_Mat_getColRange(Mat* self, int startcol, int endrow) {
    return new Mat(self->colRange(startcol, endrow));
}
Mat* cv_Mat_diag(Mat* self) {
    return new Mat(self->diag());
}

Mat* cv_Mat_diag_d(Mat* self, int d) {
    return new Mat(self->diag(d));
}

Mat* cv_create_diagMat(Mat* d) {
    return new Mat(Mat::diag(*d));
}

Mat* cv_Mat_clone(Mat* self) {
    return new Mat(self->clone());
}

void cv_Mat_copyTo(Mat* self, Mat* m) {
    self->copyTo(*m);
}

void cv_Mat_copyTo_masked(Mat* self, Mat* m, Mat* mask) {
    self->copyTo(*m, *mask);
}

void cv_Mat_assignTo(Mat* self, Mat* m) {
    self->assignTo(*m);
}

void cv_Mat_assignTo_t(Mat*self, Mat* m, int t) {
    self->assignTo(*m, t);
}

Mat* cv_Mat_setTo(Mat* self, Scalar* value) {
    Mat* m = new Mat;
    *m = *value;
    self->setTo(*m);
}

Mat* cv_Mat_setTo_masked(Mat* self, Scalar* value, Mat* mask) {
    Mat* m = new Mat;
    *m = *value;
    self->setTo(*m, *mask);
}

Mat* cv_Mat_reshape(Mat* self, int cn) {
   return new Mat(self->reshape(cn)); 
}

Mat* cv_Mat_reshape_rows(Mat* self, int cn, int rows) {
   return new Mat(self->reshape(cn, rows)); 
}
}
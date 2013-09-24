cflags = $(shell pkg-config --cflags opencv)
Main: Revelation/Main.hs opencv-generated.o types.o
	
	ghc -v -Icbits/ --make Revelation/Main.hs Revelation/Bindings/Types.hsc -pgml g++ opencv-generated.o types.o -L/usr/lib -L/usr/local/lib /usr/local/lib/libopencv_bioinspired.so /usr/local/lib/libopencv_calib3d.so /usr/local/lib/libopencv_contrib.so /usr/local/lib/libopencv_core.so /usr/local/lib/libopencv_features2d.so /usr/local/lib/libopencv_flann.so /usr/local/lib/libopencv_gpu.so /usr/local/lib/libopencv_gpuarithm.so /usr/local/lib/libopencv_gpubgsegm.so /usr/local/lib/libopencv_gpucodec.so /usr/local/lib/libopencv_gpufeatures2d.so /usr/local/lib/libopencv_gpufilters.so /usr/local/lib/libopencv_gpuimgproc.so /usr/local/lib/libopencv_gpuoptflow.so /usr/local/lib/libopencv_gpustereo.so /usr/local/lib/libopencv_gpuwarping.so /usr/local/lib/libopencv_highgui.so /usr/local/lib/libopencv_imgproc.so /usr/local/lib/libopencv_legacy.so /usr/local/lib/libopencv_ml.so /usr/local/lib/libopencv_nonfree.so /usr/local/lib/libopencv_objdetect.so /usr/local/lib/libopencv_optim.so /usr/local/lib/libopencv_photo.so /usr/local/lib/libopencv_softcascade.so /usr/local/lib/libopencv_stitching.so /usr/local/lib/libopencv_superres.so /usr/local/lib/libopencv_ts.a /usr/local/lib/libopencv_video.so /usr/local/lib/libopencv_videostab.so

opencv-generated.o: cbits/opencv_generated.cpp cbits/opencv_generated.hpp
	g++ $(cflags) -c -o opencv-generated.o cbits/opencv_generated.cpp 

types.o: cbits/types.cpp cbits/types.hpp cbits/opencv_generated.hpp
	g++ -Icbits/ $(cflags) -c -o types.o cbits/types.cpp

cbits/opencv_generated.cpp: cbits/genc.py
	python cbits/genc.py cbits/

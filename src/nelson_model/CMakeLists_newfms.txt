
CMAKE_MINIMUM_REQUIRED(VERSION 2.6)
PROJECT(newfms)


SET(SOURCE
    newfms.cpp
    computeFuelMoisture.cpp
)

SET(HEADERS
    newfms.h
)
	
ADD_EXECUTABLE(compute_fms ${SOURCE})

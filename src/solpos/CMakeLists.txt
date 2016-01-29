
CMAKE_MINIMUM_REQUIRED(VERSION 2.6)
PROJECT(solpos)


SET(SOURCE
    solpos.cpp
    computeSolar.cpp
)

SET(HEADERS
    solpos00.h
)
	
ADD_EXECUTABLE(compute_solar ${SOURCE})

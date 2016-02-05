
CMAKE_MINIMUM_REQUIRED(VERSION 2.6)
PROJECT(dfm)


SET(SOURCE
    semtime.cpp
    deadfuelmoisture.cpp
    computeFuelMoisture.cpp
)

SET(HEADERS
    semtime.h
    deadfuelmoisture.h
)
	
ADD_EXECUTABLE(compute_dfm ${SOURCE})

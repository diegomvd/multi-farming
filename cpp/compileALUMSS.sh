#!/bin/bash

c++ mainALUMSS.cpp functionsALUMSS.cpp -o alumss-exec -lgsl -lgslcblas -lm -Wall -Weffc++ --std=c++17 -lstdc++fs

# c++ mainALUMSS.cpp functionsALUMSS.cpp -o alumss-exec-moran -lgsl -lgslcblas -lm -Wall -Weffc++ --std=c++17 -lstdc++fs

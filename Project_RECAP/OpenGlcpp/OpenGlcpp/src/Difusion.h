#pragma once
// basic Libs
#include <stdio.h>
#include <stdlib.h>
#include <iostream>
// Math Libs
#define _USE_MATH_DEFINES // M_PI constant
#include <math.h>
#include <vector>
// Useful defines
#define log(x) std::cout << x << std::endl
//#define read(x) std::cin >> read
//int read;




class Simulation
{
public:
	Simulation();
	~Simulation();


	const int N = 31;                                                        // Number of cells (x = 22 , y = 22)
	const double L = 4.0;                                                    // Length of the simulation square domain (meters)
	const double ds = (double)L / (N - 1);                                   // Cells length
	const double alpha = (double) 97.0;                                      // Thermal condutivity (aluminium)
	const double cfl = 1.0;                                                  // valor de convergencia
	double dt = pow(L / (400 - 1), 2) / (4 * alpha);                         // time step length (seconds)
	const double tempo = 5.0;                                                // Time (seconds)
	const double u = 5.0;                                                    // X velocity (m/s)
	const double v = 5.0;                                                    // Y velocity (m/s)
	const double incremento = (double)1 * pow(10, -9);                       // Imcrement for the implicit method

};

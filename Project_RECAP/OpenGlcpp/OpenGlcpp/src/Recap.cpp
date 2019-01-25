#include<GL/glew.h>
#include<GLFW/glfw3.h>
#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <math.h>
#include <vector>

#define log(x) std::cout << x << std::endl


int main(void)
{





	int tempo;
	double ds, dt, L, alpha, u, v;
	log("Program recap: Routine of termal simplifyed simulation");

	// inicializing constants

	const int N = 100;               // Number of cells
	L = 1.0;                         // Length of the simulation square domain (meters)
	ds = L / (N - 1);                // Cells length
	alpha = 1.0;                     // Thermal condutivity
	dt = pow(ds, 2) / (2 * alpha);   // time step length
	tempo = 1000;                    // Time steps
	u = 1.0;                         // X velocity
	v = 1.0;                         // Y velocity

	// Declaring matrixes
	double T1[N][N];
	double T2[N][N];

	// creating initial conditions for the simulation

	for (int i = 0; i < N; i++)
	{
		for (int ii = 0; ii < N; ii++)
		{
			T1[i][ii] = 0;
		}
	}


	// time iterations 

	for (int j = 0; j < tempo + 1; j++)
	{
		for (int i = 1; i < N - 1; i++)
		{
			for (int ii = 1; ii < N - 1; ii++)
			{

				T2[i][ii] = T1[i][ii] * (1, 0 - 4, 0 * (alpha * dt) / pow(ds, 2) - (u + v)*dt / ds) + (T1[i + 1][ii] + T1[i][ii + 1]) * (alpha * dt / pow(ds, 2))
					+ T1[i - 1][ii] * ((alpha * dt) / pow(ds, 2) + u * dt / ds) + T1[i][ii - 1] * ((alpha * dt) / pow(ds, 2) + v * dt / ds);
			}
		}

		for (int i = 1; i < N - 1; i++)
		{
			for (int ii = 1; ii < N - 1; ii++)
			{

				T1[i][ii] = T2[i][ii];
			}
		}

		std::cout << j << j * dt << std::endl;
	}

	std::cin.get();






}
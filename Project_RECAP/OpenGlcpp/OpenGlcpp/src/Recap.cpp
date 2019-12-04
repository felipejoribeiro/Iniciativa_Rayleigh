#include <stdio.h>
#include <stdlib.h>
#include <iostream>
// Math Libs
#define _USE_MATH_DEFINES // M_PI constant
#include <math.h>
#include <vector>

typedef struct
{
	GLfloat x, y, z;       //position
	GLfloat r, g, b, a;    //color and alpha channels

}Vertex;


double G(double x, double y, double t, double alpha, double u, double v);


int main(void)
{
	// Initialization OPENGL

	const int WINDOWS_WIDTH = 600;
	const int WINDOWS_HEIGHT = 600;

	if (!glfwInit())
		exit(EXIT_FAILURE);

	window = glfwCreateWindow(WINDOWS_WIDTH, WINDOWS_HEIGHT, "Chapter 2: Primitive drawings", NULL, NULL);

	if (!window)
	{
		glfwTerminate();
		exit(EXIT_FAILURE);
	}



	glfwMakeContextCurrent(window);

	glEnable(GL_POINT_SMOOTH);
	glHint(GL_POINT_SMOOTH_HINT, GL_NICEST);
	glEnable(GL_BLEND);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);





	// Program simulation RECAP
	int tempo, j;
	double ds, dt, L, alpha, u, v , Li , L2, m2, cfl, incremento, dtt;
	log("Program recap: Routine of termal simplifyed simulation");

	// inicializing constants

	const int N = 50;                                           // Number of cells (x = 22 , y = 22)
	L = (double) 2 * M_PI;                                      // Length of the simulation square domain (meters)
	ds = (double) L / (N-1);                                    // Cells length
	alpha = (double) 1.0;                                       // Thermal condutivity (aluminium)
	cfl = 0.1;                                                  // valor de convergencia
	dt = cfl * pow(ds,2) / alpha;                               // time step length (seconds)
	tempo = 5;                                                  // Time (seconds)
	u = 0.0;                                                    // X velocity (m/s)
	v = 0.0;                                                    // Y velocity (m/s)
	incremento =  (double) 1 * pow(10, -5);                     // Imcrement for the implicit method


	// Declaring matrixes

	double** T1 = new double*[N];
	for (int i = 0; i < N; ++i)
		T1[i] = new double[N];

	double** T2 = new double*[N];
	for (int i = 0; i < N; ++i)
		T2[i] = new double[N];

	double** T3 = new double*[N];
	for (int i = 0; i < N; ++i)
		T3[i] = new double[N];

	// Initialization OPENGL
	graficos print;

	// creating initial conditions for the simulation numeric bealtiful

	for (int i = 0; i < N; i++)
	{
		for (int ii = 0; ii < N; ii++)
		{
			T1[i][ii] = sin(i * ds)* sin(ii * ds) ;
		}
	}

	// Update window

	print.update_window();
	print.Draw(T1, T2, T3, N);


	// time iterations
	j = 0;
	while(j * dt < tempo - dt)
	{
		// Simulation Explicity...
		//for (int i = 1; i < N - 1; i++)
		//{
		//	for (int ii = 1; ii < N - 1; ii++)
		//	{
		//		T2[i][ii] = T1[i][ii] * (1 - 4 * alpha * dt /(ds * ds)) 
		//            + T1[i + 1][ii] * (alpha * dt / (ds * ds) - u * dt/(2 * ds)) 
		//			+ T1[i][ii + 1] * (alpha * dt /(ds * ds) - v * dt / (2 * ds))
		//			+ T1[i - 1][ii] * ((alpha * dt) / (ds * ds) + u * dt /(2 * ds)) 
		//            + T1[i][ii - 1] * ((alpha * dt) /(ds * ds) + v * dt/(2 * ds));
		//	}
		//}


		// Simulation Implicity...

		// Initial coeficiente zeros
		incremento = 0;
		for (int i = 0; i < N; i++)
		{
			for (int ii = 0; ii < N; ii++)
			{

				T2[i][ii] = 0;
			}
		}
		// Implicit equation developed
		m2 = 10.0;
		while (m2 > incremento)
		{
			m2 = T2[(int)N / 4][(int)N / 4];
			for (int i = 1; i < N - 1; i++)
			{
				for (int ii = 1; ii < N - 1; ii++)
				{
					T2[i][ii] = (T1[i][ii]
						+ T2[i + 1][ii] * (alpha * dt / (ds * ds) - u * dt / (2 * ds))
						+ T2[i][ii + 1] * (alpha * dt / (ds * ds) - v * dt / (2 * ds))
						+ T2[i - 1][ii] * ((alpha * dt) / (ds * ds) + u * dt / (2 * ds))
						+ T2[i][ii - 1] * ((alpha * dt) / (ds * ds) + v * dt / (2 * ds))) /
						(1 + 4 * alpha * dt / (ds * ds));
				}
			}

			for (int i = 0; i < N; i++)
			{
				T2[i][0] = 0.0;
				T2[i][N - 1] = 0.0;
			}

			for (int i = 0; i < N; i++)
			{
				T2[0][i] = 0.0;
				T2[N - 1][i] = 0.0;
			}

			T2[0][0] = 0.0;
			T2[N - 1][N - 1] = 0.0;
			T2[0][N - 1] = 0.0;
			T2[N - 1][0] = 0.0;
			m2 = abs(m2 - T2[(int)N / 4][(int)N / 4]);
		}

	// time iterations

		// modelo numerico:

		for (int i = 0; i < N; i++)
		{
			T2[i][0] = 0.0;
			T2[i][N - 1] = 0.0;
		}
		for (int i = 0; i < N; i++)
		{
			T2[0][i] = 0.0;
			T2[N - 1][i] = 0.0;
		}
		for (int i = 1; i < N - 1; i++)
		{
			for (int ii = 1; ii < N - 1; ii++)
			{

				//T2[i][ii] = T1[i][ii] + (alpha * dt / (ds*ds)) * (T1[i + 1][ii] - 4 * T1[i][ii] + T1[i - 1][ii]
				//	+ T1[i][ii + 1] + T1[i][ii - 1]);

				T2[i][ii] = T1[i][ii] * (1 - 4 * alpha * dt /(ds * ds)) + T1[i + 1][ii] * (alpha * dt / (ds * ds) - u * dt/(2 * ds))
					+ T1[i][ii + 1] * (alpha * dt /(ds * ds) - v * dt / (2 * ds))
					+ T1[i - 1][ii] * ((alpha * dt) / (ds * ds) + u * dt /(2 * ds)) + T1[i][ii - 1] * ((alpha * dt) /(ds * ds) + v * dt/(2 * ds));
			}
		}

		if (j % 10 == 0)
		{
			print.update_window();
			print.Draw(T1, T2, T3, N);
			//std::cin>> j;
			log(j << "|" << T1[N/ 4][N/4]);
		}
		j++;
	}

	// Simulation Explicity...
	
	dtt = tempo - j * dt;

	//for (int i = 1; i < N - 1; i++)
	//{
	//	for (int ii = 1; ii < N - 1; ii++)
	//	{
	//		T2[i][ii] = T1[i][ii] * (1 - 4 * alpha * dtt / (ds * ds))
	//			+ T1[i + 1][ii] * (alpha * dtt / (ds * ds) - u * dtt / (2 * ds))
	//			+ T1[i][ii + 1] * (alpha * dtt / (ds * ds) - v * dtt / (2 * ds))
	//			+ T1[i - 1][ii] * ((alpha * dtt) / (ds * ds) + u * dtt / (2 * ds))
	//			+ T1[i][ii - 1] * ((alpha * dtt) / (ds * ds) + v * dtt / (2 * ds));
	//	}
	//}

	m2 = 10.0;
	while (m2 > incremento)
	{
		m2 = T2[ (int) N / 4][ (int) N / 4];
		for (int i = 1; i < N - 1; i++)
		{
			for (int ii = 1; ii < N - 1; ii++)
			{
				T2[i][ii] = (T1[i][ii]
					+ T2[i + 1][ii] * (alpha * dtt / (ds * ds) - u * dtt / (2 * ds))
					+ T2[i][ii + 1] * (alpha * dtt / (ds * ds) - v * dtt / (2 * ds))
					+ T2[i - 1][ii] * ((alpha * dtt) / (ds * ds) + u * dtt / (2 * ds))
					+ T2[i][ii - 1] * ((alpha * dtt) / (ds * ds) + v * dtt / (2 * ds))) /
					(1 + 4 * alpha * dtt / (ds * ds));
			}
		}

		for (int i = 0; i < N; i++)
		{
			T2[i][0] = 0.0;
			T2[i][N - 1] = 0.0;
		}

		for (int i = 0; i < N; i++)
		{
			T2[0][i] = 0.0;
			T2[N - 1][i] = 0.0;
		}

		T2[0][0] = 0.0;
		T2[N - 1][N - 1] = 0.0;
		T2[0][N - 1] = 0.0;
		T2[N - 1][0] = 0.0;
		m2 = abs(m2 - T2[(int)N / 4][(int)N / 4]);
	}


	// Delete arrays


	for (int i = 0; i < N; i++)
	{
		T2[i][0] = 0.0;
		T2[i][N - 1] = 0.0;
	}
	for (int i = 0; i < N; i++)
	{
		T2[0][i] = 0.0;
		T2[N - 1][i] = 0.0;
	}
	for (int i = 1; i < N - 1; i++)
	{
		for (int ii = 1; ii < N - 1; ii++)
		{
			T1[i][ii] = T2[i][ii];
		}
	}







	// Calculo do erro! Linf !

	Li = 0.0;
	for (int i = 0; i < N; i++)
	{
		for (int ii = 0; ii < N; ii++)
		{
			if (abs(T1[i][ii] - sin(ds * i)*sin(ds * ii)* exp(-2 * tempo)) > Li)
			{
				Li = abs(T1[i][ii] - sin(ds * i)*sin(ds * ii)*exp(-2 * tempo));
			}
		}
	}

	// Calculo do erro! L2 !

	L2 = 0.0;
	for (int i = 0; i < N; i++)
	{
		for (int ii = 0; ii < N; ii++)
		{
			L2 = L2 + pow(T1[i][ii] - sin(ds * i)*sin(ds * ii)*exp(-2 * tempo) , 2);
		}
	}
	
	L2 = sqrt(L2/(N*N) );

	log("Linf error =" << Li << " L2 = " << L2);
	std::cin >> incremento;
	// Delete arrays
	for (int i = 0; i < N; ++i) {
		delete[] T1[i];
	}
	delete[] T1;
	for (int i = 0; i < N; ++i) {
		delete[] T2[i];
	}
	delete[] T2;
	// Terminate grafic interface
	print.~graficos();
}


double G(double x, double y, double t, double alpha, double u, double v) 
{
	double G;
	G = exp(1 - (pow(x, 2) + pow(y, 2) + t))
		* ( 4 * alpha - 2 * u * x - 2 * v * y - 4 * alpha *(pow(x , 2) + pow(y  , 2)) - 1);
	return G;
}
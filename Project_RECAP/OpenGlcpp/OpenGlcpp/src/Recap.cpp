#include<GL/glew.h>
#include<GLFW/glfw3.h>
#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#define _USE_MATH_DEFINES // M_PI constant
#include <math.h>
#include <vector>
#define log(x) std::cout << x << std::endl

typedef struct
{
	GLfloat x, y, z;       //position
	GLfloat r, g, b, a;    //color and alpha channels
}Vertex;


typedef struct
{
	GLdouble x, y, z;
}Data;


void draw2DHeatMap(const Data *data, int num_points);
void Draw(double ** T1, double ** T2, int  N);



GLFWwindow * window;



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
	int tempo;
	double ds, dt, L, alpha, u, v;
	log("Program recap: Routine of termal simplifyed simulation");

	// inicializing constants

	const int N = 60;                                       // Number of cells (x = 22 , y = 22)
	L = (double) 440.0;                                     // Length of the simulation square domain (meters)
	ds = (double) L / (N - 1);                              // Cells length
	alpha = (double) 97.0;                                  // Thermal condutivity (aluminium)
	dt = (double) 1.0 / 500.0; //pow(ds, 2) / (2 * alpha);  // time step length
	tempo = 5000000;                                        // Time steps
	u = 5.0;                                                // X velocity
	v = 5.0;                                               // Y velocity

	// Declaring matrixes

	double** T1 = new double*[N];
	for (int i = 0; i < N; ++i)
		T1[i] = new double[N];

	double** T2 = new double*[N];
	for (int i = 0; i < N; ++i)
		T2[i] = new double[N];

	// creating initial conditions for the simulation

	for (int i = 0; i < N; i++)
	{
		for (int ii = 0; ii < N; ii++)
		{
			T1[i][ii] = 0.0;
		}
	}


	for (int i = N/4; i < N - N/4; i++)
	{
		for (int ii = N/4; ii < N - N / 4; ii++)
		{
			T1[i][ii] = 200.0;
		}
	}



	float ratio;
	int width, height;
	glfwGetFramebufferSize(window, &width, &height);
	ratio = (float)width / (float)height;
	glViewport(0, 0, width, height);
	glClear(GL_COLOR_BUFFER_BIT);
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();

	// Orthographic Projection

	glOrtho(-ratio, ratio, -1.f, 1.f, 1.f, -1.f);
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);


	Draw(T1, T2, N); // Desenha o heatMAP



	// time iterations

	int n;

	for (int j = 0; j < tempo + 1; j++)
	{
		// set up window
		float ratio;
		int width, height;
		glfwGetFramebufferSize(window, &width, &height);
		ratio = (float)width / (float)height;
		glViewport(0, 0, width, height);
		glClear(GL_COLOR_BUFFER_BIT);
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity();

		// Orthographic Projection

		glOrtho(-ratio, ratio, -1.f, 1.f, 1.f, -1.f);
		glMatrixMode(GL_MODELVIEW);
		glLoadIdentity();
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

		// Simulation
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

		for (int i = 0; i < N; i++)
		{
			T1[i][0] = T2[i][1];
			T1[i][N - 1] = T2[i][N - 2];
		}

		for (int i = 0; i < N; i++)
		{
			T1[0][i] = T2[1][i];
			T1[N - 1][i] = T2[N - 2][i];
		}

		T1[0][0] = T2[1][1];
		T1[N-1][N-1] = T2[N-2][N-2];
		T1[0][N-1] = T2[1][N-2];
		T1[N - 1][0] = T2[N - 2][1];


		for (int i = 1; i < N - 1; i++)
		{
			for (int ii = 1; ii < N - 1; ii++)
			{

				T1[i][ii] = T2[i][ii];
			}
		}

		T1[(int)N / 2][(int)N / 2] = 200.0;


		if (j % 100 == 0)
		{
			Draw(T1, T2, N);
			log(j * dt << " | " << T1[0][N / 2] << " | " << T1[N-1][N / 2]);
			//std::cin >> n;
		}

	}


	// Delete arrays

	for (int i = 0; i < N; ++i) {
		delete[] T1[i];
	}
	delete[] T1;

	for (int i = 0; i < N; ++i) {
		delete[] T2[i];
	}
	delete[] T2;


	// Stop the window.
	glfwDestroyWindow(window);
	glfwTerminate();

}


void draw2DHeatMap(const Data *data, int num_points)
{
	//locate the maximum and minimum values in the dataset
	float max_value = -9999.9f;
	float min_value = 9999.9f;
	for (int i = 0; i < num_points; i++)
	{
		const Data d = data[i];
		if (d.z > max_value) {
			max_value = d.z;
		}
		if (d.z < min_value) {
			min_value = d.z;
		}
	}

	max_value = 201,0;
	min_value = -1,0;

	const float halfmax = (max_value + min_value) / 2;

	//display the result
	glPointSize(10.0f);
	glBegin(GL_POINTS);
	for (int i = 0; i < num_points; i++)
	{
		const Data d = data[i];
		float value = d.z;
		float b = 1.0f - value / halfmax;
		float r = value / halfmax - 1.0f;
		if (b < 0)
			b = 0;
		if (r < 0)
			r = 0;
		float g = 1.0f - b - r;
		glColor4f(r, g, b, 0.5f);
		glVertex3f(d.x, d.y, 0.0f);
	}
	glEnd();
}



void Draw(double ** T1, double ** T2 ,int  N)
{
	// Organizing DATA

	const int grid_x = N;
	const int grid_y = N;
	const int num_points = grid_x * grid_y;
	Data *data = (Data*)malloc(sizeof(Data)*num_points);

	int data_counter = 0;
	for (int x = -grid_x / 2; x < grid_x / 2; x += 1)
	{
		for (int y = -grid_y / 2; y < grid_y / 2; y += 1)
		{
			float x_data = 2.0f*x / grid_x;
			float y_data = 2.0f*y / grid_y;
			float z_data = T1[x + grid_x / 2][y + grid_y / 2];

			data[data_counter].x = x_data;
			data[data_counter].y = y_data;
			data[data_counter].z = z_data;
			data_counter++;
		}
	}


	// Drawning
	draw2DHeatMap(data, num_points);
	free(data);

	glfwSwapBuffers(window);
	glfwPollEvents();
	if (glfwWindowShouldClose(window))
	{
		for (int i = 0; i < N; ++i) {
			delete[] T1[i];
		}
		delete[] T1;

		for (int i = 0; i < N; ++i) {
			delete[] T2[i];
		}
		delete[] T2;


		// Stop the window.
		glfwDestroyWindow(window);
		glfwTerminate();
		exit(EXIT_SUCCESS);
	}
}
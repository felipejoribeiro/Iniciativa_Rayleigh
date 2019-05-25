#pragma once
#include<GL/glew.h>
#include<GLFW/glfw3.h>
#include <stdio.h>
#include <stdlib.h>
#include<iostream>
#include <vector>
#define log(x) std::cout << x << std::endl


typedef struct
{
	GLfloat x, y, z;      //position
	GLfloat r, g, b, a;   //color and alpha channels
}Vertex;


typedef struct
{
	GLdouble x, y, z;
}Data;


class graficos
{
public:
	graficos();
	~graficos();
	void create_window(int windowswide, int windowshight, const char string[200]);
	void update_window();
	void Draw(double ** T1, double ** T2, double ** T3, int  N);
	void ddraw2DHeatMap(const Data *data, int num_points);




private:
	GLFWwindow * window;
	int WINDOWS_WIDTH;
	int WINDOWS_HEIGHT;

};


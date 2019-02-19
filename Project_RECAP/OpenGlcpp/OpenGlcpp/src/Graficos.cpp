#include"Graficos.h"


//Constructor, função chamada na criação do objeto
graficos::graficos()
{
	graficos::create_window(600, 600, "janela premiada");
}


// Destructor, função chamada na distruição do objeto
graficos::~graficos()
{
	// Stop the window.
	glfwDestroyWindow(window);
	glfwTerminate();
}

void graficos::update_window() 
{
	// Other update window
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
}



void graficos::create_window(int WINDOWS_WIDTH, int WINDOWS_HEIGHT, const char string[200])
{

	if (!glfwInit())
		exit(EXIT_FAILURE);

	window = glfwCreateWindow(WINDOWS_WIDTH, WINDOWS_HEIGHT, string, NULL, NULL);

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
}

void graficos::Draw(double ** T1, double ** T2, double ** T3, int  N)
{
	glfwMakeContextCurrent(window);
	// Organizing DATA

	const int grid_x = N;
	const int grid_y = N;
	const int num_points = grid_x * grid_y;
	Data *data = (Data*)malloc(sizeof(Data)*num_points);


	if (N % 2 != 0) {
		int data_counter = 0;
		for (int x = -grid_x / 2; x < grid_x / 2 + 1; x += 1)
		{
			for (int y = -grid_y / 2; y < grid_y / 2 + 1; y += 1)
			{
				float x_data = 2.0f*x / grid_x;
				float y_data = 2.0f*y / grid_y;
				float z_data = T1[(int)x + grid_x / 2][(int)y + grid_y / 2];

				data[data_counter].x = x_data;
				data[data_counter].y = y_data;
				data[data_counter].z = z_data;
				data_counter++;
			}
		}
	}
	else {
		int data_counter = 0;
		for (int x = -grid_x / 2; x < grid_x / 2; x += 1)
		{
			for (int y = -grid_y / 2; y < grid_y / 2; y += 1)
			{
				float x_data = 2.0f*x / grid_x;
				float y_data = 2.0f*y / grid_y;
				float z_data = T1[(int)x + grid_x / 2][(int)y + grid_y / 2];

				data[data_counter].x = x_data;
				data[data_counter].y = y_data;
				data[data_counter].z = z_data;
				data_counter++;
			}
		}
	}

	// Drawning
	graficos::ddraw2DHeatMap(data, num_points);
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

		for (int i = 0; i < N; ++i) {
			delete[] T3[i];
		}
		delete[] T3;


		// Stop the window.
		glfwDestroyWindow(window);
		glfwTerminate();
		exit(EXIT_SUCCESS);
	}
}



void graficos::ddraw2DHeatMap(const Data *data, int num_points)
{
	//locate the maximum and minimum values in the dataset
	float max_value = -999999.0f;
	float min_value =  999999.0f;

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

	//log(max_value);


	const float halfmax = (max_value + min_value) / 2;

	//display the result
	glPointSize(10.0f);
	glBegin(GL_POINTS);
	for (int i = 0; i < num_points; i++)
	{
		const Data d = data[i];
		float value = d.z;
		float b = (halfmax - value) / (halfmax - min_value);
		float r = (value - halfmax) / (max_value - halfmax);
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
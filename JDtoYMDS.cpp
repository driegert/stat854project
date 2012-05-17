#include <cstdlib>
#include <stdio.h>

using namespace std;

int main(int argc, char* argv[]){
	float JD = atof(argv[1]);
	int day, hour, min, sec;

	day = JD / 86400;
	JD -= (86400 * day);
	hour = JD / 3600;
	JD -= (3600 * hour);
	min = JD / 60;
	JD -= (60 * min);
	sec = JD;

	day++;

	printf("\n%3s%3s%3s%3s\n", "Day", "H", "M", "S");
	printf("%3d%3d%3d%3d\n\n", day, hour, min, sec);
}

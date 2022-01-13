#include <iostream>
using namespace std;

int main() {
	int a, d, n, x, y;

	cin >> a;
	cin >> d;
	cin >> n;
	x = 1;
	y = n - x;
	y = n * y;
	y = y * d;
	x = 2;
	y = y / x;
	x = n * a;
	x = x + y;
	cout << x;
}

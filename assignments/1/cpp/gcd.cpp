#include <iostream>
using namespace std;

int main() {
int x, y, z, cmp, zero;

cin >> x;
cin >> y;
zero = 0;
cmp = x > zero;
if (cmp) goto A;
x = -x;
A: cmp = y > zero;
if (cmp) goto L;
y = -y;
L: cmp = y == zero;
if (cmp) goto E;
x = x % y;
z = x;
x = y;
y = z;
goto L;
E: cout << x;
}

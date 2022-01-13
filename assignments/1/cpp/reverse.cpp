#include <iostream>
using namespace std;

int main() {
int x, y, zero, mod, ten, cmp;

zero = 0;
ten = 10;
cin >> x;
y = 0;
L: cmp = x == zero;
if (cmp) goto E;
y = ten * y;
mod = x % ten;
y = y + mod;
x = x / ten;
goto L;
E: cout << y;
}

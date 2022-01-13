#include <iostream>
using namespace std;

int main() {
int x, y, res, zero, two, cmp;

zero = 0;
two = 2;
cin >> x;
cin >> y;
res = 0;
cmp = y > zero;
if (cmp) goto L;
x = zero - x;
L: cmp = y == zero;
if (cmp) goto E;
cmp = y % two;
cmp = cmp == zero;
if (cmp) goto SKIP;
res = res + x;
SKIP: x = x + x;
y = y / two;
goto L;
E: cout << res;
}

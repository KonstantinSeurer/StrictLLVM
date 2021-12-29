
#include "Time.h"

Time operator-(Time a, Time b)
{
	return Time(a.value - b.value);
}

Time operator+(Time a, Time b)
{
	return Time(a.value + b.value);
}

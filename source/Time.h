/*
 * Time.h
 *
 *  Created on: Jun 13, 2021
 *      Author: konstantin
 */

#ifndef TIME_H_
#define TIME_H_

#include <chrono>

#include "Base.h"

class Time
{
public:
	UInt64 value;

public:
	Time()
	{
		std::chrono::time_point<std::chrono::system_clock> now = std::chrono::system_clock::now();
		auto duration = now.time_since_epoch();
		auto nanoseconds = std::chrono::duration_cast<std::chrono::nanoseconds>(duration);
		value = nanoseconds.count();
	}

	Time(UInt64 value)
		: value(value)
	{
	}

	Float64 seconds() const
	{
		return value / 1000000000.0;
	}

	Float64 milliSeconds() const
	{
		return value / 1000000.0;
	}

	Float64 nanoSeconds() const
	{
		return value;
	}

public:
	static Time seconds(Float64 seconds)
	{
		return Time((UInt64)(seconds * 1000000000));
	}

	void operator+=(Time t)
	{
		value += t.value;
	}
};

Time operator-(Time a, Time b);

Time operator+(Time a, Time b);

#endif /* TIME_H_ */

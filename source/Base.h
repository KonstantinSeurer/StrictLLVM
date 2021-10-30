#ifndef SOURCE_BASE
#define SOURCE_BASE

#include <string>
#include <vector>
#include <memory>

using Int8 = char;
using UInt8 = unsigned char;

using Int16 = short;
using UInt6 = unsigned short;

using Int32 = int;
using UInt32 = unsigned int;

using Int64 = long long;
using UInt64 = unsigned long long;

using Float32 = float;
using Float64 = double;

using Bool = bool;

using String = std::string;

template <typename T>
using Ref = std::shared_ptr<T>;

template <typename T, typename... Args>
inline Ref<T> allocate(Args &&...args)
{
	return std::make_shared<T>(args...);
}

template <typename T>
using Array = std::vector<T>;

#endif /* SOURCE_BASE */

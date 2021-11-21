#ifndef SOURCE_BASE
#define SOURCE_BASE

#include <string>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <memory>
#include <any>

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
inline Ref<T> Allocate(Args &&...args)
{
	return std::make_shared<T>(args...);
}

template <typename T>
using Array = std::vector<T>;

template <typename K, typename V>
using HashMap = std::unordered_map<K, V>;

template <typename T>
using HashSet = std::unordered_set<T>;

template <typename K, typename V>
using Pair = std::pair<K, V>;

using Any = std::any;

namespace strict
{

	void GetEnumNames(const String &enumDefinition, HashMap<UInt64, String> &target0, HashMap<String, UInt64> &target1);

}

// static blocks
#define CONCATENATE(s1, s2) s1##s2
#define EXPAND_THEN_CONCATENATE(s1, s2) CONCATENATE(s1, s2)
#ifdef __COUNTER__
#define UNIQUE_IDENTIFIER(prefix) EXPAND_THEN_CONCATENATE(prefix, __COUNTER__)
#else
#define UNIQUE_IDENTIFIER(prefix) EXPAND_THEN_CONCATENATE(prefix, __LINE__)
#endif // __COUNTER__

#define static_block STATIC_BLOCK_IMPL1(UNIQUE_IDENTIFIER(_static_block_))

#define STATIC_BLOCK_IMPL1(prefix) \
	STATIC_BLOCK_IMPL2(CONCATENATE(prefix, _fn), CONCATENATE(prefix, _var))

#define STATIC_BLOCK_IMPL2(function_name, var_name)                   \
	static void function_name();                                      \
	static int var_name __attribute((unused)) = (function_name(), 0); \
	static void function_name()

// enum declaration
#ifdef STRICT_ENUM_IMPLEMENTATION
#define STRICT_ENUM(name, ...)                                                          \
	enum class name                                                                     \
	{                                                                                   \
		__VA_ARGS__                                                                     \
	};                                                                                  \
	static HashMap<UInt64, String> name##Names;                                         \
	static HashMap<String, UInt64> name##Values;                                        \
	static_block { strict::GetEnumNames(#__VA_ARGS__, name##Names, name##Values); }     \
	const String &ToString(name value) { return name##Names.at((UInt64)value); }        \
	name StringTo##name(const String &string) { return (name)name##Values.at(string); } \
	name operator|(name a, name b) { return (name)((UInt64)a | (UInt64)b); }            \
	name operator&(name a, name b) { return (name)((UInt64)a & (UInt64)b); }
#else
#define STRICT_ENUM(name, ...)                 \
	enum class name                            \
	{                                          \
		__VA_ARGS__                            \
	};                                         \
	const String &ToString(name value);        \
	name StringTo##name(const String &string); \
	name operator|(name a, name b);            \
	name operator&(name a, name b);
#endif

#endif /* SOURCE_BASE */

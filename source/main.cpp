
#include <filesystem>
#include <iostream>

#include "Base.h"
#include "BuildContext.h"
#include "DebugAllocator.h"
#include "Time.h"

enum class Flag
{
	HELP,
	MODULE_PATH,
	CLEAN,
	OUTPUT_PATH,
	CACHE_PATH,
	OPTIMIZATION_LEVEL,
	LOG_FILE,
	DUMP_IR,
	COUNT
};

struct FlagData
{
public:
	char shortName;
	String longName;
	String description;
	Flag flag;
	bool hasValue;

public:
	FlagData(char shortName, String longName, String description, Flag flag, bool hasValue)
		: shortName(shortName), longName(longName), description(description), flag(flag),
		  hasValue(hasValue)
	{
	}
};

static FlagData flagData[(UInt64)Flag::COUNT] = {
	{'h', "help", "Print this help output.", Flag::HELP, false},
	{'p', "module-path",
     "Semicolon seperated list of every caconical path that contains modules. (-p /path/to/module)",
     Flag::MODULE_PATH, true},
	{'c', "clean", "Clear the output directory before building.", Flag::CLEAN, false},
	{'C', "cache-path", "Directory where the cache files will be written to. (-C /path/to/cache)",
     Flag::CACHE_PATH, true},
	{'o', "output-path",
     "Directory where the output files will be written to. (-o /path/to/output)", Flag::OUTPUT_PATH,
     true},
	{'O', "optimization-level",
     "Indicates what optimizations should be done. Can be debug, performance or size. (-O size)",
     Flag::OPTIMIZATION_LEVEL, true},
	{'d', "dump-ir", "Dump generated IR into the output directory.", Flag::DUMP_IR, false},
	{'l', "log-file",
     "File to log the compiler output (stdout, stderr) to. (-l /path/to/logfile.log)",
     Flag::LOG_FILE, true}};

static void PrintHelp()
{
	std::cout << "Usage:" << std::endl;
	std::cout << "\tstrictllvm [flags...] modules..." << std::endl;
	std::cout << std::endl;
	std::cout << "Flags:" << std::endl;

	UInt64 maxLongNameLength = 0;
	for (const auto& flag : flagData)
	{
		if (maxLongNameLength < flag.longName.length())
		{
			maxLongNameLength = flag.longName.length();
		}
	}

	for (const auto& flag : flagData)
	{
		std::cout << "\t-" << flag.shortName << " --" << flag.longName;

		const UInt64 spaceCount = maxLongNameLength - flag.longName.length() + 1;
		for (UInt64 spaceIndex = 0; spaceIndex < spaceCount; spaceIndex++)
		{
			std::cout << " ";
		}
		std::cout << flag.description << std::endl;
	}
}

void Split(const String& string, const char delim, Array<String>& out)
{
	size_t start;
	size_t end = 0;

	while ((start = string.find_first_not_of(delim, end)) != String::npos)
	{
		end = string.find(delim, start);
		out.push_back(string.substr(start, end - start));
	}
}

static OptimizationLevel ArgumentToOptimizationLevel(const String& argument)
{
	if (argument == "size")
	{
		return OptimizationLevel::SIZE;
	}

	if (argument == "performance")
	{
		return OptimizationLevel::PERFORMANCE;
	}

	if (argument == "debug")
	{
		return OptimizationLevel::DEBUGGING;
	}

	std::cerr << "Invalid optimization level '" << argument << "'!" << std::endl;
	return OptimizationLevel::DEBUGGING;
}

int main(int argc, const char** args)
{
	if (argc < 2)
	{
		PrintHelp();
		return 0;
	}

	Array<Pair<Flag, String>> valueFlags;
	Array<Flag> flags;
	Array<String> moduleNames;
	OptimizationLevel optimizationLevel = OptimizationLevel::DEBUGGING;

	UInt64 flagValueIndex = 0;
	for (UInt64 argumentIndex = 1; argumentIndex < argc; argumentIndex++)
	{
		const char* argument = args[argumentIndex];

		if (argument[0] == '-')
		{
			if (argument[1] == '-')
			{
				const char* argumentFlag = &(argument[2]);

				Bool found = false;
				for (const auto& flag : flagData)
				{
					if (flag.longName == argumentFlag)
					{
						if (flag.hasValue)
						{
							valueFlags.emplace_back(flag.flag, "");
						}
						else
						{
							flags.emplace_back(flag.flag);
						}
						found = true;
						break;
					}
				}

				if (!found)
				{
					std::cerr << "Undefines argument flag '" << argumentFlag << "'!" << std::endl;
				}
			}
			else
			{
				for (UInt64 characterIndex = 1; argument[characterIndex] != '\0'; characterIndex++)
				{
					Bool found = false;
					for (const auto& flag : flagData)
					{
						if (flag.shortName == argument[characterIndex])
						{
							if (flag.hasValue)
							{
								valueFlags.emplace_back(flag.flag, "");
							}
							else
							{
								flags.emplace_back(flag.flag);
							}
							found = true;
							break;
						}
					}

					if (!found)
					{
						std::cerr << "Undefines argument flag '" << argument[characterIndex] << "'!"
								  << std::endl;
					}
				}
			}
			continue;
		}

		if (flagValueIndex < valueFlags.size())
		{
			valueFlags[flagValueIndex].second = argument;
			flagValueIndex++;
			continue;
		}

		moduleNames.push_back(argument);
	}

	Array<String> modulePath;
	String outputPath = String(std::filesystem::current_path()) + "/output";
	String cachePath = String(std::filesystem::current_path()) + "/cache";
	Optional<String> logFile;

	for (const auto& flag : valueFlags)
	{
		if (flag.first == Flag::MODULE_PATH)
		{
			Split(flag.second, ';', modulePath);
		}
		else if (flag.first == Flag::OUTPUT_PATH)
		{
			outputPath = flag.second;
		}
		else if (flag.first == Flag::CACHE_PATH)
		{
			cachePath = flag.second;
		}
		else if (flag.first == Flag::LOG_FILE)
		{
			logFile = flag.second;
		}
		else if (flag.first == Flag::OPTIMIZATION_LEVEL)
		{
			optimizationLevel = ArgumentToOptimizationLevel(flag.second);
		}
	}

	bool dumpIR = false;
	for (const Flag& flag : flags)
	{
		if (flag == Flag::HELP)
		{
			PrintHelp();
		}
		else if (flag == Flag::CLEAN)
		{
			if (std::filesystem::exists(outputPath))
			{
				std::filesystem::remove_all(outputPath);
			}

			if (std::filesystem::exists(cachePath))
			{
				std::filesystem::remove_all(cachePath);
			}
		}
		else if (flag == Flag::DUMP_IR)
		{
			dumpIR = true;
		}
	}

	const TargetFlags target = TargetFlags::BIT64 | TargetFlags::LINUX | TargetFlags::X86;

	BuildContext context(modulePath, outputPath, cachePath, logFile, target, dumpIR,
	                     optimizationLevel);

	std::cout << "Scanning modules...";
	Time scanStart;

	for (const String& moduleName : moduleNames)
	{
		context.AddModule(moduleName);
	}

	std::cout.precision(1);
	std::cout << " (" << (Time() - scanStart).milliSeconds() << "ms)" << std::endl;

	context.Build();

#if USE_DEBUG_ALLOCATOR
	std::cout << "Maximum allocation size: " << (GetMaxAllocationSize() >> 10) << "kB" << std::endl;
#endif

	return 0;
}

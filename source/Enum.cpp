
#define STRICT_ENUM_IMPLEMENTATION
#include "Base.h"

#include "Lexer.h"
#include "ast/Module.h"
#include "ast/Unit.h"

#include <iostream>

namespace strict
{

	void GetEnumNames(const String &enumDefinition, HashMap<UInt64, String> &target0, HashMap<String, UInt64> &target1)
	{
		String source = enumDefinition + ",";
		String name = "";
		String valueString = "";
		UInt64 value = 0;
		bool assigned = false;

		for (UInt32 i = 0; i < source.length(); i++)
		{
			if (isspace(source[i]))
			{
				continue;
			}

			if (source[i] == '=')
			{
				assigned = true;
				continue;
			}

			if (source[i] == ',')
			{
				if (assigned)
				{
					if (valueString.length() > 2 && valueString[0] == '0')
					{
						switch (valueString[1])
						{
						case 'x':
							value = strtoull(valueString.substr(2, valueString.length() - 2).c_str(), nullptr, 16);
							break;
						case 'b':
							value = strtoull(valueString.substr(2, valueString.length() - 2).c_str(), nullptr, 2);
							break;
						default:
							std::cerr << "Enexpected character '" << valueString[1]
									  << "' while trying to parse the source of an enum!" << std::endl;
						}
					}
					else
					{
						value = strtoull(valueString.c_str(), nullptr, 10);
					}
				}

				target0[value] = name;
				target1[name] = value;

				assigned = false;
				value++;
				valueString = "";
				name = "";
			}
			else if (assigned)
			{
				valueString += source[i];
			}
			else
			{
				name += source[i];
			}
		}
	}

}

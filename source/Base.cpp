
#include "Base.h"

String ReplaceChar(const String& string, char character, char replacement)
{
	String result = string;
	for (UInt32 characterIndex = 0; characterIndex < result.size(); characterIndex++)
	{
		if (result[characterIndex] == character)
		{
			result[characterIndex] = replacement;
		}
	}
	return result;
}

String RemoveChar(const String& string, char character)
{
	String result = string;
	result.erase(remove(result.begin(), result.end(), character), result.end());
	return result;
}

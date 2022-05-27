
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

UInt32 GetLineNumber(const String& string, UInt32 characterIndex, UInt32 start)
{
	UInt32 result = start;
	for (Int64 i = 0; i <= characterIndex; i++)
	{
		if (string[i] == '\n')
		{
			result++;
		}
	}
	return result;
}

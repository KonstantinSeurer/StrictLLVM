
#include "ErrorStream.h"

void ErrorStream::PrintError(const String &message)
{
	print(fileName);
	print(":\n\t");
	print(message);
	print("\n");

	errorOccured = true;
}

#include <iostream>

void ErrorStream::PrintError(const Token &location, const String &message)
{
	const String &source = lexer->GetSource();

	Int64 lineStart = location.characterIndex;
	for (; lineStart >= 0 && source[lineStart] != '\n'; lineStart--)
		;
	lineStart++;

	Int64 lineEnd = location.characterIndex;
	for (; lineEnd < source.length() && source[lineEnd] != '\n'; lineEnd++)
		;

	Int64 lineCount = 1;
	for (Int64 characterIndex = 0; characterIndex <= location.characterIndex; characterIndex++)
	{
		if (source[characterIndex] == '\n')
		{
			lineCount++;
		}
	}

	print(fileName);
	print("(line ");
	print(std::to_string(lineCount));
	print("):\n\t");

	print(source.substr(lineStart, lineEnd - lineStart));
	print("\n\t");

	for (Int64 index = lineStart; index < location.characterIndex; index++)
	{
		if (source[index] == '\t')
		{
			print("\t");
		}
		else
		{
			print(" ");
		}
	}
	print("^\n\t");

	print(message);
	print("\n");

	errorOccured = true;
}
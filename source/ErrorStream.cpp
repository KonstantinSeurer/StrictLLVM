
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
	errorOccured = true;

	if (tryCatchLexel > 0)
	{
		return;
	}

	errorCount++;

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
}

void ErrorStream::Try()
{
	if (errorOccured)
	{
		print("Compiler bug: Uncaught error (the error flag was set when calling ErrorStream::Try)!");
	}

	errorOccured = false;
	tryCatchLexel++;
}

Bool ErrorStream::Catch()
{
	if (tryCatchLexel == 0)
	{
		print("Compiler bug: Can't call ErrorStream::Catch without calling ErrorStream::Try!");
		return errorOccured;
	}

	Bool result = errorOccured;

	errorOccured = false;
	tryCatchLexel--;

	return result;
}

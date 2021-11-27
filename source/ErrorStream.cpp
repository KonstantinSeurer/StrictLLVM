
#include "ErrorStream.h"

void ErrorStream::PrintError(const String &message)
{
	print(fileName);
	print(":\n\t");
	print(message);
	print("\n");

	errorOccured = true;
}

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

	print(fileName);
	print(":\n\t");

	print(source.substr(lineStart, lineEnd - lineStart));
	print("\n\t");

	for (Int64 index = lineStart; index < location.characterIndex; index++)
	{
		print(" ");
	}
	print("^\n\t");

	print(message);
	print("\n");

	errorOccured = true;
}

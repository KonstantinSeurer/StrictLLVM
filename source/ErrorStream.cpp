
#include "ErrorStream.h"

void ErrorStream::PrintError(const String &message)
{
	print(fileName);
	print(":\n\t");
	print(message);
	print("\n");
}

void ErrorStream::PrintError(const Token &location, const String &message)
{
	print(fileName);
	print(":\n\t");
	print(message);
	print("\n");
}

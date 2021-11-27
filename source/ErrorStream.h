#ifndef SOURCE_ERRORSTREAM
#define SOURCE_ERRORSTREAM

#include "Base.h"
#include "Lexer.h"

class ErrorStream
{
private:
	String fileName;
	Function<void(const String &)> print;

public:
	ErrorStream(const String &fileName, Function<void(const String &)> print)
		: fileName(fileName), print(print)
	{
	}

public:
	void PrintError(const String &message);
	void PrintError(const Token &location, const String &message);
};

#endif /* SOURCE_ERRORSTREAM */

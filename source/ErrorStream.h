#ifndef SOURCE_ERRORSTREAM
#define SOURCE_ERRORSTREAM

#include "Base.h"
#include "Lexer.h"

class ErrorStream
{
private:
	String fileName;
	Function<void(const String &)> print;
	Ref<const Lexer> lexer;
	bool errorOccured;

public:
	ErrorStream(const String &fileName, Function<void(const String &)> print, Ref<const Lexer> lexer)
		: fileName(fileName), print(print), lexer(lexer), errorOccured(false)
	{
	}

public:
	void PrintError(const String &message);
	void PrintError(const Token &location, const String &message);

	bool HasErrorOccured() const
	{
		return errorOccured;
	}
};

#endif /* SOURCE_ERRORSTREAM */

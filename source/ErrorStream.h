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
	UInt32 tryCatchLexel;

public:
	ErrorStream(const String &fileName, Function<void(const String &)> print, Ref<const Lexer> lexer)
		: fileName(fileName), print(print), lexer(lexer), errorOccured(false), tryCatchLexel(0)
	{
	}

public:
	void PrintError(const String &message);
	void PrintError(const Token &location, const String &message);

	void Try();
	Bool Catch();

	bool HasErrorOccured() const
	{
		return errorOccured;
	}
};

#endif /* SOURCE_ERRORSTREAM */

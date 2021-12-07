#ifndef SOURCE_ERRORSTREAM
#define SOURCE_ERRORSTREAM

#include "Base.h"
#include "Lexer.h"

using PrintFunction = Function<void(const String &)>;

class ErrorStream
{
private:
	String fileName;
	PrintFunction print;
	Ref<const Lexer> lexer;
	bool errorOccured;
	UInt32 tryCatchLexel;
	UInt32 errorCount;

public:
	ErrorStream(const String &fileName, PrintFunction print, Ref<const Lexer> lexer)
		: fileName(fileName), print(print), lexer(lexer), errorOccured(false), tryCatchLexel(0), errorCount(0)
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

	UInt32 GetErrorCount() const
	{
		return errorCount;
	}
};

#endif /* SOURCE_ERRORSTREAM */

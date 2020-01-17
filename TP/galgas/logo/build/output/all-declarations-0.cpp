#include "galgas2/C_Compiler.h"
#include "galgas2/C_galgas_io.h"
#include "galgas2/C_galgas_CLI_Options.h"
#include "utilities/C_PrologueEpilogue.h"

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

#include "all-declarations-0.h"

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//                                                                                                                     *
//     L E X I Q U E                                                                                                   *
//                                                                                                                     *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

#include "strings/unicode_character_cpp.h"
#include "galgas2/scanner_actions.h"
#include "galgas2/cLexiqueIntrospection.h"

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

cTokenFor_logo_5F_lexique::cTokenFor_logo_5F_lexique (void) :
mLexicalAttribute_tokenString (),
mLexicalAttribute_uint_33__32_value () {
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

C_Lexique_logo_5F_lexique::C_Lexique_logo_5F_lexique (C_Compiler * inCallerCompiler,
                                                      const C_String & inSourceFileName
                                                      COMMA_LOCATION_ARGS) :
C_Lexique (inCallerCompiler, inSourceFileName COMMA_THERE) {
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

C_Lexique_logo_5F_lexique::C_Lexique_logo_5F_lexique (C_Compiler * inCallerCompiler,
                                                      const C_String & inSourceString,
                                                      const C_String & inStringForError
                                                      COMMA_LOCATION_ARGS) :
C_Lexique (inCallerCompiler, inSourceString, inStringForError COMMA_THERE) {
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//                 I N D E X I N G    D I R E C T O R Y                                                                *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

C_String C_Lexique_logo_5F_lexique::indexingDirectory (void) const {
  return "" ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//                        Lexical error message list                                                                   *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

static const char * gLexicalMessage_logo_5F_lexique_decimalNumberTooLarge = "decimal number too large" ;

static const char * gLexicalMessage_logo_5F_lexique_incorrectStringEnd = "string does not end with '\"'" ;

static const char * gLexicalMessage_logo_5F_lexique_internalError = "internal error" ;

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//          Syntax error messages, for every terminal symbol                                                           *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

//--- Syntax error message for terminal '$identifier$' :
static const char * gSyntaxErrorMessage_logo_5F_lexique_identifier = "an identifier" ;

//--- Syntax error message for terminal '$integer$' :
static const char * gSyntaxErrorMessage_logo_5F_lexique_integer = "a 32-bit unsigned decimal number" ;

//--- Syntax error message for terminal '$"string"$' :
static const char * gSyntaxErrorMessage_logo_5F_lexique__22_string_22_ = "a character string constant \"...\"" ;

//--- Syntax error message for terminal '$comment$' :
static const char * gSyntaxErrorMessage_logo_5F_lexique_comment = "a comment" ;

//--- Syntax error message for terminal '$PROGRAM$' :
static const char * gSyntaxErrorMessage_logo_5F_lexique_PROGRAM = "the 'PROGRAM' keyword" ;

//--- Syntax error message for terminal '$ROUTINE$' :
static const char * gSyntaxErrorMessage_logo_5F_lexique_ROUTINE = "the 'ROUTINE' keyword" ;

//--- Syntax error message for terminal '$BEGIN$' :
static const char * gSyntaxErrorMessage_logo_5F_lexique_BEGIN = "the 'BEGIN' keyword" ;

//--- Syntax error message for terminal '$END$' :
static const char * gSyntaxErrorMessage_logo_5F_lexique_END = "the 'END' keyword" ;

//--- Syntax error message for terminal '$FORWARD$' :
static const char * gSyntaxErrorMessage_logo_5F_lexique_FORWARD = "the 'FORWARD' keyword" ;

//--- Syntax error message for terminal '$ROTATE$' :
static const char * gSyntaxErrorMessage_logo_5F_lexique_ROTATE = "the 'ROTATE' keyword" ;

//--- Syntax error message for terminal '$PEN$' :
static const char * gSyntaxErrorMessage_logo_5F_lexique_PEN = "the 'PEN' keyword" ;

//--- Syntax error message for terminal '$UP$' :
static const char * gSyntaxErrorMessage_logo_5F_lexique_UP = "the 'UP' keyword" ;

//--- Syntax error message for terminal '$DOWN$' :
static const char * gSyntaxErrorMessage_logo_5F_lexique_DOWN = "the 'DOWN' keyword" ;

//--- Syntax error message for terminal '$CALL$' :
static const char * gSyntaxErrorMessage_logo_5F_lexique_CALL = "the 'CALL' keyword" ;

//--- Syntax error message for terminal '$.$' :
static const char * gSyntaxErrorMessage_logo_5F_lexique__2E_ = "the '.' delimitor" ;

//--- Syntax error message for terminal '$;$' :
static const char * gSyntaxErrorMessage_logo_5F_lexique__3B_ = "the ';' delimitor" ;

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//                getMessageForTerminal                                                                                *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

C_String C_Lexique_logo_5F_lexique::getMessageForTerminal (const int16_t inTerminalIndex) const {
  C_String result = "<unknown>" ;
  if ((inTerminalIndex >= 0) && (inTerminalIndex < 17)) {
    static const char * syntaxErrorMessageArray [17] = {kEndOfSourceLexicalErrorMessage,
        gSyntaxErrorMessage_logo_5F_lexique_identifier,
        gSyntaxErrorMessage_logo_5F_lexique_integer,
        gSyntaxErrorMessage_logo_5F_lexique__22_string_22_,
        gSyntaxErrorMessage_logo_5F_lexique_comment,
        gSyntaxErrorMessage_logo_5F_lexique_PROGRAM,
        gSyntaxErrorMessage_logo_5F_lexique_ROUTINE,
        gSyntaxErrorMessage_logo_5F_lexique_BEGIN,
        gSyntaxErrorMessage_logo_5F_lexique_END,
        gSyntaxErrorMessage_logo_5F_lexique_FORWARD,
        gSyntaxErrorMessage_logo_5F_lexique_ROTATE,
        gSyntaxErrorMessage_logo_5F_lexique_PEN,
        gSyntaxErrorMessage_logo_5F_lexique_UP,
        gSyntaxErrorMessage_logo_5F_lexique_DOWN,
        gSyntaxErrorMessage_logo_5F_lexique_CALL,
        gSyntaxErrorMessage_logo_5F_lexique__2E_,
        gSyntaxErrorMessage_logo_5F_lexique__3B_
    } ;
    result = syntaxErrorMessageArray [inTerminalIndex] ;
  }
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//                      U N I C O D E    S T R I N G S                                                                 *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

//--- Unicode string for '$_2E_$'
static const utf32 kUnicodeString_logo_5F_lexique__2E_ [] = {
  TO_UNICODE ('.'),
  TO_UNICODE (0)
} ;

//--- Unicode string for '$_3B_$'
static const utf32 kUnicodeString_logo_5F_lexique__3B_ [] = {
  TO_UNICODE (';'),
  TO_UNICODE (0)
} ;

//--- Unicode string for '$BEGIN$'
static const utf32 kUnicodeString_logo_5F_lexique_BEGIN [] = {
  TO_UNICODE ('B'),
  TO_UNICODE ('E'),
  TO_UNICODE ('G'),
  TO_UNICODE ('I'),
  TO_UNICODE ('N'),
  TO_UNICODE (0)
} ;

//--- Unicode string for '$CALL$'
static const utf32 kUnicodeString_logo_5F_lexique_CALL [] = {
  TO_UNICODE ('C'),
  TO_UNICODE ('A'),
  TO_UNICODE ('L'),
  TO_UNICODE ('L'),
  TO_UNICODE (0)
} ;

//--- Unicode string for '$DOWN$'
static const utf32 kUnicodeString_logo_5F_lexique_DOWN [] = {
  TO_UNICODE ('D'),
  TO_UNICODE ('O'),
  TO_UNICODE ('W'),
  TO_UNICODE ('N'),
  TO_UNICODE (0)
} ;

//--- Unicode string for '$END$'
static const utf32 kUnicodeString_logo_5F_lexique_END [] = {
  TO_UNICODE ('E'),
  TO_UNICODE ('N'),
  TO_UNICODE ('D'),
  TO_UNICODE (0)
} ;

//--- Unicode string for '$FORWARD$'
static const utf32 kUnicodeString_logo_5F_lexique_FORWARD [] = {
  TO_UNICODE ('F'),
  TO_UNICODE ('O'),
  TO_UNICODE ('R'),
  TO_UNICODE ('W'),
  TO_UNICODE ('A'),
  TO_UNICODE ('R'),
  TO_UNICODE ('D'),
  TO_UNICODE (0)
} ;

//--- Unicode string for '$PEN$'
static const utf32 kUnicodeString_logo_5F_lexique_PEN [] = {
  TO_UNICODE ('P'),
  TO_UNICODE ('E'),
  TO_UNICODE ('N'),
  TO_UNICODE (0)
} ;

//--- Unicode string for '$PROGRAM$'
static const utf32 kUnicodeString_logo_5F_lexique_PROGRAM [] = {
  TO_UNICODE ('P'),
  TO_UNICODE ('R'),
  TO_UNICODE ('O'),
  TO_UNICODE ('G'),
  TO_UNICODE ('R'),
  TO_UNICODE ('A'),
  TO_UNICODE ('M'),
  TO_UNICODE (0)
} ;

//--- Unicode string for '$ROTATE$'
static const utf32 kUnicodeString_logo_5F_lexique_ROTATE [] = {
  TO_UNICODE ('R'),
  TO_UNICODE ('O'),
  TO_UNICODE ('T'),
  TO_UNICODE ('A'),
  TO_UNICODE ('T'),
  TO_UNICODE ('E'),
  TO_UNICODE (0)
} ;

//--- Unicode string for '$ROUTINE$'
static const utf32 kUnicodeString_logo_5F_lexique_ROUTINE [] = {
  TO_UNICODE ('R'),
  TO_UNICODE ('O'),
  TO_UNICODE ('U'),
  TO_UNICODE ('T'),
  TO_UNICODE ('I'),
  TO_UNICODE ('N'),
  TO_UNICODE ('E'),
  TO_UNICODE (0)
} ;

//--- Unicode string for '$UP$'
static const utf32 kUnicodeString_logo_5F_lexique_UP [] = {
  TO_UNICODE ('U'),
  TO_UNICODE ('P'),
  TO_UNICODE (0)
} ;

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//             Key words table 'delimitorsList'                            *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

static const int32_t ktable_size_logo_5F_lexique_delimitorsList = 2 ;

static const C_unicode_lexique_table_entry ktable_for_logo_5F_lexique_delimitorsList [ktable_size_logo_5F_lexique_delimitorsList] = {
  C_unicode_lexique_table_entry (kUnicodeString_logo_5F_lexique__2E_, 1, C_Lexique_logo_5F_lexique::kToken__2E_),
  C_unicode_lexique_table_entry (kUnicodeString_logo_5F_lexique__3B_, 1, C_Lexique_logo_5F_lexique::kToken__3B_)
} ;

int16_t C_Lexique_logo_5F_lexique::search_into_delimitorsList (const C_String & inSearchedString) {
  return searchInList (inSearchedString, ktable_for_logo_5F_lexique_delimitorsList, ktable_size_logo_5F_lexique_delimitorsList) ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//             Key words table 'keyWordList'                            *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

static const int32_t ktable_size_logo_5F_lexique_keyWordList = 10 ;

static const C_unicode_lexique_table_entry ktable_for_logo_5F_lexique_keyWordList [ktable_size_logo_5F_lexique_keyWordList] = {
  C_unicode_lexique_table_entry (kUnicodeString_logo_5F_lexique_UP, 2, C_Lexique_logo_5F_lexique::kToken_UP),
  C_unicode_lexique_table_entry (kUnicodeString_logo_5F_lexique_END, 3, C_Lexique_logo_5F_lexique::kToken_END),
  C_unicode_lexique_table_entry (kUnicodeString_logo_5F_lexique_PEN, 3, C_Lexique_logo_5F_lexique::kToken_PEN),
  C_unicode_lexique_table_entry (kUnicodeString_logo_5F_lexique_CALL, 4, C_Lexique_logo_5F_lexique::kToken_CALL),
  C_unicode_lexique_table_entry (kUnicodeString_logo_5F_lexique_DOWN, 4, C_Lexique_logo_5F_lexique::kToken_DOWN),
  C_unicode_lexique_table_entry (kUnicodeString_logo_5F_lexique_BEGIN, 5, C_Lexique_logo_5F_lexique::kToken_BEGIN),
  C_unicode_lexique_table_entry (kUnicodeString_logo_5F_lexique_ROTATE, 6, C_Lexique_logo_5F_lexique::kToken_ROTATE),
  C_unicode_lexique_table_entry (kUnicodeString_logo_5F_lexique_FORWARD, 7, C_Lexique_logo_5F_lexique::kToken_FORWARD),
  C_unicode_lexique_table_entry (kUnicodeString_logo_5F_lexique_PROGRAM, 7, C_Lexique_logo_5F_lexique::kToken_PROGRAM),
  C_unicode_lexique_table_entry (kUnicodeString_logo_5F_lexique_ROUTINE, 7, C_Lexique_logo_5F_lexique::kToken_ROUTINE)
} ;

int16_t C_Lexique_logo_5F_lexique::search_into_keyWordList (const C_String & inSearchedString) {
  return searchInList (inSearchedString, ktable_for_logo_5F_lexique_keyWordList, ktable_size_logo_5F_lexique_keyWordList) ;
}


//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//                          getCurrentTokenString                                                                      *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

C_String C_Lexique_logo_5F_lexique::getCurrentTokenString (const cToken * inTokenPtr) const {
  const cTokenFor_logo_5F_lexique * ptr = (const cTokenFor_logo_5F_lexique *) inTokenPtr ;
  C_String s ;
  if (ptr == NULL) {
    s.appendCString("$$") ;
  }else{
    switch (ptr->mTokenCode) {
    case kToken_:
      s.appendCString("$$") ;
      break ;
    case kToken_identifier:
      s.appendUnicodeCharacter (TO_UNICODE ('$') COMMA_HERE) ;
      s.appendCString ("identifier") ;
      s.appendUnicodeCharacter (TO_UNICODE ('$') COMMA_HERE) ;
      s.appendUnicodeCharacter (TO_UNICODE (' ') COMMA_HERE) ;
      s.appendCLiteralStringConstant (ptr->mLexicalAttribute_tokenString) ;
      break ;
    case kToken_integer:
      s.appendUnicodeCharacter (TO_UNICODE ('$') COMMA_HERE) ;
      s.appendCString ("integer") ;
      s.appendUnicodeCharacter (TO_UNICODE ('$') COMMA_HERE) ;
      s.appendUnicodeCharacter (TO_UNICODE (' ') COMMA_HERE) ;
      s.appendUnsigned (ptr->mLexicalAttribute_uint_33__32_value) ;
      break ;
    case kToken__22_string_22_:
      s.appendUnicodeCharacter (TO_UNICODE ('$') COMMA_HERE) ;
      s.appendCString ("\"string\"") ;
      s.appendUnicodeCharacter (TO_UNICODE ('$') COMMA_HERE) ;
      s.appendUnicodeCharacter (TO_UNICODE (' ') COMMA_HERE) ;
      s.appendCLiteralStringConstant (ptr->mLexicalAttribute_tokenString) ;
      break ;
    case kToken_comment:
      s.appendUnicodeCharacter (TO_UNICODE ('$') COMMA_HERE) ;
      s.appendCString ("comment") ;
      s.appendUnicodeCharacter (TO_UNICODE ('$') COMMA_HERE) ;
      break ;
    case kToken_PROGRAM:
      s.appendUnicodeCharacter (TO_UNICODE ('$') COMMA_HERE) ;
      s.appendCString ("PROGRAM") ;
      s.appendUnicodeCharacter (TO_UNICODE ('$') COMMA_HERE) ;
      break ;
    case kToken_ROUTINE:
      s.appendUnicodeCharacter (TO_UNICODE ('$') COMMA_HERE) ;
      s.appendCString ("ROUTINE") ;
      s.appendUnicodeCharacter (TO_UNICODE ('$') COMMA_HERE) ;
      break ;
    case kToken_BEGIN:
      s.appendUnicodeCharacter (TO_UNICODE ('$') COMMA_HERE) ;
      s.appendCString ("BEGIN") ;
      s.appendUnicodeCharacter (TO_UNICODE ('$') COMMA_HERE) ;
      break ;
    case kToken_END:
      s.appendUnicodeCharacter (TO_UNICODE ('$') COMMA_HERE) ;
      s.appendCString ("END") ;
      s.appendUnicodeCharacter (TO_UNICODE ('$') COMMA_HERE) ;
      break ;
    case kToken_FORWARD:
      s.appendUnicodeCharacter (TO_UNICODE ('$') COMMA_HERE) ;
      s.appendCString ("FORWARD") ;
      s.appendUnicodeCharacter (TO_UNICODE ('$') COMMA_HERE) ;
      break ;
    case kToken_ROTATE:
      s.appendUnicodeCharacter (TO_UNICODE ('$') COMMA_HERE) ;
      s.appendCString ("ROTATE") ;
      s.appendUnicodeCharacter (TO_UNICODE ('$') COMMA_HERE) ;
      break ;
    case kToken_PEN:
      s.appendUnicodeCharacter (TO_UNICODE ('$') COMMA_HERE) ;
      s.appendCString ("PEN") ;
      s.appendUnicodeCharacter (TO_UNICODE ('$') COMMA_HERE) ;
      break ;
    case kToken_UP:
      s.appendUnicodeCharacter (TO_UNICODE ('$') COMMA_HERE) ;
      s.appendCString ("UP") ;
      s.appendUnicodeCharacter (TO_UNICODE ('$') COMMA_HERE) ;
      break ;
    case kToken_DOWN:
      s.appendUnicodeCharacter (TO_UNICODE ('$') COMMA_HERE) ;
      s.appendCString ("DOWN") ;
      s.appendUnicodeCharacter (TO_UNICODE ('$') COMMA_HERE) ;
      break ;
    case kToken_CALL:
      s.appendUnicodeCharacter (TO_UNICODE ('$') COMMA_HERE) ;
      s.appendCString ("CALL") ;
      s.appendUnicodeCharacter (TO_UNICODE ('$') COMMA_HERE) ;
      break ;
    case kToken__2E_:
      s.appendUnicodeCharacter (TO_UNICODE ('$') COMMA_HERE) ;
      s.appendCString (".") ;
      s.appendUnicodeCharacter (TO_UNICODE ('$') COMMA_HERE) ;
      break ;
    case kToken__3B_:
      s.appendUnicodeCharacter (TO_UNICODE ('$') COMMA_HERE) ;
      s.appendCString (";") ;
      s.appendUnicodeCharacter (TO_UNICODE ('$') COMMA_HERE) ;
      break ;
    default:
      break ;
    }
  }
  return s ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//                           Template Delimiters                                                                       *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*


//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//                           Template Replacements                                                                     *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*


//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//            Terminal Symbols as end of script in template mark                                                       *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*


//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//            Unicode test functions                                                                                   *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
 
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//               P A R S E    L E X I C A L    T O K E N                                                               *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

bool C_Lexique_logo_5F_lexique::parseLexicalToken (void) {
  cTokenFor_logo_5F_lexique token ;
  mLoop = true ;
  token.mTokenCode = -1 ;
  while ((token.mTokenCode < 0) && (UNICODE_VALUE (mCurrentChar) != '\0')) {
    token.mLexicalAttribute_tokenString.setLengthToZero () ;
    token.mLexicalAttribute_uint_33__32_value = 0 ;
    mTokenStartLocation = mCurrentLocation ;
    try{
      if (testForInputUTF32CharRange (TO_UNICODE ('a'), TO_UNICODE ('z')) || testForInputUTF32CharRange (TO_UNICODE ('A'), TO_UNICODE ('Z'))) {
        do {
          ::scanner_routine_enterCharacterIntoString (*this, token.mLexicalAttribute_tokenString, previousChar ()) ;
          if (testForInputUTF32CharRange (TO_UNICODE ('a'), TO_UNICODE ('z')) || testForInputUTF32CharRange (TO_UNICODE ('A'), TO_UNICODE ('Z')) || testForInputUTF32Char (TO_UNICODE ('_')) || testForInputUTF32CharRange (TO_UNICODE ('0'), TO_UNICODE ('9'))) {
          }else{
            mLoop = false ;
          }
        }while (mLoop) ;
        mLoop = true ;
        if (token.mTokenCode == -1) {
          token.mTokenCode = search_into_keyWordList (token.mLexicalAttribute_tokenString) ;
        }
        if (token.mTokenCode == -1) {
          token.mTokenCode = kToken_identifier ;
        }
        enterToken (token) ;
      }else if (testForInputUTF32CharRange (TO_UNICODE ('0'), TO_UNICODE ('9'))) {
        ::scanner_routine_enterCharacterIntoString (*this, token.mLexicalAttribute_tokenString, previousChar ()) ;
        do {
          if (testForInputUTF32CharRange (TO_UNICODE ('0'), TO_UNICODE ('9'))) {
            ::scanner_routine_enterCharacterIntoString (*this, token.mLexicalAttribute_tokenString, previousChar ()) ;
          }else if (testForInputUTF32Char (TO_UNICODE ('_'))) {
          }else{
            mLoop = false ;
          }
        }while (mLoop) ;
        mLoop = true ;
        ::scanner_routine_convertDecimalStringIntoUInt (*this, token.mLexicalAttribute_tokenString, token.mLexicalAttribute_uint_33__32_value, gLexicalMessage_logo_5F_lexique_decimalNumberTooLarge, gLexicalMessage_logo_5F_lexique_internalError) ;
        token.mTokenCode = kToken_integer ;
        enterToken (token) ;
      }else if (testForInputUTF32Char (TO_UNICODE ('\"'))) {
        do {
          if (testForInputUTF32Char (TO_UNICODE (' ')) || testForInputUTF32Char (TO_UNICODE ('!')) || testForInputUTF32CharRange (TO_UNICODE ('#'), TO_UNICODE (65533))) {
            ::scanner_routine_enterCharacterIntoString (*this, token.mLexicalAttribute_tokenString, previousChar ()) ;
          }else{
            mLoop = false ;
          }
        }while (mLoop) ;
        mLoop = true ;
        if (testForInputUTF32Char (TO_UNICODE ('\"'))) {
          token.mTokenCode = kToken__22_string_22_ ;
          enterToken (token) ;
        }else{
          lexicalError (gLexicalMessage_logo_5F_lexique_incorrectStringEnd COMMA_LINE_AND_SOURCE_FILE) ;
        }
      }else if (testForInputUTF32String (kUnicodeString_logo_5F_lexique__3B_, 1, true)) {
        token.mTokenCode = kToken__3B_ ;
        enterToken (token) ;
      }else if (testForInputUTF32String (kUnicodeString_logo_5F_lexique__2E_, 1, true)) {
        token.mTokenCode = kToken__2E_ ;
        enterToken (token) ;
      }else if (testForInputUTF32Char (TO_UNICODE ('#'))) {
        do {
          if (testForInputUTF32CharRange (TO_UNICODE (1), TO_UNICODE ('\t')) || testForInputUTF32Char (TO_UNICODE ('\v')) || testForInputUTF32Char (TO_UNICODE ('\f')) || testForInputUTF32CharRange (TO_UNICODE (14), TO_UNICODE (65533))) {
          }else{
            mLoop = false ;
          }
        }while (mLoop) ;
        mLoop = true ;
        enterDroppedTerminal (kToken_comment) ;
      }else if (testForInputUTF32CharRange (TO_UNICODE (1), TO_UNICODE (' '))) {
      }else if (testForInputUTF32Char (TO_UNICODE ('\0'))) { // End of source text ? 
        token.mTokenCode = kToken_ ; // Empty string code
      }else{ // Unknown input character
        unknownCharacterLexicalError (LINE_AND_SOURCE_FILE) ;
        token.mTokenCode = -1 ; // No token
        advance () ; // ... go throught unknown character
      }
    }catch (const C_lexicalErrorException &) {
      token.mTokenCode = -1 ; // No token
      advance () ; // ... go throught unknown character
    }
  }
  if (UNICODE_VALUE (mCurrentChar) == '\0') {
    token.mTokenCode = 0 ;
    enterToken (token) ;
  }
  return token.mTokenCode > 0 ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//                         E N T E R    T O K E N                                                                      *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

void C_Lexique_logo_5F_lexique::enterToken (cTokenFor_logo_5F_lexique & ioToken) {
  cTokenFor_logo_5F_lexique * ptr = NULL ;
  macroMyNew (ptr, cTokenFor_logo_5F_lexique ()) ;
  ptr->mTokenCode = ioToken.mTokenCode ;
  // ptr->mIsOptional = ioToken.mIsOptional ;
  ptr->mStartLocation = mTokenStartLocation ;
  ptr->mEndLocation = mTokenEndLocation ;
  ptr->mTemplateStringBeforeToken = ioToken.mTemplateStringBeforeToken ;
  ioToken.mTemplateStringBeforeToken = "" ;
  ptr->mLexicalAttribute_tokenString = ioToken.mLexicalAttribute_tokenString ;
  ptr->mLexicalAttribute_uint_33__32_value = ioToken.mLexicalAttribute_uint_33__32_value ;
  enterTokenFromPointer (ptr) ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//               A T T R I B U T E   A C C E S S                                                                       *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

C_String C_Lexique_logo_5F_lexique::attributeValue_tokenString (void) const {
  cTokenFor_logo_5F_lexique * ptr = (cTokenFor_logo_5F_lexique *) currentTokenPtr (HERE) ;
  return ptr->mLexicalAttribute_tokenString ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

uint32_t C_Lexique_logo_5F_lexique::attributeValue_uint_33__32_value (void) const {
  cTokenFor_logo_5F_lexique * ptr = (cTokenFor_logo_5F_lexique *) currentTokenPtr (HERE) ;
  return ptr->mLexicalAttribute_uint_33__32_value ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//         A S S I G N    F R O M    A T T R I B U T E                                                                 *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_lstring C_Lexique_logo_5F_lexique::synthetizedAttribute_tokenString (void) const {
  cTokenFor_logo_5F_lexique * ptr = (cTokenFor_logo_5F_lexique *) currentTokenPtr (HERE) ;
  macroValidSharedObject (ptr, cTokenFor_logo_5F_lexique) ;
  GALGAS_location currentLocation (ptr->mStartLocation, ptr->mEndLocation, sourceText ()) ;
  GALGAS_string value (ptr->mLexicalAttribute_tokenString) ;
  GALGAS_lstring result (value, currentLocation) ;
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_luint C_Lexique_logo_5F_lexique::synthetizedAttribute_uint_33__32_value (void) const {
  cTokenFor_logo_5F_lexique * ptr = (cTokenFor_logo_5F_lexique *) currentTokenPtr (HERE) ;
  macroValidSharedObject (ptr, cTokenFor_logo_5F_lexique) ;
  GALGAS_location currentLocation (ptr->mStartLocation, ptr->mEndLocation, sourceText ()) ;
  GALGAS_uint value (ptr->mLexicalAttribute_uint_33__32_value) ;
  GALGAS_luint result (value, currentLocation) ;
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//                         I N T R O S P E C T I O N                                                                   *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_stringlist C_Lexique_logo_5F_lexique::symbols (LOCATION_ARGS) {
  GALGAS_stringlist result = GALGAS_stringlist::constructor_emptyList (THERE) ;
  result.addAssign_operation (GALGAS_string ("identifier") COMMA_THERE) ;
  result.addAssign_operation (GALGAS_string ("integer") COMMA_THERE) ;
  result.addAssign_operation (GALGAS_string ("\"string\"") COMMA_THERE) ;
  result.addAssign_operation (GALGAS_string ("comment") COMMA_THERE) ;
  result.addAssign_operation (GALGAS_string ("PROGRAM") COMMA_THERE) ;
  result.addAssign_operation (GALGAS_string ("ROUTINE") COMMA_THERE) ;
  result.addAssign_operation (GALGAS_string ("BEGIN") COMMA_THERE) ;
  result.addAssign_operation (GALGAS_string ("END") COMMA_THERE) ;
  result.addAssign_operation (GALGAS_string ("FORWARD") COMMA_THERE) ;
  result.addAssign_operation (GALGAS_string ("ROTATE") COMMA_THERE) ;
  result.addAssign_operation (GALGAS_string ("PEN") COMMA_THERE) ;
  result.addAssign_operation (GALGAS_string ("UP") COMMA_THERE) ;
  result.addAssign_operation (GALGAS_string ("DOWN") COMMA_THERE) ;
  result.addAssign_operation (GALGAS_string ("CALL") COMMA_THERE) ;
  result.addAssign_operation (GALGAS_string (".") COMMA_THERE) ;
  result.addAssign_operation (GALGAS_string (";") COMMA_THERE) ;
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

static void getKeywordLists_logo_5F_lexique (TC_UniqueArray <C_String> & ioList) {
  ioList.appendObject ("logo_lexique:delimitorsList") ;
  ioList.appendObject ("logo_lexique:keyWordList") ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

static void getKeywordsForIdentifier_logo_5F_lexique (const C_String & inIdentifier,
                                                      bool & ioFound,
                                                      TC_UniqueArray <C_String> & ioList) {
  if (inIdentifier == "logo_lexique:delimitorsList") {
    ioFound = true ;
    ioList.appendObject (".") ;
    ioList.appendObject (";") ;
    ioList.sortArrayUsingCompareMethod() ;
  }
  if (inIdentifier == "logo_lexique:keyWordList") {
    ioFound = true ;
    ioList.appendObject ("UP") ;
    ioList.appendObject ("END") ;
    ioList.appendObject ("PEN") ;
    ioList.appendObject ("CALL") ;
    ioList.appendObject ("DOWN") ;
    ioList.appendObject ("BEGIN") ;
    ioList.appendObject ("ROTATE") ;
    ioList.appendObject ("FORWARD") ;
    ioList.appendObject ("PROGRAM") ;
    ioList.appendObject ("ROUTINE") ;
    ioList.sortArrayUsingCompareMethod() ;
  }
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

static cLexiqueIntrospection lexiqueIntrospection_logo_5F_lexique
__attribute__ ((used))
__attribute__ ((unused)) (getKeywordLists_logo_5F_lexique, getKeywordsForIdentifier_logo_5F_lexique) ;

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//   S T Y L E   I N D E X    F O R    T E R M I N A L                                                                 *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

uint32_t C_Lexique_logo_5F_lexique::styleIndexForTerminal (const int32_t inTerminalIndex) const {
  static const uint32_t kTerminalSymbolStyles [17] = {0,
    0 /* logo_lexique_1_identifier */,
    2 /* logo_lexique_1_integer */,
    3 /* logo_lexique_1__22_string_22_ */,
    5 /* logo_lexique_1_comment */,
    1 /* logo_lexique_1_PROGRAM */,
    1 /* logo_lexique_1_ROUTINE */,
    1 /* logo_lexique_1_BEGIN */,
    1 /* logo_lexique_1_END */,
    1 /* logo_lexique_1_FORWARD */,
    1 /* logo_lexique_1_ROTATE */,
    1 /* logo_lexique_1_PEN */,
    1 /* logo_lexique_1_UP */,
    1 /* logo_lexique_1_DOWN */,
    1 /* logo_lexique_1_CALL */,
    4 /* logo_lexique_1__2E_ */,
    4 /* logo_lexique_1__3B_ */
  } ;
  return (inTerminalIndex >= 0) ? kTerminalSymbolStyles [inTerminalIndex] : 0 ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//   S T Y L E   N A M E    F O R    S T Y L E    I N D E X                                                            *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

C_String C_Lexique_logo_5F_lexique::styleNameForIndex (const uint32_t inStyleIndex) const {
  C_String result ;
  if (inStyleIndex < 6) {
    static const char * kStyleArray [6] = {
      "",
      "keywordsStyle",
      "integerStyle",
      "stringStyle",
      "delimitersStyle",
      "commentStyle"
    } ;
    result = kStyleArray [inStyleIndex] ;
  }
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

cMapElement_routineMap::cMapElement_routineMap (const GALGAS_lstring & inKey,
                                                const GALGAS_instructionList & in_mInstructionList
                                                COMMA_LOCATION_ARGS) :
cMapElement (inKey COMMA_THERE),
mProperty_mInstructionList (in_mInstructionList) {
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

bool cMapElement_routineMap::isValid (void) const {
  return mProperty_lkey.isValid () && mProperty_mInstructionList.isValid () ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

cMapElement * cMapElement_routineMap::copy (void) {
  cMapElement * result = NULL ;
  macroMyNew (result, cMapElement_routineMap (mProperty_lkey, mProperty_mInstructionList COMMA_HERE)) ;
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

void cMapElement_routineMap::description (C_String & ioString, const int32_t inIndentation) const {
  ioString << "\n" ;
  ioString.writeStringMultiple ("| ", inIndentation) ;
  ioString << "mInstructionList" ":" ;
  mProperty_mInstructionList.description (ioString, inIndentation) ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

typeComparisonResult cMapElement_routineMap::compare (const cCollectionElement * inOperand) const {
  cMapElement_routineMap * operand = (cMapElement_routineMap *) inOperand ;
  typeComparisonResult result = mProperty_lkey.objectCompare (operand->mProperty_lkey) ;
  if (kOperandEqual == result) {
    result = mProperty_mInstructionList.objectCompare (operand->mProperty_mInstructionList) ;
  }
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_routineMap::GALGAS_routineMap (void) :
AC_GALGAS_map () {
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_routineMap::GALGAS_routineMap (const GALGAS_routineMap & inSource) :
AC_GALGAS_map (inSource) {
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_routineMap & GALGAS_routineMap::operator = (const GALGAS_routineMap & inSource) {
  * ((AC_GALGAS_map *) this) = inSource ;
  return * this ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_routineMap GALGAS_routineMap::constructor_emptyMap (LOCATION_ARGS) {
  GALGAS_routineMap result ;
  result.makeNewEmptyMap (THERE) ;
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_routineMap GALGAS_routineMap::constructor_mapWithMapToOverride (const GALGAS_routineMap & inMapToOverride
                                                                       COMMA_LOCATION_ARGS) {
  GALGAS_routineMap result ;
  result.makeNewEmptyMapWithMapToOverride (inMapToOverride COMMA_THERE) ;
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_routineMap GALGAS_routineMap::getter_overriddenMap (C_Compiler * inCompiler
                                                           COMMA_LOCATION_ARGS) const {
  GALGAS_routineMap result ;
  getOverridenMap (result, inCompiler COMMA_THERE) ;
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

void GALGAS_routineMap::addAssign_operation (const GALGAS_lstring & inKey,
                                             const GALGAS_instructionList & inArgument0,
                                             C_Compiler * inCompiler
                                             COMMA_LOCATION_ARGS) {
  cMapElement_routineMap * p = NULL ;
  macroMyNew (p, cMapElement_routineMap (inKey, inArgument0 COMMA_HERE)) ;
  capCollectionElement attributes ;
  attributes.setPointer (p) ;
  macroDetachSharedObject (p) ;
  const char * kInsertErrorMessage = "@routineMap insert error: '%K' already in map" ;
  const char * kShadowErrorMessage = "" ;
  performInsert (attributes, inCompiler, kInsertErrorMessage, kShadowErrorMessage COMMA_THERE) ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

void GALGAS_routineMap::setter_insertKey (GALGAS_lstring inKey,
                                          GALGAS_instructionList inArgument0,
                                          C_Compiler * inCompiler
                                          COMMA_LOCATION_ARGS) {
  cMapElement_routineMap * p = NULL ;
  macroMyNew (p, cMapElement_routineMap (inKey, inArgument0 COMMA_HERE)) ;
  capCollectionElement attributes ;
  attributes.setPointer (p) ;
  macroDetachSharedObject (p) ;
  const char * kInsertErrorMessage = "the '%K' routine has been already declared" ;
  const char * kShadowErrorMessage = "" ;
  performInsert (attributes, inCompiler, kInsertErrorMessage, kShadowErrorMessage COMMA_THERE) ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

const char * kSearchErrorMessage_routineMap_searchKey = "the '%K' routine is not declared" ;

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

void GALGAS_routineMap::method_searchKey (GALGAS_lstring inKey,
                                          GALGAS_instructionList & outArgument0,
                                          C_Compiler * inCompiler
                                          COMMA_LOCATION_ARGS) const {
  const cMapElement_routineMap * p = (const cMapElement_routineMap *) performSearch (inKey,
                                                                                     inCompiler,
                                                                                     kSearchErrorMessage_routineMap_searchKey
                                                                                     COMMA_THERE) ;
  if (NULL == p) {
    outArgument0.drop () ;
  }else{
    macroValidSharedObject (p, cMapElement_routineMap) ;
    outArgument0 = p->mProperty_mInstructionList ;
  }
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_instructionList GALGAS_routineMap::getter_mInstructionListForKey (const GALGAS_string & inKey,
                                                                         C_Compiler * inCompiler
                                                                         COMMA_LOCATION_ARGS) const {
  const cCollectionElement * attributes = searchForReadingAttribute (inKey, inCompiler COMMA_THERE) ;
  const cMapElement_routineMap * p = (const cMapElement_routineMap *) attributes ;
  GALGAS_instructionList result ;
  if (NULL != p) {
    macroValidSharedObject (p, cMapElement_routineMap) ;
    result = p->mProperty_mInstructionList ;
  }
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

void GALGAS_routineMap::setter_setMInstructionListForKey (GALGAS_instructionList inAttributeValue,
                                                          GALGAS_string inKey,
                                                          C_Compiler * inCompiler
                                                          COMMA_LOCATION_ARGS) {
  cCollectionElement * attributes = searchForReadWriteAttribute (inKey, true, inCompiler COMMA_THERE) ;
  cMapElement_routineMap * p = (cMapElement_routineMap *) attributes ;
  if (NULL != p) {
    macroValidSharedObject (p, cMapElement_routineMap) ;
    p->mProperty_mInstructionList = inAttributeValue ;
  }
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

cMapElement_routineMap * GALGAS_routineMap::readWriteAccessForWithInstruction (C_Compiler * inCompiler,
                                                                               const GALGAS_string & inKey
                                                                               COMMA_LOCATION_ARGS) {
  cMapElement_routineMap * result = (cMapElement_routineMap *) searchForReadWriteAttribute (inKey, false, inCompiler COMMA_THERE) ;
  macroNullOrValidSharedObject (result, cMapElement_routineMap) ;
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

cEnumerator_routineMap::cEnumerator_routineMap (const GALGAS_routineMap & inEnumeratedObject,
                                                const typeEnumerationOrder inOrder) :
cGenericAbstractEnumerator (inOrder) {
  inEnumeratedObject.populateEnumerationArray (mEnumerationArray) ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_routineMap_2D_element cEnumerator_routineMap::current (LOCATION_ARGS) const {
  const cMapElement_routineMap * p = (const cMapElement_routineMap *) currentObjectPtr (THERE) ;
  macroValidSharedObject (p, cMapElement_routineMap) ;
  return GALGAS_routineMap_2D_element (p->mProperty_lkey, p->mProperty_mInstructionList) ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_lstring cEnumerator_routineMap::current_lkey (LOCATION_ARGS) const {
  const cMapElement * p = (const cMapElement *) currentObjectPtr (THERE) ;
  macroValidSharedObject (p, cMapElement) ;
  return p->mProperty_lkey ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_instructionList cEnumerator_routineMap::current_mInstructionList (LOCATION_ARGS) const {
  const cMapElement_routineMap * p = (const cMapElement_routineMap *) currentObjectPtr (THERE) ;
  macroValidSharedObject (p, cMapElement_routineMap) ;
  return p->mProperty_mInstructionList ;
}



//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//                                                                                                                     *
//                                                  @routineMap type                                                   *
//                                                                                                                     *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

const C_galgas_type_descriptor
kTypeDescriptor_GALGAS_routineMap ("routineMap",
                                   NULL) ;

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

const C_galgas_type_descriptor * GALGAS_routineMap::staticTypeDescriptor (void) const {
  return & kTypeDescriptor_GALGAS_routineMap ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

AC_GALGAS_root * GALGAS_routineMap::clonedObject (void) const {
  AC_GALGAS_root * result = NULL ;
  if (isValid ()) {
    macroMyNew (result, GALGAS_routineMap (*this)) ;
  }
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_routineMap GALGAS_routineMap::extractObject (const GALGAS_object & inObject,
                                                    C_Compiler * inCompiler
                                                    COMMA_LOCATION_ARGS) {
  GALGAS_routineMap result ;
  const GALGAS_routineMap * p = (const GALGAS_routineMap *) inObject.embeddedObject () ;
  if (NULL != p) {
    if (NULL != dynamic_cast <const GALGAS_routineMap *> (p)) {
      result = *p ;
    }else{
      inCompiler->castError ("routineMap", p->dynamicTypeDescriptor () COMMA_THERE) ;
    }  
  }
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//                                                                                                                     *
//                                    Class for element of '@instructionList' list                                     *
//                                                                                                                     *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

class cCollectionElement_instructionList : public cCollectionElement {
  public : GALGAS_instructionList_2D_element mObject ;

//--- Constructors
  public : cCollectionElement_instructionList (const GALGAS_instruction & in_mInstruction
                                               COMMA_LOCATION_ARGS) ;
  public : cCollectionElement_instructionList (const GALGAS_instructionList_2D_element & inElement COMMA_LOCATION_ARGS) ;

//--- Virtual method for comparing elements
  public : virtual typeComparisonResult compare (const cCollectionElement * inOperand) const ;

//--- Virtual method that checks that all attributes are valid
  public : virtual bool isValid (void) const ;

//--- Virtual method that returns a copy of current object
  public : virtual cCollectionElement * copy (void) ;

//--- Description
  public : virtual void description (C_String & ioString, const int32_t inIndentation) const ;
} ;

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

cCollectionElement_instructionList::cCollectionElement_instructionList (const GALGAS_instruction & in_mInstruction
                                                                        COMMA_LOCATION_ARGS) :
cCollectionElement (THERE),
mObject (in_mInstruction) {
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

cCollectionElement_instructionList::cCollectionElement_instructionList (const GALGAS_instructionList_2D_element & inElement COMMA_LOCATION_ARGS) :
cCollectionElement (THERE),
mObject (inElement.mProperty_mInstruction) {
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

bool cCollectionElement_instructionList::isValid (void) const {
  return mObject.isValid () ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

cCollectionElement * cCollectionElement_instructionList::copy (void) {
  cCollectionElement * result = NULL ;
  macroMyNew (result, cCollectionElement_instructionList (mObject.mProperty_mInstruction COMMA_HERE)) ;
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

void cCollectionElement_instructionList::description (C_String & ioString, const int32_t inIndentation) const {
  ioString << "\n" ;
  ioString.writeStringMultiple ("| ", inIndentation) ;
  ioString << "mInstruction" ":" ;
  mObject.mProperty_mInstruction.description (ioString, inIndentation) ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

typeComparisonResult cCollectionElement_instructionList::compare (const cCollectionElement * inOperand) const {
  cCollectionElement_instructionList * operand = (cCollectionElement_instructionList *) inOperand ;
  macroValidSharedObject (operand, cCollectionElement_instructionList) ;
  return mObject.objectCompare (operand->mObject) ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_instructionList::GALGAS_instructionList (void) :
AC_GALGAS_list () {
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_instructionList::GALGAS_instructionList (const capCollectionElementArray & inSharedArray) :
AC_GALGAS_list (inSharedArray) {
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_instructionList GALGAS_instructionList::constructor_emptyList (UNUSED_LOCATION_ARGS) {
  return GALGAS_instructionList  (capCollectionElementArray ()) ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_instructionList GALGAS_instructionList::constructor_listWithValue (const GALGAS_instruction & inOperand0
                                                                          COMMA_LOCATION_ARGS) {
  GALGAS_instructionList result ;
  if (inOperand0.isValid ()) {
    result = GALGAS_instructionList (capCollectionElementArray ()) ;
    capCollectionElement attributes ;
    GALGAS_instructionList::makeAttributesFromObjects (attributes, inOperand0 COMMA_THERE) ;
    result.appendObject (attributes) ;
  }
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

void GALGAS_instructionList::makeAttributesFromObjects (capCollectionElement & outAttributes,
                                                        const GALGAS_instruction & in_mInstruction
                                                        COMMA_LOCATION_ARGS) {
  cCollectionElement_instructionList * p = NULL ;
  macroMyNew (p, cCollectionElement_instructionList (in_mInstruction COMMA_THERE)) ;
  outAttributes.setPointer (p) ;
  macroDetachSharedObject (p) ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

void GALGAS_instructionList::addAssign_operation (const GALGAS_instruction & inOperand0
                                                  COMMA_LOCATION_ARGS) {
  if (isValid () && inOperand0.isValid ()) {
    cCollectionElement * p = NULL ;
    macroMyNew (p, cCollectionElement_instructionList (inOperand0 COMMA_THERE)) ;
    capCollectionElement attributes ;
    attributes.setPointer (p) ;
    macroDetachSharedObject (p) ;
    appendObject (attributes) ;
  }
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

void GALGAS_instructionList::setter_append (GALGAS_instructionList_2D_element inElement,
                                            C_Compiler * /* inCompiler */
                                            COMMA_LOCATION_ARGS) {
  if (isValid () && inElement.isValid ()) {
    cCollectionElement * p = NULL ;
    macroMyNew (p, cCollectionElement_instructionList (inElement COMMA_THERE)) ;
    capCollectionElement attributes ;
    attributes.setPointer (p) ;
    macroDetachSharedObject (p) ;
    appendObject (attributes) ;
  }
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

void GALGAS_instructionList::setter_insertAtIndex (const GALGAS_instruction inOperand0,
                                                   const GALGAS_uint inInsertionIndex,
                                                   C_Compiler * inCompiler
                                                   COMMA_LOCATION_ARGS) {
  if (isValid () && inInsertionIndex.isValid () && inOperand0.isValid ()) {
    cCollectionElement * p = NULL ;
    macroMyNew (p, cCollectionElement_instructionList (inOperand0 COMMA_THERE)) ;
    capCollectionElement attributes ;
    attributes.setPointer (p) ;
    macroDetachSharedObject (p) ;
    insertObjectAtIndex (attributes, inInsertionIndex.uintValue (), inCompiler COMMA_THERE) ;
  }
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

void GALGAS_instructionList::setter_removeAtIndex (GALGAS_instruction & outOperand0,
                                                   const GALGAS_uint inRemoveIndex,
                                                   C_Compiler * inCompiler
                                                   COMMA_LOCATION_ARGS) {
  if (isValid () && inRemoveIndex.isValid ()) {
    capCollectionElement attributes ;
    removeObjectAtIndex (attributes, inRemoveIndex.uintValue (), inCompiler COMMA_THERE) ;
    cCollectionElement_instructionList * p = (cCollectionElement_instructionList *) attributes.ptr () ;
    if (NULL == p) {
      outOperand0.drop () ;
    }else{
      macroValidSharedObject (p, cCollectionElement_instructionList) ;
      outOperand0 = p->mObject.mProperty_mInstruction ;
    }
  }
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

void GALGAS_instructionList::setter_popFirst (GALGAS_instruction & outOperand0,
                                              C_Compiler * inCompiler
                                              COMMA_LOCATION_ARGS) {
  capCollectionElement attributes ;
  removeFirstObject (attributes, inCompiler COMMA_THERE) ;
  cCollectionElement_instructionList * p = (cCollectionElement_instructionList *) attributes.ptr () ;
  if (NULL == p) {
    outOperand0.drop () ;
  }else{
    macroValidSharedObject (p, cCollectionElement_instructionList) ;
    outOperand0 = p->mObject.mProperty_mInstruction ;
  }
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

void GALGAS_instructionList::setter_popLast (GALGAS_instruction & outOperand0,
                                             C_Compiler * inCompiler
                                             COMMA_LOCATION_ARGS) {
  capCollectionElement attributes ;
  removeLastObject (attributes, inCompiler COMMA_THERE) ;
  cCollectionElement_instructionList * p = (cCollectionElement_instructionList *) attributes.ptr () ;
  if (NULL == p) {
    outOperand0.drop () ;
  }else{
    macroValidSharedObject (p, cCollectionElement_instructionList) ;
    outOperand0 = p->mObject.mProperty_mInstruction ;
  }
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

void GALGAS_instructionList::method_first (GALGAS_instruction & outOperand0,
                                           C_Compiler * inCompiler
                                           COMMA_LOCATION_ARGS) const {
  capCollectionElement attributes ;
  readFirst (attributes, inCompiler COMMA_THERE) ;
  cCollectionElement_instructionList * p = (cCollectionElement_instructionList *) attributes.ptr () ;
  if (NULL == p) {
    outOperand0.drop () ;
  }else{
    macroValidSharedObject (p, cCollectionElement_instructionList) ;
    outOperand0 = p->mObject.mProperty_mInstruction ;
  }
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

void GALGAS_instructionList::method_last (GALGAS_instruction & outOperand0,
                                          C_Compiler * inCompiler
                                          COMMA_LOCATION_ARGS) const {
  capCollectionElement attributes ;
  readLast (attributes, inCompiler COMMA_THERE) ;
  cCollectionElement_instructionList * p = (cCollectionElement_instructionList *) attributes.ptr () ;
  if (NULL == p) {
    outOperand0.drop () ;
  }else{
    macroValidSharedObject (p, cCollectionElement_instructionList) ;
    outOperand0 = p->mObject.mProperty_mInstruction ;
  }
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_instructionList GALGAS_instructionList::add_operation (const GALGAS_instructionList & inOperand,
                                                              C_Compiler * /* inCompiler */
                                                              COMMA_UNUSED_LOCATION_ARGS) const {
  GALGAS_instructionList result ;
  if (isValid () && inOperand.isValid ()) {
    result = *this ;
    result.appendList (inOperand) ;
  }
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_instructionList GALGAS_instructionList::getter_subListWithRange (const GALGAS_range & inRange,
                                                                        C_Compiler * inCompiler
                                                                        COMMA_LOCATION_ARGS) const {
  GALGAS_instructionList result = GALGAS_instructionList::constructor_emptyList (THERE) ;
  subListWithRange (result, inRange, inCompiler COMMA_THERE) ;
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_instructionList GALGAS_instructionList::getter_subListFromIndex (const GALGAS_uint & inIndex,
                                                                        C_Compiler * inCompiler
                                                                        COMMA_LOCATION_ARGS) const {
  GALGAS_instructionList result = GALGAS_instructionList::constructor_emptyList (THERE) ;
  subListFromIndex (result, inIndex, inCompiler COMMA_THERE) ;
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_instructionList GALGAS_instructionList::getter_subListToIndex (const GALGAS_uint & inIndex,
                                                                      C_Compiler * inCompiler
                                                                      COMMA_LOCATION_ARGS) const {
  GALGAS_instructionList result = GALGAS_instructionList::constructor_emptyList (THERE) ;
  subListToIndex (result, inIndex, inCompiler COMMA_THERE) ;
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

void GALGAS_instructionList::plusAssign_operation (const GALGAS_instructionList inOperand,
                                                   C_Compiler * /* inCompiler */
                                                   COMMA_UNUSED_LOCATION_ARGS) {
  appendList (inOperand) ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

void GALGAS_instructionList::setter_setMInstructionAtIndex (GALGAS_instruction inOperand,
                                                            GALGAS_uint inIndex,
                                                            C_Compiler * inCompiler
                                                            COMMA_LOCATION_ARGS) {
  cCollectionElement_instructionList * p = (cCollectionElement_instructionList *) uniquelyReferencedPointerAtIndex (inIndex, inCompiler COMMA_THERE) ;
  if (NULL != p) {
    macroValidSharedObject (p, cCollectionElement_instructionList) ;
    macroUniqueSharedObject (p) ;
    p->mObject.mProperty_mInstruction = inOperand ;
  }
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_instruction GALGAS_instructionList::getter_mInstructionAtIndex (const GALGAS_uint & inIndex,
                                                                       C_Compiler * inCompiler
                                                                       COMMA_LOCATION_ARGS) const {
  capCollectionElement attributes = readObjectAtIndex (inIndex, inCompiler COMMA_THERE) ;
  cCollectionElement_instructionList * p = (cCollectionElement_instructionList *) attributes.ptr () ;
  GALGAS_instruction result ;
  if (NULL != p) {
    macroValidSharedObject (p, cCollectionElement_instructionList) ;
    result = p->mObject.mProperty_mInstruction ;
  }
  return result ;
}



//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

cEnumerator_instructionList::cEnumerator_instructionList (const GALGAS_instructionList & inEnumeratedObject,
                                                          const typeEnumerationOrder inOrder) :
cGenericAbstractEnumerator (inOrder) {
  inEnumeratedObject.populateEnumerationArray (mEnumerationArray) ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_instructionList_2D_element cEnumerator_instructionList::current (LOCATION_ARGS) const {
  const cCollectionElement_instructionList * p = (const cCollectionElement_instructionList *) currentObjectPtr (THERE) ;
  macroValidSharedObject (p, cCollectionElement_instructionList) ;
  return p->mObject ;
}


//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_instruction cEnumerator_instructionList::current_mInstruction (LOCATION_ARGS) const {
  const cCollectionElement_instructionList * p = (const cCollectionElement_instructionList *) currentObjectPtr (THERE) ;
  macroValidSharedObject (p, cCollectionElement_instructionList) ;
  return p->mObject.mProperty_mInstruction ;
}




//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//                                                                                                                     *
//                                                @instructionList type                                                *
//                                                                                                                     *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

const C_galgas_type_descriptor
kTypeDescriptor_GALGAS_instructionList ("instructionList",
                                        NULL) ;

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

const C_galgas_type_descriptor * GALGAS_instructionList::staticTypeDescriptor (void) const {
  return & kTypeDescriptor_GALGAS_instructionList ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

AC_GALGAS_root * GALGAS_instructionList::clonedObject (void) const {
  AC_GALGAS_root * result = NULL ;
  if (isValid ()) {
    macroMyNew (result, GALGAS_instructionList (*this)) ;
  }
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_instructionList GALGAS_instructionList::extractObject (const GALGAS_object & inObject,
                                                              C_Compiler * inCompiler
                                                              COMMA_LOCATION_ARGS) {
  GALGAS_instructionList result ;
  const GALGAS_instructionList * p = (const GALGAS_instructionList *) inObject.embeddedObject () ;
  if (NULL != p) {
    if (NULL != dynamic_cast <const GALGAS_instructionList *> (p)) {
      result = *p ;
    }else{
      inCompiler->castError ("instructionList", p->dynamicTypeDescriptor () COMMA_THERE) ;
    }  
  }
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//   Object comparison                                                                                                 *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*



typeComparisonResult GALGAS_instruction::objectCompare (const GALGAS_instruction & inOperand) const {
  typeComparisonResult result = kOperandNotValid ;
  if (isValid () && inOperand.isValid ()) {
    const int32_t mySlot = mObjectPtr->classDescriptor ()->mSlotID ;
    const int32_t operandSlot = inOperand.mObjectPtr->classDescriptor ()->mSlotID ;
    if (mySlot < operandSlot) {
      result = kFirstOperandLowerThanSecond ;
    }else if (mySlot > operandSlot) {
      result = kFirstOperandGreaterThanSecond ;
    }else{
      result = mObjectPtr->dynamicObjectCompare (inOperand.mObjectPtr) ;
    }
  }
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_instruction::GALGAS_instruction (void) :
AC_GALGAS_class (false) {
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_instruction::GALGAS_instruction (const cPtr_instruction * inSourcePtr) :
AC_GALGAS_class (inSourcePtr, false) {
  macroNullOrValidSharedObject (inSourcePtr, cPtr_instruction) ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//                                        Pointer class for @instruction class                                         *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

cPtr_instruction::cPtr_instruction (LOCATION_ARGS) :
acPtr_class (THERE) {
}


//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//                                                                                                                     *
//                                                  @instruction type                                                  *
//                                                                                                                     *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

const C_galgas_type_descriptor
kTypeDescriptor_GALGAS_instruction ("instruction",
                                    NULL) ;

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

const C_galgas_type_descriptor * GALGAS_instruction::staticTypeDescriptor (void) const {
  return & kTypeDescriptor_GALGAS_instruction ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

AC_GALGAS_root * GALGAS_instruction::clonedObject (void) const {
  AC_GALGAS_root * result = NULL ;
  if (isValid ()) {
    macroMyNew (result, GALGAS_instruction (*this)) ;
  }
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_instruction GALGAS_instruction::extractObject (const GALGAS_object & inObject,
                                                      C_Compiler * inCompiler
                                                      COMMA_LOCATION_ARGS) {
  GALGAS_instruction result ;
  const GALGAS_instruction * p = (const GALGAS_instruction *) inObject.embeddedObject () ;
  if (NULL != p) {
    if (NULL != dynamic_cast <const GALGAS_instruction *> (p)) {
      result = *p ;
    }else{
      inCompiler->castError ("instruction", p->dynamicTypeDescriptor () COMMA_THERE) ;
    }  
  }
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//   Object comparison                                                                                                 *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

typeComparisonResult cPtr_penUp::dynamicObjectCompare (const acPtr_class * /* inOperandPtr */) const {
  return kOperandEqual ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*


typeComparisonResult GALGAS_penUp::objectCompare (const GALGAS_penUp & inOperand) const {
  typeComparisonResult result = kOperandNotValid ;
  if (isValid () && inOperand.isValid ()) {
    const int32_t mySlot = mObjectPtr->classDescriptor ()->mSlotID ;
    const int32_t operandSlot = inOperand.mObjectPtr->classDescriptor ()->mSlotID ;
    if (mySlot < operandSlot) {
      result = kFirstOperandLowerThanSecond ;
    }else if (mySlot > operandSlot) {
      result = kFirstOperandGreaterThanSecond ;
    }else{
      result = mObjectPtr->dynamicObjectCompare (inOperand.mObjectPtr) ;
    }
  }
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_penUp::GALGAS_penUp (void) :
GALGAS_instruction () {
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_penUp GALGAS_penUp::constructor_default (LOCATION_ARGS) {
  return GALGAS_penUp::constructor_new (THERE) ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_penUp::GALGAS_penUp (const cPtr_penUp * inSourcePtr) :
GALGAS_instruction (inSourcePtr) {
  macroNullOrValidSharedObject (inSourcePtr, cPtr_penUp) ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_penUp GALGAS_penUp::constructor_new (LOCATION_ARGS) {
  GALGAS_penUp result ;
  macroMyNew (result.mObjectPtr, cPtr_penUp (THERE)) ;
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//                                           Pointer class for @penUp class                                            *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

cPtr_penUp::cPtr_penUp (LOCATION_ARGS) :
cPtr_instruction (THERE) {
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

const C_galgas_type_descriptor * cPtr_penUp::classDescriptor (void) const {
  return & kTypeDescriptor_GALGAS_penUp ;
}

void cPtr_penUp::description (C_String & ioString,
                              const int32_t /* inIndentation */) const {
  ioString << "[@penUp]" ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

acPtr_class * cPtr_penUp::duplicate (LOCATION_ARGS) const {
  acPtr_class * ptr = NULL ;
  macroMyNew (ptr, cPtr_penUp (THERE)) ;
  return ptr ;
}


//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//                                                                                                                     *
//                                                     @penUp type                                                     *
//                                                                                                                     *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

const C_galgas_type_descriptor
kTypeDescriptor_GALGAS_penUp ("penUp",
                              & kTypeDescriptor_GALGAS_instruction) ;

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

const C_galgas_type_descriptor * GALGAS_penUp::staticTypeDescriptor (void) const {
  return & kTypeDescriptor_GALGAS_penUp ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

AC_GALGAS_root * GALGAS_penUp::clonedObject (void) const {
  AC_GALGAS_root * result = NULL ;
  if (isValid ()) {
    macroMyNew (result, GALGAS_penUp (*this)) ;
  }
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_penUp GALGAS_penUp::extractObject (const GALGAS_object & inObject,
                                          C_Compiler * inCompiler
                                          COMMA_LOCATION_ARGS) {
  GALGAS_penUp result ;
  const GALGAS_penUp * p = (const GALGAS_penUp *) inObject.embeddedObject () ;
  if (NULL != p) {
    if (NULL != dynamic_cast <const GALGAS_penUp *> (p)) {
      result = *p ;
    }else{
      inCompiler->castError ("penUp", p->dynamicTypeDescriptor () COMMA_THERE) ;
    }  
  }
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//   Object comparison                                                                                                 *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

typeComparisonResult cPtr_penDown::dynamicObjectCompare (const acPtr_class * /* inOperandPtr */) const {
  return kOperandEqual ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*


typeComparisonResult GALGAS_penDown::objectCompare (const GALGAS_penDown & inOperand) const {
  typeComparisonResult result = kOperandNotValid ;
  if (isValid () && inOperand.isValid ()) {
    const int32_t mySlot = mObjectPtr->classDescriptor ()->mSlotID ;
    const int32_t operandSlot = inOperand.mObjectPtr->classDescriptor ()->mSlotID ;
    if (mySlot < operandSlot) {
      result = kFirstOperandLowerThanSecond ;
    }else if (mySlot > operandSlot) {
      result = kFirstOperandGreaterThanSecond ;
    }else{
      result = mObjectPtr->dynamicObjectCompare (inOperand.mObjectPtr) ;
    }
  }
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_penDown::GALGAS_penDown (void) :
GALGAS_instruction () {
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_penDown GALGAS_penDown::constructor_default (LOCATION_ARGS) {
  return GALGAS_penDown::constructor_new (THERE) ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_penDown::GALGAS_penDown (const cPtr_penDown * inSourcePtr) :
GALGAS_instruction (inSourcePtr) {
  macroNullOrValidSharedObject (inSourcePtr, cPtr_penDown) ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_penDown GALGAS_penDown::constructor_new (LOCATION_ARGS) {
  GALGAS_penDown result ;
  macroMyNew (result.mObjectPtr, cPtr_penDown (THERE)) ;
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//                                          Pointer class for @penDown class                                           *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

cPtr_penDown::cPtr_penDown (LOCATION_ARGS) :
cPtr_instruction (THERE) {
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

const C_galgas_type_descriptor * cPtr_penDown::classDescriptor (void) const {
  return & kTypeDescriptor_GALGAS_penDown ;
}

void cPtr_penDown::description (C_String & ioString,
                                const int32_t /* inIndentation */) const {
  ioString << "[@penDown]" ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

acPtr_class * cPtr_penDown::duplicate (LOCATION_ARGS) const {
  acPtr_class * ptr = NULL ;
  macroMyNew (ptr, cPtr_penDown (THERE)) ;
  return ptr ;
}


//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//                                                                                                                     *
//                                                    @penDown type                                                    *
//                                                                                                                     *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

const C_galgas_type_descriptor
kTypeDescriptor_GALGAS_penDown ("penDown",
                                & kTypeDescriptor_GALGAS_instruction) ;

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

const C_galgas_type_descriptor * GALGAS_penDown::staticTypeDescriptor (void) const {
  return & kTypeDescriptor_GALGAS_penDown ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

AC_GALGAS_root * GALGAS_penDown::clonedObject (void) const {
  AC_GALGAS_root * result = NULL ;
  if (isValid ()) {
    macroMyNew (result, GALGAS_penDown (*this)) ;
  }
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_penDown GALGAS_penDown::extractObject (const GALGAS_object & inObject,
                                              C_Compiler * inCompiler
                                              COMMA_LOCATION_ARGS) {
  GALGAS_penDown result ;
  const GALGAS_penDown * p = (const GALGAS_penDown *) inObject.embeddedObject () ;
  if (NULL != p) {
    if (NULL != dynamic_cast <const GALGAS_penDown *> (p)) {
      result = *p ;
    }else{
      inCompiler->castError ("penDown", p->dynamicTypeDescriptor () COMMA_THERE) ;
    }  
  }
  return result ;
}



//---------------------------------------------------------------------------------------------------------------------*

void cParser_logo_5F_syntax::rule_logo_5F_syntax_start_5F_symbol_i0_ (C_Lexique_logo_5F_lexique * inCompiler) {
  inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_PROGRAM COMMA_SOURCE_FILE ("logo-syntax.galgas", 5)) ;
  GALGAS_routineMap var_tableRoutines_160 = GALGAS_routineMap::constructor_emptyMap (SOURCE_FILE ("logo-syntax.galgas", 7)) ;
  GALGAS_instructionList var_maListe_192 = GALGAS_instructionList::constructor_emptyList (SOURCE_FILE ("logo-syntax.galgas", 8)) ;
  bool repeatFlag_0 = true ;
  while (repeatFlag_0) {
    switch (select_logo_5F_syntax_0 (inCompiler)) {
    case 2: {
      nt_routine_5F_definition_ (var_tableRoutines_160, inCompiler) ;
    } break ;
    default:
      repeatFlag_0 = false ;
      break ;
    }
  }
  inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_BEGIN COMMA_SOURCE_FILE ("logo-syntax.galgas", 14)) ;
  nt_instruction_5F_list_ (var_tableRoutines_160, var_maListe_192, inCompiler) ;
  inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_END COMMA_SOURCE_FILE ("logo-syntax.galgas", 16)) ;
  inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken__2E_ COMMA_SOURCE_FILE ("logo-syntax.galgas", 17)) ;
}

//---------------------------------------------------------------------------------------------------------------------*

void cParser_logo_5F_syntax::rule_logo_5F_syntax_start_5F_symbol_i0_parse (C_Lexique_logo_5F_lexique * inCompiler) {
  inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_PROGRAM COMMA_SOURCE_FILE ("logo-syntax.galgas", 5)) ;
  bool repeatFlag_0 = true ;
  while (repeatFlag_0) {
    switch (select_logo_5F_syntax_0 (inCompiler)) {
    case 2: {
      nt_routine_5F_definition_parse (inCompiler) ;
    } break ;
    default:
      repeatFlag_0 = false ;
      break ;
    }
  }
  inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_BEGIN COMMA_SOURCE_FILE ("logo-syntax.galgas", 14)) ;
  nt_instruction_5F_list_parse (inCompiler) ;
  inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_END COMMA_SOURCE_FILE ("logo-syntax.galgas", 16)) ;
  inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken__2E_ COMMA_SOURCE_FILE ("logo-syntax.galgas", 17)) ;
  inCompiler->resetTemplateString () ;
}

//---------------------------------------------------------------------------------------------------------------------*

void cParser_logo_5F_syntax::rule_logo_5F_syntax_routine_5F_definition_i1_ (GALGAS_routineMap & ioArgument_ioTableRoutines,
                                                                            C_Lexique_logo_5F_lexique * inCompiler) {
  GALGAS_instructionList var_routineList_453 = GALGAS_instructionList::constructor_emptyList (SOURCE_FILE ("logo-syntax.galgas", 21)) ;
  inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_ROUTINE COMMA_SOURCE_FILE ("logo-syntax.galgas", 22)) ;
  GALGAS_lstring var_identifier_5F_id_514 = inCompiler->synthetizedAttribute_tokenString () ;
  inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_identifier COMMA_SOURCE_FILE ("logo-syntax.galgas", 23)) ;
  inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_BEGIN COMMA_SOURCE_FILE ("logo-syntax.galgas", 24)) ;
  nt_instruction_5F_list_ (ioArgument_ioTableRoutines, var_routineList_453, inCompiler) ;
  inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_END COMMA_SOURCE_FILE ("logo-syntax.galgas", 26)) ;
  {
  ioArgument_ioTableRoutines.setter_insertKey (var_identifier_5F_id_514, var_routineList_453, inCompiler COMMA_SOURCE_FILE ("logo-syntax.galgas", 27)) ;
  }
}

//---------------------------------------------------------------------------------------------------------------------*

void cParser_logo_5F_syntax::rule_logo_5F_syntax_routine_5F_definition_i1_parse (C_Lexique_logo_5F_lexique * inCompiler) {
  inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_ROUTINE COMMA_SOURCE_FILE ("logo-syntax.galgas", 22)) ;
  inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_identifier COMMA_SOURCE_FILE ("logo-syntax.galgas", 23)) ;
  inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_BEGIN COMMA_SOURCE_FILE ("logo-syntax.galgas", 24)) ;
  nt_instruction_5F_list_parse (inCompiler) ;
  inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_END COMMA_SOURCE_FILE ("logo-syntax.galgas", 26)) ;
  inCompiler->resetTemplateString () ;
}

//---------------------------------------------------------------------------------------------------------------------*

void cParser_logo_5F_syntax::rule_logo_5F_syntax_instruction_5F_list_i2_ (GALGAS_routineMap inArgument_ioTableRoutines,
                                                                          GALGAS_instructionList & ioArgument_maListe,
                                                                          C_Lexique_logo_5F_lexique * inCompiler) {
  bool repeatFlag_0 = true ;
  while (repeatFlag_0) {
    switch (select_logo_5F_syntax_1 (inCompiler)) {
    case 2: {
      nt_instruction_ (inArgument_ioTableRoutines, ioArgument_maListe, inCompiler) ;
    } break ;
    default:
      repeatFlag_0 = false ;
      break ;
    }
  }
}

//---------------------------------------------------------------------------------------------------------------------*

void cParser_logo_5F_syntax::rule_logo_5F_syntax_instruction_5F_list_i2_parse (C_Lexique_logo_5F_lexique * inCompiler) {
  bool repeatFlag_0 = true ;
  while (repeatFlag_0) {
    switch (select_logo_5F_syntax_1 (inCompiler)) {
    case 2: {
      nt_instruction_parse (inCompiler) ;
    } break ;
    default:
      repeatFlag_0 = false ;
      break ;
    }
  }
  inCompiler->resetTemplateString () ;
}

//---------------------------------------------------------------------------------------------------------------------*

void cParser_logo_5F_syntax::rule_logo_5F_syntax_instruction_i3_ (GALGAS_routineMap inArgument_ioTableRoutines,
                                                                  GALGAS_instructionList & ioArgument_maListe,
                                                                  C_Lexique_logo_5F_lexique * inCompiler) {
  switch (select_logo_5F_syntax_2 (inCompiler)) {
  case 1: {
    inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_FORWARD COMMA_SOURCE_FILE ("logo-syntax.galgas", 39)) ;
    GALGAS_luint var_mLength_935 = inCompiler->synthetizedAttribute_uint_33__32_value () ;
    inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_integer COMMA_SOURCE_FILE ("logo-syntax.galgas", 40)) ;
    GALGAS_instruction var_instruction_966 = GALGAS_forward::constructor_new (var_mLength_935  COMMA_SOURCE_FILE ("logo-syntax.galgas", 41)) ;
    ioArgument_maListe.addAssign_operation (var_instruction_966  COMMA_SOURCE_FILE ("logo-syntax.galgas", 42)) ;
  } break ;
  case 2: {
    inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_ROTATE COMMA_SOURCE_FILE ("logo-syntax.galgas", 44)) ;
    GALGAS_luint var_mAngle_1070 = inCompiler->synthetizedAttribute_uint_33__32_value () ;
    inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_integer COMMA_SOURCE_FILE ("logo-syntax.galgas", 45)) ;
    GALGAS_instruction var_instruction_1099 = GALGAS_rotate::constructor_new (var_mAngle_1070  COMMA_SOURCE_FILE ("logo-syntax.galgas", 46)) ;
    ioArgument_maListe.addAssign_operation (var_instruction_1099  COMMA_SOURCE_FILE ("logo-syntax.galgas", 47)) ;
  } break ;
  case 3: {
    inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_PEN COMMA_SOURCE_FILE ("logo-syntax.galgas", 49)) ;
    inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_UP COMMA_SOURCE_FILE ("logo-syntax.galgas", 50)) ;
    inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_integer COMMA_SOURCE_FILE ("logo-syntax.galgas", 51)) ;
    GALGAS_instruction var_instruction_1222 = GALGAS_penUp::constructor_new (SOURCE_FILE ("logo-syntax.galgas", 52)) ;
    ioArgument_maListe.addAssign_operation (var_instruction_1222  COMMA_SOURCE_FILE ("logo-syntax.galgas", 53)) ;
  } break ;
  case 4: {
    inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_PEN COMMA_SOURCE_FILE ("logo-syntax.galgas", 55)) ;
    inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_DOWN COMMA_SOURCE_FILE ("logo-syntax.galgas", 56)) ;
    inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_integer COMMA_SOURCE_FILE ("logo-syntax.galgas", 57)) ;
    GALGAS_instruction var_instruction_1333 = GALGAS_penDown::constructor_new (SOURCE_FILE ("logo-syntax.galgas", 58)) ;
    ioArgument_maListe.addAssign_operation (var_instruction_1333  COMMA_SOURCE_FILE ("logo-syntax.galgas", 59)) ;
  } break ;
  case 5: {
    inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_CALL COMMA_SOURCE_FILE ("logo-syntax.galgas", 61)) ;
    GALGAS_instructionList var_routineList_1426 = GALGAS_instructionList::constructor_emptyList (SOURCE_FILE ("logo-syntax.galgas", 62)) ;
    GALGAS_lstring var_nomIden_1471 = inCompiler->synthetizedAttribute_tokenString () ;
    inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_identifier COMMA_SOURCE_FILE ("logo-syntax.galgas", 63)) ;
    inArgument_ioTableRoutines.method_searchKey (var_nomIden_1471, var_routineList_1426, inCompiler COMMA_SOURCE_FILE ("logo-syntax.galgas", 64)) ;
    cEnumerator_instructionList enumerator_1550 (var_routineList_1426, kENUMERATION_UP) ;
    while (enumerator_1550.hasCurrentObject ()) {
      var_routineList_1426.addAssign_operation (enumerator_1550.current (HERE).getter_mInstruction (HERE)  COMMA_SOURCE_FILE ("logo-syntax.galgas", 66)) ;
      enumerator_1550.gotoNextObject () ;
    }
  } break ;
  default:
    break ;
  }
  inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken__3B_ COMMA_SOURCE_FILE ("logo-syntax.galgas", 69)) ;
}

//---------------------------------------------------------------------------------------------------------------------*

void cParser_logo_5F_syntax::rule_logo_5F_syntax_instruction_i3_parse (C_Lexique_logo_5F_lexique * inCompiler) {
  switch (select_logo_5F_syntax_2 (inCompiler)) {
  case 1: {
    inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_FORWARD COMMA_SOURCE_FILE ("logo-syntax.galgas", 39)) ;
    inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_integer COMMA_SOURCE_FILE ("logo-syntax.galgas", 40)) ;
  } break ;
  case 2: {
    inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_ROTATE COMMA_SOURCE_FILE ("logo-syntax.galgas", 44)) ;
    inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_integer COMMA_SOURCE_FILE ("logo-syntax.galgas", 45)) ;
  } break ;
  case 3: {
    inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_PEN COMMA_SOURCE_FILE ("logo-syntax.galgas", 49)) ;
    inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_UP COMMA_SOURCE_FILE ("logo-syntax.galgas", 50)) ;
    inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_integer COMMA_SOURCE_FILE ("logo-syntax.galgas", 51)) ;
  } break ;
  case 4: {
    inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_PEN COMMA_SOURCE_FILE ("logo-syntax.galgas", 55)) ;
    inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_DOWN COMMA_SOURCE_FILE ("logo-syntax.galgas", 56)) ;
    inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_integer COMMA_SOURCE_FILE ("logo-syntax.galgas", 57)) ;
  } break ;
  case 5: {
    inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_CALL COMMA_SOURCE_FILE ("logo-syntax.galgas", 61)) ;
    inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken_identifier COMMA_SOURCE_FILE ("logo-syntax.galgas", 63)) ;
  } break ;
  default:
    break ;
  }
  inCompiler->acceptTerminal (C_Lexique_logo_5F_lexique::kToken__3B_ COMMA_SOURCE_FILE ("logo-syntax.galgas", 69)) ;
  inCompiler->resetTemplateString () ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_routineMap_2D_element::GALGAS_routineMap_2D_element (void) :
mProperty_lkey (),
mProperty_mInstructionList () {
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_routineMap_2D_element::~ GALGAS_routineMap_2D_element (void) {
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_routineMap_2D_element::GALGAS_routineMap_2D_element (const GALGAS_lstring & inOperand0,
                                                            const GALGAS_instructionList & inOperand1) :
mProperty_lkey (inOperand0),
mProperty_mInstructionList (inOperand1) {
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_routineMap_2D_element GALGAS_routineMap_2D_element::constructor_default (UNUSED_LOCATION_ARGS) {
  return GALGAS_routineMap_2D_element (GALGAS_lstring::constructor_default (HERE),
                                       GALGAS_instructionList::constructor_emptyList (HERE)) ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_routineMap_2D_element GALGAS_routineMap_2D_element::constructor_new (const GALGAS_lstring & inOperand0,
                                                                            const GALGAS_instructionList & inOperand1 
                                                                            COMMA_UNUSED_LOCATION_ARGS) {
  GALGAS_routineMap_2D_element result ;
  if (inOperand0.isValid () && inOperand1.isValid ()) {
    result = GALGAS_routineMap_2D_element (inOperand0, inOperand1) ;
  }
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

typeComparisonResult GALGAS_routineMap_2D_element::objectCompare (const GALGAS_routineMap_2D_element & inOperand) const {
   typeComparisonResult result = kOperandEqual ;
  if (result == kOperandEqual) {
    result = mProperty_lkey.objectCompare (inOperand.mProperty_lkey) ;
  }
  if (result == kOperandEqual) {
    result = mProperty_mInstructionList.objectCompare (inOperand.mProperty_mInstructionList) ;
  }
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

bool GALGAS_routineMap_2D_element::isValid (void) const {
  return mProperty_lkey.isValid () && mProperty_mInstructionList.isValid () ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

void GALGAS_routineMap_2D_element::drop (void) {
  mProperty_lkey.drop () ;
  mProperty_mInstructionList.drop () ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

void GALGAS_routineMap_2D_element::description (C_String & ioString,
                                                const int32_t inIndentation) const {
  ioString << "<struct @routineMap-element:" ;
  if (! isValid ()) {
    ioString << " not built" ;
  }else{
    mProperty_lkey.description (ioString, inIndentation+1) ;
    ioString << ", " ;
    mProperty_mInstructionList.description (ioString, inIndentation+1) ;
  }
  ioString << ">" ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_lstring GALGAS_routineMap_2D_element::getter_lkey (UNUSED_LOCATION_ARGS) const {
  return mProperty_lkey ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_instructionList GALGAS_routineMap_2D_element::getter_mInstructionList (UNUSED_LOCATION_ARGS) const {
  return mProperty_mInstructionList ;
}



//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//                                                                                                                     *
//                                              @routineMap-element type                                               *
//                                                                                                                     *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

const C_galgas_type_descriptor
kTypeDescriptor_GALGAS_routineMap_2D_element ("routineMap-element",
                                              NULL) ;

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

const C_galgas_type_descriptor * GALGAS_routineMap_2D_element::staticTypeDescriptor (void) const {
  return & kTypeDescriptor_GALGAS_routineMap_2D_element ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

AC_GALGAS_root * GALGAS_routineMap_2D_element::clonedObject (void) const {
  AC_GALGAS_root * result = NULL ;
  if (isValid ()) {
    macroMyNew (result, GALGAS_routineMap_2D_element (*this)) ;
  }
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_routineMap_2D_element GALGAS_routineMap_2D_element::extractObject (const GALGAS_object & inObject,
                                                                          C_Compiler * inCompiler
                                                                          COMMA_LOCATION_ARGS) {
  GALGAS_routineMap_2D_element result ;
  const GALGAS_routineMap_2D_element * p = (const GALGAS_routineMap_2D_element *) inObject.embeddedObject () ;
  if (NULL != p) {
    if (NULL != dynamic_cast <const GALGAS_routineMap_2D_element *> (p)) {
      result = *p ;
    }else{
      inCompiler->castError ("routineMap-element", p->dynamicTypeDescriptor () COMMA_THERE) ;
    }  
  }
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_instructionList_2D_element::GALGAS_instructionList_2D_element (void) :
mProperty_mInstruction () {
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_instructionList_2D_element::~ GALGAS_instructionList_2D_element (void) {
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_instructionList_2D_element::GALGAS_instructionList_2D_element (const GALGAS_instruction & inOperand0) :
mProperty_mInstruction (inOperand0) {
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_instructionList_2D_element GALGAS_instructionList_2D_element::constructor_new (const GALGAS_instruction & inOperand0 
                                                                                      COMMA_UNUSED_LOCATION_ARGS) {
  GALGAS_instructionList_2D_element result ;
  if (inOperand0.isValid ()) {
    result = GALGAS_instructionList_2D_element (inOperand0) ;
  }
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

typeComparisonResult GALGAS_instructionList_2D_element::objectCompare (const GALGAS_instructionList_2D_element & inOperand) const {
   typeComparisonResult result = kOperandEqual ;
  if (result == kOperandEqual) {
    result = mProperty_mInstruction.objectCompare (inOperand.mProperty_mInstruction) ;
  }
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

bool GALGAS_instructionList_2D_element::isValid (void) const {
  return mProperty_mInstruction.isValid () ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

void GALGAS_instructionList_2D_element::drop (void) {
  mProperty_mInstruction.drop () ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

void GALGAS_instructionList_2D_element::description (C_String & ioString,
                                                     const int32_t inIndentation) const {
  ioString << "<struct @instructionList-element:" ;
  if (! isValid ()) {
    ioString << " not built" ;
  }else{
    mProperty_mInstruction.description (ioString, inIndentation+1) ;
  }
  ioString << ">" ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_instruction GALGAS_instructionList_2D_element::getter_mInstruction (UNUSED_LOCATION_ARGS) const {
  return mProperty_mInstruction ;
}



//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//                                                                                                                     *
//                                            @instructionList-element type                                            *
//                                                                                                                     *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

const C_galgas_type_descriptor
kTypeDescriptor_GALGAS_instructionList_2D_element ("instructionList-element",
                                                   NULL) ;

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

const C_galgas_type_descriptor * GALGAS_instructionList_2D_element::staticTypeDescriptor (void) const {
  return & kTypeDescriptor_GALGAS_instructionList_2D_element ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

AC_GALGAS_root * GALGAS_instructionList_2D_element::clonedObject (void) const {
  AC_GALGAS_root * result = NULL ;
  if (isValid ()) {
    macroMyNew (result, GALGAS_instructionList_2D_element (*this)) ;
  }
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_instructionList_2D_element GALGAS_instructionList_2D_element::extractObject (const GALGAS_object & inObject,
                                                                                    C_Compiler * inCompiler
                                                                                    COMMA_LOCATION_ARGS) {
  GALGAS_instructionList_2D_element result ;
  const GALGAS_instructionList_2D_element * p = (const GALGAS_instructionList_2D_element *) inObject.embeddedObject () ;
  if (NULL != p) {
    if (NULL != dynamic_cast <const GALGAS_instructionList_2D_element *> (p)) {
      result = *p ;
    }else{
      inCompiler->castError ("instructionList-element", p->dynamicTypeDescriptor () COMMA_THERE) ;
    }  
  }
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//   Object comparison                                                                                                 *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

typeComparisonResult cPtr_rotate::dynamicObjectCompare (const acPtr_class * inOperandPtr) const {
  typeComparisonResult result = kOperandEqual ;
  const cPtr_rotate * p = (const cPtr_rotate *) inOperandPtr ;
  macroValidSharedObject (p, cPtr_rotate) ;
  if (kOperandEqual == result) {
    result = mProperty_mAngle.objectCompare (p->mProperty_mAngle) ;
  }
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*


typeComparisonResult GALGAS_rotate::objectCompare (const GALGAS_rotate & inOperand) const {
  typeComparisonResult result = kOperandNotValid ;
  if (isValid () && inOperand.isValid ()) {
    const int32_t mySlot = mObjectPtr->classDescriptor ()->mSlotID ;
    const int32_t operandSlot = inOperand.mObjectPtr->classDescriptor ()->mSlotID ;
    if (mySlot < operandSlot) {
      result = kFirstOperandLowerThanSecond ;
    }else if (mySlot > operandSlot) {
      result = kFirstOperandGreaterThanSecond ;
    }else{
      result = mObjectPtr->dynamicObjectCompare (inOperand.mObjectPtr) ;
    }
  }
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_rotate::GALGAS_rotate (void) :
GALGAS_instruction () {
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_rotate GALGAS_rotate::constructor_default (LOCATION_ARGS) {
  return GALGAS_rotate::constructor_new (GALGAS_luint::constructor_default (HERE)
                                         COMMA_THERE) ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_rotate::GALGAS_rotate (const cPtr_rotate * inSourcePtr) :
GALGAS_instruction (inSourcePtr) {
  macroNullOrValidSharedObject (inSourcePtr, cPtr_rotate) ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_rotate GALGAS_rotate::constructor_new (const GALGAS_luint & inAttribute_mAngle
                                              COMMA_LOCATION_ARGS) {
  GALGAS_rotate result ;
  if (inAttribute_mAngle.isValid ()) {
    macroMyNew (result.mObjectPtr, cPtr_rotate (inAttribute_mAngle COMMA_THERE)) ;
  }
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_luint GALGAS_rotate::getter_mAngle (UNUSED_LOCATION_ARGS) const {
  GALGAS_luint result ;
  if (NULL != mObjectPtr) {
    const cPtr_rotate * p = (const cPtr_rotate *) mObjectPtr ;
    macroValidSharedObject (p, cPtr_rotate) ;
    result = p->mProperty_mAngle ;
  }
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_luint cPtr_rotate::getter_mAngle (UNUSED_LOCATION_ARGS) const {
  return mProperty_mAngle ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

void GALGAS_rotate::setter_setMAngle (GALGAS_luint inValue
                                      COMMA_LOCATION_ARGS) {
  if (NULL != mObjectPtr) {
    insulate (THERE) ;
    cPtr_rotate * p = (cPtr_rotate *) mObjectPtr ;
    macroValidSharedObject (p, cPtr_rotate) ;
    p->mProperty_mAngle = inValue ;
  }
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

void cPtr_rotate::setter_setMAngle (GALGAS_luint inValue
                                    COMMA_UNUSED_LOCATION_ARGS) {
  mProperty_mAngle = inValue ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//                                           Pointer class for @rotate class                                           *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

cPtr_rotate::cPtr_rotate (const GALGAS_luint & in_mAngle
                          COMMA_LOCATION_ARGS) :
cPtr_instruction (THERE),
mProperty_mAngle (in_mAngle) {
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

const C_galgas_type_descriptor * cPtr_rotate::classDescriptor (void) const {
  return & kTypeDescriptor_GALGAS_rotate ;
}

void cPtr_rotate::description (C_String & ioString,
                               const int32_t inIndentation) const {
  ioString << "[@rotate:" ;
  mProperty_mAngle.description (ioString, inIndentation+1) ;
  ioString << "]" ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

acPtr_class * cPtr_rotate::duplicate (LOCATION_ARGS) const {
  acPtr_class * ptr = NULL ;
  macroMyNew (ptr, cPtr_rotate (mProperty_mAngle COMMA_THERE)) ;
  return ptr ;
}


//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//                                                                                                                     *
//                                                    @rotate type                                                     *
//                                                                                                                     *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

const C_galgas_type_descriptor
kTypeDescriptor_GALGAS_rotate ("rotate",
                               & kTypeDescriptor_GALGAS_instruction) ;

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

const C_galgas_type_descriptor * GALGAS_rotate::staticTypeDescriptor (void) const {
  return & kTypeDescriptor_GALGAS_rotate ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

AC_GALGAS_root * GALGAS_rotate::clonedObject (void) const {
  AC_GALGAS_root * result = NULL ;
  if (isValid ()) {
    macroMyNew (result, GALGAS_rotate (*this)) ;
  }
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_rotate GALGAS_rotate::extractObject (const GALGAS_object & inObject,
                                            C_Compiler * inCompiler
                                            COMMA_LOCATION_ARGS) {
  GALGAS_rotate result ;
  const GALGAS_rotate * p = (const GALGAS_rotate *) inObject.embeddedObject () ;
  if (NULL != p) {
    if (NULL != dynamic_cast <const GALGAS_rotate *> (p)) {
      result = *p ;
    }else{
      inCompiler->castError ("rotate", p->dynamicTypeDescriptor () COMMA_THERE) ;
    }  
  }
  return result ;
}

#include "utilities/MF_MemoryControl.h"
#include "galgas2/C_galgas_CLI_Options.h"

#include "files/C_FileManager.h"

//---------------------------------------------------------------------------------------------------------------------*


//---------------------------------------------------------------------------------------------------------------------*
//                                                                                                                      
//                                       N O N    T E R M I N A L    N A M E S                                          
//                                                                                                                      
//---------------------------------------------------------------------------------------------------------------------*

static const char * gNonTerminalNames_logo_grammar [8] = {
  "<start_symbol>",// Index 0
  "<routine_definition>",// Index 1
  "<instruction_list>",// Index 2
  "<instruction>",// Index 3
  "<select_logo_5F_syntax_0>",// Index 4
  "<select_logo_5F_syntax_1>",// Index 5
  "<select_logo_5F_syntax_2>",// Index 6
  "<>"// Index 7
} ;

//---------------------------------------------------------------------------------------------------------------------*
//                                                                                                                      
//                              L R ( 1 )    A N A L Y Z E R    A C T I O N    T A B L E                                
//                                                                                                                      
//---------------------------------------------------------------------------------------------------------------------*

// Action tables handle shift and reduce actions ;
//  - a shift action is (terminal_symbol, SHIFT (n)) : if shifts to state n ;
//  - the accept action is (terminal_symbol, ACCEPT) ;
//  - a reduce action is (terminal_symbol, REDUCE (n)) ; if reduces to state n.

#define SHIFT(a) ((a) + 2)
#define REDUCE(a) (-(a) - 1)
#define ACCEPT (1)
#define END (-1)

static const int16_t gActionTable_logo_grammar [] = {
// State S0 (index = 0)
  C_Lexique_logo_5F_lexique::kToken_PROGRAM, SHIFT (1)
, END
// State S1 (index = 3)
, C_Lexique_logo_5F_lexique::kToken_BEGIN, REDUCE (4)
, C_Lexique_logo_5F_lexique::kToken_ROUTINE, SHIFT (3)
, END
// State S2 (index = 8)
, C_Lexique_logo_5F_lexique::kToken_, ACCEPT
, END
// State S3 (index = 11)
, C_Lexique_logo_5F_lexique::kToken_identifier, SHIFT (6)
, END
// State S4 (index = 14)
, C_Lexique_logo_5F_lexique::kToken_BEGIN, REDUCE (4)
, C_Lexique_logo_5F_lexique::kToken_ROUTINE, SHIFT (3)
, END
// State S5 (index = 19)
, C_Lexique_logo_5F_lexique::kToken_BEGIN, SHIFT (8)
, END
// State S6 (index = 22)
, C_Lexique_logo_5F_lexique::kToken_BEGIN, SHIFT (9)
, END
// State S7 (index = 25)
, C_Lexique_logo_5F_lexique::kToken_BEGIN, REDUCE (5)
, END
// State S8 (index = 28)
, C_Lexique_logo_5F_lexique::kToken_END, REDUCE (6)
, C_Lexique_logo_5F_lexique::kToken_FORWARD, SHIFT (10)
, C_Lexique_logo_5F_lexique::kToken_ROTATE, SHIFT (11)
, C_Lexique_logo_5F_lexique::kToken_PEN, SHIFT (12)
, C_Lexique_logo_5F_lexique::kToken_CALL, SHIFT (13)
, END
// State S9 (index = 39)
, C_Lexique_logo_5F_lexique::kToken_END, REDUCE (6)
, C_Lexique_logo_5F_lexique::kToken_FORWARD, SHIFT (10)
, C_Lexique_logo_5F_lexique::kToken_ROTATE, SHIFT (11)
, C_Lexique_logo_5F_lexique::kToken_PEN, SHIFT (12)
, C_Lexique_logo_5F_lexique::kToken_CALL, SHIFT (13)
, END
// State S10 (index = 50)
, C_Lexique_logo_5F_lexique::kToken_integer, SHIFT (19)
, END
// State S11 (index = 53)
, C_Lexique_logo_5F_lexique::kToken_integer, SHIFT (20)
, END
// State S12 (index = 56)
, C_Lexique_logo_5F_lexique::kToken_UP, SHIFT (21)
, C_Lexique_logo_5F_lexique::kToken_DOWN, SHIFT (22)
, END
// State S13 (index = 61)
, C_Lexique_logo_5F_lexique::kToken_identifier, SHIFT (23)
, END
// State S14 (index = 64)
, C_Lexique_logo_5F_lexique::kToken_END, SHIFT (24)
, END
// State S15 (index = 67)
, C_Lexique_logo_5F_lexique::kToken_END, REDUCE (6)
, C_Lexique_logo_5F_lexique::kToken_FORWARD, SHIFT (10)
, C_Lexique_logo_5F_lexique::kToken_ROTATE, SHIFT (11)
, C_Lexique_logo_5F_lexique::kToken_PEN, SHIFT (12)
, C_Lexique_logo_5F_lexique::kToken_CALL, SHIFT (13)
, END
// State S16 (index = 78)
, C_Lexique_logo_5F_lexique::kToken_END, REDUCE (2)
, END
// State S17 (index = 81)
, C_Lexique_logo_5F_lexique::kToken__3B_, SHIFT (26)
, END
// State S18 (index = 84)
, C_Lexique_logo_5F_lexique::kToken_END, SHIFT (27)
, END
// State S19 (index = 87)
, C_Lexique_logo_5F_lexique::kToken__3B_, REDUCE (8)
, END
// State S20 (index = 90)
, C_Lexique_logo_5F_lexique::kToken__3B_, REDUCE (9)
, END
// State S21 (index = 93)
, C_Lexique_logo_5F_lexique::kToken_integer, SHIFT (28)
, END
// State S22 (index = 96)
, C_Lexique_logo_5F_lexique::kToken_integer, SHIFT (29)
, END
// State S23 (index = 99)
, C_Lexique_logo_5F_lexique::kToken__3B_, REDUCE (12)
, END
// State S24 (index = 102)
, C_Lexique_logo_5F_lexique::kToken__2E_, SHIFT (30)
, END
// State S25 (index = 105)
, C_Lexique_logo_5F_lexique::kToken_END, REDUCE (7)
, END
// State S26 (index = 108)
, C_Lexique_logo_5F_lexique::kToken_END, REDUCE (3)
, C_Lexique_logo_5F_lexique::kToken_FORWARD, REDUCE (3)
, C_Lexique_logo_5F_lexique::kToken_ROTATE, REDUCE (3)
, C_Lexique_logo_5F_lexique::kToken_PEN, REDUCE (3)
, C_Lexique_logo_5F_lexique::kToken_CALL, REDUCE (3)
, C_Lexique_logo_5F_lexique::kToken_, REDUCE (3)
, END
// State S27 (index = 121)
, C_Lexique_logo_5F_lexique::kToken_BEGIN, REDUCE (1)
, C_Lexique_logo_5F_lexique::kToken_ROUTINE, REDUCE (1)
, C_Lexique_logo_5F_lexique::kToken_, REDUCE (1)
, END
// State S28 (index = 128)
, C_Lexique_logo_5F_lexique::kToken__3B_, REDUCE (10)
, END
// State S29 (index = 131)
, C_Lexique_logo_5F_lexique::kToken__3B_, REDUCE (11)
, END
// State S30 (index = 134)
, C_Lexique_logo_5F_lexique::kToken_, REDUCE (0)
, END} ;

static const uint32_t gActionTableIndex_logo_grammar [31] = {
  0  // S0
, 3  // S1
, 8  // S2
, 11  // S3
, 14  // S4
, 19  // S5
, 22  // S6
, 25  // S7
, 28  // S8
, 39  // S9
, 50  // S10
, 53  // S11
, 56  // S12
, 61  // S13
, 64  // S14
, 67  // S15
, 78  // S16
, 81  // S17
, 84  // S18
, 87  // S19
, 90  // S20
, 93  // S21
, 96  // S22
, 99  // S23
, 102  // S24
, 105  // S25
, 108  // S26
, 121  // S27
, 128  // S28
, 131  // S29
, 134  // S30
} ;

//---------------------------------------------------------------------------------------------------------------------*
//                                                                                                                      
//                                            SLR states successors table                                               
//                                                                                                                      
//---------------------------------------------------------------------------------------------------------------------*

// Successor tables handle non terminal successors ;
// an entry is (non_terminal_symbol, n) ; successor is state n.

static const int16_t gSuccessorTable_logo_grammar_0 [3] = {0, 2, -1} ;

static const int16_t gSuccessorTable_logo_grammar_1 [5] = {1, 4,
  4, 5, -1} ;

static const int16_t gSuccessorTable_logo_grammar_4 [5] = {1, 4,
  4, 7, -1} ;

static const int16_t gSuccessorTable_logo_grammar_8 [9] = {2, 14,
  3, 15,
  5, 16,
  6, 17, -1} ;

static const int16_t gSuccessorTable_logo_grammar_9 [9] = {2, 18,
  3, 15,
  5, 16,
  6, 17, -1} ;

static const int16_t gSuccessorTable_logo_grammar_15 [7] = {3, 15,
  5, 25,
  6, 17, -1} ;

static const int16_t * gSuccessorTable_logo_grammar [31] = {
gSuccessorTable_logo_grammar_0, gSuccessorTable_logo_grammar_1, NULL, NULL, 
  gSuccessorTable_logo_grammar_4, NULL, NULL, NULL, 
  gSuccessorTable_logo_grammar_8, gSuccessorTable_logo_grammar_9, NULL, NULL, 
  NULL, NULL, NULL, gSuccessorTable_logo_grammar_15, 
  NULL, NULL, NULL, NULL, 
  NULL, NULL, NULL, NULL, 
  NULL, NULL, NULL, NULL, 
  NULL, NULL, NULL} ;

//---------------------------------------------------------------------------------------------------------------------*
//                                                                                                                      
//                          Production rules infos (left non terminal, size of right string)                            
//                                                                                                                      
//---------------------------------------------------------------------------------------------------------------------*

static const int16_t gProductionsTable_logo_grammar [14 * 2] = {
  0, 6,
  1, 5,
  2, 1,
  3, 2,
  4, 0,
  4, 2,
  5, 0,
  5, 2,
  6, 2,
  6, 2,
  6, 3,
  6, 3,
  6, 2,
  7, 1
} ;

//---------------------------------------------------------------------------------------------------------------------*
//                                                                                                                      
//                                     'start_symbol' non terminal implementation                                       
//                                                                                                                      
//---------------------------------------------------------------------------------------------------------------------*

void cGrammar_logo_5F_grammar::nt_start_5F_symbol_parse (C_Lexique_logo_5F_lexique * inLexique) {
  switch (inLexique->nextProductionIndex ()) {
  case 0 :
      rule_logo_5F_syntax_start_5F_symbol_i0_parse(inLexique) ;
    break ;
  default :
    inLexique->internalBottomUpParserError (HERE) ;
    break ;
  }
}

void cGrammar_logo_5F_grammar::nt_start_5F_symbol_ (C_Lexique_logo_5F_lexique * inLexique) {
  switch (inLexique->nextProductionIndex ()) {
  case 0 :
      rule_logo_5F_syntax_start_5F_symbol_i0_(inLexique) ;
    break ;
  default :
    inLexique->internalBottomUpParserError (HERE) ;
    break ;
  }
}

void cGrammar_logo_5F_grammar::performIndexing (C_Compiler * /* inCompiler */,
             const C_String & /* inSourceFilePath */) {
}

void cGrammar_logo_5F_grammar::performOnlyLexicalAnalysis (C_Compiler * inCompiler,
             const C_String & inSourceFilePath) {
  C_Lexique_logo_5F_lexique * scanner = NULL ;
  macroMyNew (scanner, C_Lexique_logo_5F_lexique (inCompiler, inSourceFilePath COMMA_HERE)) ;
  if (scanner->sourceText ().isValid ()) {
    scanner->performLexicalAnalysis () ;
  }
  macroDetachSharedObject (scanner) ;
}

void cGrammar_logo_5F_grammar::performOnlySyntaxAnalysis (C_Compiler * inCompiler,
             const C_String & inSourceFilePath) {
  C_Lexique_logo_5F_lexique * scanner = NULL ;
  macroMyNew (scanner, C_Lexique_logo_5F_lexique (inCompiler, inSourceFilePath COMMA_HERE)) ;
  if (scanner->sourceText ().isValid ()) {
    scanner->performBottomUpParsing (gActionTable_logo_grammar, gNonTerminalNames_logo_grammar,
                                     gActionTableIndex_logo_grammar, gSuccessorTable_logo_grammar,
                                     gProductionsTable_logo_grammar) ;
  }
  macroDetachSharedObject (scanner) ;
}

//---------------------------------------------------------------------------------------------------------------------*
//                                                                                                                      
//                                        Grammar start symbol implementation                                           
//                                                                                                                      
//---------------------------------------------------------------------------------------------------------------------*

//---------------------------------------------------------------------------------------------------------------------*

void cGrammar_logo_5F_grammar::_performSourceFileParsing_ (C_Compiler * inCompiler,
                                GALGAS_lstring inFilePath
                                COMMA_LOCATION_ARGS) {
  if (inFilePath.isValid ()) {
    const GALGAS_string filePathAsString = inFilePath.getter_string (HERE) ;
    C_String filePath = filePathAsString.stringValue () ;
    if (! C_FileManager::isAbsolutePath (filePath)) {
      filePath = inCompiler->sourceFilePath ().stringByDeletingLastPathComponent ().stringByAppendingPathComponent (filePath) ;
    }
    if (C_FileManager::fileExistsAtPath (filePath)) {
      C_Lexique_logo_5F_lexique * scanner = NULL ;
      macroMyNew (scanner, C_Lexique_logo_5F_lexique (inCompiler, filePath COMMA_HERE)) ;
      if (scanner->sourceText ().isValid ()) {
        const bool ok = scanner->performBottomUpParsing (gActionTable_logo_grammar, gNonTerminalNames_logo_grammar,
                                                         gActionTableIndex_logo_grammar, gSuccessorTable_logo_grammar,
                                                         gProductionsTable_logo_grammar) ;
        if (ok && ! executionModeIsSyntaxAnalysisOnly ()) {
          cGrammar_logo_5F_grammar grammar ;
          grammar.nt_start_5F_symbol_ (scanner) ;
        }
      }else{
        C_String message ;
        message << "the '" << filePath << "' file exists, but cannot be read" ;
        const GALGAS_location errorLocation (inFilePath.getter_location (THERE)) ;
        inCompiler->semanticErrorAtLocation (errorLocation, message, TC_Array <C_FixItDescription> () COMMA_THERE) ;
        }
        macroDetachSharedObject (scanner) ;
      }else{
        C_String message ;
        message << "the '" << filePath << "' file does not exist" ;
        const GALGAS_location errorLocation (inFilePath.getter_location (THERE)) ;
        inCompiler->semanticErrorAtLocation (errorLocation, message, TC_Array <C_FixItDescription> () COMMA_THERE) ;
    }
  }
}

void cGrammar_logo_5F_grammar::_performSourceStringParsing_ (C_Compiler * inCompiler,
                                GALGAS_string inSourceString,
                                GALGAS_string inNameString
                                COMMA_UNUSED_LOCATION_ARGS) {
  if (inSourceString.isValid () && inNameString.isValid ()) {
    const C_String sourceString = inSourceString.stringValue () ;
    const C_String nameString = inNameString.stringValue () ;
    C_Lexique_logo_5F_lexique * scanner = NULL ;
    macroMyNew (scanner, C_Lexique_logo_5F_lexique (inCompiler, sourceString, nameString COMMA_HERE)) ;
    const bool ok = scanner->performBottomUpParsing (gActionTable_logo_grammar, gNonTerminalNames_logo_grammar,
                                                     gActionTableIndex_logo_grammar, gSuccessorTable_logo_grammar,
                                                     gProductionsTable_logo_grammar) ;
    if (ok && ! executionModeIsSyntaxAnalysisOnly ()) {
      cGrammar_logo_5F_grammar grammar ;
      grammar.nt_start_5F_symbol_ (scanner) ;
    }
    macroDetachSharedObject (scanner) ;
  }
}

//---------------------------------------------------------------------------------------------------------------------*
//                                                                                                                      
//                                  'routine_definition' non terminal implementation                                    
//                                                                                                                      
//---------------------------------------------------------------------------------------------------------------------*

void cGrammar_logo_5F_grammar::nt_routine_5F_definition_parse (C_Lexique_logo_5F_lexique * inLexique) {
  switch (inLexique->nextProductionIndex ()) {
  case 1 :
      rule_logo_5F_syntax_routine_5F_definition_i1_parse(inLexique) ;
    break ;
  default :
    inLexique->internalBottomUpParserError (HERE) ;
    break ;
  }
}

void cGrammar_logo_5F_grammar::nt_routine_5F_definition_ (GALGAS_routineMap &  parameter_1,
                                C_Lexique_logo_5F_lexique * inLexique) {
  switch (inLexique->nextProductionIndex ()) {
  case 1 :
      rule_logo_5F_syntax_routine_5F_definition_i1_(parameter_1, inLexique) ;
    break ;
  default :
    inLexique->internalBottomUpParserError (HERE) ;
    break ;
  }
}

//---------------------------------------------------------------------------------------------------------------------*
//                                                                                                                      
//                                   'instruction_list' non terminal implementation                                     
//                                                                                                                      
//---------------------------------------------------------------------------------------------------------------------*

void cGrammar_logo_5F_grammar::nt_instruction_5F_list_parse (C_Lexique_logo_5F_lexique * inLexique) {
  switch (inLexique->nextProductionIndex ()) {
  case 2 :
      rule_logo_5F_syntax_instruction_5F_list_i2_parse(inLexique) ;
    break ;
  default :
    inLexique->internalBottomUpParserError (HERE) ;
    break ;
  }
}

void cGrammar_logo_5F_grammar::nt_instruction_5F_list_ (GALGAS_routineMap  parameter_1,
                                GALGAS_instructionList &  parameter_2,
                                C_Lexique_logo_5F_lexique * inLexique) {
  switch (inLexique->nextProductionIndex ()) {
  case 2 :
      rule_logo_5F_syntax_instruction_5F_list_i2_(parameter_1, parameter_2, inLexique) ;
    break ;
  default :
    inLexique->internalBottomUpParserError (HERE) ;
    break ;
  }
}

//---------------------------------------------------------------------------------------------------------------------*
//                                                                                                                      
//                                     'instruction' non terminal implementation                                        
//                                                                                                                      
//---------------------------------------------------------------------------------------------------------------------*

void cGrammar_logo_5F_grammar::nt_instruction_parse (C_Lexique_logo_5F_lexique * inLexique) {
  switch (inLexique->nextProductionIndex ()) {
  case 3 :
      rule_logo_5F_syntax_instruction_i3_parse(inLexique) ;
    break ;
  default :
    inLexique->internalBottomUpParserError (HERE) ;
    break ;
  }
}

void cGrammar_logo_5F_grammar::nt_instruction_ (GALGAS_routineMap  parameter_1,
                                GALGAS_instructionList &  parameter_2,
                                C_Lexique_logo_5F_lexique * inLexique) {
  switch (inLexique->nextProductionIndex ()) {
  case 3 :
      rule_logo_5F_syntax_instruction_i3_(parameter_1, parameter_2, inLexique) ;
    break ;
  default :
    inLexique->internalBottomUpParserError (HERE) ;
    break ;
  }
}

//---------------------------------------------------------------------------------------------------------------------*
//                                                                                                                      
//                               'select_logo_5F_syntax_0' non terminal implementation                                  
//                                                                                                                      
//---------------------------------------------------------------------------------------------------------------------*

int32_t cGrammar_logo_5F_grammar::select_logo_5F_syntax_0 (C_Lexique_logo_5F_lexique * inLexique) {
// Productions numbers : 4 5
  return inLexique->nextProductionIndex () - 3 ;
}

//---------------------------------------------------------------------------------------------------------------------*
//                                                                                                                      
//                               'select_logo_5F_syntax_1' non terminal implementation                                  
//                                                                                                                      
//---------------------------------------------------------------------------------------------------------------------*

int32_t cGrammar_logo_5F_grammar::select_logo_5F_syntax_1 (C_Lexique_logo_5F_lexique * inLexique) {
// Productions numbers : 6 7
  return inLexique->nextProductionIndex () - 5 ;
}

//---------------------------------------------------------------------------------------------------------------------*
//                                                                                                                      
//                               'select_logo_5F_syntax_2' non terminal implementation                                  
//                                                                                                                      
//---------------------------------------------------------------------------------------------------------------------*

int32_t cGrammar_logo_5F_grammar::select_logo_5F_syntax_2 (C_Lexique_logo_5F_lexique * inLexique) {
// Productions numbers : 8 9 10 11 12
  return inLexique->nextProductionIndex () - 7 ;
}

//---------------------------------------------------------------------------------------------------------------------*

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//   Object comparison                                                                                                 *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

typeComparisonResult cPtr_forward::dynamicObjectCompare (const acPtr_class * inOperandPtr) const {
  typeComparisonResult result = kOperandEqual ;
  const cPtr_forward * p = (const cPtr_forward *) inOperandPtr ;
  macroValidSharedObject (p, cPtr_forward) ;
  if (kOperandEqual == result) {
    result = mProperty_mLength.objectCompare (p->mProperty_mLength) ;
  }
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*


typeComparisonResult GALGAS_forward::objectCompare (const GALGAS_forward & inOperand) const {
  typeComparisonResult result = kOperandNotValid ;
  if (isValid () && inOperand.isValid ()) {
    const int32_t mySlot = mObjectPtr->classDescriptor ()->mSlotID ;
    const int32_t operandSlot = inOperand.mObjectPtr->classDescriptor ()->mSlotID ;
    if (mySlot < operandSlot) {
      result = kFirstOperandLowerThanSecond ;
    }else if (mySlot > operandSlot) {
      result = kFirstOperandGreaterThanSecond ;
    }else{
      result = mObjectPtr->dynamicObjectCompare (inOperand.mObjectPtr) ;
    }
  }
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_forward::GALGAS_forward (void) :
GALGAS_instruction () {
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_forward GALGAS_forward::constructor_default (LOCATION_ARGS) {
  return GALGAS_forward::constructor_new (GALGAS_luint::constructor_default (HERE)
                                          COMMA_THERE) ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_forward::GALGAS_forward (const cPtr_forward * inSourcePtr) :
GALGAS_instruction (inSourcePtr) {
  macroNullOrValidSharedObject (inSourcePtr, cPtr_forward) ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_forward GALGAS_forward::constructor_new (const GALGAS_luint & inAttribute_mLength
                                                COMMA_LOCATION_ARGS) {
  GALGAS_forward result ;
  if (inAttribute_mLength.isValid ()) {
    macroMyNew (result.mObjectPtr, cPtr_forward (inAttribute_mLength COMMA_THERE)) ;
  }
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_luint GALGAS_forward::getter_mLength (UNUSED_LOCATION_ARGS) const {
  GALGAS_luint result ;
  if (NULL != mObjectPtr) {
    const cPtr_forward * p = (const cPtr_forward *) mObjectPtr ;
    macroValidSharedObject (p, cPtr_forward) ;
    result = p->mProperty_mLength ;
  }
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_luint cPtr_forward::getter_mLength (UNUSED_LOCATION_ARGS) const {
  return mProperty_mLength ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

void GALGAS_forward::setter_setMLength (GALGAS_luint inValue
                                        COMMA_LOCATION_ARGS) {
  if (NULL != mObjectPtr) {
    insulate (THERE) ;
    cPtr_forward * p = (cPtr_forward *) mObjectPtr ;
    macroValidSharedObject (p, cPtr_forward) ;
    p->mProperty_mLength = inValue ;
  }
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

void cPtr_forward::setter_setMLength (GALGAS_luint inValue
                                      COMMA_UNUSED_LOCATION_ARGS) {
  mProperty_mLength = inValue ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//                                          Pointer class for @forward class                                           *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

cPtr_forward::cPtr_forward (const GALGAS_luint & in_mLength
                            COMMA_LOCATION_ARGS) :
cPtr_instruction (THERE),
mProperty_mLength (in_mLength) {
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

const C_galgas_type_descriptor * cPtr_forward::classDescriptor (void) const {
  return & kTypeDescriptor_GALGAS_forward ;
}

void cPtr_forward::description (C_String & ioString,
                                const int32_t inIndentation) const {
  ioString << "[@forward:" ;
  mProperty_mLength.description (ioString, inIndentation+1) ;
  ioString << "]" ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

acPtr_class * cPtr_forward::duplicate (LOCATION_ARGS) const {
  acPtr_class * ptr = NULL ;
  macroMyNew (ptr, cPtr_forward (mProperty_mLength COMMA_THERE)) ;
  return ptr ;
}


//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//                                                                                                                     *
//                                                    @forward type                                                    *
//                                                                                                                     *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

const C_galgas_type_descriptor
kTypeDescriptor_GALGAS_forward ("forward",
                                & kTypeDescriptor_GALGAS_instruction) ;

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

const C_galgas_type_descriptor * GALGAS_forward::staticTypeDescriptor (void) const {
  return & kTypeDescriptor_GALGAS_forward ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

AC_GALGAS_root * GALGAS_forward::clonedObject (void) const {
  AC_GALGAS_root * result = NULL ;
  if (isValid ()) {
    macroMyNew (result, GALGAS_forward (*this)) ;
  }
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

GALGAS_forward GALGAS_forward::extractObject (const GALGAS_object & inObject,
                                              C_Compiler * inCompiler
                                              COMMA_LOCATION_ARGS) {
  GALGAS_forward result ;
  const GALGAS_forward * p = (const GALGAS_forward *) inObject.embeddedObject () ;
  if (NULL != p) {
    if (NULL != dynamic_cast <const GALGAS_forward *> (p)) {
      result = *p ;
    }else{
      inCompiler->castError ("forward", p->dynamicTypeDescriptor () COMMA_THERE) ;
    }  
  }
  return result ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//                                                                                                                     *
//                               Bool options                                                                          *
//                                                                                                                     *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//                                                                                                                     *
//                               UInt options                                                                          *
//                                                                                                                     *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//                                                                                                                     *
//                              String options                                                                         *
//                                                                                                                     *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//                                                                                                                     *
//                              String List options                                                                    *
//                                                                                                                     *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*


#include "project_header.h"
#include "command_line_interface/F_mainForLIBPM.h"
#include "command_line_interface/F_Analyze_CLI_Options.h"
#include "utilities/F_DisplayException.h"
#include "galgas2/C_galgas_CLI_Options.h"
#include "galgas2/F_verbose_output.h"
#include "galgas2/cLexiqueIntrospection.h"
#include "command_line_interface/C_builtin_CLI_Options.h"

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//                                                                                                                     *
//                      print_tool_help_message                                                                        *
//                                                                                                                     *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

static void print_tool_help_message (void) {
  co << "Compiled with GALGAS revision NUMERO_REVISION_GALGAS\n" ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

static const char * kSourceFileExtensions [] = {
  "logo",
  NULL
} ;    

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

static const char * kSourceFileHelpMessages [] = {
  "a source text file with the .logo extension",
  NULL
} ;    

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

const char * projectVersionString (void) {
  return "0.0.1" ;
}

//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//                                                                                                                     *
//                                                  Routine 'before'                                                   *
//                                                                                                                     *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

static void routine_before (C_Compiler * /* inCompiler */
                            COMMA_UNUSED_LOCATION_ARGS) {
  {
  }
}


//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//                                                                                                                     *
//                                                   Routine 'after'                                                   *
//                                                                                                                     *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

static void routine_after (C_Compiler * /* inCompiler */
                           COMMA_UNUSED_LOCATION_ARGS) {
  {
  }
}


//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//                                                                                                                     *
//                                               Routine 'programRule_0'                                               *
//                                                                                                                     *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

static void routine_programRule_5F__30_ (const GALGAS_lstring constinArgument_inSourceFile,
                                         C_Compiler * inCompiler
                                         COMMA_UNUSED_LOCATION_ARGS) {
  cGrammar_logo_5F_grammar::_performSourceFileParsing_ (inCompiler, constinArgument_inSourceFile  COMMA_SOURCE_FILE ("logo-program.galgas", 10)) ;
}


//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*
//                                                                                                                     *
//                      M A I N    F O R    L I B P M                                                                  *
//                                                                                                                     *
//—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————*

int mainForLIBPM (int inArgc, const char * inArgv []) {
//--- Analyze Command Line Options
  TC_UniqueArray <C_String> sourceFilesArray ;
  F_Analyze_CLI_Options (inArgc, inArgv,
                         sourceFilesArray,
                         kSourceFileExtensions,
                         kSourceFileHelpMessages,
                         print_tool_help_message) ;
//---
  int returnCode = 0 ; // No error
//--- Set Execution mode
  C_String executionModeOptionErrorMessage ;
  setExecutionMode (executionModeOptionErrorMessage) ;
  if (executionModeOptionErrorMessage.length () > 0) {
    co << executionModeOptionErrorMessage ;
    returnCode = 1 ;
  }else{
  //--- Common lexique object
    C_Compiler * commonCompiler = NULL ;
    macroMyNew (commonCompiler, C_Compiler (NULL COMMA_HERE)) ;
    try{
      routine_before (commonCompiler COMMA_HERE) ;
      cLexiqueIntrospection::handleGetKeywordListOption (commonCompiler) ;
      const bool verboseOptionOn = verboseOutput () ;
      for (int32_t i=0 ; i<sourceFilesArray.count () ; i++) {
        const C_String fileExtension = sourceFilesArray (i COMMA_HERE).pathExtension () ;
        const GALGAS_string sfp = GALGAS_string (sourceFilesArray (i COMMA_HERE)) ;
        const GALGAS_location location = commonCompiler->here () ;
        const GALGAS_lstring sourceFilePath (sfp, location) ;
        int r = 0 ;
        if (fileExtension == "logo") {
          switch (executionMode ()) {
          case kExecutionModeNormal :
            routine_programRule_5F__30_ (sourceFilePath, commonCompiler COMMA_HERE) ;
            break ;
          case kExecutionModeLexicalAnalysisOnly :
            cGrammar_logo_5F_grammar::performOnlyLexicalAnalysis (commonCompiler, sourceFilesArray (i COMMA_HERE)) ;
            break ;
          case kExecutionModeSyntaxAnalysisOnly :
            cGrammar_logo_5F_grammar::performOnlySyntaxAnalysis (commonCompiler, sourceFilesArray (i COMMA_HERE)) ;
            break ;
          case kExecutionModeIndexing :
            cGrammar_logo_5F_grammar::performIndexing (commonCompiler, sourceFilesArray (i COMMA_HERE)) ;
            break ;
          case kExecutionModeLatex :
            cGrammar_logo_5F_grammar::performOnlyLexicalAnalysis (commonCompiler, sourceFilesArray (i COMMA_HERE)) ;
            break ;
          }
        }else{
          printf ("*** Error: unhandled extension for file '%s' ***\n", sourceFilesArray (i COMMA_HERE).cString (HERE)) ;
          r = 1 ;
        }
        if (r != 0) {
          returnCode = r ;
        }
      }
    //--- Error or warnings ?
      if (totalErrorCount () > 0) {
        returnCode = 1 ; // Error code
      }else if (totalWarningCount () > 0) {
        if (gOption_galgas_5F_builtin_5F_options_treat_5F_warnings_5F_as_5F_error.mValue) {
          returnCode = 1 ; // Error code
          if (verboseOptionOn) {
            printf ("** Note: warnings are treated as errors. **\n") ;
          }
        }
      }
    //--- Epilogue
      routine_after (commonCompiler COMMA_HERE) ;
    //--- Emit JSON issue file ?
      if (gOption_generic_5F_cli_5F_options_emit_5F_issue_5F_json_5F_file.mValue != "") {
        commonCompiler->writeIssueJSONFile (gOption_generic_5F_cli_5F_options_emit_5F_issue_5F_json_5F_file.mValue) ;
      }
    //--- Display error and warnings count
      if (verboseOptionOn || (totalWarningCount () > 0) || (totalErrorCount () > 0)) {
        C_String message ;
        if (totalWarningCount () == 0) {
          message << "No warning" ;
        }else if (totalWarningCount () == 1) {
          message << "1 warning" ;
        }else{
          message << cStringWithSigned (totalWarningCount ()) << " warnings" ;
        }
        message << ", " ;
        if (totalErrorCount () == 0) {
          message << "no error" ;
        }else if (totalErrorCount () == 1) {
          message << "1 error" ;
        }else{
          message << cStringWithSigned (totalErrorCount ()) << " errors" ;
        }
        message << ".\n" ;
        ggs_printMessage (message COMMA_HERE) ;
      }
    }catch (const ::std:: exception & e) {
      F_default_display_exception (e) ;
      returnCode = 1 ; // Error code
    }catch (...) {
      printf ("**** Unknow exception ****\n") ;
      throw ;
    }
    macroDetachSharedObject (commonCompiler) ;
  }
  return returnCode ;
}


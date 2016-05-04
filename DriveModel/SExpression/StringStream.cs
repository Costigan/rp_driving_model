using System;

namespace SExpression
{
    public class StringStream
    {
        private readonly String _string;
        private int _pos;

        public StringStream(string s)
        {
            _string = s;
            _pos = 0;
        }

        public char Peek()
        {
            return _pos >= _string.Length ? '\0' : _string[_pos];
        }

        public char Read()
        {
            return _pos >= _string.Length ? '\0' : _string[_pos++];
        }

        public void SkipWhitespace()
        {
            while (IsWhitespace(Peek()))
                Read();
        }

        public static bool IsWhitespace(char c)
        {
            return c == ' ' || c == '\t';
        }

        public static bool IsDigit(char c)
        {
            return char.IsDigit(c);
        }

        public static bool IsSymbolChar(char c)
        {
            return char.IsLetterOrDigit(c) || c == '-' || c == '_' || c == ':';
        }

        public static bool IsInsideSymbolChar(char c)
        {
            return char.IsLetterOrDigit(c) || c == '-' || c == '_' || c == ':' || c == '+';
        }

        public static bool IsAlphabetic(char c)
        {
            return char.IsLetter(c);
        }

        public bool IsEmpty()
        {
            return _pos >= _string.Length;
        }

        internal void Push(char c)
        {
            _pos--;
            if (_pos < 0) _pos = 0;
            if (_string[_pos]!=c)
                throw new Exception("Pushed the wrong character back onto a stream");
        }
    }
}

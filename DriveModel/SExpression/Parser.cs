using System;
using System.Collections.Generic;
using System.Security.Cryptography;
using System.Text;

namespace SExpression
{
    public class Parser
    {
        private readonly StringBuilder _builder = new StringBuilder();
        
        public dynamic Parse(StringStream stream)
        {
            var s = ReadSExpr(stream);
            if (!(s is List<dynamic>)) throw new Exception("Unrecognized event");
            return s;
        }

        public dynamic ReadSExpr(StringStream s)
        {
            s.SkipWhitespace();
            var c = s.Read();
            if (c == '\0') return null;
            if (c == '-' || char.IsDigit(c))
            {
                s.Push(c);
                return ReadNumber(s);
            }
            if (c == '(' || c == '{') return ReadList(s);
            if (StringStream.IsSymbolChar(c))
            {
                s.Push(c);
                return ReadSymbol(s);
            }
            throw new Exception("unrecognized character in S expression: " + c);
        }

        private List<dynamic> ReadList(StringStream s)
        {
            var l = new List<dynamic>();
            while (true)
            {
                s.SkipWhitespace();
                var c = s.Peek();
                if (c == '\0') throw new Exception("Stream ended in the middle of an S expression");
                if (c == ')' || c == '}')
                {
                    s.Read();
                    return l;
                }
                l.Add(ReadSExpr(s));
            }
        }

        private static dynamic ReadNumber(StringStream s)
        {
            dynamic n = 0;
            var sign = 1;
            while (!s.IsEmpty() && !StringStream.IsWhitespace(s.Peek()))
            {
                var c = s.Read();
                switch (c)
                {
                    case '0':
                    case '1':
                    case '2':
                    case '3':
                    case '4':
                    case '5':
                    case '6':
                    case '7':
                    case '8':
                    case '9':
                        n = n * 10 + (c - '0');
                        break;
                    case '-':
                        sign = -sign;
                        break;
                    case '.':
                        return sign * ReadFloat(n, s);
                    case 'E':
                    case 'e':
                        return sign *ReadScientific(n, s);
                    default:
                        s.Push(c);
                        return sign * n;
                }
            }
            return n;
        }

        private static dynamic ReadFloat(dynamic n, StringStream s)
        {
            var numerator = 0L;
            var denominator = 1L;
            while (!s.IsEmpty() && !StringStream.IsWhitespace(s.Peek()))
            {
                var c = s.Read();
                if (char.IsDigit(c))
                {
                    numerator = numerator * 10 + (c - '0');
                    denominator = denominator * 10;
                }
                else if (c == 'e' || c == 'E')
                    return ReadScientific(n + ((double)numerator) / denominator, s);
                else
                {
                    s.Push(c);
                    return n + ((double)numerator) / denominator;
                }
            }
            return n + ((double)numerator) / denominator;
        }

        private static dynamic ReadScientific(dynamic n, StringStream s)
        {
            var power = 0L;
            var sign = 1d;
            while (!s.IsEmpty() && !StringStream.IsWhitespace(s.Peek()))
            {
                var c = s.Read();
                if (char.IsDigit(c))
                    power = power * 10 + (c - '0');
                else if (c == '-')
                    sign = -sign;
                else throw new Exception("Illegal character in number: " + c);
            }
            return n * Math.Pow(10d, sign*power);
        }

        private dynamic ReadSymbol(StringStream s)
        {
            _builder.Clear();
            while (!s.IsEmpty() && StringStream.IsInsideSymbolChar(s.Peek()))
                _builder.Append(char.ToUpperInvariant(s.Read()));
            return _builder.Length == 0 ? null : String.Intern(_builder.ToString());
        }

    }
}

using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;

namespace TraceVisualization
{
    public partial class Broken : Form
    {
        public const float RoverFromY = 20f;
        public const float RoverToY = 25f;
        public const float GDSFromY = 50f;
        public const float GDSToY = 55f;
        public const float RealtimeFromY = 80f;
        public const float RealtimeToY = 85f;
        public const float DriverFromY = 110f;
        public const float DriverToY = 115f;
        public const float ActorHeight = 20f;

        public List<TraceEvent> Steps;
        public List<string> TraceEvents; 
        private readonly StringBuilder _builder = new StringBuilder();

        private StateInterval RoverState;
        private StateInterval GDSState;
        private StateInterval RealtimeState;
        private StateInterval DriverState;

        public Broken()
        {
            InitializeComponent();
        }

        private void openToolStripMenuItem_Click(object sender, EventArgs e)
        {
            var d = new OpenFileDialog();
            var result = d.ShowDialog();
            if (result != DialogResult.OK) return;
            LoadTraceSteps(d.FileName);
        }

        private void LoadTraceSteps(string filename)
        {
            RoverState = new StateInterval{State="WAITING",Height=ActorHeight,X=0f,Y=RoverFromY};
            GDSState = new StateInterval { State = "WAITING", Height = ActorHeight, X = 0f, Y = GDSFromY };
            RealtimeState = new StateInterval { State = "WAITING", Height = ActorHeight, X = 0f, Y = RealtimeFromY };
            DriverState = new StateInterval { State = "IDLE", Height = ActorHeight, X = 0f, Y = DriverFromY };
            var steps = new List<TraceEvent>();
            var events = new List<string>();
            string line;
            var count = 0;
            using (var sr = new StreamReader(filename))
                while (null != (line = sr.ReadLine()))
                {
                    events.Add(line);
                    var evt = ParseEvent(new StringStream(line));
                    if (evt == null) continue;
                    evt.Index = count++;
                    steps.Add(evt);
                }

            Steps = steps;
            TraceEvents = events;
        }

        public TraceEvent ParseEvent(StringStream stream)
        {
            var s = ReadSExpr(stream);
            if (!(s is List<dynamic>)) throw new Exception("Unrecognized event");
            var typ = s[4] as string;
            switch (typ)
            {
                case ":SEND":
                {
                    return new MessageEvent
                    {
                        Color = Pens.Red,
                        FromX = (float)s[0],
                        FromY = ActorNameToFromY(s[6]),
                        ToX = (float)s[12],
                        ToY = ActorNameToToY(s[8])
                    };
                }
                case ":SET-STATE":
                {
                    var actor = s[6] as string;
                    var newState = s[8] as string;
                    var now = (float) s[0];
                    var stateInterval = GetStateInterval(actor);
                    stateInterval.Width = now - stateInterval.X;
                    if (stateInterval.State == newState) return null;
                    switch (actor)
                    {
                        case "ROVER":
                        case "AUTONOMOUS-ROVER":
                            return RoverState = new StateInterval
                            {
                                State = newState,
                                Height = ActorHeight,
                                X = now,
                                Y = RoverFromY
                            };
                        case "DRIVER":
                        case "AUTONOMY-DRIVER":
                            return DriverState = new StateInterval
                            {
                                State = newState,
                                Height = ActorHeight,
                                X = now,
                                Y = DriverFromY
                            };
                        case "GDS":
                            return GDSState = new StateInterval {State = newState, Height = ActorHeight, X = now, Y = GDSFromY};
                        case "REALTIME":
                            return RealtimeState = new StateInterval
                            {
                                State = newState,
                                Height = ActorHeight,
                                X = now,
                                Y = RealtimeFromY
                            };
                        default:
                            throw new Exception("Illegal actor in GetStateInterval");
                    }
                }
                default:
                    return null;
            }
        }

        public StateInterval GetStateInterval(string actor)
        {
            switch (actor)
            {
                case "ROVER":
                case "AUTONOMOUS-ROVER":
                    return RoverState;
                case "DRIVER":
                case "AUTONOMY-DRIVER":
                    return DriverState;
                case "GDS":
                    return GDSState;
                case "REALTIME":
                    return RealtimeState;
                default:
                    throw new Exception("Illegal actor in GetStateInterval");
            }
        }

        public float ActorNameToFromY(string name)
        {
            switch (name)
            {
                case "DRIVER":
                case "AUTONOMY-DRIVER":
                    return DriverFromY;
                case  "GDS":
                    return GDSFromY;
                case "REALTIME":
                    return RealtimeFromY;
                case "ROVER":
                case  "AUTONOMY-ROVER":
                    return RoverFromY;
                default:
                    return 0f;
            }
        }

        public float ActorNameToToY(string name)
        {
            switch (name)
            {
                case "DRIVER":
                case "AUTONOMY-DRIVER":
                    return DriverFromY;
                case "GDS":
                    return GDSFromY;
                case "REALTIME":
                    return RealtimeFromY;
                case "ROVER":
                case "AUTONOMY-ROVER":
                    return RoverFromY;
                default:
                    return 0f;
            }
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
            throw new Exception("unrecognized character in S expression: "+c);
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
                        n = n*10 + (c - '0');
                        break;
                    case '-':
                        n = -n;
                        break;
                    case '.':
                        return ReadFloat(n, s);
                    case 'E':
                    case 'e':
                        return ReadScientific(n, s);
                    default:
                        s.Push(c);
                        return n;
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
                    numerator = numerator*10 + (c - '0');
                    denominator = denominator*10;
                } else if (c == 'e' || c == 'E')
                    return ReadScientific(n + ((double) numerator)/denominator, s);
                else
                {
                    s.Push(c);
                    return n + ((double)numerator) / denominator;
                }
            }
            return n + ((double) numerator)/denominator;
        }

        private static dynamic ReadScientific(dynamic n, StringStream s)
        {
            var power = 0L;
            while (!s.IsEmpty() && !StringStream.IsWhitespace(s.Peek()))
            {
                var c = s.Read();
                if (char.IsDigit(c))
                    power = power*10 + (c - '0');
                else throw new Exception("Illegal character in number: " + c);
            }
            return n*Math.Pow(10d, power);
        }

        private dynamic ReadSymbol(StringStream s)
        {
            _builder.Clear();
            while (!s.IsEmpty() && StringStream.IsSymbolChar(s.Peek()))
                _builder.Append(char.ToUpperInvariant(s.Read()));
            return _builder.Length == 0 ? null : String.Intern(_builder.ToString());
        }
    }
}
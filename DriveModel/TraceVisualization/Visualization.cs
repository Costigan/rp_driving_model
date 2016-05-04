using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using SExpression;

namespace TraceVisualization
{
    public partial class Visualization : Form
    {
        public const float RoverFromY = 20f;
        public const float RoverToY = 25f;
        public const float GDSFromY = 50f;
// ReSharper disable InconsistentNaming
        public const float GDSToY = 55f;
// ReSharper restore InconsistentNaming
        public const float RealtimeFromY = 80f;
        public const float RealtimeToY = 85f;
        public const float DriverFromY = 110f;
        public const float DriverToY = 115f;
        public const float ActorHeight = 20f;

        public List<TraceEvent> Steps;
        public List<StateInterval> States;
        public List<MessageEvent> Messages;
 
        public List<string> Lines = new List<string>();
        public List<float> LineTimes; 
        private readonly Parser _parser = new Parser();

        private StateInterval _roverState;
        private StateInterval _gdsState;
        private StateInterval _realtimeState;
        private StateInterval _driverState;

        private float _drawScale = 1f;
        private float _time = 10f;
        private bool _draggingTime = false;
        private bool _draggingPanel = false;
        private int _draggingPanelStartX;
        private Point _lastScreenPoint;
        private int _scaleY = 1;
        private bool _updatingRowSelection = false;

        public Visualization()
        {
            InitializeComponent();
        }

        private void openToolStripMenuItem_Click(object sender, EventArgs e)
        {
            var d = new OpenFileDialog();
            var result = d.ShowDialog();
            if (result != DialogResult.OK) return;
            LoadTraceSteps(d.FileName);
            States = Steps.OfType<StateInterval>().ToList();
            Messages = Steps.OfType<MessageEvent>().ToList();
            LayoutPanel();
            dgEvents.RowCount = Lines.Count;
        }

        private void LoadTraceSteps(string filename)
        {
            _roverState = new StateInterval{State="WAITING",Height=ActorHeight,X=0f,Y=RoverFromY};
            _gdsState = new StateInterval { State = "WAITING", Height = ActorHeight, X = 0f, Y = GDSFromY };
            _realtimeState = new StateInterval { State = "WAITING", Height = ActorHeight, X = 0f, Y = RealtimeFromY };
            _driverState = new StateInterval { State = "IDLE", Height = ActorHeight, X = 0f, Y = DriverFromY };
            var steps = new List<TraceEvent>();
            var lines = new List<string>();
            var lineTimes = new List<float>();
            string line;
            var count = 0;
            using (var sr = new StreamReader(filename))
                while (null != (line = sr.ReadLine()))
                {
                    var sexpr = _parser.Parse(new StringStream(line));
                    var evt = MakeEvent(sexpr);
                    if (evt == null) continue;
                    evt.Index = count++;
                    steps.Add(evt);
                    lineTimes.Add((float)sexpr[0]);
                    lines.Add(line);
                }

            steps.Add(_roverState);
            steps.Add(_gdsState);
            steps.Add(_realtimeState);
            steps.Add(_driverState);

            Steps = steps;
            Lines = lines;
            LineTimes = lineTimes;
        }

        public TraceEvent MakeEvent(dynamic s)
        {
            var typ = s[4] as string;
            switch (typ)
            {
                case ":SEND":
                {
                    return new MessageEvent
                    {
                        Color = Pens.Red,
                        FromX = (float)s[0],
                        FromY = ActorNameToFromY(s[6])+4f,
                        ToX = (float)s[12],
                        ToY = ActorNameToToY(s[8])+4f
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
                            return _roverState = new StateInterval
                            {
                                State = newState,
                                Height = ActorHeight,
                                X = now,
                                Y = RoverFromY
                            };
                        case "DRIVER":
                        case "AUTONOMY-DRIVER":
                            return _driverState = new StateInterval
                            {
                                State = newState,
                                Height = ActorHeight,
                                X = now,
                                Y = DriverFromY
                            };
                        case "GDS":
                            return _gdsState = new StateInterval {State = newState, Height = ActorHeight, X = now, Y = GDSFromY};
                        case "REALTIME":
                            return _realtimeState = new StateInterval
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
                    return _roverState;
                case "DRIVER":
                case "AUTONOMY-DRIVER":
                    return _driverState;
                case "GDS":
                    return _gdsState;
                case "REALTIME":
                    return _realtimeState;
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

        private void LayoutPanel()
        {
            if (States==null || Messages==null) return;
            pnlDrawing.Width = (int)(_drawScale*States.Max(s => s.MaxX()))+10;
            pnlDrawing.Height = (int)States.Max(s => s.MaxY())*_scaleY + 10;
            pnlDrawing.Invalidate();
        }

        private void pnlDrawing_Paint(object sender, PaintEventArgs e)
        {
            if (States == null || Messages == null) return;
            var g = e.Graphics;
            if (_scaleY != 1)
                g.ScaleTransform(1f, _scaleY);
            var w = (float) pnlDrawing.Width;
            g.FillRectangle(Brushes.LightGray, 0f, RoverFromY, w, ActorHeight);
            g.FillRectangle(Brushes.LightGray, 0f, GDSFromY, w, ActorHeight);
            g.FillRectangle(Brushes.LightGray, 0f, RealtimeFromY, w, ActorHeight);
            g.FillRectangle(Brushes.LightGray, 0f, DriverFromY, w, ActorHeight);

            if (chkStates.Checked)
                foreach (var state in States)
                    state.Draw(g, _drawScale);
            if (chkMessages.Checked)
                foreach (var msg in Messages)
                    msg.Draw(g, _drawScale);

            TextRenderer.DrawText(g, "Rover", SystemFonts.SmallCaptionFont, new Point(0, (int) RoverFromY), Color.Black,
                Color.White,
                TextFormatFlags.Top | TextFormatFlags.Left);
            TextRenderer.DrawText(g, "GDS", SystemFonts.SmallCaptionFont, new Point(0, (int) GDSFromY), Color.Black,
                Color.White,
                TextFormatFlags.Top | TextFormatFlags.Left);
            TextRenderer.DrawText(g, "Realtime", SystemFonts.SmallCaptionFont, new Point(0, (int) RealtimeFromY),
                Color.Black, Color.White,
                TextFormatFlags.Top | TextFormatFlags.Left);
            TextRenderer.DrawText(g, "Driver", SystemFonts.SmallCaptionFont, new Point(0, (int) DriverFromY),
                Color.Black, Color.White,
                TextFormatFlags.Top | TextFormatFlags.Left);

            g.DrawLine(Pens.CornflowerBlue, _time*_drawScale, 0f, _time*_drawScale, 1000f);
        }

        private void pnlDrawing_MouseDown(object sender, MouseEventArgs e)
        {
            var dtime = (int)(_time*_drawScale);
            Console.WriteLine(@"dtime={0} e.X={1}", dtime, e.X);
            _draggingTime = Math.Abs(dtime - e.X) < 5;
            _draggingPanel = !_draggingTime;
            if (_draggingPanel)
                _draggingPanelStartX = pnlDrawing.Location.X - pnlDrawing.PointToScreen(e.Location).X;
        }

        private void pnlDrawing_MouseUp(object sender, MouseEventArgs e)
        {
            _draggingTime = false;
            _draggingPanel = false;
        }

        private void pnlDrawing_MouseMove(object sender, MouseEventArgs e)
        {
            if (_draggingTime)
            {
                UpdateTime(e.X/_drawScale);
                pnlDrawing.Invalidate();
            } else if (_draggingPanel)
            {
                var screenpt = pnlDrawing.PointToScreen(e.Location);
                if (screenpt.Equals(_lastScreenPoint)) return;
                _lastScreenPoint = screenpt;
                pnlDrawing.Location = new Point(_draggingPanelStartX + screenpt.X, 0);
            }
        }

        private void UpdateTime(float time)
        {
            _time = time;
            var idx = LineTimes.BinarySearch(time);
            if (idx < 0) idx = ~idx;
            Console.WriteLine(idx);
            pnlDrawing.Invalidate();

            if (_updatingRowSelection) return;

            var rowCount = dgEvents.DisplayedRowCount(true);
            var topRow = idx - rowCount / 2;
            if (topRow < 0) topRow = 0;
            dgEvents.FirstDisplayedScrollingRowIndex = topRow;
            var rows = dgEvents.Rows;
            idx = Math.Min(rows.Count - 1, idx);
            dgEvents.CurrentCell = rows[idx].Cells[0];
        }

        private void tbScale_ValueChanged(object sender, EventArgs e)
        {
            _drawScale = (float)Math.Pow(2d, (tbScale.Value-50d)/5d);
            LayoutPanel();
        }

        private void dgEvents_CellValueNeeded(object sender, DataGridViewCellValueEventArgs e)
        {
            if (e.RowIndex == dgEvents.RowCount - 1) return;
            e.Value = Lines[e.RowIndex];
        }

        private void toolStripMenuItem2_Click(object sender, EventArgs e)
        {
            _scaleY = 1;
            LayoutPanel();
        }

        private void toolStripMenuItem3_Click(object sender, EventArgs e)
        {
            _scaleY = 2;
            LayoutPanel();
        }

        private void toolStripMenuItem4_Click(object sender, EventArgs e)
        {
            _scaleY = 4;
            LayoutPanel();
        }

        private void toolStripMenuItem5_Click(object sender, EventArgs e)
        {
            _scaleY = 8;
            LayoutPanel();
        }

        private void toolStripMenuItem6_Click(object sender, EventArgs e)
        {
            _scaleY = 16;
            LayoutPanel();
        }

        private void dgEvents_SelectionChanged(object sender, EventArgs e)
        {
            if (_updatingRowSelection) return;
            var cells = dgEvents.SelectedCells;
            if (cells.Count < 1) return;
            var min = cells[0].RowIndex;
            var max = cells[cells.Count - 1].RowIndex;
            var time = LineTimes[min];
            _updatingRowSelection = true;
            UpdateTime(time);
            _updatingRowSelection = false;
        }

        private void chkStates_CheckedChanged(object sender, EventArgs e)
        {
            pnlDrawing.Invalidate();
        }

        private void chkMessages_CheckedChanged(object sender, EventArgs e)
        {
            pnlDrawing.Invalidate();
        }

        private void pnlDrawing_MouseDoubleClick(object sender, MouseEventArgs e)
        {
            _updatingRowSelection = true;
            UpdateTime(e.X / _drawScale);
            _updatingRowSelection = false;
        }
    }
}

using System;
using System.Collections.Generic;
using System.Drawing;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Windows.Forms;
using SExpression;
using ZedGraph;
using Label = System.Windows.Forms.Label;

namespace SolutionPlotter
{
    public partial class Plotter : Form
    {
        public static string DesignCaseVariableName = @"*DESIGN-CASE-NAME*";
        public const int EnumerationMaxCount = 10;
        public Solution CurrentSolution;
        private Variable _xAxisVariable;
        private Variable _yAxisVariable;

        private readonly List<Color> _caseColors = new List<Color>
        {
            Color.Red,Color.LightSeaGreen, Color.DodgerBlue, Color.SandyBrown, Color.Blue,
            Color.Lime, Color.DarkViolet, Color.Sienna, Color.OliveDrab, Color.DarkOrange
        };
        private List<SymbolType> _caseSymbols = new List<SymbolType> { SymbolType.Square, SymbolType.Diamond, SymbolType.Circle, 
            SymbolType.Plus, SymbolType.Triangle, 
            SymbolType.TriangleDown, SymbolType.Star, SymbolType.XCross, SymbolType.Diamond, SymbolType.HDash}; 

        public Plotter()
        {
            InitializeComponent();
        }

        public Solution ReadSolution(string filename)
        {
            using (var sr = new StreamReader(filename))
            {
                sr.ReadLine();
                var vars = ReadList(sr);
                var enumline = sr.ReadLine();
                var trials = ReadInt(sr);
                var count = ReadInt(sr);

                var solutions = new List<dynamic>();
                var parser = new Parser();
                for (var i = 0; i < count; i++)
                {
                    var sexpr = parser.Parse(new StringStream(sr.ReadLine()));
                    solutions.Add(sexpr);
                }

                try
                {
                    var variables = new List<Variable>();
                    for (var i = 0; i < vars.Count; i++)
                    {
                        var v = vars[i];

                        //if ("*BUMPER-TRIGGER-RATE*".Equals(v))
                        //    Console.WriteLine(@"here");

                        if (solutions.Exists(s => s[i] is string))
                        {
                            var vals = solutions.Select(s => s[i].ToString()).Distinct().ToList();
                            if (DesignCaseVariableName.Equals(v))
                                variables.Add(new CaseVariable
                                {
                                    Name = vars[i],
                                    Index = i,
                                    Values = vals,
                                    Varies = vals.Count > 1
                                });
                            else
                                variables.Add(new SymbolEnumeration
                                {
                                    Name = vars[i],
                                    Index = i,
                                    Values = vals,
                                    Varies = vals.Count > 1
                                });
                        }
                        else
                        {
                            //var vals = DistinctValues(solutions, i);
                            var vals = solutions.Select(s => s[i]).Distinct().ToList();
                            if (vals.Count > 0 && vals.Count <= EnumerationMaxCount)
                                variables.Add(new DoubleEnumeration
                                {
                                    Name = vars[i],
                                    Index = i,
                                    Min = vals.Min(v1 => (double)v1),
                                    Max = vals.Max(v1 => (double)v1),
                                    Values = vals,
                                    Varies = vals.Count > 1
                                });
                            else
                            {
                                variables.Add(new Output
                                {
                                    Name = vars[i],
                                    Index = i,
                                    Min = vals.Min(v1 => (double)v1),
                                    Max = vals.Max(v1 => (double)v1),
                                    Values = vals,
                                    Varies = vals.Count > 1
                                });
                            }
                        }
                    }

                    return new Solution
                    {
                        Variables = variables,
                        Solutions = solutions,
                        Trials = trials
                    };
                }
                catch (Exception)
                {
                    Application.Exit();
                    throw;
                }
            }
        }

        private List<dynamic> ReadList(TextReader sr)
        {
            return ReadList(sr.ReadLine());
        }

        private List<dynamic> ReadList(string s)
        {
            s = s.Trim();
            if (s.Length < 2) throw new Exception(@"malformed list: no content");
            if (s[0] != '(' || s[s.Length - 1] != ')') throw new Exception(@"malformed list: missing parens");
            s = s.Substring(1, s.Length - 2);
            var tokens = s.Split(' ');
            var vals = tokens.Select(t =>
            {
                double v;
                return double.TryParse(t, out v) ? (dynamic)v : (dynamic)t;
            }).ToList();
            return vals;
        }

        private int ReadInt(TextReader sr)
        {
            var l = sr.ReadLine();
            return int.Parse(l);
        }

        private void openToolStripMenuItem_Click(object sender, EventArgs e)
        {
            var d = new OpenFileDialog();
            var result = d.ShowDialog();
            if (result != DialogResult.OK) return;
            CurrentSolution = ReadSolution(d.FileName);
            LoadGui(CurrentSolution);
            Text = d.FileName;
        }

        private void LoadGui(Solution s)
        {
            var xlist = new List<CheckBox>();
            var ylist = new List<CheckBox>();
            s.Variables = s.Variables.OrderBy(v => v.Name).ToList();

            foreach (var v in s.Variables)
                v.SelectedValues = v.Values.Select(v1 => v1).ToList(); // copy list

            //var temp = s.Variables.FirstOrDefault(v => DesignCaseVariableName.Equals(v.Name));
            //Console.WriteLine(temp);

            // Load constants
            var constants = s.Variables.Where(v => !v.Varies).ToList();
            foreach (var v in constants)
            {
                var p = new Panel {Dock = DockStyle.Top, Height = 20};
                p.Controls.Add(new Label {Text = v.Values.FirstOrDefault().ToString(CultureInfo.InvariantCulture), AutoSize = false, Width = 60, Dock = DockStyle.Right});
                p.Controls.Add(new Label {Text = v.Name, AutoSize = false, Dock = DockStyle.Fill});
                pnlConstants.Controls.Add(p);
            }
            var variables = s.Variables.Where(v => v.Varies).ToList();
            foreach (var v in variables.Where(v => (v is Output || v is DoubleEnumeration) && v.Values.Count > EnumerationMaxCount).Reverse())
            {
                pnlVariables.Controls.Add(new Panel {Height = 4, Dock = DockStyle.Top});
                var varcb = CheckboxLabel(v, xlist, ylist);
                varcb.XBox.CheckedChanged += (sender, args) => HandleXBoxChanged((CheckBox) sender, xlist);
                varcb.YBox.CheckedChanged += (sender, args) => HandleYBoxChanged((CheckBox) sender, ylist);
                pnlVariables.Controls.Add(varcb);
            }
            // load enumerations
            var enumerations = variables.Where(v => v.Values.Count > 0 && v.Values.Count <= EnumerationMaxCount).Reverse().ToList();
            foreach (var v in enumerations)
            {
                pnlVariables.Controls.Add(new Panel { Height = 4, Dock = DockStyle.Top });
                var p = new FlowLayoutPanel {AutoSize = true, Dock = DockStyle.Top};
                v.SelectedValues = v.Values.Select(v1 => v1).ToList(); // copy list
                foreach (var val in v.Values)
                {
                    var label = new EnumerationLabel
                    {
                        Text = val.ToString(CultureInfo.InvariantCulture),
                        AutoSize = true,
                        Variable = v,
                        Value = val
                    };

                    label.MouseClick += (sender, args) =>
                    {
                        label.Selected = !label.Selected;
                        label.BackColor = label.Selected ? Color.LightCoral : Color.WhiteSmoke;
                        if (label.Selected)
                        {
                            if (!label.Variable.SelectedValues.Contains(label.Value))
                                label.Variable.SelectedValues.Add(label.Value);
                        }
                        else
                        {
                            label.Variable.SelectedValues.Remove(label.Value);
                        }
                        UpdatePlot();
                    };

                    p.Controls.Add(label);
                }
                pnlVariables.Controls.Add(p);
                if (v is DoubleEnumeration)
                {
                    var varcb = CheckboxLabel(v, xlist, ylist);
                    varcb.XBox.CheckedChanged += (sender, args) => HandleXBoxChanged((CheckBox) sender, xlist);
                    varcb.YBox.CheckedChanged += (sender, args) => HandleYBoxChanged((CheckBox) sender, ylist);
                    pnlVariables.Controls.Add(varcb);
                }
                else
                {
                    var varlbl = new Label {Text = v.Name, Dock = DockStyle.Top};
                    pnlVariables.Controls.Add(varlbl);
                }
            }

            var caseVar = s.Variables.OfType<CaseVariable>().FirstOrDefault();
            if (caseVar != null)
            {
                for (var i=0; i<caseVar.Values.Count;i++)
                {
                    var val = caseVar.Values[i];
                    var cb = new CheckBox {Text = val as string, Dock = DockStyle.Top};
                    cb.CheckedChanged += (sender, args) =>
                    {
                        UpdateCaseColors();
                        UpdatePlot();
                    };
                    pnlCases.Controls.Add(cb);
                }
            }
        }

        private void UpdateCaseColors()
        {
            var idx = 0;
            foreach (var c in pnlCases.Controls.OfType<CheckBox>().Reverse())
            {
                if (c.Checked && idx < _caseColors.Count)
                {
                    if (idx < _caseColors.Count)
                    {
                        c.Tag = _caseSymbols[idx];
                        c.BackColor = _caseColors[idx];
                    }
                    else
                    {
                        c.Tag = SymbolType.Default;
                        c.BackColor = Color.Gray;
                    }
                    idx++;
                }
                else
                {
                    c.Tag = SymbolType.Default;
                    c.BackColor = Color.Gray;                
                }
            }
        }

        private void HandleXBoxChanged(CheckBox cb, IEnumerable<CheckBox> xlist)
        {
            var c = cb.Checked;
            if (!c) return;
            var v = ((VariablePanel) cb.Parent).Variable;
            _xAxisVariable = v;
            foreach (var othercb in xlist.Where(othercb => othercb != cb))
                othercb.Checked = false;
            UpdatePlot();
        }

        private void HandleYBoxChanged(CheckBox cb, IEnumerable<CheckBox> ylist)
        {
            var c = cb.Checked;
            if (!c) return;
            var v = ((VariablePanel) cb.Parent).Variable;
            _yAxisVariable = v;
            foreach (var othercb in ylist.Where(othercb => othercb != cb))
                othercb.Checked = false;
            UpdatePlot();
        }

        private VariablePanel CheckboxLabel(Variable v, List<CheckBox> xlist, List<CheckBox> ylist)
        {
            var p = new VariablePanel {Height = 20, Dock = DockStyle.Top, Variable = v};
            var x = new CheckBox {Text = "", AutoSize = true, Dock = DockStyle.Left};
            var y = new CheckBox {Text = v.Name, AutoSize = true, Dock = DockStyle.Left};
            p.Controls.Add(y);
            p.Controls.Add(x);
            xlist.Add(x);
            ylist.Add(y);
            p.XBox = x;
            p.YBox = y;
            return p;
        }

        private List<dynamic> DistinctValues(IEnumerable<dynamic> solutions, int i)
        {
            var distinct = new List<dynamic>();
            foreach (var val in solutions.Select(s => s is dynamic[] && s[i]).Where(val => !distinct.Contains(val)))
                distinct.Add(val);
            distinct.Sort();
            return distinct;
        }

        private void UpdateSelectedCases()
        {
            var caseVar = CurrentSolution.Variables.FirstOrDefault(v => v.Name.Equals(DesignCaseVariableName));
            if (caseVar == null) return;
            caseVar.SelectedValues = new List<dynamic>();
            foreach (var ctrl in pnlCases.Controls)
            {
                var cb = ctrl as CheckBox;
                if (cb == null) continue;
                if (cb.Checked) caseVar.SelectedValues.Add(string.Intern(cb.Text));
            }
        }

        private void UpdatePlot()
        {
            UpdateSelectedCases();
            if (_xAxisVariable == null || _yAxisVariable == null) return;
            var gp = zgc1.GraphPane;
            gp.CurveList.Clear();
            gp.Title.Text = @"Simulation run";
            gp.XAxis.Title.Text = _xAxisVariable.Name;
            gp.YAxis.Title.Text = _yAxisVariable.Name;

            var matchingSolutions =
                CurrentSolution.Solutions.Where(soln => CurrentSolution.Variables.TrueForAll(v => v.Include(soln))).ToList();

            var caseVar = CurrentSolution.Variables.FirstOrDefault(v => v.Name.Equals(DesignCaseVariableName));
            if (caseVar == null)
            {
                var points = new PointPairList();
                foreach (var soln in matchingSolutions)
                    points.Add(soln[_xAxisVariable.Index], soln[_yAxisVariable.Index]);
                var curve = gp.AddCurve("All solutions", points, Color.Red, SymbolType.Square);
            }
            else
            {
                var caseIndex = caseVar.Index;
                var otherSolutions = new List<dynamic>(); 
                foreach (var ctrl in pnlCases.Controls)
                {
                    var cb = ctrl as CheckBox;
                    if (cb == null) continue;
                    var text = cb.Text;
                    if (cb.Checked)
                    {
                        var points = new PointPairList();
                        foreach (var soln in matchingSolutions.Where(s => s[caseIndex].Equals(text)))
                            points.Add(soln[_xAxisVariable.Index], soln[_yAxisVariable.Index]);
                        var curve = gp.AddCurve(cb.Text, points, cb.BackColor, (SymbolType)cb.Tag);
                        curve.Line.IsVisible = false;
                    }
                    else
                        otherSolutions.AddRange(matchingSolutions.Where(s => s[caseIndex].Equals(text)));
                }
                var points1 = new PointPairList();
                foreach (var soln in otherSolutions)
                    points1.Add(soln[_xAxisVariable.Index], soln[_yAxisVariable.Index]);
                var curve1 = gp.AddCurve("Other", points1, Color.Gray, SymbolType.Default);
                curve1.Line.IsVisible = false;
            }

            zgc1.AxisChange();
            zgc1.Invalidate();
        }

        

        private void printPnlVariableSizeToolStripMenuItem_Click(object sender, EventArgs e)
        {
            Console.WriteLine(pnlVariables.Size);
        }

        private void drawLinesToolStripMenuItem_Click(object sender, EventArgs e)
        {
            drawLinesToolStripMenuItem.Checked = !drawLinesToolStripMenuItem.Checked;
            foreach (var t in zgc1.GraphPane.CurveList.OfType<LineItem>())
                t.Line.IsVisible = drawLinesToolStripMenuItem.Checked;
            zgc1.Invalidate();
        }
    }

    public class Solution
    {
        public List<dynamic> Solutions;
        public int Trials;
        public List<Variable> Variables;
    }

    public class Variable
    {
        public int Index;
        public double Max;
        public double Min;
        public string Name;
        public bool Varies;

        public List<dynamic> SelectedValues;
        public List<dynamic> Values;

        public virtual bool Include(dynamic a)
        {
            return SelectedValues.Contains(a[Index]);
        }

        public override string ToString()
        {
            return "{"+Name+"}";
        }
    }

    public class DoubleEnumeration : Variable
    {
    }

    public class SymbolEnumeration : Variable
    {
    }

    public class Random : Variable
    {
        public double High;
        public double Low;
    }

    public class Output : Variable
    {
    }

    public class EnumerationLabel : Label
    {
        public bool Selected = true;
        public dynamic Value;
        public Variable Variable;

        public EnumerationLabel()
        {
            BackColor = Color.LightCoral;
        }
    }

    public class CaseVariable : Variable
    {
        
    }

    public class VariablePanel : Panel
    {
        public Variable Variable;
        public CheckBox XBox;
        public CheckBox YBox;
    }
}
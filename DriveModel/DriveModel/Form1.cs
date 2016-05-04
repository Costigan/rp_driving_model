using System;
using System.Collections.Generic;
using System.Drawing;
using System.Globalization;
using System.Linq;
using System.Windows.Forms;

namespace DriveModel
{
    public partial class TheModel : Form
    {
        public int ImageSize = 1024*1024;
        public int BitsPerPixel = 12;
        public int SubFraming = 2;
        public float Compression = 4;
        public int DownlinkRate = 100000;
        public float DownlinkLatency = 10;
        public float TimeToDownlinkPair = 0;

        public float UplinkLatency = 10f;
        public float LookaheadDistance = 3f;
        public float DrivingSpeed = 0.1f;

        public float DriverPairDecision = 10;
        public float GroundProcessing = 2;
        public float RTScienceConsult = 30;
        public float GroundTurnaround = 0;
        public float BumperEval = 300;

        public float RTScienceConsultationProb = 0f;
        public float BumperTriggerProb = 0f;

        private List<OutputLink> _outputs = new List<OutputLink>();

        public float MERTotalCycleTime;
        public float MERWaitingTime;
        public float MERDutyCycle;
        public float MERSpeedMadeGood;
        public float MERTimeToDrive100M;

        public float ContinuousTimeToDrive100M;
        public float ContinuousSpeedMadeGood;

        public int DebugLevel = 1;

        public TheModel()
        {
            InitializeComponent();
            InitializeVariables();
        }

        private void InitializeVariables()
        {
            LinkVariable(ImageSize, "image size (pixels)", () => ImageSize, (v) => ImageSize = v);
            LinkVariable(BitsPerPixel, "bits per pixel", () => BitsPerPixel, (v) => BitsPerPixel = v);
            LinkVariable(SubFraming, "sub_framing", () => SubFraming, (v) => SubFraming = v);
            LinkVariable(Compression, "compression", () => Compression, (v) => Compression = v);
            LinkVariable(DownlinkRate, "downlink (bits/sec)", () => DownlinkRate, (v) => DownlinkRate = v);
            LinkVariable(DownlinkLatency, "downlink latency", () => DownlinkLatency, (v) => DownlinkLatency = v);

            LinkVariable(DriverPairDecision, "driver pair decision (sec)", () => DriverPairDecision, (v) => DriverPairDecision = v);
            LinkVariable(GroundProcessing, "ground processing (sec)", () => GroundProcessing, (v) => GroundProcessing = v);
            LinkVariable(RTScienceConsult, "RT science consult", () => RTScienceConsult, (v) => RTScienceConsult = v);
            LinkVariable(BumperEval, "virtual bumper eval (sec)", () => BumperEval, (v) => BumperEval = v);

            LinkVariable(RTScienceConsultationProb, "RT science consultation", () => RTScienceConsultationProb, (v) => RTScienceConsultationProb = v);
            LinkVariable(BumperTriggerProb, "virtual bumper trigger", () => BumperTriggerProb, (v) => BumperTriggerProb = v);

            LinkVariable(UplinkLatency, "uplink latency", () => UplinkLatency, (v) => UplinkLatency = v);
            LinkVariable(LookaheadDistance, "lookahead distance", () => LookaheadDistance, (v) => LookaheadDistance = v);
            LinkVariable(DrivingSpeed, "driving speed", () => DrivingSpeed, (v) => DrivingSpeed = v);

            LinkOutput("time to downlink pair", () => TimeToDownlinkPair);
            LinkOutput("ground turnaround (sec)", () => GroundTurnaround);

            LinkOutput("MER total cycle time", ()=>MERTotalCycleTime);
            LinkOutput("MER waiting time", () => MERWaitingTime);
            LinkOutput("MER duty cycle", () => MERDutyCycle);
            LinkOutput("MER speed made good", () => MERSpeedMadeGood);
            LinkOutput("MER time to drive 100m", () => MERTimeToDrive100M);

            LinkOutput("continuous speed made good", ()=>ContinuousSpeedMadeGood);
            LinkOutput("continuous time to drive", () => ContinuousTimeToDrive100M);
        }

        private void LinkVariable(int initval, string name, Func<int> getter, Action<int> setter)
        {
            var l = FindLabel(name);
            var tb = FindTextBox(l);

            if (tb == null)
            {
                Console.WriteLine(@"Couldn't find btext box for {0}", name);
                return;
            }

            l.ForeColor = Color.Black;
            tb.Text = initval.ToString(CultureInfo.InvariantCulture);

            tb.TextChanged += (sender, args) => tb.ForeColor = Color.Gray;

            tb.KeyDown += (sender, args) =>
            {
                if (args.KeyData == Keys.Return)
                {
                    int val;
                    if (int.TryParse(tb.Text, out val))
                    {
                        setter(val);
                        tb.ForeColor = Color.Black;
                        RunMaybe();
                    }
                    else
                        tb.ForeColor = Color.Red;
                }
                if (args.KeyData == Keys.Up)
                {
                    setter(getter() + 1);
                }
                if (args.KeyData == Keys.Down)
                {
                    setter(getter() - 1);
                }
            };
        }

        private void LinkVariable(float initval, string name, Func<float> getter, Action<float> setter)
        {
            var l = FindLabel(name);
            var tb = FindTextBox(l);

            if (tb == null)
            {
                Console.WriteLine(@"Couldn't find btext box for {0}", name);
                return;
            }

            l.ForeColor = Color.Black;
            tb.Text = initval.ToString(CultureInfo.InvariantCulture);

            tb.TextChanged += (sender, args) => tb.ForeColor = Color.Gray;

            tb.KeyDown += (sender, args) =>
            {
                if (args.KeyData == Keys.Return)
                {
                    float val;
                    if (float.TryParse(tb.Text, out val))
                    {
                        setter(val);
                        tb.ForeColor = Color.Black;
                        RunMaybe();
                    }
                    else
                        tb.ForeColor = Color.Red;
                }
                if (args.KeyData == Keys.Up)
                {
                    setter(getter() + 1);
                }
                if (args.KeyData == Keys.Down)
                {
                    setter(getter() - 1);
                }
            };
        }

        private Label FindLabel(string label)
        {
            return FindLabel(this, label);
        }

        private Label FindLabel(TheModel theModel, string label)
        {
            var c1 = Controls[0];

            foreach (var c in Controls)
            {
                var v = FindLabel(c as Panel, label);
                if (v != null) return v;
            }
            return null;
        }

        private Label FindLabel(Panel p, string label)
        {
            if (p == null) return null;
            foreach (var sc in p.Controls)
            {
                var l = sc as Label;
                if (l != null && l.Text.Equals(label))
                    return l;
                if (!(sc is Panel)) continue;
                var val = FindLabel(sc as Panel, label);
                if (val != null) return val;
            }
            return null;
        }

        private TextBox FindTextBox(Label l)
        {
            return l == null ? null : l.Parent.Controls.OfType<TextBox>().FirstOrDefault();
        }

        private void LinkOutput(string label, Func<dynamic> getter)
        {
            var l = FindLabel(label);
            var tb = FindTextBox(l);
            if (tb == null)
            {
                Console.WriteLine(@"Couldn't find btext box for {0}", label);
                return;
            }
            l.ForeColor = Color.Black;
            _outputs.Add(new OutputLink{Getter = getter, TheBox = tb});
        }

        private void btnRecalculate_Click(object sender, EventArgs e)
        {
            UpdateInputs();
            RunModels();
        }

        private void UpdateInputs()
        {
            TimeToDownlinkPair = DownlinkLatency + (2*ImageSize*BitsPerPixel)/(SubFraming*Compression*DownlinkRate);
            GroundTurnaround = DriverPairDecision + GroundProcessing + RTScienceConsultationProb * RTScienceConsult +
                                  BumperTriggerProb*BumperEval + UplinkLatency;
        }

        private void RunMaybe()
        {
            if (ckbRecalculate.Checked)
                RunModels();
        }

        private void RunModels()
        {
            try
            {
                RunMER();
                RunContinuousDriving();  // Assumes MER has been run first
                ShowOutputs();
            }
            catch (Exception e1)
            {
                Console.WriteLine(e1);
            }
        }

        private void RunMER()
        {
            MERWaitingTime = TimeToDownlinkPair + GroundTurnaround;  // includes uplink latency
            var drivingTime = LookaheadDistance/DrivingSpeed;
            MERTotalCycleTime = MERWaitingTime + drivingTime;
            MERDutyCycle = drivingTime/MERTotalCycleTime;
            MERSpeedMadeGood = LookaheadDistance / MERTotalCycleTime;
            MERTimeToDrive100M = 100/(MERSpeedMadeGood*60f);
        }

        private void ShowOutputs()
        {
            foreach (var o in _outputs)
                o.Output();
        }

        private enum RoverState
        {
            Waiting,
            Driving
        }

        private void RunContinuousDriving()
        {
            var targetPos = 100f;
            var q = new SimQueue();
            var time = 0f;
            var tick = 0.1f;
            var pos = 0f;
            var lastStereoTime = 0f;
            var lastStereoPos = 0f;
            var stereoArrivals = new Queue<float>();
            var driveTarget = 0f;
            var roverState = RoverState.Waiting;

            Action commandStep = null, uplinkStep = null, driveStep = null, cameraStep = null, roverStep = null;

            cameraStep = () =>
            {
                lastStereoPos = pos;
                lastStereoTime = time;
                Debug(3, ()=>Console.WriteLine(@"{0:f6} Camera step", time));
                var nextArrival = time + TimeToDownlinkPair;
                stereoArrivals.Enqueue(nextArrival);
                q.Enqueue(nextArrival, cameraStep);
                q.Enqueue(time + TimeToDownlinkPair, commandStep);
            };
            commandStep = () =>
            {
                if (stereoArrivals.Count > 0 && stereoArrivals.Peek() >= time)
                {
                    Debug(1, ()=>Console.WriteLine(@"{0:f6} Driver starts work", time));
                    stereoArrivals.Dequeue();
                    q.Enqueue(time + DriverPairDecision, uplinkStep);
                }
                else
                {
                    //Console.WriteLine(@"commandStep ticking");
                    q.Enqueue(time + tick, commandStep);
                }
            };
            uplinkStep = () =>
            {
                Debug(1, ()=>Console.WriteLine(@"{0:f6} Sending command", time));
                q.Enqueue(time+UplinkLatency, driveStep);
            };
            driveStep = () =>
            {
                var perceptionLimit = Math.Min(targetPos, lastStereoPos + LookaheadDistance);
                var timeToLimit = (perceptionLimit - pos)/DrivingSpeed;
                if (timeToLimit > 0f)
                {
                    Debug(3, ()=>Console.WriteLine(@"{0:F6} last stereo position={1} last stereo time={2} perceptionLimit={3}", time, lastStereoPos, lastStereoTime, perceptionLimit));
                    Debug(1, ()=>Console.WriteLine(@"{0:f6} Start driving for {1} sec and {2} meters", time, timeToLimit,
                        perceptionLimit-pos));
                    driveTarget = perceptionLimit;
                }
            };
            roverStep = () =>
            {
                if (pos < driveTarget)
                {
                    Debug(4, () => Console.WriteLine(@"{0:F6} drive from {1} to {2}", time, pos, driveTarget));
                    Debug(1, () =>
                    {
                        if (roverState != RoverState.Driving)
                            Console.WriteLine(@"{0:F6} waiting->driving", time);
                    });
                    roverState = RoverState.Driving;
                    pos += tick*DrivingSpeed;
                }
                else
                {
                    Debug(1, () =>
                    {
                        if (roverState != RoverState.Waiting)
                            Console.WriteLine(@"{0:F6} driving->waiting", time);
                    });
                    roverState = RoverState.Waiting;
                    //Console.WriteLine(@"{0:F6} wait at pos {1}", time, pos);               
                }

                q.Enqueue(time+tick,roverStep);
            };
            q.Enqueue(0f, cameraStep);
            q.Enqueue(0f, commandStep);
            q.Enqueue(0f, roverStep);

            Console.WriteLine(@"START CONTINUOUS DRIVING ***");
            while (pos < targetPos && !q.IsEmpty() && time < 5000f)
            {
                //Console.WriteLine(@"{0:f6} pos={1} q.Count={2} nextEvent={3}", time, pos, q.Events.Count, q.Events.Count > 0 ? q.Events[0].Time : -1f);
                q.TakeStep(ref time);
            }

            ContinuousTimeToDrive100M = time/60f;
            ContinuousSpeedMadeGood = targetPos/time;
        }

        private void Debug(int debug, Action a)
        {
            if (debug <= DebugLevel)
                a();
        }
    }

    public class OutputLink
    {
        public Func<dynamic> Getter;
        public TextBox TheBox;
        public void Output()
        {
            TheBox.Text = Getter().ToString();
        }
    }

    public class SimEvent : IComparable<SimEvent>
    {
        public float Time;
        public Action Action;

        public int Compare(SimEvent x, SimEvent y)
        {
            if (x.Time < y.Time) return -1;
            return x.Time.Equals(y.Time) ? 0 : 1;
        }

        public int CompareTo(SimEvent other)
        {
            if (Time < other.Time) return -1;
            return Time.Equals(other.Time) ? 0 : 1;
        }
    }

    public class SimQueue
    {
        //public List<SimEvent> Events = new List<SimEvent>();
        public Heap<SimEvent> Events = new MinHeap<SimEvent>(); 

        public bool IsEmpty()
        {
            return Events.Count < 1;
        }

        public void Enqueue(float time, Action action)
        {
            Enqueue(new SimEvent{Time = time, Action = action });
        }

        public void Enqueue(SimEvent e)
        {
            Events.Add(e);
            //Events = Events.OrderBy(ev => ev.Time).ToList();
        }

        public void EnqueueAtNext(float now, Action action)
        {
            if (Events.Count > 0 && Events.GetMin().Time > now)
                Enqueue(Events.GetMin().Time, action);
            else
                Enqueue(now + 0.1f, action);
        }


        public void TakeStep(ref float time)
        {
            if (Events.Count < 1) return;
            var e = Events.ExtractDominating();
            time = e.Time;
            e.Action();
        }
    }

}

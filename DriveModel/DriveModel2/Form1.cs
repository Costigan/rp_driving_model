using System;
using System.Collections.Generic;
using System.Diagnostics.Eventing.Reader;
using System.Drawing;
using System.Globalization;
using System.Linq;
using System.Windows.Forms;
using DriveModel;

namespace DriveModel2
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

        public float Time = 0f;
        public SimQueue EventQueue;
        public const float TickDelay = 0.1f;

        public Queue<ImagePair> DownlinkQueue;
        public Queue<float> UplinkQueue;

        public Rover Rover;
        public Driver Driver;
        public StereoCamera Camera;

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

            Time = 0f;
            EventQueue = new SimQueue();

            Camera = new StereoCamera();
            DownlinkQueue = new Queue<ImagePair>();
            UplinkQueue = new Queue<float>();  
            Rover = new Rover { Position = 0f, UplinkQueue = UplinkQueue };
            Driver = new Driver {DownlinkQueue = DownlinkQueue, UplinkQueue = UplinkQueue};

            var lastStereoTime = 0f;
            var lastStereoPos = 0f;

            var driveTarget = 0f;
            var roverState = RoverState.Waiting;

            Action commandStep = null, uplinkStep = null, driveStep = null, cameraStep = null, roverStep = null;

            cameraStep = () =>
            {
                lastStereoPos = Rover.Position;
                lastStereoTime = Time;
                Debug(3, ()=>Console.WriteLine(@"{0:f6} Camera step", Time));
                DownlinkQueue.Enqueue(new ImagePair(Time, TimeToDownlinkPair));
                EventQueue.Enqueue(Time+TimeToDownlinkPair, cameraStep);
                EventQueue.Enqueue(Time + TimeToDownlinkPair, commandStep);
            };
            commandStep = () =>
            {
                if (DownlinkQueue.Count > 0 && DownlinkQueue.Peek().ArrivalTime >= Time)
                {
                    Debug(1, () => Console.WriteLine(@"{0:f6} Driver starts work", Time));
                    DownlinkQueue.Dequeue();
                    EventQueue.Enqueue(Time + DriverPairDecision, uplinkStep);
                }
                else
                {
                    //Console.WriteLine(@"commandStep ticking");
                    EventQueue.Enqueue(Time + TickDelay, commandStep);
                }
            };
            uplinkStep = () =>
            {
                Debug(1, () => Console.WriteLine(@"{0:f6} Sending command", Time));
                EventQueue.Enqueue(Time + UplinkLatency, driveStep);
            };
            driveStep = () =>
            {
                var perceptionLimit = Math.Min(targetPos, lastStereoPos + LookaheadDistance);
                var timeToLimit = (perceptionLimit - Rover.Position)/DrivingSpeed;
                if (timeToLimit > 0f)
                {
                    Debug(3, () => Console.WriteLine(@"{0:F6} last stereo position={1} last stereo time={2} perceptionLimit={3}", Time, lastStereoPos, lastStereoTime, perceptionLimit));
                    Debug(1, () => Console.WriteLine(@"{0:f6} Start driving for {1} sec and {2} meters", Time, timeToLimit,
                        perceptionLimit - Rover.Position));
                    driveTarget = perceptionLimit;
                }
            };
            roverStep = () =>
            {
                if (Rover.Position < driveTarget)
                {
                    Debug(4, () => Console.WriteLine(@"{0:F6} drive from {1} to {2}", Time, Rover.Position, driveTarget));
                    Debug(1, () =>
                    {
                        if (roverState != RoverState.Driving)
                            Console.WriteLine(@"{0:F6} waiting->driving", Time);
                    });
                    roverState = RoverState.Driving;
                    Rover.Position += TickDelay * DrivingSpeed;
                }
                else
                {
                    Debug(1, () =>
                    {
                        if (roverState != RoverState.Waiting)
                            Console.WriteLine(@"{0:F6} driving->waiting", Time);
                    });
                    roverState = RoverState.Waiting;
                    //Console.WriteLine(@"{0:F6} wait at pos {1}", time, pos);               
                }

                EventQueue.Enqueue(Time + TickDelay, roverStep);
            };
            EventQueue.Enqueue(0f, cameraStep);
            EventQueue.Enqueue(0f, commandStep);
            EventQueue.Enqueue(0f, roverStep);

            Console.WriteLine(@"START CONTINUOUS DRIVING ***");
            while (Rover.Position < targetPos && !EventQueue.IsEmpty() && Time < 5000f)
            {
                //Console.WriteLine(@"{0:f6} pos={1} q.Count={2} nextEvent={3}", time, pos, q.Events.Count, q.Events.Count > 0 ? q.Events[0].Time : -1f);
                EventQueue.TakeStep(ref Time);
            }

            ContinuousTimeToDrive100M = Time / 60f;
            ContinuousSpeedMadeGood = targetPos / Time;
        }

        public void Debug(int debug, Action a)
        {
            if (debug <= DebugLevel)
                a();
        }

    }

    public class ModelComponent
    {
        public TheModel Model;
    }

    public class Driver : ModelComponent
    {
        public enum State
        {
            Idle,
            EvaluatingImagePair,
            EvaluatingBumperStop
        }

        private State _state;

        public State CurrentState
        {
            get { return _state; }
            set
            {
                if (_state != value)
                    Model.Debug(1,
                        () =>
                            Console.WriteLine(@"{0:F6} driver changes state from {1} to {2}", TheModel.Time, _state,
                                value));
                _state = value;
            }
        }

        public Queue<ImagePair> DownlinkQueue;
        public Queue<float> UplinkQueue; 
        public float WorkingUntil;
        public ImagePair CurrentImagePair;

        public void Tick()
        {
            switch (CurrentState)
            {
                case State.Idle:
                    Model.EventQueue.AtNextTick(Tick);
                    break;
                case State.EvaluatingImagePair:
                    if (Model.Time >= WorkingUntil)
                        SendCommand();
                    else
                        Model.EventQueue.AtNextTick(Tick);
                    break;
                case State.EvaluatingBumperStop:
                    if (Model.Time >= WorkingUntil)
                    {
                        GetLatestImage();
                        CurrentState = State.EvaluatingImagePair;
                    }
                    else
                        Model.EventQueue.AtNextTick(Tick);
                    break;
            }
        }

        public void HandleImagePair(ImagePair pair)
        {
            CurrentState = State.EvaluatingImagePair;
            WorkingUntil = pair.ArrivalTime + Model.GroundProcessing + Model.DriverPairDecision;
            TheModel.EventQueue.Enqueue(targetTime, () => Tick(TheModel.Time));
        }
    }

    public class Rover : ModelComponent
    {
        public enum State
        {
            Waiting,
            Driving
        }

        private State _state;

        public State CurrentState
        {
            get { return _state; }
            set
            {
                if (_state != value)
                    TheModel.Debug(1,
                        () =>
                            Console.WriteLine(@"{0:F6} rover changes state from {1} to {2}", TheModel.Time, _state,
                                value));
                _state = value;
            }
        }

        public float Position;
        public Queue<float> UplinkQueue;
    }


    public class StereoCamera : ModelComponent
    {
        public Queue<ImagePair> Queue;

        public void TakeImage()
        {
            Queue.Enqueue(new ImagePair(Model.Time, Model.DownlinkLatency));
        }
    }

    public class ImagePair : ModelComponent
    {
        public static int Counter = 0;
        public float ExposureTime;
        public float ArrivalTime;
        public int SequenceNumber;

        public ImagePair(float now, float downlinkLatency)
        {
            ExposureTime = now;
            ArrivalTime = now + downlinkLatency;
            SequenceNumber = Counter++;
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

        public void AtNextTick(Action action)
        {
            Enqueue(TheModel.Time+TheModel.TickDelay, action);
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

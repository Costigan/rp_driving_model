using System.Drawing;

namespace TraceVisualization
{
    public class StateInterval : TraceEvent
    {
        public static Brush RoverWaiting = Brushes.AntiqueWhite;
        public static Brush RoverDriving = Brushes.LightGreen;
        public static Brush DriverIdle = Brushes.AntiqueWhite;
        public static Brush DriverEvaluating = Brushes.Green;
        public static Brush DriverToPSR = Brushes.Plum;
        public static Brush DriverWaitingForAuthorization = Brushes.PowderBlue;
        public static Brush UnknownColor = Brushes.Black;

        public float X;
        public float Y;
        public float Width;
        public float Height;
        public Brush Color;
        private string _state;

        public string State
        {
            get { return _state; }
            set
            {
                switch (_state = value)
                {
                    case "WAITING":
                        Color = RoverWaiting;
                        break;
                    case "DRIVING":
                        Color = RoverDriving;
                        break;
                    case "IDLE":
                        Color = DriverIdle;
                        break;
                    case "EVALUATING-IMAGE":
                        Color = DriverEvaluating;
                        break;
                    case "DRIVING-TO-PSR-EDGE":
                        Color = DriverToPSR;
                        break;
                    case "WAITING-FOR-AUTHORIZATION-TO-ENTER-PSR":
                        Color = DriverWaitingForAuthorization;
                        break;
                    default:
                        Color = UnknownColor;
                        break;
                }
            }
        }

        public override void Draw(Graphics g, float s)
        {
            g.FillRectangle(Color, X*s, Y, Width*s, Height);
        }
        public override float MaxX()
        {
            return X + Width;
        }
        public override float MaxY()
        {
            return Y + Height;
        }

    }
}
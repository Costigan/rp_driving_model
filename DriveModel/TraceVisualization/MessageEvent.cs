using System;
using System.Drawing;

namespace TraceVisualization
{
    public class MessageEvent : TraceEvent
    {
        public float FromX;
        public float FromY;
        public float ToX;
        public float ToY;
        public Pen Color;

        public override void Draw(Graphics g, float s)
        {
            g.DrawLine(Color,FromX*s,FromY,ToX*s,ToY);
        }

        public override float MaxX()
        {
            return Math.Max(FromX,ToX);
        }
        public override float MaxY()
        {
            return Math.Max(FromY, ToY);
        }
    }
}
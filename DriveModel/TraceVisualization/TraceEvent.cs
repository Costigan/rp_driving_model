using System.Drawing;

namespace TraceVisualization
{
    public class TraceEvent
    {
        public int Index;

        public virtual void Draw(Graphics g, float s)
        {
        }

        public virtual float MaxX()
        {
            return 0f;
        }

        public virtual float MaxY()
        {
            return 0f;
        }
    }
}
namespace TraceVisualization
{
    partial class Broken
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.menuStrip1 = new System.Windows.Forms.MenuStrip();
            this.fileToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.openToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.panel1 = new System.Windows.Forms.Panel();
            this.pnlHolder = new System.Windows.Forms.Panel();
            this.plnDrawing = new System.Windows.Forms.Panel();
            this.dgEvents = new System.Windows.Forms.DataGridView();
            this.splitter1 = new System.Windows.Forms.Splitter();
            this.tbScale = new System.Windows.Forms.TrackBar();
            this.menuStrip1.SuspendLayout();
            this.panel1.SuspendLayout();
            this.pnlHolder.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.dgEvents)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.tbScale)).BeginInit();
            this.SuspendLayout();
            // 
            // menuStrip1
            // 
            this.menuStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.fileToolStripMenuItem});
            this.menuStrip1.Location = new System.Drawing.Point(0, 0);
            this.menuStrip1.Name = "menuStrip1";
            this.menuStrip1.Size = new System.Drawing.Size(898, 24);
            this.menuStrip1.TabIndex = 0;
            this.menuStrip1.Text = "menuStrip1";
            // 
            // fileToolStripMenuItem
            // 
            this.fileToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.openToolStripMenuItem});
            this.fileToolStripMenuItem.Name = "fileToolStripMenuItem";
            this.fileToolStripMenuItem.Size = new System.Drawing.Size(37, 20);
            this.fileToolStripMenuItem.Text = "&File";
            // 
            // openToolStripMenuItem
            // 
            this.openToolStripMenuItem.Name = "openToolStripMenuItem";
            this.openToolStripMenuItem.Size = new System.Drawing.Size(152, 22);
            this.openToolStripMenuItem.Text = "&Open";
            this.openToolStripMenuItem.Click += new System.EventHandler(this.openToolStripMenuItem_Click);
            // 
            // panel1
            // 
            this.panel1.Controls.Add(this.tbScale);
            this.panel1.Dock = System.Windows.Forms.DockStyle.Top;
            this.panel1.Location = new System.Drawing.Point(0, 24);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(898, 36);
            this.panel1.TabIndex = 1;
            // 
            // pnlHolder
            // 
            this.pnlHolder.AutoScroll = true;
            this.pnlHolder.Controls.Add(this.plnDrawing);
            this.pnlHolder.Dock = System.Windows.Forms.DockStyle.Fill;
            this.pnlHolder.Location = new System.Drawing.Point(0, 60);
            this.pnlHolder.Name = "pnlHolder";
            this.pnlHolder.Size = new System.Drawing.Size(898, 438);
            this.pnlHolder.TabIndex = 2;
            // 
            // plnDrawing
            // 
            this.plnDrawing.Location = new System.Drawing.Point(0, 0);
            this.plnDrawing.Name = "plnDrawing";
            this.plnDrawing.Size = new System.Drawing.Size(231, 157);
            this.plnDrawing.TabIndex = 0;
            // 
            // dgEvents
            // 
            this.dgEvents.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.dgEvents.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.dgEvents.Location = new System.Drawing.Point(0, 501);
            this.dgEvents.Name = "dgEvents";
            this.dgEvents.Size = new System.Drawing.Size(898, 101);
            this.dgEvents.TabIndex = 3;
            // 
            // splitter1
            // 
            this.splitter1.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.splitter1.Location = new System.Drawing.Point(0, 498);
            this.splitter1.Name = "splitter1";
            this.splitter1.Size = new System.Drawing.Size(898, 3);
            this.splitter1.TabIndex = 4;
            this.splitter1.TabStop = false;
            // 
            // tbScale
            // 
            this.tbScale.Dock = System.Windows.Forms.DockStyle.Right;
            this.tbScale.Location = new System.Drawing.Point(659, 0);
            this.tbScale.Maximum = 100;
            this.tbScale.Name = "tbScale";
            this.tbScale.Size = new System.Drawing.Size(239, 36);
            this.tbScale.TabIndex = 0;
            this.tbScale.TickStyle = System.Windows.Forms.TickStyle.None;
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(898, 602);
            this.Controls.Add(this.pnlHolder);
            this.Controls.Add(this.splitter1);
            this.Controls.Add(this.dgEvents);
            this.Controls.Add(this.panel1);
            this.Controls.Add(this.menuStrip1);
            this.MainMenuStrip = this.menuStrip1;
            this.Name = "Form1";
            this.Text = "Trace Visualization";
            this.menuStrip1.ResumeLayout(false);
            this.menuStrip1.PerformLayout();
            this.panel1.ResumeLayout(false);
            this.panel1.PerformLayout();
            this.pnlHolder.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.dgEvents)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.tbScale)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.MenuStrip menuStrip1;
        private System.Windows.Forms.ToolStripMenuItem fileToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem openToolStripMenuItem;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.Panel pnlHolder;
        private System.Windows.Forms.Panel plnDrawing;
        private System.Windows.Forms.TrackBar tbScale;
        private System.Windows.Forms.DataGridView dgEvents;
        private System.Windows.Forms.Splitter splitter1;
    }
}


namespace TraceVisualization
{
    partial class Visualization
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
            this.viewToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.scaleYToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem2 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem3 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem4 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem5 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem6 = new System.Windows.Forms.ToolStripMenuItem();
            this.panel1 = new System.Windows.Forms.Panel();
            this.tbScale = new System.Windows.Forms.TrackBar();
            this.pnlHolder = new System.Windows.Forms.Panel();
            this.pnlDrawing = new System.Windows.Forms.Panel();
            this.dgEvents = new System.Windows.Forms.DataGridView();
            this.EventColumn = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.splitter1 = new System.Windows.Forms.Splitter();
            this.chkStates = new System.Windows.Forms.CheckBox();
            this.chkMessages = new System.Windows.Forms.CheckBox();
            this.menuStrip1.SuspendLayout();
            this.panel1.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.tbScale)).BeginInit();
            this.pnlHolder.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.dgEvents)).BeginInit();
            this.SuspendLayout();
            // 
            // menuStrip1
            // 
            this.menuStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.fileToolStripMenuItem,
            this.viewToolStripMenuItem});
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
            this.openToolStripMenuItem.Size = new System.Drawing.Size(103, 22);
            this.openToolStripMenuItem.Text = "&Open";
            this.openToolStripMenuItem.Click += new System.EventHandler(this.openToolStripMenuItem_Click);
            // 
            // viewToolStripMenuItem
            // 
            this.viewToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.scaleYToolStripMenuItem});
            this.viewToolStripMenuItem.Name = "viewToolStripMenuItem";
            this.viewToolStripMenuItem.Size = new System.Drawing.Size(44, 20);
            this.viewToolStripMenuItem.Text = "&View";
            // 
            // scaleYToolStripMenuItem
            // 
            this.scaleYToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolStripMenuItem2,
            this.toolStripMenuItem3,
            this.toolStripMenuItem4,
            this.toolStripMenuItem5,
            this.toolStripMenuItem6});
            this.scaleYToolStripMenuItem.Name = "scaleYToolStripMenuItem";
            this.scaleYToolStripMenuItem.Size = new System.Drawing.Size(111, 22);
            this.scaleYToolStripMenuItem.Text = "Scale Y";
            // 
            // toolStripMenuItem2
            // 
            this.toolStripMenuItem2.Name = "toolStripMenuItem2";
            this.toolStripMenuItem2.Size = new System.Drawing.Size(86, 22);
            this.toolStripMenuItem2.Text = "1";
            this.toolStripMenuItem2.Click += new System.EventHandler(this.toolStripMenuItem2_Click);
            // 
            // toolStripMenuItem3
            // 
            this.toolStripMenuItem3.Name = "toolStripMenuItem3";
            this.toolStripMenuItem3.Size = new System.Drawing.Size(86, 22);
            this.toolStripMenuItem3.Text = "2";
            this.toolStripMenuItem3.Click += new System.EventHandler(this.toolStripMenuItem3_Click);
            // 
            // toolStripMenuItem4
            // 
            this.toolStripMenuItem4.Name = "toolStripMenuItem4";
            this.toolStripMenuItem4.Size = new System.Drawing.Size(86, 22);
            this.toolStripMenuItem4.Text = "4";
            this.toolStripMenuItem4.Click += new System.EventHandler(this.toolStripMenuItem4_Click);
            // 
            // toolStripMenuItem5
            // 
            this.toolStripMenuItem5.Name = "toolStripMenuItem5";
            this.toolStripMenuItem5.Size = new System.Drawing.Size(86, 22);
            this.toolStripMenuItem5.Text = "8";
            this.toolStripMenuItem5.Click += new System.EventHandler(this.toolStripMenuItem5_Click);
            // 
            // toolStripMenuItem6
            // 
            this.toolStripMenuItem6.Name = "toolStripMenuItem6";
            this.toolStripMenuItem6.Size = new System.Drawing.Size(86, 22);
            this.toolStripMenuItem6.Text = "16";
            this.toolStripMenuItem6.Click += new System.EventHandler(this.toolStripMenuItem6_Click);
            // 
            // panel1
            // 
            this.panel1.Controls.Add(this.chkMessages);
            this.panel1.Controls.Add(this.chkStates);
            this.panel1.Controls.Add(this.tbScale);
            this.panel1.Dock = System.Windows.Forms.DockStyle.Top;
            this.panel1.Location = new System.Drawing.Point(0, 24);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(898, 36);
            this.panel1.TabIndex = 1;
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
            this.tbScale.Value = 50;
            this.tbScale.ValueChanged += new System.EventHandler(this.tbScale_ValueChanged);
            // 
            // pnlHolder
            // 
            this.pnlHolder.AutoScroll = true;
            this.pnlHolder.Controls.Add(this.pnlDrawing);
            this.pnlHolder.Dock = System.Windows.Forms.DockStyle.Fill;
            this.pnlHolder.Location = new System.Drawing.Point(0, 60);
            this.pnlHolder.Name = "pnlHolder";
            this.pnlHolder.Size = new System.Drawing.Size(898, 216);
            this.pnlHolder.TabIndex = 2;
            // 
            // pnlDrawing
            // 
            this.pnlDrawing.BackColor = System.Drawing.Color.White;
            this.pnlDrawing.Location = new System.Drawing.Point(0, 0);
            this.pnlDrawing.Name = "pnlDrawing";
            this.pnlDrawing.Size = new System.Drawing.Size(231, 157);
            this.pnlDrawing.TabIndex = 0;
            this.pnlDrawing.Paint += new System.Windows.Forms.PaintEventHandler(this.pnlDrawing_Paint);
            this.pnlDrawing.MouseDoubleClick += new System.Windows.Forms.MouseEventHandler(this.pnlDrawing_MouseDoubleClick);
            this.pnlDrawing.MouseDown += new System.Windows.Forms.MouseEventHandler(this.pnlDrawing_MouseDown);
            this.pnlDrawing.MouseMove += new System.Windows.Forms.MouseEventHandler(this.pnlDrawing_MouseMove);
            this.pnlDrawing.MouseUp += new System.Windows.Forms.MouseEventHandler(this.pnlDrawing_MouseUp);
            // 
            // dgEvents
            // 
            this.dgEvents.AllowUserToAddRows = false;
            this.dgEvents.AllowUserToDeleteRows = false;
            this.dgEvents.AllowUserToOrderColumns = true;
            this.dgEvents.AllowUserToResizeRows = false;
            this.dgEvents.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.dgEvents.Columns.AddRange(new System.Windows.Forms.DataGridViewColumn[] {
            this.EventColumn});
            this.dgEvents.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.dgEvents.Location = new System.Drawing.Point(0, 279);
            this.dgEvents.Name = "dgEvents";
            this.dgEvents.RowHeadersVisible = false;
            this.dgEvents.Size = new System.Drawing.Size(898, 323);
            this.dgEvents.TabIndex = 3;
            this.dgEvents.VirtualMode = true;
            this.dgEvents.CellValueNeeded += new System.Windows.Forms.DataGridViewCellValueEventHandler(this.dgEvents_CellValueNeeded);
            this.dgEvents.SelectionChanged += new System.EventHandler(this.dgEvents_SelectionChanged);
            // 
            // EventColumn
            // 
            this.EventColumn.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill;
            this.EventColumn.HeaderText = "Event";
            this.EventColumn.MinimumWidth = 2000;
            this.EventColumn.Name = "EventColumn";
            this.EventColumn.ReadOnly = true;
            this.EventColumn.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable;
            this.EventColumn.ToolTipText = "Event";
            // 
            // splitter1
            // 
            this.splitter1.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.splitter1.Location = new System.Drawing.Point(0, 276);
            this.splitter1.Name = "splitter1";
            this.splitter1.Size = new System.Drawing.Size(898, 3);
            this.splitter1.TabIndex = 4;
            this.splitter1.TabStop = false;
            // 
            // chkStates
            // 
            this.chkStates.AutoSize = true;
            this.chkStates.Checked = true;
            this.chkStates.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chkStates.Location = new System.Drawing.Point(12, 3);
            this.chkStates.Name = "chkStates";
            this.chkStates.Size = new System.Drawing.Size(56, 17);
            this.chkStates.TabIndex = 1;
            this.chkStates.Text = "States";
            this.chkStates.UseVisualStyleBackColor = true;
            this.chkStates.CheckedChanged += new System.EventHandler(this.chkStates_CheckedChanged);
            // 
            // chkMessages
            // 
            this.chkMessages.AutoSize = true;
            this.chkMessages.Checked = true;
            this.chkMessages.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chkMessages.Location = new System.Drawing.Point(74, 3);
            this.chkMessages.Name = "chkMessages";
            this.chkMessages.Size = new System.Drawing.Size(74, 17);
            this.chkMessages.TabIndex = 1;
            this.chkMessages.Text = "Messages";
            this.chkMessages.UseVisualStyleBackColor = true;
            this.chkMessages.CheckedChanged += new System.EventHandler(this.chkMessages_CheckedChanged);
            // 
            // Visualization
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
            this.Name = "Visualization";
            this.Text = "Trace Visualization";
            this.menuStrip1.ResumeLayout(false);
            this.menuStrip1.PerformLayout();
            this.panel1.ResumeLayout(false);
            this.panel1.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.tbScale)).EndInit();
            this.pnlHolder.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.dgEvents)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.MenuStrip menuStrip1;
        private System.Windows.Forms.ToolStripMenuItem fileToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem openToolStripMenuItem;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.Panel pnlHolder;
        private System.Windows.Forms.Panel pnlDrawing;
        private System.Windows.Forms.TrackBar tbScale;
        private System.Windows.Forms.DataGridView dgEvents;
        private System.Windows.Forms.Splitter splitter1;
        private System.Windows.Forms.DataGridViewTextBoxColumn EventColumn;
        private System.Windows.Forms.ToolStripMenuItem viewToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem scaleYToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem2;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem3;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem4;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem5;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem6;
        private System.Windows.Forms.CheckBox chkMessages;
        private System.Windows.Forms.CheckBox chkStates;
    }
}
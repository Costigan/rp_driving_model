namespace SolutionPlotter
{
    partial class Plotter
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
            this.components = new System.ComponentModel.Container();
            this.menuStrip1 = new System.Windows.Forms.MenuStrip();
            this.fileToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.openToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.testToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.printPnlVariableSizeToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.zgc1 = new ZedGraph.ZedGraphControl();
            this.pnlSymbolsContainer = new System.Windows.Forms.GroupBox();
            this.pnlCases = new System.Windows.Forms.Panel();
            this.pnlVariablesContainer = new System.Windows.Forms.GroupBox();
            this.pnlVariables = new System.Windows.Forms.Panel();
            this.pnlConstantsContainer = new System.Windows.Forms.GroupBox();
            this.pnlConstants = new System.Windows.Forms.Panel();
            this.splitContainer1 = new System.Windows.Forms.SplitContainer();
            this.splitContainer2 = new System.Windows.Forms.SplitContainer();
            this.splitter1 = new System.Windows.Forms.Splitter();
            this.viewToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.drawLinesToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.menuStrip1.SuspendLayout();
            this.pnlSymbolsContainer.SuspendLayout();
            this.pnlVariablesContainer.SuspendLayout();
            this.pnlConstantsContainer.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer1)).BeginInit();
            this.splitContainer1.Panel1.SuspendLayout();
            this.splitContainer1.Panel2.SuspendLayout();
            this.splitContainer1.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer2)).BeginInit();
            this.splitContainer2.Panel1.SuspendLayout();
            this.splitContainer2.Panel2.SuspendLayout();
            this.splitContainer2.SuspendLayout();
            this.SuspendLayout();
            // 
            // menuStrip1
            // 
            this.menuStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.fileToolStripMenuItem,
            this.testToolStripMenuItem,
            this.viewToolStripMenuItem});
            this.menuStrip1.Location = new System.Drawing.Point(0, 0);
            this.menuStrip1.Name = "menuStrip1";
            this.menuStrip1.Size = new System.Drawing.Size(878, 24);
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
            // testToolStripMenuItem
            // 
            this.testToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.printPnlVariableSizeToolStripMenuItem});
            this.testToolStripMenuItem.Name = "testToolStripMenuItem";
            this.testToolStripMenuItem.Size = new System.Drawing.Size(41, 20);
            this.testToolStripMenuItem.Text = "&Test";
            // 
            // printPnlVariableSizeToolStripMenuItem
            // 
            this.printPnlVariableSizeToolStripMenuItem.Name = "printPnlVariableSizeToolStripMenuItem";
            this.printPnlVariableSizeToolStripMenuItem.Size = new System.Drawing.Size(184, 22);
            this.printPnlVariableSizeToolStripMenuItem.Text = "Print pnlVariable Size";
            this.printPnlVariableSizeToolStripMenuItem.Click += new System.EventHandler(this.printPnlVariableSizeToolStripMenuItem_Click);
            // 
            // zgc1
            // 
            this.zgc1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.zgc1.Location = new System.Drawing.Point(153, 24);
            this.zgc1.Name = "zgc1";
            this.zgc1.ScrollGrace = 0D;
            this.zgc1.ScrollMaxX = 0D;
            this.zgc1.ScrollMaxY = 0D;
            this.zgc1.ScrollMaxY2 = 0D;
            this.zgc1.ScrollMinX = 0D;
            this.zgc1.ScrollMinY = 0D;
            this.zgc1.ScrollMinY2 = 0D;
            this.zgc1.Size = new System.Drawing.Size(725, 515);
            this.zgc1.TabIndex = 4;
            // 
            // pnlSymbolsContainer
            // 
            this.pnlSymbolsContainer.Controls.Add(this.pnlCases);
            this.pnlSymbolsContainer.Dock = System.Windows.Forms.DockStyle.Fill;
            this.pnlSymbolsContainer.Location = new System.Drawing.Point(0, 0);
            this.pnlSymbolsContainer.Name = "pnlSymbolsContainer";
            this.pnlSymbolsContainer.Size = new System.Drawing.Size(150, 163);
            this.pnlSymbolsContainer.TabIndex = 3;
            this.pnlSymbolsContainer.TabStop = false;
            this.pnlSymbolsContainer.Text = "Design Cases";
            // 
            // pnlCases
            // 
            this.pnlCases.AutoScroll = true;
            this.pnlCases.AutoSize = true;
            this.pnlCases.Dock = System.Windows.Forms.DockStyle.Fill;
            this.pnlCases.Location = new System.Drawing.Point(3, 16);
            this.pnlCases.Name = "pnlCases";
            this.pnlCases.Size = new System.Drawing.Size(144, 144);
            this.pnlCases.TabIndex = 0;
            // 
            // pnlVariablesContainer
            // 
            this.pnlVariablesContainer.Controls.Add(this.pnlVariables);
            this.pnlVariablesContainer.Dock = System.Windows.Forms.DockStyle.Fill;
            this.pnlVariablesContainer.Location = new System.Drawing.Point(0, 0);
            this.pnlVariablesContainer.Name = "pnlVariablesContainer";
            this.pnlVariablesContainer.Size = new System.Drawing.Size(150, 203);
            this.pnlVariablesContainer.TabIndex = 8;
            this.pnlVariablesContainer.TabStop = false;
            this.pnlVariablesContainer.Text = "Variables";
            // 
            // pnlVariables
            // 
            this.pnlVariables.AutoScroll = true;
            this.pnlVariables.AutoSize = true;
            this.pnlVariables.Dock = System.Windows.Forms.DockStyle.Fill;
            this.pnlVariables.Location = new System.Drawing.Point(3, 16);
            this.pnlVariables.Name = "pnlVariables";
            this.pnlVariables.Size = new System.Drawing.Size(144, 184);
            this.pnlVariables.TabIndex = 1;
            // 
            // pnlConstantsContainer
            // 
            this.pnlConstantsContainer.Controls.Add(this.pnlConstants);
            this.pnlConstantsContainer.Dock = System.Windows.Forms.DockStyle.Fill;
            this.pnlConstantsContainer.Location = new System.Drawing.Point(0, 0);
            this.pnlConstantsContainer.Name = "pnlConstantsContainer";
            this.pnlConstantsContainer.Size = new System.Drawing.Size(150, 141);
            this.pnlConstantsContainer.TabIndex = 9;
            this.pnlConstantsContainer.TabStop = false;
            this.pnlConstantsContainer.Text = "Constants";
            // 
            // pnlConstants
            // 
            this.pnlConstants.AutoScroll = true;
            this.pnlConstants.AutoSize = true;
            this.pnlConstants.Dock = System.Windows.Forms.DockStyle.Fill;
            this.pnlConstants.Location = new System.Drawing.Point(3, 16);
            this.pnlConstants.Name = "pnlConstants";
            this.pnlConstants.Size = new System.Drawing.Size(144, 122);
            this.pnlConstants.TabIndex = 0;
            // 
            // splitContainer1
            // 
            this.splitContainer1.Dock = System.Windows.Forms.DockStyle.Left;
            this.splitContainer1.Location = new System.Drawing.Point(0, 24);
            this.splitContainer1.Name = "splitContainer1";
            this.splitContainer1.Orientation = System.Windows.Forms.Orientation.Horizontal;
            // 
            // splitContainer1.Panel1
            // 
            this.splitContainer1.Panel1.Controls.Add(this.pnlVariablesContainer);
            // 
            // splitContainer1.Panel2
            // 
            this.splitContainer1.Panel2.Controls.Add(this.splitContainer2);
            this.splitContainer1.Size = new System.Drawing.Size(150, 515);
            this.splitContainer1.SplitterDistance = 203;
            this.splitContainer1.TabIndex = 10;
            // 
            // splitContainer2
            // 
            this.splitContainer2.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainer2.Location = new System.Drawing.Point(0, 0);
            this.splitContainer2.Name = "splitContainer2";
            this.splitContainer2.Orientation = System.Windows.Forms.Orientation.Horizontal;
            // 
            // splitContainer2.Panel1
            // 
            this.splitContainer2.Panel1.Controls.Add(this.pnlConstantsContainer);
            // 
            // splitContainer2.Panel2
            // 
            this.splitContainer2.Panel2.Controls.Add(this.pnlSymbolsContainer);
            this.splitContainer2.Size = new System.Drawing.Size(150, 308);
            this.splitContainer2.SplitterDistance = 141;
            this.splitContainer2.TabIndex = 0;
            // 
            // splitter1
            // 
            this.splitter1.Location = new System.Drawing.Point(150, 24);
            this.splitter1.Name = "splitter1";
            this.splitter1.Size = new System.Drawing.Size(3, 515);
            this.splitter1.TabIndex = 11;
            this.splitter1.TabStop = false;
            // 
            // viewToolStripMenuItem
            // 
            this.viewToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.drawLinesToolStripMenuItem});
            this.viewToolStripMenuItem.Name = "viewToolStripMenuItem";
            this.viewToolStripMenuItem.Size = new System.Drawing.Size(44, 20);
            this.viewToolStripMenuItem.Text = "&View";
            // 
            // drawLinesToolStripMenuItem
            // 
            this.drawLinesToolStripMenuItem.Name = "drawLinesToolStripMenuItem";
            this.drawLinesToolStripMenuItem.Size = new System.Drawing.Size(152, 22);
            this.drawLinesToolStripMenuItem.Text = "Draw Lines";
            this.drawLinesToolStripMenuItem.Click += new System.EventHandler(this.drawLinesToolStripMenuItem_Click);
            // 
            // Plotter
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(878, 539);
            this.Controls.Add(this.zgc1);
            this.Controls.Add(this.splitter1);
            this.Controls.Add(this.splitContainer1);
            this.Controls.Add(this.menuStrip1);
            this.MainMenuStrip = this.menuStrip1;
            this.Name = "Plotter";
            this.Text = "Form1";
            this.menuStrip1.ResumeLayout(false);
            this.menuStrip1.PerformLayout();
            this.pnlSymbolsContainer.ResumeLayout(false);
            this.pnlSymbolsContainer.PerformLayout();
            this.pnlVariablesContainer.ResumeLayout(false);
            this.pnlVariablesContainer.PerformLayout();
            this.pnlConstantsContainer.ResumeLayout(false);
            this.pnlConstantsContainer.PerformLayout();
            this.splitContainer1.Panel1.ResumeLayout(false);
            this.splitContainer1.Panel2.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer1)).EndInit();
            this.splitContainer1.ResumeLayout(false);
            this.splitContainer2.Panel1.ResumeLayout(false);
            this.splitContainer2.Panel2.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer2)).EndInit();
            this.splitContainer2.ResumeLayout(false);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.MenuStrip menuStrip1;
        private System.Windows.Forms.ToolStripMenuItem fileToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem openToolStripMenuItem;
        private ZedGraph.ZedGraphControl zgc1;
        private System.Windows.Forms.GroupBox pnlSymbolsContainer;
        private System.Windows.Forms.Panel pnlCases;
        private System.Windows.Forms.ToolStripMenuItem testToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem printPnlVariableSizeToolStripMenuItem;
        private System.Windows.Forms.GroupBox pnlVariablesContainer;
        private System.Windows.Forms.Panel pnlVariables;
        private System.Windows.Forms.GroupBox pnlConstantsContainer;
        private System.Windows.Forms.Panel pnlConstants;
        private System.Windows.Forms.SplitContainer splitContainer1;
        private System.Windows.Forms.SplitContainer splitContainer2;
        private System.Windows.Forms.Splitter splitter1;
        private System.Windows.Forms.ToolStripMenuItem viewToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem drawLinesToolStripMenuItem;
    }
}


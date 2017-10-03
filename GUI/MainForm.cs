using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace GUI
{
    public partial class MainForm : Form
    {
        Semaphore s;
        FileSystemWatcher sceneWatcher, dllWatcher;
        public MainForm()
        {
            InitializeComponent();
            s = new Semaphore(1, 1);
        }

        protected override void OnLoad(EventArgs e)
        {
            base.OnLoad(e);

            // Create a new FileSystemWatcher and set its properties.
            (dllWatcher = CreateWatcher(@"..\..\..\bin\Debug\netcoreapp2.0")).Filter = "FuncTracer.dll";
            sceneWatcher = CreateWatcher(@"..\..\..");

            tbPath.TextChanged += TbPath_TextChanged;

            tbPath.Text = Path.GetFullPath(Path.Combine(@"..\..\..", "sample.scene"));

            RunFuncTracer();
        }

        private void TbPath_TextChanged(object sender, EventArgs e)
        {
            sceneWatcher.EnableRaisingEvents = false;
            sceneWatcher.Path = Path.GetDirectoryName(tbPath.Text);
            sceneWatcher.Filter = Path.GetFileName(tbPath.Text);
            sceneWatcher.EnableRaisingEvents = true;
        }

        private FileSystemWatcher CreateWatcher(string path)
        {
            FileSystemWatcher watcher = new FileSystemWatcher();
            watcher.Path = path;

            watcher.NotifyFilter = NotifyFilters.LastWrite;

            // Add event handlers.
            watcher.Changed += new FileSystemEventHandler(OnChanged);
            watcher.Created += new FileSystemEventHandler(OnChanged);
            
            // Begin watching.
            watcher.EnableRaisingEvents = true;

            return watcher;
        }

        void OnChanged(object source, FileSystemEventArgs e)
        {
            Invoke(new CrossAppDomainDelegate(RunFuncTracer));
        }

        private void bRun_Click(object sender, EventArgs e)
        {
            RunFuncTracer();
        }

        private void RunFuncTracer()
        {
            if (s.WaitOne(0))
            {
                textBox1.Text = "Running FuncTracer";

                var path = Path.GetFullPath(tbPath.Text);

                ThreadPool.QueueUserWorkItem(delegate (object state)
                {
                    try
                    {
                        string messages;

                        var data = FuncTracerWrapper.Run(path, out messages);

                        Invoke(new CrossAppDomainDelegate(() => textBox1.Text = messages));

                        var image = Image.FromStream(data);

                        Invoke(new CrossAppDomainDelegate(() => pictureBox1.Image = image));
                    }
                    catch (Exception ex)
                    {
                        Invoke(new CrossAppDomainDelegate(() => textBox1.Text = "Error! " + ex.Message + System.Environment.NewLine + textBox1.Text));
                    }
                    finally { s.Release(); };
                });
            }
            //else textBox1.Text += System.Environment.NewLine + "FuncTracer already running, additional invocation aborted.";
        }

        private void bChooseFile_Click(object sender, EventArgs e)
        {
            openFileDialog.InitialDirectory = Path.GetFullPath(@"..\..\..");
            openFileDialog.Filter = "Scene files|*.scene";
            openFileDialog.FileName = null;
            if (openFileDialog.ShowDialog() == DialogResult.OK)
            {
                tbPath.Text = openFileDialog.FileName;
                RunFuncTracer();
            }
        }
    }
}

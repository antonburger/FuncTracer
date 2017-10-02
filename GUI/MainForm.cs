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
        public MainForm()
        {
            InitializeComponent();
            s = new Semaphore(1, 1);
        }

        protected override void OnLoad(EventArgs e)
        {
            base.OnLoad(e);

            // Create a new FileSystemWatcher and set its properties.
            FileSystemWatcher watcher = new FileSystemWatcher();
            watcher.Path = @"..\..\..\bin\Debug\netcoreapp2.0";
            /* Watch for changes in LastAccess and LastWrite times, and
               the renaming of files or directories. */
            watcher.NotifyFilter = NotifyFilters.LastAccess | NotifyFilters.LastWrite
               | NotifyFilters.FileName | NotifyFilters.DirectoryName;
            // Only watch text files.
            watcher.Filter = "*.dll";

            // Add event handlers.
            watcher.Changed += new FileSystemEventHandler(OnChanged);
            watcher.Created += new FileSystemEventHandler(OnChanged);
            watcher.Deleted += new FileSystemEventHandler(OnChanged);

            // Begin watching.
            watcher.EnableRaisingEvents = true;
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

                ThreadPool.QueueUserWorkItem(delegate (object state)
                {
                    try
                    {
                        string messages;

                        var data = FuncTracerWrapper.Run(out messages);
                        var image = Image.FromStream(data);

                        Invoke(new CrossAppDomainDelegate(delegate ()
                        {
                            textBox1.Text = messages;
                            pictureBox1.Image = image;
                        }));
                    }
                    catch (Exception ex)
                    {
                        Invoke(new CrossAppDomainDelegate(() => textBox1.Text = ex.Message));
                    }
                    finally { s.Release(); };
                });
            }
            else textBox1.Text += System.Environment.NewLine + "FuncTracer already running, additional invocation aborted.";
        }
    }
}

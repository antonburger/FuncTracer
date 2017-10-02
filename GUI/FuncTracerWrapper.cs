using System;
using System.Diagnostics;
using System.IO;
using System.Text;
using System.Threading;

namespace GUI
{
    internal class FuncTracerWrapper : MarshalByRefObject
    {
        public FuncTracerWrapper()
        {
        }

        public static MemoryStream Run(out string messages)
        {
            var info = new ProcessStartInfo
            {
                FileName = "dotnet",
                Arguments = "run sample.scene",
                WorkingDirectory = @"..\..\..",
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                CreateNoWindow = true,
                WindowStyle = ProcessWindowStyle.Hidden
            };

            var process = Process.Start(info);

            string msgs = null;
            ThreadPool.QueueUserWorkItem((obj) => msgs = process.StandardError.ReadToEnd());

            var ms = new MemoryStream();
            using (process.StandardOutput.BaseStream)
                process.StandardOutput.BaseStream.CopyTo(ms);

            process.WaitForExit();

            messages = msgs;

            return ms;
        }
    }
}
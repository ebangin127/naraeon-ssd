import shlex
import subprocess
class ProcessRunner:
    def run(self, args):
        shellargs = shlex.split(args)
        process = subprocess.Popen(shellargs, shell=False, stdout=subprocess.PIPE)
        output, err = process.communicate()
        return output
    def asyncrun(self, args):
        shellargs = shlex.split(args)
        process = subprocess.Popen(shellargs, shell=False, stdout=subprocess.PIPE)
        return process
    def isProcessEnd(self, process):
        return process.poll() != None;

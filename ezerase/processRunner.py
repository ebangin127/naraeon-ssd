import shlex
import subprocess
import json
class ProcessRunner:
    def run(self, args):
        shellargs = shlex.split(args)
        process = subprocess.Popen(shellargs, shell=False, stdout=subprocess.PIPE)
        output, err = process.communicate()
        return output

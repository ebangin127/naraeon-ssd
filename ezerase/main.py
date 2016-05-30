import processRunner
import fixedStorageFinder
import driveSelector
import json
runner = processRunner.ProcessRunner()
jsonoutput = runner.run('lsblk -Jo model,vendor,serial,size,kname,type')
runresult = json.loads(jsonoutput.decode())
finder = fixedStorageFinder.FixedStorageFinder()
findresult = finder.find(runresult)
selector = driveSelector.DriveSelector()
selector.select(findresult)
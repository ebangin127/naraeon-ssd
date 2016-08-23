import processRunner
import fixedStorageFinder
import selectorView
import json
import nvmeRefiner
class Selector:
    def select(self):
        runner = processRunner.ProcessRunner()
        jsonoutput = runner.run('lsblk -Jo model,vendor,serial,size,kname,type')
        runresult = json.loads(jsonoutput.decode())
        finder = fixedStorageFinder.FixedStorageFinder()
        findresult = finder.find(runresult)
        refiner = nvmeRefiner.NVMeRefiner()
        findresult = refiner.refine(findresult)
        selector = selectorView.SelectorView()
        selector.select(findresult)
        return selector.selected(), findresult
import processRunner
class NVMeRefiner:
    def __getModel(self, nvmeresult):
        return nvmeresult[4][10:]
    def __getSerial(self, nvmeresult):
        return nvmeresult[5][10:]
    def __getModelSerial(self, nvmeresult):
        return (self.__getModel(nvmeresult), self.__getSerial(nvmeresult))
    def __runNVMe(self, kname):
        runner = processRunner.ProcessRunner()
        nvmeresult = runner.run('nvme id-ctrl /dev/' + kname + ' -H').decode('utf-8')
        nvmeresult = nvmeresult.split('\n')
        model, serial = self.__getModelSerial(nvmeresult)
        model = model.strip()
        serial = serial.strip()
        return (model, serial)
    def refine(self, findresult):
        fixedstorages = list()
        for device in findresult:
            if device['kname'].find('nvme') != -1:
                device['model'], device['serial'] = self.__runNVMe(device['kname'])
                fixedstorages.append(device)
            else:
                fixedstorages.append(device)
        return findresult
